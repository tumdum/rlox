use crate::allocator::Allocator;
use crate::chunk::InvalidOpCode;
use crate::compiler::Parser;
use crate::value::{self, Callable, Class, Closure, NativeMethod, ObjInner, ObjString, Value};
use crate::{Chunk, OpCode};
use arrayvec::ArrayVec;
use fxhash::FxHashMap;
use std::cell::RefCell;
use std::collections::hash_map::Entry::*;
use std::io::Write;
use std::path::Path;
use std::rc::Rc;
use thiserror::Error;

mod builtins;

#[cfg(test)]
#[cfg(not(debug_assertions))]
mod aoc_tests;

const MAX_CALL_STACK_DEPTH: usize = 1000;
const MAX_STACK_SIZE: usize = 256;
pub const INITIALIZER_NAME: &str = "init";

macro_rules! binary_op {
    ($target: expr, $op: tt) => {{
        let b_val = $target.pop()?;
        let a_val = $target.pop()?;
        let b = match &b_val {
            Value::Number(b) => b,
            _ => return Err($target.new_runtime_error(RuntimeProblem::InvalidOperands(a_val, stringify!($op).to_owned(), b_val))),
        };
        let a = match &a_val {
            Value::Number(a) => a,
            _ => return Err($target.new_runtime_error(RuntimeProblem::InvalidOperands(a_val, stringify!($op).to_owned(), b_val))),
        };

        let result = a $op b;
        $target.push(result);
    }}
}

#[derive(Debug, Error)]
pub enum RuntimeProblem {
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("Invalid operand, expected {0}, got {1:?}")]
    InvalidOperand(String, Value),
    #[error("Invalid operands for '{1}': '{0}' and '{2}'")]
    InvalidOperands(Value, String, Value),
    #[error("Pop from empty stack")]
    PopFromEmptyStack,
    #[error("Undefined global variable '{0}'")]
    UndefinedGlobalVariable(String),
    #[error("Tried to call non callable value: {0}")]
    CallToNonCallableValue(Value),
    #[error(
        "Tried to call with invalid number of arguments. Expected {expected}, passed {passed}"
    )]
    InvalidNumberOfArguments { expected: usize, passed: usize },
    #[error("Maximum call stack depth reached {MAX_CALL_STACK_DEPTH}")]
    MaxCallStackDepthReached,
    #[error("Undefined property '{0}'")]
    UndefinedProperty(String),
    #[error("Only instances have fields, tried access fields of {0}")]
    InvalidPropertyAccess(String),
    #[error("Tried to call method '{1}' on {0} which doesn't have it")]
    InvalidMethodAccess(String, String),
    #[error("Class {0} which has no initializer called with {1} parameters")]
    DefaultInitializerWithParameters(String, usize),
    #[error("Value operation failed: {0}")]
    ValueError(#[from] value::Error),
    #[error("Superclass must be a class. Subclass {0} inherits form {1}")]
    SuperclassMustBeAClass(Value, Value),
}

#[derive(Debug, Error)]
#[error("Runtime error: {problem}")]
pub struct RuntimeError {
    callstack: Vec<String>,
    problem: RuntimeProblem,
}

impl RuntimeError {
    pub fn print_callstack(&self, mut output: impl Write) {
        writeln!(output, "Callstack:").unwrap();
        for (i, frame) in self.callstack.iter().enumerate() {
            writeln!(output, "{:>3}: {}", i, frame).unwrap();
        }
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Run time error: {0}")]
    InvalidOpCode(#[from] InvalidOpCode),
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("Compilation failed")]
    CompilationFailed,
    #[error("{0}")]
    RuntimeError(#[from] RuntimeError),
}

#[derive(Debug)]
struct CallFrame {
    closure: Value,
    pc: usize,
    slots_offset: usize,
}

impl CallFrame {
    fn new(closure: Value, slots_offset: usize) -> Self {
        Self {
            closure,
            slots_offset,
            pc: 0,
        }
    }

    fn function(&self) -> &Closure {
        self.closure.closure().unwrap()
    }
}

pub struct VM {
    stack: ArrayVec<Value, MAX_STACK_SIZE>,
    frames: ArrayVec<CallFrame, MAX_CALL_STACK_DEPTH>,
    open_upvalues: Vec<Value>,
    globals: FxHashMap<String, Value>,
    allocator: Rc<RefCell<Allocator>>,
    output: Rc<RefCell<dyn std::io::Write>>,
    input: Rc<RefCell<dyn std::io::BufRead>>,
    current_parser: Option<Parser>,
}

impl Drop for VM {
    fn drop(&mut self) {
        self.collect_garbage(true);
    }
}

impl VM {
    pub fn new(
        output: Rc<RefCell<dyn std::io::Write>>,
        input: Rc<RefCell<dyn std::io::BufRead>>,
    ) -> Self {
        Self {
            stack: ArrayVec::<Value, MAX_STACK_SIZE>::default(),
            frames: ArrayVec::<CallFrame, MAX_CALL_STACK_DEPTH>::default(),
            open_upvalues: vec![],
            globals: FxHashMap::default(),
            allocator: Rc::new(RefCell::new(Allocator::default())),
            output,
            input,
            current_parser: None,
        }
    }

    fn current_callstack(&self) -> Vec<String> {
        self.frames
            .iter()
            .map(|frame| (frame.function().function.function().unwrap(), frame.pc))
            .map(|(fun, pc)| {
                format!(
                    "{:?}, line {}",
                    fun,
                    fun.line(pc)
                        .map(|i| i.to_string())
                        .unwrap_or_else(|| "unknown".to_owned())
                )
            })
            .rev()
            .collect()
    }

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn read_byte(&mut self) -> u8 {
        let ret = self
            .current_frame()
            .function()
            .function
            .function()
            .unwrap()
            .chunk
            .code[self.current_frame().pc];
        self.current_frame_mut().pc += 1;
        ret
    }

    fn read_opcode(&mut self) -> Result<OpCode, InvalidOpCode> {
        self.read_byte().try_into()
    }

    fn read_constant(&mut self) -> &Value {
        let constant_index = self.read_byte() as usize;
        &self
            .current_frame()
            .function()
            .function
            .function()
            .unwrap()
            .chunk
            .constants[constant_index]
    }

    fn read_string(&mut self) -> &str {
        self.read_constant().string().unwrap()
    }

    fn read_u16(&mut self) -> u16 {
        let pc = self.current_frame().pc;
        let chunk: &Chunk = &self
            .current_frame()
            .function()
            .function
            .function()
            .unwrap()
            .chunk;
        let ret = ((chunk.code[pc] as u16) << 8) | (chunk.code[pc + 1]) as u16;
        self.current_frame_mut().pc += 2;
        ret
    }

    #[cfg(not(feature = "trace"))]
    fn trace(&self) {}
    #[cfg(feature = "trace")]
    fn trace(&self) {
        println!("     stack: {:?}", self.stack.len());
        for v in &self.stack {
            println!("            {:?}", v);
        }
        // println!("   globals: {:?}", self.globals);
        println!("    frames: {:?}", self.frames);
        self.current_frame()
            .function()
            .function
            .function()
            .unwrap()
            .chunk
            .disassemble_instruction(self.current_frame().pc);
        println!();
    }

    fn push(&mut self, v: impl Into<Value>) {
        let v = v.into();
        self.stack.push(v);
    }

    fn new_runtime_error(&self, problem: RuntimeProblem) -> Error {
        Error::RuntimeError(RuntimeError {
            callstack: self.current_callstack(),
            problem,
        })
    }

    fn pop(&mut self) -> Result<Value, Error> {
        self.stack
            .pop()
            .ok_or_else(|| self.new_runtime_error(RuntimeProblem::PopFromEmptyStack))
    }

    fn peek(&self, offset: usize) -> Result<Value, Error> {
        self.stack
            .get(self.stack.len() - 1 - offset)
            .cloned()
            .ok_or_else(|| self.new_runtime_error(RuntimeProblem::PopFromEmptyStack))
    }

    fn call_value(&mut self, callee: Value, arg_count: u8) -> Result<(), Error> {
        match callee.callable() {
            Some(Callable::Function(_fun)) => {
                unreachable!()
            }
            Some(Callable::Native(nf)) => {
                let l = self.stack.len();
                let ret = (nf.function)(&self.stack[l - arg_count as usize..]);
                self.stack.drain((l - arg_count as usize - 1)..);
                self.stack.push(ret);
                Ok(())
            }
            Some(Callable::Closure(closure)) => {
                if arg_count as usize != closure.function.function().unwrap().arity {
                    return Err(
                        self.new_runtime_error(RuntimeProblem::InvalidNumberOfArguments {
                            expected: closure.function.function().unwrap().arity,
                            passed: arg_count as usize,
                        }),
                    );
                }
                self.call(callee, arg_count)
            }
            Some(Callable::Class(class)) => {
                let instance = self
                    .allocator
                    .borrow_mut()
                    .allocate_obj_instance(callee.clone());
                let l = self.stack.len();
                self.stack[l - arg_count as usize - 1] = instance;
                if let Some(initializer) = class.get_method(INITIALIZER_NAME) {
                    return self.call_value(initializer.clone(), arg_count);
                } else if arg_count != 0 {
                    return Err(self.new_runtime_error(
                        RuntimeProblem::DefaultInitializerWithParameters(
                            class.name.clone(),
                            arg_count as usize,
                        ),
                    ));
                }
                Ok(())
            }
            Some(Callable::BoundMethod(bm)) => {
                let l = self.stack.len();
                self.stack[l - arg_count as usize - 1] = bm.receiver.clone();
                self.call(bm.method.clone(), arg_count)
            }
            None => Err(self.new_runtime_error(RuntimeProblem::CallToNonCallableValue(callee))),
        }
    }

    fn invoke(&mut self, method: String, arg_count: usize) -> Result<(), Error> {
        let receiver = self.peek(arg_count)?;

        if let Some(native_method) = receiver.get_native_method(&method) {
            return self.call_native_method(native_method, receiver, arg_count as u8);
        }

        let instance = match receiver.instance() {
            Some(instance) => instance,
            None => {
                return Err(self.new_runtime_error(RuntimeProblem::InvalidMethodAccess(
                    receiver.type_name().to_owned(),
                    method,
                )))
            }
        };

        if let Some(value) = instance.get_field(&method) {
            let l = self.stack.len();
            self.stack[l - arg_count as usize - 1] = value.clone();
            return self.call_value(value.clone(), arg_count as u8);
        }

        self.invoke_from_class(instance.class.class().unwrap(), method, arg_count)
    }

    fn invoke_from_class(
        &mut self,
        class: &Class,
        method: String,
        arg_count: usize,
    ) -> Result<(), Error> {
        match class.get_method(&method) {
            Some(method) => self.call(method.clone(), arg_count as u8),
            None => Err(self.new_runtime_error(RuntimeProblem::UndefinedProperty(method))),
        }
    }

    fn bind_method(&mut self, class: &Value, name: &str) -> Result<(), Error> {
        let method = match class.class().unwrap().get_method(name) {
            Some(method) => method.clone(),
            None => {
                return Err(
                    self.new_runtime_error(RuntimeProblem::UndefinedProperty(name.to_owned()))
                )
            }
        };

        let receiver = self.peek(0)?;

        let bound = self
            .allocator
            .borrow_mut()
            .allocate_bound_method(receiver, method);

        self.pop()?;
        self.push(bound);
        Ok(())
    }

    fn capture_upvalue(&mut self, stack_index: usize) -> Value {
        let local: *mut Value = &mut self.stack[stack_index];
        for value in &self.open_upvalues {
            let upvalue = value.upvalue().unwrap();
            if upvalue.location == local {
                return value.clone();
            }
        }
        let created_upvalue = self.allocator.borrow_mut().allocate_upvalue(local);
        self.open_upvalues.push(created_upvalue.clone());
        created_upvalue
    }

    fn close_upvalues(&mut self, last: *mut Value) {
        // TODO: drain?
        let mut tmp = vec![];
        for v in &mut self.open_upvalues {
            let mut upvalue = v.upvalue_mut().unwrap();
            if upvalue.location >= last {
                upvalue.closed = Some((unsafe { &*upvalue.location }).clone());
                upvalue.location = upvalue.closed.as_mut().unwrap();
            } else {
                tmp.push(v.clone());
            }
        }
        self.open_upvalues = tmp;
    }

    fn define_method(&mut self, method_name: &str) -> Result<(), Error> {
        let method = self.peek(0)?;
        let mut class = self.peek(1)?;
        let class = class.class_mut().unwrap();
        class.add_method(method_name, method);
        self.pop()?;

        Ok(())
    }

    fn call(&mut self, function: Value, arg_count: u8) -> Result<(), Error> {
        if self.frames.len() > MAX_CALL_STACK_DEPTH {
            return Err(self.new_runtime_error(RuntimeProblem::MaxCallStackDepthReached));
        }
        self.frames.push(CallFrame::new(
            function,
            self.stack.len() - arg_count as usize,
        ));

        Ok(())
    }
    fn call_native_method(
        &mut self,
        method: NativeMethod,
        mut receiver: Value,
        arg_count: u8,
    ) -> Result<(), Error> {
        let l = self.stack.len();
        let ret = (method)(
            &self.globals,
            &mut self.allocator.borrow_mut(),
            &mut receiver,
            &self.stack[l - arg_count as usize..],
        );
        self.stack.drain((l - arg_count as usize - 1)..);
        self.stack.push(ret);
        Ok(())
    }

    pub fn run(&mut self) -> Result<(), Error> {
        loop {
            #[cfg(debug_assertions)]
            self.trace();

            #[cfg(any(debug_assert, feature = "always_gc"))]
            self.collect_garbage(true);

            match self.read_opcode()? {
                OpCode::GetUpValue => {
                    let slot = self.read_byte() as usize;
                    let value: *mut Value =
                        self.current_frame().closure.closure().unwrap().upvalues[slot]
                            .upvalue()
                            .unwrap()
                            .location;
                    let value: Value = unsafe { (&*value).clone() };
                    self.push(value);
                }
                OpCode::SetUpValue => {
                    let slot = self.read_byte() as usize;
                    let new_value = self.peek(0)?;
                    let value: *mut Value =
                        self.current_frame().closure.closure().unwrap().upvalues[slot]
                            .upvalue()
                            .unwrap()
                            .location;
                    unsafe { *value = new_value };
                }
                OpCode::Jump => {
                    let offset = self.read_u16();
                    self.current_frame_mut().pc += offset as usize;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_u16();
                    if self.stack.last().unwrap().is_falsey() {
                        self.current_frame_mut().pc += offset as usize;
                    }
                }
                OpCode::Loop => {
                    let offset = self.read_u16();
                    self.current_frame_mut().pc -= offset as usize;
                }
                OpCode::Call => {
                    let arg_count = self.read_byte();
                    self.call_value(self.peek(arg_count as usize)?, arg_count)?;
                    self.collect_garbage(false);
                }
                OpCode::CloseUpValue => {
                    let last: *mut Value = self.stack.last_mut().unwrap();
                    self.close_upvalues(last);
                    self.pop()?;
                }
                OpCode::Return => {
                    let result = self.pop()?;
                    let frame = self.frames.pop();
                    if self.frames.is_empty() {
                        return Ok(());
                    }
                    let frame = frame.unwrap();
                    let frame_stack_start = frame.slots_offset - 1;
                    // debug_assert!(self.stack[frame_stack_start].callable().is_some());
                    let returning_closure: *mut Value = &mut self.stack[frame_stack_start];
                    self.close_upvalues(returning_closure);
                    self.stack.drain(frame_stack_start..);
                    self.push(result);
                }
                OpCode::Print => {
                    let value = self.pop()?;
                    writeln!(self.output.borrow_mut(), "{}", value)?; // TODO: this is a runtime error!
                }
                OpCode::Greater => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(a > b);
                }
                OpCode::Less => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(a < b);
                }
                OpCode::Add => {
                    let b = self.pop()?;
                    let a = self.pop()?;

                    use Value::*;
                    let result: Value = match (&a, &b) {
                        (Number(l), Number(r)) => (l + r).into(),
                        (Obj(l), Obj(r)) => unsafe {
                            let l = &**l;
                            let r = &**r;
                            match (&l.inner, &r.inner) {
                                (ObjInner::String(l), ObjInner::String(r)) => {
                                    let tmp = format!("{}{}", l, r);
                                    self.allocator.borrow_mut().allocate_string(tmp)
                                }
                                (_, _) => {
                                    return Err(self.new_runtime_error(
                                        RuntimeProblem::InvalidOperands(
                                            a.clone(),
                                            "+".to_owned(),
                                            b.clone(),
                                        ),
                                    ))
                                }
                            }
                        },
                        _ => {
                            return Err(self.new_runtime_error(RuntimeProblem::InvalidOperands(
                                a.clone(),
                                "+".to_owned(),
                                b.clone(),
                            )))
                        }
                    };
                    self.push(result);
                    self.collect_garbage(false);
                }
                OpCode::Subtract => binary_op!(self, -),
                OpCode::Multiply => binary_op!(self, *),
                OpCode::Divide => binary_op!(self, /),
                OpCode::Not => {
                    let val = self.pop()?;
                    self.push(val.is_falsey());
                }
                OpCode::Negate => {
                    let val = self.pop()?;
                    match &val {
                        Value::Number(n) => self.push(-n),
                        _ => {
                            return Err(self.new_runtime_error(RuntimeProblem::InvalidOperand(
                                "-".to_owned(),
                                val,
                            )))
                        }
                    }
                }
                OpCode::Constant => {
                    let constant = self.read_constant().clone();
                    self.push(constant);
                }
                OpCode::Invoke => {
                    let method = self.read_string().to_owned();
                    let arg_count = self.read_byte() as usize;
                    self.invoke(method, arg_count)?;
                }
                OpCode::SuperInvoke => {
                    let method = self.read_string().to_owned();
                    let arg_count = self.read_byte() as usize;
                    let superclass = self.pop()?;
                    let superclass = superclass.class().unwrap();
                    self.invoke_from_class(superclass, method, arg_count)?;
                }
                OpCode::Closure => {
                    let constant = self.read_constant().clone();
                    let mut closure = self.allocator.borrow_mut().allocate_closure(constant);
                    let upvalue_count = closure
                        .closure()
                        .unwrap()
                        .function
                        .function()
                        .unwrap()
                        .upvalue_count;
                    for _ in 0..upvalue_count {
                        let is_local = self.read_byte() == 1;
                        let index = self.read_byte() as usize;
                        if is_local {
                            let slot_offset = self.current_frame().slots_offset - 1;
                            let upvalue = self.capture_upvalue(slot_offset + index);
                            closure.closure_mut().unwrap().upvalues.push(upvalue);
                        } else {
                            let upvalue = self.current_frame().closure.closure().unwrap().upvalues
                                [index]
                                .clone();
                            closure.closure_mut().unwrap().upvalues.push(upvalue);
                        }
                        debug_assert!(closure.closure().unwrap().upvalues.len() <= upvalue_count);
                    }
                    debug_assert!(closure.closure().unwrap().upvalues.len() == upvalue_count);

                    self.push(closure);
                    self.collect_garbage(false);
                }
                OpCode::Nil => self.push(Value::Nil),
                OpCode::True => self.push(true),
                OpCode::False => self.push(false),
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::GetLocal => {
                    let slot = self.read_byte() as usize;
                    let slot_offset = self.current_frame().slots_offset;
                    self.push(self.stack[slot_offset + slot - 1].clone());
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte() as usize;
                    let slot_offset = self.current_frame().slots_offset;
                    self.stack[slot_offset + slot - 1] = self.stack.last().unwrap().clone();
                }
                OpCode::GetGlobal => {
                    let name = self.read_string().to_owned();
                    match self.globals.entry(name) {
                        Occupied(e) => {
                            let value = e.get().clone();
                            self.push(value);
                        }
                        Vacant(e) => {
                            let key = e.key().to_owned();
                            return Err(self
                                .new_runtime_error(RuntimeProblem::UndefinedGlobalVariable(key)));
                        }
                    }
                }
                OpCode::DefineGlobal => {
                    let name = self.read_string().to_owned();
                    let value = self.stack.last().unwrap().clone();
                    self.globals.insert(name, value);
                    self.pop()?;
                }
                OpCode::SetGlobal => {
                    let name = self.read_string().to_owned();
                    let value = self.stack.last().unwrap().clone();
                    match self.globals.entry(name) {
                        Occupied(mut e) => {
                            e.insert(value);
                        }
                        Vacant(e) => {
                            let key = e.key().to_owned();
                            return Err(self
                                .new_runtime_error(RuntimeProblem::UndefinedGlobalVariable(key)));
                        }
                    }
                }
                OpCode::GetProperty => {
                    let instance = self.peek(0)?;
                    let instance = match instance.instance() {
                        Some(i) => i,
                        None => {
                            return Err(self.new_runtime_error(
                                RuntimeProblem::InvalidPropertyAccess(
                                    instance.type_name().to_owned(),
                                ),
                            ))
                        }
                    };
                    let name = self.read_string().to_owned();
                    match instance.get_field(&name) {
                        Some(v) => {
                            self.pop()?;
                            self.push(v.clone());
                        }
                        None => {
                            self.bind_method(&instance.class, &name)?;
                        }
                    }
                }
                OpCode::SetProperty => {
                    let value = self.pop()?;
                    let mut instance = self.pop()?;
                    let instance = match instance.instance_mut() {
                        Some(i) => i,
                        None => {
                            return Err(self.new_runtime_error(
                                RuntimeProblem::InvalidPropertyAccess(
                                    instance.type_name().to_owned(),
                                ),
                            ))
                        }
                    };
                    let field = self.read_string();
                    instance.set_field(field.to_owned(), value.clone());
                    self.push(value);
                }
                OpCode::GetSuper => {
                    let name = self.read_string().to_owned();
                    let superclass = self.pop()?;
                    self.bind_method(&superclass, &name)?;
                }
                OpCode::Equal => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(a == b);
                }
                OpCode::Class => {
                    let class_name: ObjString = self.read_constant().string().unwrap().clone();
                    let class = self
                        .allocator
                        .borrow_mut()
                        .allocate_class((*class_name).to_owned());
                    self.push(class);
                    self.collect_garbage(false);
                }
                OpCode::Inherit => {
                    let superclass = self.peek(1)?;
                    let mut subclass = self.peek(0)?;
                    match superclass.class() {
                        Some(superclass) => {
                            superclass.copy_methods_to(subclass.class_mut().unwrap())
                        }
                        None => {
                            return Err(self.new_runtime_error(
                                RuntimeProblem::SuperclassMustBeAClass(subclass, superclass),
                            ))
                        }
                    }
                    self.pop()?; // subclass
                }
                OpCode::Method => {
                    let method = self.read_string().to_owned();
                    self.define_method(&method)?;
                }
            }
        }
    }

    fn collect_garbage(&mut self, force: bool) {
        #[cfg(not(feature = "always_gc"))]
        if !force && !self.allocator.borrow().should_gc() {
            return;
        }
        #[cfg(feature = "trace")]
        println!("== GC START == ");
        self.mark_roots();
        self.sweep();
        #[cfg(feature = "trace")]
        println!("==  GC END  == ");
    }

    fn mark_roots(&mut self) {
        self.stack.iter_mut().for_each(|v| v.mark());
        self.frames.iter_mut().for_each(|v| v.closure.mark());
        self.open_upvalues.iter_mut().for_each(|v| v.mark());
        self.globals.values_mut().for_each(|v| v.mark());

        self.mark_compiler_roots();
    }

    fn mark_compiler_roots(&mut self) {
        self.current_parser
            .as_mut()
            .iter_mut()
            .for_each(|p| p.mark());
    }

    fn sweep(&mut self) {
        self.allocator.borrow_mut().sweep();
    }

    fn clear_stack(&mut self) {
        self.stack.clear();
    }

    pub fn repl(&mut self) -> Result<(), Error> {
        loop {
            self.clear_stack();
            print!("> ");
            std::io::stdout().flush()?;
            let mut line = String::new();
            std::io::stdin().read_line(&mut line)?;

            match self.interpret(line.trim()) {
                Ok(_) => {}
                Err(Error::RuntimeError(e)) => {
                    println!("{}", e);
                    e.print_callstack(&mut *self.output.borrow_mut());
                }
                Err(Error::CompilationFailed) => {}
                other => return other,
            }
        }
    }

    pub fn run_file(&mut self, path: &Path) -> Result<(), Error> {
        let source = std::fs::read_to_string(path)?;
        self.interpret(&source)?;
        Ok(())
    }

    pub fn load_prelude(&mut self) -> Result<(), Error> {
        self.interpret(include_str!("./prelude.lox"))
    }

    fn interpret(&mut self, source: &str) -> Result<(), Error> {
        self.current_parser = Some(Parser::new(source, self.allocator.clone()));
        let function = if let Some(function) = self.current_parser.as_mut().unwrap().compile() {
            function
        } else {
            return Err(Error::CompilationFailed);
        };

        self.push(function.clone());
        let closure = self.allocator.borrow_mut().allocate_closure(function);
        self.pop()?;
        self.push(closure.clone());
        self.frames.push(CallFrame::new(closure, self.stack.len()));
        self.run()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;

    macro_rules! test_eval {
        ($input: expr, $expected: expr) => {{
            let output = Rc::new(RefCell::new(vec![]));
            let input: &[u8] = &[];
            let input = Rc::new(RefCell::new(input));
            let mut vm = VM::new(output.clone(), input);
            vm.load_prelude().unwrap();
            vm.register_bulitins();
            let got = vm.interpret(&format!("{}", $input));
            assert_matches!(got, Ok(_));
            let output = std::str::from_utf8(&output.borrow()).unwrap().to_owned();
            let output = output.trim();
            assert_eq!(
                output,
                format!("{}", $expected),
                "for expression: {}",
                stringify!($input)
            );
        }};
    }

    #[test]
    fn literals() {
        test_eval!("print 1;", Value::Number(1f64));
        test_eval!("print 1.1;", Value::Number(1.1f64));
        test_eval!("print true;", Value::Boolean(true));
        test_eval!("print false;", Value::Boolean(false));
        test_eval!("print nil;", Value::Nil);
    }

    #[test]
    fn unary() {
        test_eval!("print -13.109;", Value::Number(-13.109f64));
        test_eval!("print -0;", Value::Number(-0f64));
        test_eval!("print !true;", Value::Boolean(false));
        test_eval!("print !!true;", Value::Boolean(true));
        test_eval!("print !false;", Value::Boolean(true));
        test_eval!("print !!false;", Value::Boolean(false));
        test_eval!("print !nil;", Value::Boolean(true));
        test_eval!("print !!nil;", Value::Boolean(false));
    }

    #[test]
    fn arithmetic() {
        test_eval!("print 1+2;", Value::Number(3.0));
        test_eval!("print 1-2;", Value::Number(-1.0));
        test_eval!("print 3*2;", Value::Number(6.0));
        test_eval!("print 9.3/3;", Value::Number(3.1));
        test_eval!("print 2*3+5;", Value::Number(11.0));
        test_eval!("print -2*(3+5);", Value::Number(-16.0));
    }

    #[test]
    fn binary() {
        test_eval!("print nil!=nil;", Value::Boolean(false));
        test_eval!("print nil<nil;", Value::Boolean(false));
        test_eval!("print nil==nil;", Value::Boolean(true));
        test_eval!("print 1!=1;", Value::Boolean(false));
        test_eval!("print 1==1;", Value::Boolean(true));
        test_eval!(r#"print "test"=="test";"#, Value::Boolean(true));
        test_eval!(r#"print "test"<"test";"#, Value::Boolean(false));
        test_eval!(r#"print "test"!="test";"#, Value::Boolean(false));
        test_eval!(r#"print "test"+"1"=="test1";"#, Value::Boolean(true));
        test_eval!("print 1+2==3;", Value::Boolean(true));
        test_eval!("print true!=false;", Value::Boolean(true));
        test_eval!("print true<false;", Value::Boolean(false));
        test_eval!("print true==false;", Value::Boolean(false));
        test_eval!("print 2<3;", Value::Boolean(true));
        test_eval!("print 2<=3;", Value::Boolean(true));
        test_eval!("print 2>3;", Value::Boolean(false));
        test_eval!("print 2>=3;", Value::Boolean(false));
    }

    #[test]
    fn globals() {
        test_eval!("var x = 1; var y = 2; print x + y;", Value::Number(3.0));
        test_eval!("var x = \"abc\"; var y = \"ABC\"; print x + y;", "abcABC");
        test_eval!(
            r#"var breakfast = "beignets"; var beverage = "cafe au lait"; breakfast = "beignets with " + beverage; print breakfast;"#,
            "beignets with cafe au lait"
        );
    }

    #[test]
    fn locals() {
        test_eval!("{var x = 123; print x;}", Value::Number(123.0));
        test_eval!("{var x = 1; var y = 2; print x + y;}", Value::Number(3.0));
        test_eval!("{var x = 1; {var y = 2; print x + y;}}", Value::Number(3.0));
        test_eval!(
            "{var x = 1; {var y = 2; {print x + y;}}}",
            Value::Number(3.0)
        );
        test_eval!(
            r#"
        var x = 0;
        {
            var x = 1;
            {
                var x = 2;
                {
                    var x = 3;
                    print x;
                }
                print x;
            }
            print x;
        }
        print x;
        "#,
            "3\n2\n1\n0"
        );
        test_eval!(
            r#"
        {
            var x = 1;
            {
                x = 2;
                {
                    x = 3;
                    print x;
                }
                print x;
            }
            print x;
        }
        "#,
            "3\n3\n3"
        );
    }

    #[test]
    fn and_or() {
        test_eval!("print 1>2 and nil;", "false");
        test_eval!("print nil and 1>2;", "nil");
        test_eval!("print 3>2 and 2>1 and 1>0;", "true");
        test_eval!("print 3>2 and 2>1 and 1>0 and false;", "false");

        test_eval!("print 1>2 or nil;", "nil");
        test_eval!("print nil or 1>2;", "false");
        test_eval!("print 3>2 or 2>1 or 1>0;", "true");
        test_eval!("print 3>2 or 2>1 or 1>0 or false;", "true");
    }

    #[test]
    fn r#while() {
        let fib = r#"
var target = 14;
var a = 0;
var b = 1;
var i = 0;

while (i < target) {
    i = i + 1;
    var tmp = b;
    b = a + b;
    a = tmp;
}
print a;
        "#;
        test_eval!(fib, "377");
    }

    #[test]
    fn r#for() {
        let fib = r#"
print 0;
print 1;
var prev = 0;
var current = 1;
for (var a = 1; a < 14; a = a + 1) {
    var tmp = prev;
    prev = current;
    current = current + tmp;
    print current;
}
        "#;
        test_eval!(
            fib,
            "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233\n377"
        );
        test_eval!("for(;false;) { print 1; }", "");
        test_eval!("var x = 1; for(;x < 2;x = x + 1) { print x; }", "1");
        test_eval!("var x = 1; for(;x < 2;) { print x; x = x + 1; }", "1");
    }

    #[test]
    fn lox_functions() {
        test_eval!(
            "fun areWeThereYet(a,b,c) { print 1; } print areWeThereYet;",
            "<fn areWeThereYet@3>"
        );
        test_eval!("fun x(a) { print a; } x(1);", "1");
        test_eval!("fun x(a,b,c) { print a + b + c; } x(1,100,10000);", "10101");
        test_eval!(
            r#"var a = 999.3; fun x(a) { print a; } x("test"); print a;"#,
            "test\n999.3"
        );
        test_eval!(
            r#"var a = 999.3; fun x(a,b,c) { print a + b + c; } x("1","100","10000"); print a;"#,
            "110010000\n999.3"
        );
        test_eval!(r#"fun x(a) { print a; } x("test"); x("foo");"#, "test\nfoo");
        test_eval!(
            r#"fun x(a) { return 100 * a; } print x(1); print x(13);"#,
            "100\n1300"
        );
        test_eval!(
            r#"fun x() { return 100; } fun y() { print x() + x(); } y();"#,
            "200"
        );
        test_eval!("fun x(a) { a = a - 10; print a; } x(100);", "90");
        test_eval!("fun x(a) { return 10*a; } fun y(a) { return 100*a; } fun z(a) { return x(a+1) + y(a+3); } print z(5);", "860");
        test_eval!(
            r#"
fun fib(n) {
    if (n == 1) {
        return 0;
    }
    if (n == 2) {
        return 1;
    }
    return fib(n-1) + fib(n-2);
}

for (var a = 1; a < 15; a = a + 1) {
    print fib(a);
}
            "#,
            "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233"
        );
        test_eval!(
            r#"
fun fact(n) {
    if (n <= 1) { return 1; }
    return n * fact(n-1);
}
print fact(11);"#,
            "39916800"
        );
    }
    #[test]
    fn native_functions() {
        // TODO: use registered builtins
        let output = Rc::new(RefCell::new(vec![]));
        let input = Rc::new(RefCell::new([].as_slice()));
        let mut vm = VM::new(output.clone(), input);
        vm.load_prelude().unwrap();
        vm.register_bulitins();
        let got = vm.interpret(
            r#"
println(1,nil,"test");
fun x() { return 13; }
println(x, x(), 4);
fun fact(n) {
    if (n <= 1) { return 1; }
    var tmp = n * fact(n-1);
    println(tmp);
    return tmp;
}
println(fact(5));
"#,
        );
        assert_matches!(got, Ok(_));
        let output = std::str::from_utf8(&output.borrow()).unwrap().to_owned();
        let output = output.trim();
        assert_eq!(output, "1 nil test\n<fn x@0> 13 4\n2\n6\n24\n120\n120");
    }

    #[test]
    fn callstack() {
        let output = Rc::new(RefCell::new(vec![]));
        let input = Rc::new(RefCell::new([].as_slice()));
        let mut vm = VM::new(output, input);
        let got = vm.interpret(
            r#"fun a() {
    b();
}
fun b() {
    c();
}
fun c() {
    d();
    e();
}
fun d() {
    print "hello!";
}
fun e() {
    e(1,2);
}

a();
            "#,
        );
        let err = got.unwrap_err();
        let expected_callstack: Vec<String> = vec![
            "<fn e@0>, line 15".to_owned(),
            "<fn c@0>, line 9".to_owned(),
            "<fn b@0>, line 5".to_owned(),
            "<fn a@0>, line 2".to_owned(),
            "<fn @0>, line 18".to_owned(),
        ];
        assert_matches!(err, Error::RuntimeError(RuntimeError{callstack, ..}) if callstack == expected_callstack);
    }

    #[test]
    fn closure_captures_local_and_doesnt_outlive_it() {
        test_eval!(
            r#"
fun outer() {
    var x = "outside";
    fun inner() {
        print x;
    }
    inner();
}
outer();
            "#,
            "outside"
        );
    }

    #[test]
    fn closure_captures_variables_not_values() {
        test_eval!(
            r#"
var globalSet;
var globalGet;

fun main() {
  var a = "initial";

  fun set() { a = "updated"; }
  fun get() { print a; }

  globalSet = set;
  globalGet = get;
}

main();
globalGet();
globalSet();
globalGet();
            "#,
            "initial\nupdated"
        );

        test_eval!(
            r#"
var globalOne;
var globalTwo;

fun main() {
    {
        var a = "one";
        fun one() {
            print a;
        }
        globalOne = one;
    }

    {
        var a = "two";
        fun two() {
            print a;
        }
        globalTwo = two;
    }
}

main();
globalOne();
globalTwo();
        "#,
            "one\ntwo"
        );
        test_eval!(
            r#"
var globalOne;
var globalTwo;

fun main() {
  for (var a = 1; a <= 2; a = a + 1) {
    fun closure() {
      print a;
    }
    if (globalOne == nil) {
      globalOne = closure;
    } else {
      globalTwo = closure;
    }
  }
}

main();
globalOne();
globalTwo();
        "#,
            "3\n3"
        );
        test_eval!(
            r#"
            fun counter(start, step) {
                fun aux() {
                    println(start);
                    start = start + step;
                }
                return aux;
            }
            var a = counter(10,3);
            var b = counter(1,7);
            a();
            b();
            a();
            b();
            a();
            b();
            "#,
            "10\n1\n13\n8\n16\n15"
        );
    }

    #[test]
    fn classes() {
        test_eval!("class Foo {} print Foo;", "class Foo");
        test_eval!("class Foo {} print Foo();", "class Foo instance");
        test_eval!(
            r#"
        class Toast {}
        var toast = Toast();
        print toast.jam = "grape";"#,
            "grape"
        );
        test_eval!(
            r#"
        class Pair {}

        var pair = Pair();
        pair.first = 1;
        pair.second = 2;
        print pair.first + pair.second;"#,
            "3"
        );
    }

    #[test]
    fn methods() {
        test_eval!(
            r#"
class Brunch {
    eggs() {}
}

var brunch = Brunch();
var eggs = brunch.eggs;
println(eggs);"#,
            "<fn Brunch::eggs@0>"
        );
        test_eval!(
            r#"
class Scone {
      topping(first, second) {
              print "scone with " + first + " and " + second;
                }
}

var scone = Scone();
scone.topping("berries", "cream");
        "#,
            "scone with berries and cream"
        );
        test_eval!(
            r#"
class Nested {
    method() {
        fun function() {
            print this;
        }

        function();
    }
}

Nested().method();"#,
            "class Nested instance"
        );
        test_eval!(
            r#"
        class Person {
          sayName() {
            print this.name;
          }
          setName(n) {
            this.name = n;
          }
        }

       var jane = Person();
       jane.name = "Jane";

       var method = jane.sayName;
       method();
       var setter = jane.setName;
       setter("Foo");
       method();
       jane.name = "Bar";
       method();
       jane = Person();
       method();
       setter("Quux");
       method();
       "#,
            "Jane\nFoo\nBar\nBar\nQuux"
        );
    }

    #[test]
    fn initializers() {
        test_eval!(
            r#"
        class Point {
            init(x, y) {
                this.x = x;
                this.y = y;
            }

            dist() {
                return this.x + this.y;
            }
        }
        var p = Point(1,100);
        print p.dist();
        "#,
            "101"
        );
    }

    #[test]
    fn full_object() {
        test_eval!(
            r#"
class CoffeeMaker {
  init(coffee) {
    this.coffee = coffee;
  }

  brew() {
    print "Enjoy your cup of " + this.coffee;

    // No reusing the grounds!
    this.coffee = nil;
  }
}

var maker = CoffeeMaker("coffee and chicory");
maker.brew();"#,
            "Enjoy your cup of coffee and chicory"
        );
    }

    #[test]
    fn invoking_field() {
        test_eval!(
            r#"
class Oops {
    init() {
        fun f() {
            print "not a method";
        }

        this.field = f;
    }
}

var oops = Oops();
oops.field();
        "#,
            "not a method"
        );
    }

    #[test]
    fn inherit() {
        test_eval!(
            r#"
class Doughnut {
    cook() {
        print "Dunk in the fryer.";
    }
    finish() {
        print "Burn!";
    }
}

class Cruller < Doughnut {
    finish() {
        print "Glaze with icing.";
    }
}
var x = Cruller();
x.cook();
x.finish();
            "#,
            "Dunk in the fryer.\nGlaze with icing."
        );
    }

    #[test]
    fn super_value() {
        test_eval!(
            r#"
class A {
    method() {
        print "A method";
    }
}

class B < A {
    method() {
        print "B method";
    }

    test() {
        super.method();
    }
}

class C < B {}

C().test();
            "#,
            "A method"
        );
        test_eval!(
            r#"
class A {
    method() {
        print "A";
    }
}

class B < A {
    method() {
        var closure = super.method;
        closure();
    }
}

var x = B();
x.method();
        "#,
            "A"
        );
        test_eval!(
            r#"
class Doughnut {
  cook() {
    print "Dunk in the fryer.";
    this.finish("sprinkles");
  }

  finish(ingredient) {
    print "Finish with " + ingredient;
  }
}

class Cruller < Doughnut {
  finish(ingredient) {
    // No sprinkles, always icing.
    super.finish("icing");
  }
}
var x = Cruller();
x.finish("test");
"#,
            "Finish with icing"
        );
    }

    #[test]
    fn vector() {
        test_eval!("println(vec(1,nil,\"test\",vec()));", "[1, nil, test, []]");
        test_eval!("var x = vec(); println(x);", "[]");
        test_eval!("println(vec().len());", "0");
        test_eval!("println(vec(1,nil,3,true).len());", "4");
        test_eval!(
            "var x = vec(1,nil,true); println(x.get(0), x.get(1), x.get(2));",
            "1 nil true"
        );
        test_eval!("var x = vec(123,4); println(x.get(0));", "123");
        test_eval!(
            "var x = vec(123,4); println(x.get(0)); println(x[1], x[0], x[1]);",
            "123\n4 123 4"
        );
        test_eval!("class Foo {} var x = Foo(); x.y = Foo(); x.y.z = vec(1,2); println(x.y.z[0], x.y.z[1]);", "1 2");
        test_eval!("class Foo {} var x = vec(Foo(), Foo()); x[0].y = 1; x[1].z = 2; println(x[1].z, x[0].y);", "2 1");
        test_eval!(
            "var x = vec(1, 2); x[0] = 42; x[1] = 1337; println(x);",
            "[42, 1337]"
        );
        test_eval!("class Foo {} var x = Foo(); x.y = Foo(); x.y.z = vec(3,4); x.y.z[0] = 1; x.y.z[1] = 2; println(x.y.z[0], x.y.z[1]);", "1 2");
        test_eval!(
            "var x = vec(vec(1, 2), vec(3,4)); x[0][0] = 100; println(x);",
            "[[100, 2], [3, 4]]"
        );
    }

    #[test]
    fn string() {
        test_eval!("println(\"foo\".len());", "3");
    }

    #[test]
    fn for_in_loop() {
        test_eval!(r#"
        {
            var x = vec(1,2,3);
            var collection_tmp = x;
            var iter = collection_tmp.iter();
            for (var i = iter.next(); i != nil; i = iter.next()) {
                println(i);
            }
        }"#, "1\n2\n3");
        test_eval!(r#"
        var x = vec(1,2);
        for (i in x) { 
            println(i); 
        }"#, "1\n2");
        test_eval!(r#"
        var x = vec(1,2,3);
        for (i in x) { 
            println(i); 
        }
        for (i in x) { 
            println(i+7); 
        }
        "#, "1\n2\n3\n8\n9\n10");
        test_eval!(r#"
        var x = vec(1,2,3);
        for (i in x) {
            for (j in x) {
                println(i*j);
            }
        }
        "#, "1\n2\n3\n2\n4\n6\n3\n6\n9");
        test_eval!(r#"
        var x = vec(1,2,3,4);
        var y = vec(5,6);
        for (i in x) {
            for (j in y) {
                println(i*j);
            }
        }"#, "5\n6\n10\n12\n15\n18\n20\n24");
    }
}
