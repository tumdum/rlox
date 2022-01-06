use crate::allocator::Allocator;
use crate::chunk::InvalidOpCode;
use crate::compiler::Parser;
use crate::value::{Function, NativeFunction, Obj, Value};
use crate::{Chunk, OpCode};
use fxhash::FxHashMap;
use std::cell::RefCell;
use std::collections::hash_map::Entry::*;
use std::io::Write;
use std::path::Path;
use std::rc::Rc;
use thiserror::Error;

const MAX_CALL_STACK_DEPTH: usize = 1000;

macro_rules! binary_op {
    ($target: expr, $op: tt) => {{
        let b_val = $target.pop()?;
        let a_val = $target.pop()?;
        let b = match &b_val {
            Value::Number(b) => b,
            other => return Err($target.new_runtime_error(RuntimeProblem::InvalidOperands(a_val, stringify!($op).to_owned(), b_val))),
        };
        let a = match &a_val {
            Value::Number(a) => a,
            other => return Err($target.new_runtime_error(RuntimeProblem::InvalidOperands(a_val, stringify!($op).to_owned(), b_val))),
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
}

#[derive(Debug, Error)]
#[error("Runtime error: {problem}")]
pub struct RuntimeError {
    callstack: Vec<String>,
    problem: RuntimeProblem,
}

impl RuntimeError {
    pub fn print_callstack(&self, mut output: impl Write) {
        writeln!(output, "Callstack:");
        for (i, frame) in self.callstack.iter().enumerate() {
            writeln!(output, "{:>3}: {}", i, frame);
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
    function: Value,
    pc: usize,
    slots_offset: usize,
}

impl CallFrame {
    fn new(function: Value, slots_offset: usize) -> Self {
        Self {
            function,
            slots_offset,
            pc: 0,
        }
    }

    fn function_mut(&mut self) -> &mut Function {
        self.function.function_mut().unwrap()
    }
    fn function(&self) -> &Function {
        self.function.function().unwrap()
    }
}

pub struct VM {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: FxHashMap<String, Value>,
    allocator: Rc<RefCell<Allocator>>,
    output: Rc<RefCell<dyn std::io::Write>>,
}

impl VM {
    pub fn new(output: Rc<RefCell<dyn std::io::Write>>) -> Self {
        Self {
            stack: Default::default(),
            frames: Default::default(),
            globals: FxHashMap::default(),
            allocator: Rc::new(RefCell::new(Allocator::default())),
            output,
        }
    }

    fn current_callstack(&self) -> Vec<String> {
        self.frames
            .iter()
            .map(|frame| (frame.function.function().unwrap(), frame.pc))
            .map(|(fun, pc)| {
                format!(
                    "{:?}, line {}",
                    fun,
                    fun.line(pc)
                        .map(|i| i.to_string())
                        .unwrap_or("unknown".to_owned())
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

    pub fn register_bulitins(&mut self) {
        self.define_native("time", move |args| {
            match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
                Ok(t) => t.as_secs_f64().into(),
                Err(_) => Value::Nil,
            }
        });
        let output_clone = self.output.clone();
        self.define_native("println", move |args| {
            for (i, arg) in args.into_iter().enumerate() {
                if i == 0 {
                    write!(output_clone.borrow_mut(), "{}", arg);
                } else {
                    write!(output_clone.borrow_mut(), " {}", arg);
                }
            }
            writeln!(output_clone.borrow_mut());
            Value::Nil
        });
    }

    fn read_byte(&mut self) -> u8 {
        let ret = self.current_frame().function().chunk.code[self.current_frame().pc];
        self.current_frame_mut().pc += 1;
        ret
    }

    fn read_opcode(&mut self) -> Result<OpCode, InvalidOpCode> {
        self.read_byte().try_into()
    }

    fn read_constant(&mut self) -> &Value {
        let constant_index = self.read_byte() as usize;
        &self.current_frame().function().chunk.constants[constant_index]
    }

    fn read_u16(&mut self) -> u16 {
        let pc = self.current_frame().pc;
        let chunk: &Chunk = &self.current_frame().function().chunk;
        let ret = ((chunk.code[pc] as u16) << 8) | (chunk.code[pc + 1]) as u16;
        self.current_frame_mut().pc += 2;
        ret
    }

    fn trace(&self) {
        println!("     stack: {:?}", self.stack);
        println!("   globals: {:?}", self.globals);
        println!("    frames: {:?}", self.frames);
        self.current_frame()
            .function()
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
        match callee.function() {
            Some(fun) => {
                if arg_count as usize != fun.arity {
                    return Err(
                        self.new_runtime_error(RuntimeProblem::InvalidNumberOfArguments {
                            expected: fun.arity,
                            passed: arg_count as usize,
                        }),
                    );
                }
                self.call(callee, arg_count)
            }
            None => {
                if let Value::NativeFunction(nf) = callee {
                    let l = self.stack.len();
                    let ret = (nf.function)(&self.stack[l - arg_count as usize..]);
                    self.stack.drain((l - arg_count as usize - 1)..);
                    self.stack.push(ret);
                    Ok(())
                } else {
                    Err(self.new_runtime_error(RuntimeProblem::CallToNonCallableValue(callee)))
                }
            }
        }
    }

    fn define_native(&mut self, name: &str, f: impl Fn(&[Value]) -> Value + 'static) {
        self.globals.insert(
            name.to_owned(),
            Value::NativeFunction(NativeFunction {
                name: name.to_owned(),
                function: Rc::new(f),
            }),
        );
    }

    fn call(&mut self, function: Value, arg_count: u8) -> Result<(), Error> {
        if self.frames.len() > MAX_CALL_STACK_DEPTH {
            return Err(self.new_runtime_error(RuntimeProblem::MaxCallStackDepthReached));
        }
        self.frames.push(CallFrame::new(function, self.stack.len() - arg_count as usize));

        Ok(())
    }

    pub fn run(&mut self) -> Result<(), Error> {
        loop {
            #[cfg(debug_assertions)]
            self.trace();

            match self.read_opcode()? {
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
                }
                OpCode::Return => {
                    let result = self.pop()?;
                    let frame = self.frames.pop();
                    if self.frames.is_empty() {
                        return Ok(());
                    }
                    self.stack.drain((frame.unwrap().slots_offset - 1)..);
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
                            match (l, r) {
                                (self::Obj::String(l), self::Obj::String(r)) => {
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
                        other => {
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
                OpCode::Nil => self.push(Value::Nil),
                OpCode::True => self.push(true),
                OpCode::False => self.push(false),
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::GetLocal => {
                    let slot = self.read_byte() as usize;
                    let slot_offset = self.current_frame().slots_offset;
                    self.push(self.stack[slot_offset + slot].clone());
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte() as usize;
                    let slot_offset = self.current_frame().slots_offset;
                    self.stack[slot_offset + slot] = self.stack.last().unwrap().clone();
                }
                OpCode::GetGlobal => {
                    let name = self.read_constant().string().unwrap().to_owned(); // TODO
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
                    let name = self.read_constant().string().unwrap().to_owned(); // TODO
                    let value = self.stack.last().unwrap().clone();
                    self.globals.insert(name, value);
                    self.pop()?;
                }
                OpCode::SetGlobal => {
                    let name = self.read_constant().string().unwrap().to_owned(); // TODO
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
                OpCode::Equal => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(a == b);
                }
            }
        }
    }

    pub fn repl(&mut self) -> Result<(), Error> {
        loop {
            print!("> ");
            std::io::stdout().flush()?;
            let mut line = String::new();
            std::io::stdin().read_line(&mut line)?;

            match self.interpret(&line.trim()) {
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

    fn interpret(&mut self, source: &str) -> Result<(), Error> {
        let parser = Parser::new(source, self.allocator.clone());
        let function = if let Some(function) = parser.compile() {
            function
        } else {
            return Err(Error::CompilationFailed);
        };

        self.push(function.clone());
        self.frames.push(CallFrame::new(function, self.stack.len() - 1));
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
            let mut vm = VM::new(output.clone());
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
    fn functions() {
        test_eval!(
            "fun areWeThereYet() { print 1; } print areWeThereYet;",
            "<fn areWeThereYet>"
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
        let output = Rc::new(RefCell::new(vec![]));
        let output_clone = output.clone();
        let mut vm = VM::new(output.clone());
        vm.define_native("println", move |args| {
            writeln!(output_clone.borrow_mut(), "{:?}", args);
            Value::Nil
        });
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
        assert_eq!(
                output,
                "[Number(1), Nil, Obj(String(\"test\"))]\n[Obj(Function(<fn x@0>)), Number(13), Number(4)]\n[Number(2)]\n[Number(6)]\n[Number(24)]\n[Number(120)]\n[Number(120)]"
            );
    }

    #[test]
    fn callstack() {
        let output = Rc::new(RefCell::new(vec![]));
        let output_clone = output.clone();
        let mut vm = VM::new(output.clone());
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
}
