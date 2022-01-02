use crate::allocator::Allocator;
use crate::chunk::InvalidOpCode;
use crate::compiler::Parser;
use crate::value::{Obj, Value};
use crate::{Chunk, OpCode};
use std::cell::RefCell;
use std::collections::hash_map::Entry::*;
use std::collections::HashMap;
use std::io::Write;
use std::path::Path;
use std::rc::Rc;
use thiserror::Error;

macro_rules! binary_op {
    ($target: expr, $op: tt) => {{
        let b = match $target.pop()? {
            Value::Number(b) => b,
            other => return Err(Error::InvalidOperand("Number".to_owned(), other)),
        };
        let a = match $target.pop()? {
            Value::Number(a) => a,
            other => return Err(Error::InvalidOperand("Number".to_owned(), other)),
        };

        let result = a $op b;
        $target.push(result);
    }}
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Run time error: {0}")]
    InvalidOpCode(#[from] InvalidOpCode),
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("Invalid operand, expected {0}, got {1:?}")]
    InvalidOperand(String, Value),
    #[error("Invalid operands for '{1}': {0:?} {2:?}")]
    InvalidOperands(Value, String, Value),
    #[error("Compilation failed")]
    CompilationFailed,
    #[error("Pop from empty stack")]
    PopFromEmptyStack,
    #[error("Undefined global variable '{0}'")]
    UndefinedGlobalVariable(String),
}

pub struct VM {
    chunk: Chunk,
    pc: usize,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    allocator: Rc<RefCell<Allocator>>,
    output: Rc<RefCell<dyn std::io::Write>>,
}

impl VM {
    pub fn new(output: Rc<RefCell<dyn std::io::Write>>) -> Self {
        Self {
            chunk: Default::default(),
            pc: 0,
            stack: Default::default(),
            globals: HashMap::new(),
            allocator: Rc::new(RefCell::new(Allocator::default())),
            output,
        }
    }

    pub fn set_chunk(&mut self, chunk: Chunk) {
        self.chunk = chunk;
        self.pc = 0;
        self.stack.clear();
    }

    fn read_byte(&mut self) -> u8 {
        let ret = self.chunk.code[self.pc];
        self.pc += 1;
        ret
    }

    fn read_opcode(&mut self) -> Result<OpCode, InvalidOpCode> {
        self.read_byte().try_into()
    }

    fn read_constant(&mut self) -> Value {
        let constant_index = self.read_byte() as usize;
        self.chunk.constants[constant_index].clone()
    }

    fn trace(&self) {
        println!("  stack: {:?}", self.stack);
        println!("globals: {:?}", self.globals);
        self.chunk.disassemble_instruction(self.pc);
    }

    fn push(&mut self, v: impl Into<Value>) {
        let v = v.into();
        self.stack.push(v);
    }

    fn pop(&mut self) -> Result<Value, Error> {
        self.stack.pop().ok_or(Error::PopFromEmptyStack)
    }

    pub fn run(&mut self) -> Result<(), Error> {
        loop {
            #[cfg(debug_assertions)]
            self.trace();

            match self.read_opcode()? {
                OpCode::Return => {
                    // let value = self.pop()?;
                    return Ok(());
                }
                OpCode::Print => {
                    let value = self.pop()?;
                    writeln!(self.output.borrow_mut(), "{}", value)?;
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
                            }
                        },
                        _ => {
                            return Err(Error::InvalidOperands(
                                a.clone(),
                                "+".to_owned(),
                                b.clone(),
                            ))
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
                    match val {
                        Value::Number(n) => self.push(-n),
                        other => return Err(Error::InvalidOperand("Number".to_owned(), other)),
                    }
                }
                OpCode::Constant => {
                    let constant = self.read_constant();
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
                    self.push(self.stack[slot].clone());
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte() as usize;
                    self.stack[slot] = self.stack.last().unwrap().clone();
                }
                OpCode::GetGlobal => {
                    let name = self.read_constant().string().unwrap().to_owned(); // TODO
                    match self.globals.entry(name) {
                        Occupied(e) => {
                            let value = e.get().clone();
                            self.push(value);
                        }
                        Vacant(e) => {
                            return Err(Error::UndefinedGlobalVariable(e.key().to_owned()))
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
                            return Err(Error::UndefinedGlobalVariable(e.key().to_owned()))
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

            self.interpret(&line)?;
        }
    }

    pub fn run_file(&mut self, path: &Path) -> Result<(), Error> {
        let source = std::fs::read_to_string(path)?;
        self.interpret(&source)?;
        Ok(())
    }

    fn interpret(&mut self, source: &str) -> Result<(), Error> {
        let parser = Parser::new(source, self.allocator.clone());
        let chunk = if let Some(chunk) = parser.compile() {
            chunk
        } else {
            return Err(Error::CompilationFailed);
        };

        self.set_chunk(chunk);

        self.run()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;

    macro_rules! test_eval {
        ($input: literal, $expected: expr) => {{
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
        test_eval!("{var x = 1; var y = 2; print x + y;}", Value::Number(3.0));
        test_eval!("{var x = 1; {var y = 2; print x + y;}}", Value::Number(3.0));
        test_eval!(
            "{var x = 1; {var y = 2; {print x + y;}}}",
            Value::Number(3.0)
        );
        test_eval!(
            r#"
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
        "#,
            "3\n2\n1"
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
}
