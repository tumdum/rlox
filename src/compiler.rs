use crate::allocator::Allocator;
use crate::chunk::{Chunk, OpCode};
use crate::scanner::Token;
use crate::scanner::{self, Scanner, TokenType};
use crate::value::{Function, Obj, Value};
use maplit::hashmap;
use once_cell::sync::Lazy;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use thiserror::Error;

static RULES: Lazy<HashMap<TokenType, ParseRule>> = Lazy::new(|| {
    hashmap! {
       TokenType::LeftParen      => ParseRule{prefix: Some(&Parser::grouping), infix: Some(&Parser::call),   precedence: Precedence::Call},
       TokenType::RightParen     => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::LeftBrace      => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::RightBrace     => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::LeftBracket    => ParseRule{prefix: None,                    infix: Some(&Parser::index),  precedence: Precedence::Call},
       TokenType::RightBracket   => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Comma          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Dot            => ParseRule{prefix: None,                    infix: Some(&Parser::dot),    precedence: Precedence::Call},
       TokenType::Minus          => ParseRule{prefix: Some(&Parser::unary),    infix: Some(&Parser::binary), precedence: Precedence::Term},
       TokenType::Plus           => ParseRule{prefix: None,                    infix: Some(&Parser::binary), precedence: Precedence::Term},
       TokenType::Semicolon      => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Slash          => ParseRule{prefix: None,                    infix: Some(&Parser::binary), precedence: Precedence::Factor},
       TokenType::Star           => ParseRule{prefix: None,                    infix: Some(&Parser::binary), precedence: Precedence::Factor},
       TokenType::Bang           => ParseRule{prefix: Some(&Parser::unary),    infix: None,                  precedence: Precedence::None},
       TokenType::BangEqual      => ParseRule{prefix: None,                    infix: Some(&Parser::binary), precedence: Precedence::Equality},
       TokenType::Equal          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::EqualEqual     => ParseRule{prefix: None,                    infix: Some(&Parser::binary), precedence: Precedence::Equality},
       TokenType::Greater        => ParseRule{prefix: None,                    infix: Some(&Parser::binary), precedence: Precedence::Comparison},
       TokenType::GreaterEqual   => ParseRule{prefix: None,                    infix: Some(&Parser::binary), precedence: Precedence::Comparison},
       TokenType::Less           => ParseRule{prefix: None,                    infix: Some(&Parser::binary), precedence: Precedence::Comparison},
       TokenType::LessEqual      => ParseRule{prefix: None,                    infix: Some(&Parser::binary), precedence: Precedence::Comparison},
       TokenType::Identifier     => ParseRule{prefix: Some(&Parser::variable), infix: None,                  precedence: Precedence::None},
       TokenType::String         => ParseRule{prefix: Some(&Parser::string),   infix: None,                  precedence: Precedence::None},
       TokenType::Number         => ParseRule{prefix: Some(&Parser::number),   infix: None,                  precedence: Precedence::None},
       TokenType::And            => ParseRule{prefix: None,                    infix: Some(&Parser::and),    precedence: Precedence::And},
       TokenType::Class          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Else           => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::False          => ParseRule{prefix: Some(&Parser::literal),  infix: None,                  precedence: Precedence::None},
       TokenType::For            => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Fun            => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::If             => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::In             => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Nil            => ParseRule{prefix: Some(&Parser::literal),  infix: None,                  precedence: Precedence::None},
       TokenType::Or             => ParseRule{prefix: None,                    infix: Some(&Parser::or),     precedence: Precedence::Or},
       TokenType::Print          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Return         => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Super          => ParseRule{prefix: Some(&Parser::super_),   infix: None,                  precedence: Precedence::None},
       TokenType::This           => ParseRule{prefix: Some(&Parser::this),     infix: None,                  precedence: Precedence::None},
       TokenType::True           => ParseRule{prefix: Some(&Parser::literal),  infix: None,                  precedence: Precedence::None},
       TokenType::Var            => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::While          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Error          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Eof            => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
    }
});

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Precedence {
    None = 1,
    Assignment = 2,
    Or = 3,
    And = 4,
    Equality = 5,
    Comparison = 6,
    Term = 7,
    Factor = 8,
    Unary = 9,
    Call = 10,
    Primary = 11,
}

impl Precedence {
    fn next(self) -> Self {
        use Precedence::*;
        let v: u8 = self as u8;
        match v + 1 {
            1 => None,
            2 => Assignment,
            3 => Or,
            4 => And,
            5 => Equality,
            6 => Comparison,
            7 => Term,
            8 => Factor,
            9 => Unary,
            10 => Call,
            11 => Primary,
            _ => todo!(),
        }
    }
}

type ParseFn = &'static (dyn Fn(&mut Parser, bool) + Sync);

#[derive(Clone)]
struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Scanning failed: {0}")]
    ScannerError(#[from] crate::scanner::Error),
}

#[derive(Debug, PartialEq, Eq)]
enum FunctionType {
    Function,
    Initializer,
    Method,
    Script,
}

#[derive(Debug, Default)]
struct Local {
    name: String,
    depth: isize,
    is_captured: bool,
}

#[derive(Debug, Clone)]
struct UpValue {
    index: u8,
    is_local: bool,
}

#[derive(Debug)]
struct Compiler {
    locals: Vec<Local>,
    upvalues: Vec<UpValue>,
    scope_depth: isize,
    function: Value,
    function_type: FunctionType,
}

impl Compiler {
    fn new(function_type: FunctionType, allocator: Rc<RefCell<Allocator>>) -> Self {
        let function = allocator
            .borrow_mut()
            .allocate_function(Function::default());
        Self {
            locals: vec![Local {
                name: if function_type != FunctionType::Function {
                    "this".to_owned()
                } else {
                    "".to_owned()
                },
                depth: 0,
                is_captured: false,
            }],
            upvalues: vec![],
            scope_depth: 0,
            function,
            function_type,
        }
    }

    fn mark(&mut self) {
        self.function.mark();
    }

    fn add_upvalue(&mut self, index: u8, is_local: bool) -> usize {
        if let Some(idx) = self
            .upvalues
            .iter()
            .position(|v| v.index == index && v.is_local == is_local)
        {
            return idx;
        }
        self.upvalues.push(UpValue { index, is_local });
        let upvalues = self.upvalues.len();
        self.function.function_mut().unwrap().upvalue_count = upvalues;
        let ret = upvalues - 1;
        assert_eq!(self.upvalues[ret].index, index);
        assert_eq!(self.upvalues[ret].is_local, is_local);
        ret
    }
}

struct ClassCompiler {
    has_superclass: bool,
}

pub struct Parser {
    scanner: Scanner,
    compilers: Vec<Compiler>,
    classes: Vec<ClassCompiler>,
    allocator: Rc<RefCell<Allocator>>,
    current: Option<Token>,
    previous: Option<Token>,
    had_error: RefCell<bool>,
    panic_mode: RefCell<bool>,
    next_unique_id: usize,
}

impl Parser {
    pub fn new(source: &str, allocator: Rc<RefCell<Allocator>>) -> Self {
        let scanner = scanner::Scanner::new(source);
        let compiler = Compiler::new(FunctionType::Script, allocator.clone());
        Self {
            scanner,
            compilers: vec![compiler],
            classes: vec![],
            allocator,
            current: None,
            previous: None,
            had_error: RefCell::new(false),
            panic_mode: RefCell::new(false),
            next_unique_id: Default::default(),
        }
    }

    fn unique_synthetic_token(&mut self, prefix: &str) -> Token {
        let name = format!("{}-{}", prefix, self.next_unique_id);
        self.next_unique_id += 1;
        self.synthetic_token(&name)
    }

    fn add_new_unique_local(&mut self, name_prefix: &str) -> (Token, u8) {
        let token = self.unique_synthetic_token(name_prefix);
        self.add_local(token.clone());
        let id = self.identifier_constant(&token);
        (token, id)
    }

    pub fn mark(&mut self) {
        self.compilers.iter_mut().for_each(|c| c.mark());
    }

    pub fn compile(&mut self) -> Option<Value> {
        self.advance().unwrap();

        while !self.match_token(TokenType::Eof) {
            self.declaration();
        }

        let function = self.end_compiler().function;

        if *self.had_error.borrow() {
            None
        } else {
            Some(function)
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value");
        self.emit_byte(OpCode::Pop as u8);
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'");
        if self.match_token(TokenType::Semicolon) {
            // No initializer
        } else if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else if self.match_token(TokenType::Identifier) {
            return self.for_in_statement();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.current_chunk().code.len();
        let exit_jump;
        if !self.match_token(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition");

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse as u8));
            self.emit_byte(OpCode::Pop as u8);
        } else {
            exit_jump = None;
        }

        if !self.match_token(TokenType::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump as u8);
            let increment_start = self.current_chunk().code.len();
            self.expression();
            self.emit_byte(OpCode::Pop as u8);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_byte(OpCode::Pop as u8);
        }

        self.end_scope();
    }

    fn for_in_statement(&mut self) {
        let iter_var_name: Token = self.previous.as_ref().unwrap().clone();
        let name_constant = self.identifier_constant(&iter_var_name);
        self.consume(TokenType::In, "Expect 'in' after identifier in for");

        // Fake variable pointing to collection iterated over
        let (token, token_id) = self.add_new_unique_local("collection_tmp");
        self.expression();
        self.define_variable(token_id);
        let collection_tmp_id = self.resolve_local(self.compilers.len() - 1, &token);
        self.match_token(TokenType::RightParen);

        // Fake iterator iterating over fake collection
        let (iter_token, iter_token_id) = self.add_new_unique_local("iter_tmp");
        self.emit_bytes(OpCode::GetLocal as u8, collection_tmp_id as u8);
        let iter_method_token = self.synthetic_token("iter");
        let iter_method_id = self.identifier_constant(&iter_method_token);
        self.emit_bytes(OpCode::Invoke as u8, iter_method_id);
        self.emit_byte(0);
        self.define_variable(iter_token_id);
        let iter_tmp_id = self.resolve_local(self.compilers.len() - 1, &iter_token);

        // Iteration variable
        self.add_local(iter_var_name.clone());
        self.emit_bytes(OpCode::GetLocal as u8, iter_tmp_id as u8);
        let next_method_token = self.synthetic_token("next");
        let next_method_id = self.identifier_constant(&next_method_token);
        self.emit_bytes(OpCode::Invoke as u8, next_method_id);
        self.emit_byte(0);
        self.define_variable(name_constant);
        let iter_var_id = self.resolve_local(self.compilers.len() - 1, &iter_var_name);

        let loop_start = self.current_chunk().code.len();

        self.statement();

        // Update iteration var...
        self.emit_bytes(OpCode::GetLocal as u8, iter_tmp_id as u8);
        self.emit_bytes(OpCode::Invoke as u8, next_method_id);
        self.emit_byte(0);
        self.emit_bytes(OpCode::SetLocal as u8, iter_var_id as u8);
        self.emit_byte(OpCode::Pop as u8);
        // and check if it equals nil
        self.emit_bytes(OpCode::GetLocal as u8, iter_var_id as u8);
        self.emit_byte(OpCode::Nil as u8);
        self.emit_bytes(OpCode::Equal as u8, OpCode::Not as u8);

        let end_jump = self.emit_jump(OpCode::JumpIfFalse as u8);
        self.emit_byte(OpCode::Pop as u8); // condition
        self.emit_loop(loop_start);
        self.patch_jump(end_jump);
        self.emit_byte(OpCode::Pop as u8); // condition

        self.end_scope();
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse as u8);
        self.emit_byte(OpCode::Pop as u8);
        self.statement();

        let else_jump = self.emit_jump(OpCode::Jump as u8);

        self.patch_jump(then_jump);
        self.emit_byte(OpCode::Pop as u8);

        if self.match_token(TokenType::Else) {
            self.statement();
        }

        self.patch_jump(else_jump);
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block");
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name");
        self.mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global);
    }

    fn function(&mut self, function_type: FunctionType) {
        let compilers = self.compilers.len();
        self.compilers
            .push(Compiler::new(function_type, self.allocator.clone()));
        self.current_compiler_mut()
            .function
            .function_mut()
            .unwrap()
            .name = self.previous.as_ref().unwrap().value.clone();

        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after function name");
        if !self.check(TokenType::RightParen) {
            loop {
                self.current_compiler_mut()
                    .function
                    .function_mut()
                    .unwrap()
                    .arity += 1;
                if self
                    .current_compiler_mut()
                    .function
                    .function()
                    .unwrap()
                    .arity
                    > 255
                {
                    self.error(
                        self.scanner.line(),
                        "Can't have more than 255 parameters".into(),
                    );
                }
                let constant = self.parse_variable("Expected parameter name");
                self.define_variable(constant);
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters");
        self.consume(TokenType::LeftBrace, "Expect '{' after function name");
        self.block();

        let compiler = self.end_compiler();
        let fun = compiler.function;

        assert_eq!(compilers, self.compilers.len());

        let constant = self.make_constant(fun).unwrap();
        self.emit_bytes(OpCode::Closure as u8, constant);
        for UpValue { index, is_local } in compiler.upvalues {
            self.emit_byte(if is_local { 1 } else { 0 });
            self.emit_byte(index);
        }
    }

    fn method(&mut self) {
        self.consume(TokenType::Identifier, "Expected method name");
        let name = self.previous.as_ref().unwrap().clone();
        let constant = self.identifier_constant(&name);

        let function_type = if self.previous.as_ref().unwrap().value == crate::vm::INITIALIZER_NAME
        {
            FunctionType::Initializer
        } else {
            FunctionType::Method
        };
        self.function(function_type);
        self.emit_bytes(OpCode::Method as u8, constant);
    }

    fn class_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expected class name");
        let name = self.previous.as_ref().unwrap().clone();
        let name_constant = self.identifier_constant(&name);
        self.declare_variable();

        self.emit_bytes(OpCode::Class as u8, name_constant);
        self.define_variable(name_constant);

        self.classes.push(ClassCompiler {
            has_superclass: false,
        });

        if self.match_token(TokenType::Less) {
            self.consume(TokenType::Identifier, "Expected super class name");
            self.variable(false);
            if name.value == self.previous.as_ref().unwrap().value {
                self.error(
                    self.scanner.line(),
                    "A class can't inherit from itself".into(),
                );
            }

            self.begin_scope();
            self.add_local(self.synthetic_token("super"));
            self.define_variable(0);

            self.named_variable(&name, false);
            self.classes.last_mut().unwrap().has_superclass = true;
            self.emit_byte(OpCode::Inherit as u8);
        }

        self.named_variable(&name, false);
        self.consume(TokenType::LeftBrace, "Expected '{' before class body");
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.method();
        }
        self.consume(TokenType::RightBrace, "Expected '}' after class body");
        self.emit_byte(OpCode::Pop as u8);

        if self.classes.last().unwrap().has_superclass {
            self.end_scope();
        }

        self.classes.pop().unwrap();
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name");
        if self.match_token(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_byte(OpCode::Nil as u8);
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration",
        );

        self.define_variable(global);
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value");
        self.emit_byte(OpCode::Print as u8);
    }

    fn return_statement(&mut self) {
        if self.current_compiler().function_type == FunctionType::Script {
            self.error(self.scanner.line(), "Can't return from top level".into());
        }

        if self.match_token(TokenType::Semicolon) {
            self.emit_return();
        } else {
            if self.current_compiler().function_type == FunctionType::Initializer {
                self.error(
                    self.scanner.line(),
                    "Can't return a value from an initializer".into(),
                );
            }
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value");
            self.emit_byte(OpCode::Return as u8);
        }
    }

    fn while_statement(&mut self) {
        let loop_start = self.current_chunk().code.len();
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse as u8);
        self.emit_byte(OpCode::Pop as u8);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_byte(OpCode::Pop as u8);
    }

    fn declaration(&mut self) {
        if self.match_token(TokenType::Class) {
            self.class_declaration();
        } else if self.match_token(TokenType::Fun) {
            self.fun_declaration();
        } else if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if *self.panic_mode.borrow() {
            self.synchronize();
        }
    }

    fn synchronize(&mut self) {
        *self.panic_mode.borrow_mut() = false;
        use TokenType::*;

        while self.current.as_ref().unwrap().type_ != Eof {
            if self.previous.as_ref().unwrap().type_ == Semicolon {
                return;
            }

            match self.current.as_ref().unwrap().type_ {
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => { /* do nothing */ }
            }

            self.advance().unwrap();
        }
    }

    fn statement(&mut self) {
        if self.match_token(TokenType::Print) {
            self.print_statement();
        } else if self.match_token(TokenType::For) {
            self.for_statement();
        } else if self.match_token(TokenType::If) {
            self.if_statement();
        } else if self.match_token(TokenType::Return) {
            self.return_statement();
        } else if self.match_token(TokenType::While) {
            self.while_statement();
        } else if self.match_token(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn number(&mut self, _can_assign: bool) {
        let value: f64 = self.previous.as_ref().unwrap().value.parse().unwrap();
        self.emit_constant(value.into());
    }

    fn string(&mut self, _can_assign: bool) {
        let s = &self.previous.as_ref().unwrap().value;
        let l = s.len();
        let s: String = s.chars().skip(1).take(l - 2).collect();
        let s = self.allocator.borrow_mut().allocate_string(s);
        self.emit_constant(s);
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(&self.previous.clone().unwrap(), can_assign);
    }

    fn synthetic_token(&self, text: &str) -> Token {
        Token {
            value: text.to_owned(),
            type_: TokenType::Super,
            line: usize::max_value(),
        }
    }

    fn super_(&mut self, _can_assign: bool) {
        if self.classes.is_empty() {
            self.error(
                self.scanner.line(),
                "Can't use 'super' outside of a class".into(),
            );
        }
        if !self.classes.last().unwrap().has_superclass {
            self.error(
                self.scanner.line(),
                "Can't use 'super' in a class with no superclass".into(),
            );
        }
        self.consume(TokenType::Dot, "Expect '.' after 'super'");
        self.consume(TokenType::Identifier, "Expect superclass method name");
        let name = self.previous.as_ref().unwrap().clone();
        let name_constant = self.identifier_constant(&name);

        self.named_variable(&self.synthetic_token("this"), false);
        if self.match_token(TokenType::LeftParen) {
            let arg_count = self.argument_list();
            self.named_variable(&self.synthetic_token("super"), false);
            self.emit_bytes(OpCode::SuperInvoke as u8, name_constant);
            self.emit_byte(arg_count);
        } else {
            self.named_variable(&self.synthetic_token("super"), false);
            self.emit_bytes(OpCode::GetSuper as u8, name_constant);
        }
    }

    fn this(&mut self, _can_assign: bool) {
        if self.classes.is_empty() {
            self.error(
                self.scanner.line(),
                "Can't use 'this' outside of a class".into(),
            );
        }
        self.variable(false);
    }

    fn named_variable(&mut self, name: &Token, can_assign: bool) {
        let get_op;
        let set_op;

        let mut arg = self.resolve_local(self.compilers.len() - 1, name);
        if arg != -1 {
            get_op = OpCode::GetLocal;
            set_op = OpCode::SetLocal;
        } else {
            arg = self.resolve_upvalue(self.compilers.len() - 1, name);
            if arg != -1 {
                get_op = OpCode::GetUpValue;
                set_op = OpCode::SetUpValue;
            } else {
                get_op = OpCode::GetGlobal;
                set_op = OpCode::SetGlobal;
                arg = self.identifier_constant(name) as isize;
            }
        }

        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            self.emit_bytes(set_op as u8, arg as u8);
        } else {
            self.emit_bytes(get_op as u8, arg as u8);
        }
    }

    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression");
    }

    fn unary(&mut self, _can_assign: bool) {
        let operator_type = self.previous.as_ref().unwrap().type_;

        // compile the operand
        self.parse_precedence(Precedence::Unary);

        // Emit the operator instruction
        match operator_type {
            TokenType::Bang => self.emit_byte(OpCode::Not as u8),
            TokenType::Minus => self.emit_byte(OpCode::Negate as u8),
            _ => unreachable!(),
        }
    }

    fn binary(&mut self, _can_assign: bool) {
        let operator_type = self.previous.as_ref().unwrap().type_;
        let rule = self.get_rule(operator_type);
        self.parse_precedence(rule.precedence.next());

        use OpCode::*;
        match operator_type {
            TokenType::BangEqual => self.emit_bytes(Equal as u8, Not as u8),
            TokenType::EqualEqual => self.emit_byte(Equal as u8),
            TokenType::Greater => self.emit_byte(Greater as u8),
            TokenType::GreaterEqual => self.emit_bytes(Less as u8, Not as u8),
            TokenType::Less => self.emit_byte(Less as u8),
            TokenType::LessEqual => self.emit_bytes(Greater as u8, Not as u8),
            TokenType::Plus => self.emit_byte(Add as u8),
            TokenType::Minus => self.emit_byte(Subtract as u8),
            TokenType::Star => self.emit_byte(Multiply as u8),
            TokenType::Slash => self.emit_byte(Divide as u8),
            _ => unreachable!(),
        }
    }

    fn call(&mut self, _can_assign: bool) {
        let arg_count = self.argument_list();
        self.emit_bytes(OpCode::Call as u8, arg_count);
    }

    fn index(&mut self, can_assign: bool) {
        self.expression();
        self.consume(
            TokenType::RightBracket,
            "Expected ']' after index expression",
        );
        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            let token = self.synthetic_token("setByIndex");
            let name = self.identifier_constant(&token);
            self.emit_bytes(OpCode::Invoke as u8, name);
            self.emit_byte(2);
        } else {
            let token = self.synthetic_token("getByIndex");
            let name = self.identifier_constant(&token);
            self.emit_bytes(OpCode::Invoke as u8, name);
            self.emit_byte(1);
        }
    }

    fn dot(&mut self, can_assign: bool) {
        self.consume(TokenType::Identifier, "Expected property name after '.'");
        let name = self.identifier_constant(self.previous.clone().as_ref().unwrap());

        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            self.emit_bytes(OpCode::SetProperty as u8, name);
        } else if self.match_token(TokenType::LeftParen) {
            let arg_count = self.argument_list();
            self.emit_bytes(OpCode::Invoke as u8, name);
            self.emit_byte(arg_count);
        } else {
            self.emit_bytes(OpCode::GetProperty as u8, name);
        }
    }

    fn literal(&mut self, _can_assign: bool) {
        let operator_type = self.previous.as_ref().unwrap().type_;
        match operator_type {
            TokenType::False => self.emit_byte(OpCode::False as u8),
            TokenType::True => self.emit_byte(OpCode::True as u8),
            TokenType::Nil => self.emit_byte(OpCode::Nil as u8),
            _ => unreachable!(),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance().unwrap();
        let prefix_rule = self.get_rule(self.previous.as_ref().unwrap().type_).prefix;
        let prefix_rule = match prefix_rule {
            Some(prefix_rule) => prefix_rule,
            None => {
                self.error(self.scanner.line(), "Expected expression".into());
                return;
            }
        };

        let can_assign = precedence <= Precedence::Assignment;
        prefix_rule(self, can_assign);

        while precedence
            <= self
                .get_rule(self.current.as_ref().unwrap().type_)
                .precedence
        {
            self.advance().unwrap();
            if let Some(infix_rule) = self.get_rule(self.previous.as_ref().unwrap().type_).infix {
                infix_rule(self, can_assign);
            }
        }

        if can_assign && self.match_token(TokenType::Equal) {
            self.error(self.scanner.line(), "Invalid assignment target".into());
        }
    }

    fn parse_variable(&mut self, msg: &str) -> u8 {
        self.consume(TokenType::Identifier, msg);

        self.declare_variable();
        if self.current_compiler().scope_depth > 0 {
            return 0;
        }
        self.identifier_constant(self.previous.clone().as_ref().unwrap())
    }

    fn identifier_constant(&mut self, token: &Token) -> u8 {
        let name = self
            .allocator
            .borrow_mut()
            .allocate_string(token.value.clone());
        self.make_constant(name).unwrap()
    }

    fn declare_variable(&mut self) {
        if self.current_compiler().scope_depth == 0 {
            return;
        }

        let token = self.previous.as_ref().unwrap().clone();

        let compiler = self.current_compiler();
        for Local { name, depth, .. } in compiler.locals.iter().rev() {
            if *depth != -1 && *depth < compiler.scope_depth {
                break;
            }

            if &token.value == name {
                self.error(
                    self.scanner.line(),
                    "Already a variable with this name in this scope".into(),
                );
            }
        }

        self.add_local(token);
    }

    fn add_local(&mut self, name: Token) {
        if self.current_compiler().locals.len() == u8::max_value() as usize {
            self.error(
                self.scanner.line(),
                "Too many local variables in scope".into(),
            );
            return;
        }
        self.current_compiler_mut().locals.push(Local {
            name: name.value,
            depth: -1,
            is_captured: false,
        });
    }

    fn resolve_local(&mut self, compiler_id: usize, name: &Token) -> isize {
        let compiler = &self.compilers[compiler_id];
        for i in (0..compiler.locals.len()).rev() {
            if name.value == compiler.locals[i].name {
                if compiler.locals[i].depth == -1 {
                    self.error(
                        self.scanner.line(),
                        "Can't read local variable in its own initializer".into(),
                    );
                }

                return i as isize;
            }
        }
        -1
    }

    fn resolve_upvalue(&mut self, compiler_id: usize, name: &Token) -> isize {
        if compiler_id == 0 {
            return -1;
        }

        let local = self.resolve_local(compiler_id - 1, name);
        if local != -1 {
            self.compilers[compiler_id - 1].locals[local as usize].is_captured = true;
            return self.add_upvalue(compiler_id, local.try_into().unwrap(), true) as isize;
        }

        let upvalue = self.resolve_upvalue(compiler_id - 1, name);
        if upvalue != -1 {
            return self.add_upvalue(compiler_id, upvalue.try_into().unwrap(), false) as isize;
        }
        -1
    }

    fn add_upvalue(&mut self, compiler_id: usize, index: u8, is_local: bool) -> usize {
        self.compilers[compiler_id].add_upvalue(index, is_local)
    }

    fn define_variable(&mut self, global: u8) {
        if self.current_compiler().scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit_bytes(OpCode::DefineGlobal as u8, global)
    }

    fn argument_list(&mut self) -> u8 {
        let mut arg_count: usize = 0;

        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();
                if arg_count == 255 {
                    self.error(
                        self.scanner.line(),
                        "Can't have more than 255 arguments".into(),
                    );
                }
                arg_count += 1;
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after arguments");
        arg_count as u8
    }

    fn and(&mut self, _can_assign: bool) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse as u8);

        self.emit_byte(OpCode::Pop as u8);
        self.parse_precedence(Precedence::And);

        self.patch_jump(end_jump);
    }

    fn or(&mut self, _can_assign: bool) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse as u8);
        let end_jump = self.emit_jump(OpCode::Jump as u8);

        self.patch_jump(else_jump);
        self.emit_byte(OpCode::Pop as u8);

        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn mark_initialized(&mut self) {
        if self.current_compiler().scope_depth == 0 {
            return;
        }
        let scope_depth = self.current_compiler().scope_depth;
        self.current_compiler_mut().locals.last_mut().unwrap().depth = scope_depth;
    }

    fn get_rule(&self, operator_type: TokenType) -> ParseRule {
        RULES.get(&operator_type).unwrap().clone()
    }

    fn emit_constant(&mut self, value: Value) {
        if let Some(constant) = self.make_constant(value) {
            self.emit_bytes(OpCode::Constant as u8, constant);
        }
    }

    fn patch_jump(&mut self, offset: usize) {
        // -2 to adjust for the bytecode for the jump offset itself
        let jump = self.current_chunk().code.len() - offset - 2;

        if jump > u16::max_value() as usize {
            self.error(self.scanner.line(), "Too much code to jump over".into());
        }
        self.current_chunk().code[offset] = ((jump >> 8) & 0xff) as u8;
        self.current_chunk().code[offset + 1] = (jump & 0xff) as u8;
    }

    fn make_constant(&mut self, value: Value) -> Option<u8> {
        let constant = self.current_chunk().add_constant(value);
        match constant {
            Ok(constant) => Some(constant),
            Err(e) => {
                self.error(
                    self.scanner.line(),
                    format!("Too many constants: {}", e).as_str().into(),
                );
                None
            }
        }
    }

    fn consume(&mut self, token_type: TokenType, msg: &str) {
        if self
            .current
            .as_ref()
            .map(|t| t.type_ == token_type)
            .unwrap_or(false)
        {
            self.advance().unwrap();
            return;
        }

        self.error_at(self.scanner.line(), msg.into());
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.current.as_ref().unwrap().type_ == token_type
    }

    fn match_token(&mut self, token_type: TokenType) -> bool {
        if !self.check(token_type) {
            return false;
        }
        self.advance().unwrap();
        true
    }

    fn advance(&mut self) -> Result<(), Error> {
        self.previous = self.current.clone();

        loop {
            match self.scanner.scan_token() {
                Ok(token) => {
                    self.current = Some(token);
                    break;
                }
                Err(e) => {
                    self.error_from_scanner(e);
                }
            }
        }

        Ok(())
    }

    fn emit_byte(&mut self, byte: u8) {
        let line = self.previous.as_ref().map(|t| t.line).unwrap_or(0);
        self.current_chunk().write(byte, line);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(OpCode::Loop as u8);

        let offset = self.current_chunk().code.len() - loop_start + 2;
        if offset > u16::max_value() as usize {
            self.error(self.scanner.line(), "Loop body too large".into());
        }

        self.emit_byte(((offset >> 8) & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8);
    }

    fn emit_jump(&mut self, instruction: u8) -> usize {
        self.emit_byte(instruction);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        self.current_chunk().code.len() - 2
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        self.current_compiler_mut().function.chunk_mut().unwrap()
    }

    fn current_compiler(&self) -> &Compiler {
        self.compilers.last().unwrap()
    }

    fn current_compiler_mut(&mut self) -> &mut Compiler {
        self.compilers.last_mut().unwrap()
    }

    fn end_compiler(&mut self) -> Compiler {
        self.emit_return();
        if !*self.had_error.borrow() {
            let mut name = "<script>".to_owned();
            if let crate::value::Value::Obj(ptr) = self.current_compiler().function {
                let obj: &self::Obj = unsafe { &*ptr };
                if let crate::value::ObjInner::Function(f) = &**obj {
                    if !f.name.is_empty() {
                        name = f.name.clone()
                    }
                }
            }

            #[cfg(debug_assertions)]
            self.current_chunk().disassemble(&name);
        }
        self.compilers.pop().unwrap()
    }

    fn begin_scope(&mut self) {
        self.current_compiler_mut().scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.current_compiler_mut().scope_depth -= 1;

        while !self.current_compiler_mut().locals.is_empty()
            && self.current_compiler_mut().locals.last().unwrap().depth
                > self.current_compiler_mut().scope_depth
        {
            if self
                .current_compiler_mut()
                .locals
                .last()
                .unwrap()
                .is_captured
            {
                self.emit_byte(OpCode::CloseUpValue as u8);
            } else {
                self.emit_byte(OpCode::Pop as u8);
            }
            self.current_compiler_mut().locals.pop();
        }
    }

    fn emit_return(&mut self) {
        if self.current_compiler().function_type == FunctionType::Initializer {
            self.emit_bytes(OpCode::GetLocal as u8, 0);
        } else {
            self.emit_byte(OpCode::Nil as u8);
        }
        self.emit_byte(OpCode::Return as u8);
    }

    fn error_from_scanner(&self, e: scanner::Error) {
        self.error_at(e.line, e.into());
    }

    fn error(&self, line: usize, e: Box<dyn std::error::Error>) {
        self.error_at(line, e)
    }

    fn error_at(&self, line: usize, e: Box<dyn std::error::Error>) {
        if *self.panic_mode.borrow() {
            return;
        }
        *self.panic_mode.borrow_mut() = true;
        eprintln!("[line {}]: Error: {}", line, e);
        *self.had_error.borrow_mut() = true;
    }
}
