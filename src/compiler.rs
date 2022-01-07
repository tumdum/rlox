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
       TokenType::Comma          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Dot            => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
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
       TokenType::Nil            => ParseRule{prefix: Some(&Parser::literal),  infix: None,                  precedence: Precedence::None},
       TokenType::Or             => ParseRule{prefix: None,                    infix: Some(&Parser::or),     precedence: Precedence::Or},
       TokenType::Print          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Return         => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Super          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::This           => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
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
    Script,
}

#[derive(Debug, Default)]
struct Local {
    name: String,
    depth: isize,
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
    fn new(allocator: Rc<RefCell<Allocator>>) -> Self {
        let function = allocator
            .borrow_mut()
            .allocate_function(Function::default());
        Self {
            locals: vec![],
            upvalues: vec![],
            scope_depth: 0,
            function,
            function_type: FunctionType::Script,
        }
    }

    fn add_upvalue(&mut self, index: u8, is_local: bool) -> usize {
        // println!("add_upvalue index {} is_local {}", index, is_local);
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

pub struct Parser {
    scanner: Scanner,
    compilers: Vec<Compiler>,
    allocator: Rc<RefCell<Allocator>>,
    current: Option<Token>,
    previous: Option<Token>,
    had_error: RefCell<bool>,
    panic_mode: RefCell<bool>,
}

impl Parser {
    pub fn new(source: &str, allocator: Rc<RefCell<Allocator>>) -> Self {
        let scanner = scanner::Scanner::new(source);
        let mut compiler = Compiler::new(allocator.clone());
        compiler.locals.push(Local {
            name: "".to_owned(),
            depth: 0,
        });
        Self {
            scanner,
            compilers: vec![compiler],
            allocator,
            current: None,
            previous: None,
            had_error: RefCell::new(false),
            panic_mode: RefCell::new(false),
        }
    }

    pub fn compile(mut self) -> Option<Value> {
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
        self.compilers.push(Compiler::new(self.allocator.clone()));
        self.compilers
            .last_mut()
            .unwrap()
            .function
            .function_mut()
            .unwrap()
            .name = self.previous.as_ref().unwrap().value.clone();
        self.compilers.last_mut().unwrap().function_type = function_type;

        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after function name");
        if !self.check(TokenType::RightParen) {
            loop {
                self.compilers
                    .last_mut()
                    .unwrap()
                    .function
                    .function_mut()
                    .unwrap()
                    .arity += 1;
                if self
                    .compilers
                    .last()
                    .unwrap()
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
        if self.compilers.last().unwrap().function_type == FunctionType::Script {
            self.error(self.scanner.line(), "Can't return from top level".into());
        }

        if self.match_token(TokenType::Semicolon) {
            self.emit_return();
        } else {
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
        if self.match_token(TokenType::Fun) {
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

            self.advance();
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

    fn named_variable(&mut self, name: &Token, can_assign: bool) {
        let get_op;
        let set_op;

        let mut arg = self.resolve_local(self.compilers.len() - 1, name);
        if arg != -1 {
            get_op = OpCode::GetLocal;
            set_op = OpCode::SetLocal;
        } else {
            arg = self.resolve_upvalue(self.compilers.len() - 1, name);
            // println!("resolve_upvalue {:?}: {}", name, arg);
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
        if self.compilers.last().unwrap().scope_depth > 0 {
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
        if self.compilers.last().unwrap().scope_depth == 0 {
            return;
        }

        let token = self.previous.as_ref().unwrap().clone();

        let compiler = self.compilers.last().unwrap();
        for Local { name, depth } in compiler.locals.iter().rev() {
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
        if self.compilers.last().unwrap().locals.len() == u8::max_value() as usize {
            self.error(
                self.scanner.line(),
                "Too many local variables in scope".into(),
            );
            return;
        }
        // println!("Adding local {:?} @ {}", name, self.compilers.last().unwrap().scope_depth);
        self.compilers.last_mut().unwrap().locals.push(Local {
            name: name.value,
            depth: -1,
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
            return self.add_upvalue(compiler_id, local.try_into().unwrap(), true) as isize;
        }

        let upvalue = self.resolve_upvalue(compiler_id - 1, name);
        if upvalue != -1 {
            return self.add_upvalue(compiler_id, upvalue.try_into().unwrap(), false) as isize;
        }

        return -1;
    }

    fn add_upvalue(&mut self, compiler_id: usize, index: u8, is_local: bool) -> usize {
        self.compilers[compiler_id].add_upvalue(index, is_local)
    }

    fn define_variable(&mut self, global: u8) {
        if self.compilers.last().unwrap().scope_depth > 0 {
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
        if self.compilers.last().unwrap().scope_depth == 0 {
            return;
        }
        let scope_depth = self.compilers.last().unwrap().scope_depth;
        self.compilers
            .last_mut()
            .unwrap()
            .locals
            .last_mut()
            .unwrap()
            .depth = scope_depth;
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
        self.advance();
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
        self.compilers.last_mut().unwrap().function.chunk_mut()
    }

    fn end_compiler(&mut self) -> Compiler {
        self.emit_return();
        if !*self.had_error.borrow() {
            let mut name = "<script>".to_owned();
            if let crate::value::Value::Obj(ptr) = self.compilers.last().unwrap().function {
                let obj: &self::Obj = unsafe { &*ptr };
                if let crate::value::Obj::Function(f) = obj {
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
        self.compilers.last_mut().unwrap().scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compilers.last_mut().unwrap().scope_depth -= 1;

        while !self.compilers.last_mut().unwrap().locals.is_empty()
            && self
                .compilers
                .last_mut()
                .unwrap()
                .locals
                .last()
                .unwrap()
                .depth
                > self.compilers.last_mut().unwrap().scope_depth
        {
            self.emit_byte(OpCode::Pop as u8);
            self.compilers.last_mut().unwrap().locals.pop();
        }
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Nil as u8);
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
