use crate::allocator::Allocator;
use crate::chunk::{Chunk, OpCode};
use crate::scanner::Token;
use crate::scanner::{self, Scanner, TokenType};
use crate::value::Value;
use maplit::hashmap;
use once_cell::sync::Lazy;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use thiserror::Error;

static RULES: Lazy<HashMap<TokenType, ParseRule>> = Lazy::new(|| {
    hashmap! {
       TokenType::LeftParen      => ParseRule{prefix: Some(&Parser::grouping), infix: None,                  precedence: Precedence::None},
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
       TokenType::And            => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Class          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Else           => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::False          => ParseRule{prefix: Some(&Parser::literal),  infix: None,                  precedence: Precedence::None},
       TokenType::For            => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Fun            => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::If             => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Nil            => ParseRule{prefix: Some(&Parser::literal),  infix: None,                  precedence: Precedence::None},
       TokenType::Or             => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
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


#[derive(Debug, Default)]
struct Local {
    name: String,
    depth: isize,
}

#[derive(Debug, Default)]
struct Compiler {
    locals: Vec<Local>,
    scope_depth: isize,
}

pub struct Parser {
    scanner: Scanner,
    compiling_chunk: Chunk,
    compiler: Compiler,
    allocator: Rc<RefCell<Allocator>>,
    current: Option<Token>,
    previous: Option<Token>,
    had_error: RefCell<bool>,
    panic_mode: RefCell<bool>,
}

impl Parser {
    pub fn new(source: &str, allocator: Rc<RefCell<Allocator>>) -> Self {
        let scanner = scanner::Scanner::new(source);
        Self {
            scanner,
            compiling_chunk: Chunk::default(),
            compiler: Compiler::default(),
            allocator,
            current: None,
            previous: None,
            had_error: RefCell::new(false),
            panic_mode: RefCell::new(false),
        }
    }

    pub fn compile(mut self) -> Option<Chunk> {
        self.advance().unwrap();

        while !self.match_token(TokenType::Eof) {
            self.declaration();
        }

        self.end_compiler();

        if *self.had_error.borrow() {
            None
        } else {
            Some(self.compiling_chunk)
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

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block");
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

    fn declaration(&mut self) {
        if self.match_token(TokenType::Var) {
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

        let mut arg = self.resolve_local(name);
        if arg != -1 {
            get_op = OpCode::GetLocal;
            set_op = OpCode::SetLocal;
        } else {
            get_op = OpCode::GetGlobal;
            set_op = OpCode::SetGlobal;
            arg = self.identifier_constant(name) as isize;
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
        if self.compiler.scope_depth > 0 {
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
        if self.compiler.scope_depth == 0 {
            return;
        }

        let token = self.previous.as_ref().unwrap().clone();

        for Local{name, depth} in self.compiler.locals.iter().rev() {
            if *depth != -1 && *depth < self.compiler.scope_depth {
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
        if self.compiler.locals.len() == u8::max_value() as usize {
            self.error(
                self.scanner.line(),
                "Too many local variables in scope".into(),
            );
            return;
        }
        println!("Adding local {:?} @ {}", name, self.compiler.scope_depth);
        self.compiler.locals.push(Local{name: name.value, depth: -1});
    }

    fn resolve_local(&mut self, name: &Token) -> isize {
        println!("Resolving {:?} in {:?}", name, self.compiler.locals);
        for i in (0..self.compiler.locals.len()).rev() {
            if name.value == self.compiler.locals[i].name {
                if self.compiler.locals[i].depth == -1 {
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

    fn define_variable(&mut self, global: u8) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit_bytes(OpCode::DefineGlobal as u8, global)
    }

    fn mark_initialized(&mut self) {
        self.compiler.locals.last_mut().unwrap().depth = self.compiler.scope_depth;
    }

    fn get_rule(&self, operator_type: TokenType) -> ParseRule {
        RULES.get(&operator_type).unwrap().clone()
    }

    fn emit_constant(&mut self, value: Value) {
        if let Some(constant) = self.make_constant(value) {
            self.emit_bytes(OpCode::Constant as u8, constant);
        }
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

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.compiling_chunk
    }

    fn end_compiler(&mut self) {
        self.emit_return();
        if !*self.had_error.borrow() {
            #[cfg(debug_assertions)]
            self.current_chunk().disassemble("code");
        }
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;

        while !self.compiler.locals.is_empty()
            && self.compiler.locals.last().unwrap().depth > self.compiler.scope_depth
        {
            self.emit_byte(OpCode::Pop as u8);
            self.compiler.locals.pop();
        }
    }

    fn emit_return(&mut self) {
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
