use crate::chunk::{Chunk, OpCode};
use crate::scanner::Token;
use crate::scanner::{self, Scanner, TokenType};
use crate::value::Value;
use maplit::hashmap;
use once_cell::sync::Lazy;
use std::collections::HashMap;
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
       TokenType::Bang           => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::BangEqual      => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Equal          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::EqualEqual     => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Greater        => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::GreaterEqual   => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Less           => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::LessEqual      => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Identifier     => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::String         => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Number         => ParseRule{prefix: Some(&Parser::number),   infix: None,                  precedence: Precedence::None},
       TokenType::And            => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Class          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Else           => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::False          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::For            => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Fun            => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::If             => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Nil            => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Or             => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Print          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Return         => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::Super          => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::This           => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
       TokenType::True           => ParseRule{prefix: None,                    infix: None,                  precedence: Precedence::None},
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

type ParseFn = &'static (dyn Fn(&mut Parser) -> () + Sync);

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

#[derive(Debug)]
pub struct Parser {
    scanner: Scanner,
    compiling_chunk: Chunk,
    current: Option<Token>,
    previous: Option<Token>,
    had_error: bool,
    panic_mode: bool,
}

impl Parser {
    pub fn new(source: &str) -> Self {
        let scanner = scanner::Scanner::new(source);
        Self {
            scanner,
            compiling_chunk: Chunk::default(),
            current: None,
            previous: None,
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn compile(mut self) -> Option<Chunk> {
        self.advance().unwrap();
        self.expression();
        self.consume(TokenType::Eof, "Expected end of expression.");
        self.end_compiler();

        if self.had_error {
            None
        } else {
            Some(self.compiling_chunk)
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn number(&mut self) {
        let value: f64 = self.previous.as_ref().unwrap().value.parse().unwrap();
        self.emit_constant(value.into());
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression");
    }

    fn unary(&mut self) {
        let operator_type = self.previous.as_ref().unwrap().type_;

        // compile the operand
        self.parse_precedence(Precedence::Unary);

        // Emit the operator instruction
        match operator_type {
            TokenType::Minus => self.emit_byte(OpCode::Negate as u8),
            _ => unreachable!(),
        }
    }

    fn binary(&mut self) {
        let operator_type = self.previous.as_ref().unwrap().type_;
        let rule = self.get_rule(operator_type);
        self.parse_precedence(rule.precedence.next());

        match operator_type {
            TokenType::Plus => self.emit_byte(OpCode::Add as u8),
            TokenType::Minus => self.emit_byte(OpCode::Subtract as u8),
            TokenType::Star => self.emit_byte(OpCode::Multiply as u8),
            TokenType::Slash => self.emit_byte(OpCode::Divide as u8),
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

        prefix_rule(self);

        while precedence
            <= self
                .get_rule(self.current.as_ref().unwrap().type_)
                .precedence
        {
            self.advance().unwrap();
            if let Some(infix_rule) = self.get_rule(self.previous.as_ref().unwrap().type_).infix {
                infix_rule(self);
            }
        }
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
        if !self.had_error {
            #[cfg(debug_assertions)]
            self.current_chunk().disassemble("code");
        }
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return as u8);
    }

    fn error_from_scanner(&mut self, e: scanner::Error) {
        self.error_at(e.line, e.into());
    }

    fn error(&mut self, line: usize, e: Box<dyn std::error::Error>) {
        self.error_at(line, e)
    }

    fn error_at(&mut self, line: usize, e: Box<dyn std::error::Error>) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprintln!("[line {}]: Error: {}", line, e);
    }
}
