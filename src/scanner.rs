use maplit::hashmap;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use thiserror::Error;

static KEYWORDS: Lazy<HashMap<&str, TokenType>> = Lazy::new(|| {
    hashmap! {
        "and" => TokenType::And,
        "class" => TokenType::Class,
        "else" => TokenType::Else,
        "false" => TokenType::False,
        "for" => TokenType::For,
        "fun" => TokenType::Fun,
        "if" => TokenType::If,
        "nil" => TokenType::Nil,
        "or" => TokenType::Or,
        "print" => TokenType::Print,
        "return" => TokenType::Return,
        "super" => TokenType::Super,
        "this" => TokenType::This,
        "true" => TokenType::True,
        "var" => TokenType::Var,
        "while" => TokenType::While,
    }
});

#[derive(Debug)]
pub struct Scanner {
    source: Vec<char>,
    current: usize,
    line: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub type_: TokenType,
    pub line: usize,
    pub value: String,
}

#[derive(Debug, Error)]
#[error("Error at line {line}: {detail}")]
pub struct Error {
    pub line: usize,
    pub detail: ErrorDetail,
}

#[derive(Debug, Error)]
pub enum ErrorDetail {
    #[error("Unexpected character: '{0}'")]
    UnexpectedCharacter(char),
    #[error("Unterminated string")]
    UnterminatedString,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier,
    String,
    Number,
    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            current: 0,
            line: 1,
        }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn scan_token(&mut self) -> Result<Token, Error> {
        self.skip_whitespace();
        let current = self.current;
        if self.is_at_end() {
            return Ok(self.make_token(current, TokenType::Eof));
        }

        let c = self.advance();

        if c.is_ascii_alphabetic() || c == '_' {
            return self.identifier();
        }
        if c.is_digit(10) {
            return self.number();
        }

        match c {
            '(' => return Ok(self.make_token(current, TokenType::LeftParen)),
            ')' => return Ok(self.make_token(current, TokenType::RightParen)),
            '{' => return Ok(self.make_token(current, TokenType::LeftBrace)),
            '}' => return Ok(self.make_token(current, TokenType::RightBrace)),
            ';' => return Ok(self.make_token(current, TokenType::Semicolon)),
            ',' => return Ok(self.make_token(current, TokenType::Comma)),
            '.' => return Ok(self.make_token(current, TokenType::Dot)),
            '-' => return Ok(self.make_token(current, TokenType::Minus)),
            '+' => return Ok(self.make_token(current, TokenType::Plus)),
            '/' => return Ok(self.make_token(current, TokenType::Slash)),
            '*' => return Ok(self.make_token(current, TokenType::Star)),
            '!' => {
                let tt = if self.matches('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                return Ok(self.make_token(current, tt));
            }
            '=' => {
                let tt = if self.matches('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                return Ok(self.make_token(current, tt));
            }
            '<' => {
                let tt = if self.matches('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                return Ok(self.make_token(current, tt));
            }
            '>' => {
                let tt = if self.matches('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                return Ok(self.make_token(current, tt));
            }
            '"' => {
                return self.string();
            }
            _ => {}
        }

        Err(Error {
            line: self.line,
            detail: ErrorDetail::UnexpectedCharacter(c),
        })
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source[self.current] != expected {
            return false;
        }
        self.current += 1;
        return true;
    }

    fn make_token(&self, start: usize, token_type: TokenType) -> Token {
        Token {
            type_: token_type,
            line: self.line,
            value: self.source[start..self.current].iter().cloned().collect(),
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            if self.is_at_end() {
                break;
            }
            let c = self.peek();
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == Some('/') {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        todo!()
                    }
                }
                _ => break,
            }
        }
    }

    fn peek(&self) -> char {
        self.source[self.current]
    }

    fn peek_next(&self) -> Option<char> {
        self.source.get(self.current).cloned()
    }

    fn string(&mut self) -> Result<Token, Error> {
        let current = self.current - 1;
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            return Err(Error {
                line: self.line,
                detail: ErrorDetail::UnterminatedString,
            });
        }

        // The closing quote.
        self.advance();
        Ok(self.make_token(current, TokenType::String))
    }

    fn number(&mut self) -> Result<Token, Error> {
        let current = self.current - 1;
        while self.peek().is_digit(10) {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().map(|c| c.is_digit(10)).unwrap_or(false) {
            // consume '.'
            self.advance();

            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        Ok(self.make_token(current, TokenType::Number))
    }

    fn identifier(&mut self) -> Result<Token, Error> {
        let current = self.current - 1;

        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        Ok(self.make_token(current, self.identifier_type(current)))
    }

    fn identifier_type(&self, start: usize) -> TokenType {
        let key: String = self.source[start..self.current].iter().collect();
        KEYWORDS
            .get(key.as_str())
            .cloned()
            .unwrap_or(TokenType::Identifier)
    }
}
