use crate::scanner::{Scanner, TokenType};

pub fn compile(source: &str) {
    let mut scanner = Scanner::new(source);

    let mut line = usize::max_value();
    loop {
        let token = scanner.scan_token().unwrap();
        if token.line != line {
            print!("{:4} ", token.line);
            line = token.line;
        } else {
            print!("   | ");
        }
        println!("{:2?} '{}'", token.type_, token.value);

        if token.type_ == TokenType::Eof {
            break;
        }
    }
}
