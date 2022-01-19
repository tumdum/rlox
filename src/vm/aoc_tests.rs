use crate::vm::VM;
use assert_matches::assert_matches;
use std::cell::RefCell;
use std::rc::Rc;

#[test]
fn year_2021_day_01() {
    let output = Rc::new(RefCell::new(vec![]));
    let code = include_str!("../../examples/aoc2021/01/p1.lox");
    let input: &[u8] = include_bytes!("../../examples/aoc2021/01/input");
    let input = Rc::new(RefCell::new(input));
    let mut vm = VM::new(output.clone(), input);
    vm.register_bulitins();
    let got = vm.interpret(code);
    assert_matches!(got, Ok(_));
    let output = std::str::from_utf8(&output.borrow()).unwrap().to_owned();
    let output = output.trim();
    assert_eq!(output, "part1: 1602\npart2: 1633");
}
