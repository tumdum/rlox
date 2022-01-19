use crate::value::{NativeFunction, Value};
use crate::VM;
use std::rc::Rc;

impl VM {
    pub fn register_bulitins(&mut self) {
        self.define_native("time", move |_args| {
            match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
                Ok(t) => t.as_secs_f64().into(),
                Err(_) => Value::Nil,
            }
        });

        let output_clone = self.output.clone();
        self.define_native("println", move |args| {
            for (i, arg) in args.iter().enumerate() {
                if i == 0 {
                    write!(output_clone.borrow_mut(), "{}", arg).unwrap();
                } else {
                    write!(output_clone.borrow_mut(), " {}", arg).unwrap();
                }
            }
            writeln!(output_clone.borrow_mut()).unwrap();
            Value::Nil
        });

        let input = self.input.clone();
        let allocator = self.allocator.clone();
        self.define_native("readln", move |args| {
            assert!(args.is_empty());
            let mut buf = String::new();
            use std::io::BufRead;
            use std::ops::DerefMut;
            match std::io::BufReader::new(input.borrow_mut().deref_mut()).read_line(&mut buf) {
                Ok(_) => allocator.borrow_mut().allocate_string(buf),
                Err(e) => {
                    eprintln!("readln failed: {}", e);
                    Value::Nil
                }
            }
        });

        let allocator = self.allocator.clone();
        self.define_native("vec", move |args| {
            allocator.borrow_mut().allocate_vector(args.to_vec())
        });
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
}
