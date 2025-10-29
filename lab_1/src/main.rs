use std::io;
use std::env;
use std::cmp::Ordering;


fn parse_number_from_string(s: &String) -> Option<f32> {
    match s.trim().parse::<f32>() {
        Ok(result) => {
            if result.is_nan() || result.is_infinite() {
                return None;
            }

            return Some(result);
        },
        Err(_) => return None,
    }
}

fn read_coefs(msg: &'static str, i: usize) -> f32 {
    let mut input_str = String::new();

    if let Some(value) = env::args().nth(i) {
        match parse_number_from_string(&value) {
            Some(num) => return num,
            None => println!("Argument {} cannot be parsed!", i),
        };
    }

    loop {
        input_str.clear();
        println!("{}", msg);

        match io::stdin().read_line(&mut input_str) {
            Ok(_) => {},
            Err(_) => {
                println!("Failed to read line!");
                continue;
            }
        };

        match parse_number_from_string(&input_str) {
            Some(num) => return num,
            None => println!("Please, enter correct number!"),
        };
    }
}

fn print_roots_for(a: f32, b: f32, d_sqrt: f32, msg: &'static str) {
    println!("{}", msg);

    let value = (-b + d_sqrt) / (2.0 * a);

    match value.total_cmp(&0.0) {
        Ordering::Less => println!("No roots"),
        Ordering::Equal => println!("Root: 0.0"),
        Ordering::Greater => {
            let value = value.sqrt();
            println!("Roots: {} {}", value, -value);
        }
    }
}

fn main() {
    let a = read_coefs("Enter A coef:", 1);
    let b = read_coefs("Enter B coef:", 2);
    let c = read_coefs("Enter C coef:", 3);

    if a == 0.0 {
        let value = -c / b;

        match value.total_cmp(&0.0) {
            Ordering::Less => println!("No roots"),
            Ordering::Equal => println!("Root: 0.0"),
            Ordering::Greater => {
                let value = value.sqrt();
                println!("Roots: {} {}", value, -value);
            }
        }

        return;
    }

    let d = b * b - 4.0 * a * c;

    if d < 0.0 {
        println!("(D < 0) => no roots");
        return;
    }

    let d_sqrt = d.sqrt();

    if d_sqrt == 0.0 {
        print_roots_for(a, b, 0.0, "sqrt(D) == 0.0:");
        return;
    }

    print_roots_for(a, b, d_sqrt, "First case:");
    print_roots_for(a, b, -d_sqrt, "Second case:");
}
