use std::fs::{read_to_string, read_dir};
use std::path::{Path};
use std::process::Command;

#[test]
fn end_to_end_tests() {
    let src = Path::new(file!()).parent().expect("e2e_test file has no parent?").canonicalize().expect("root of project not found...");
    let tests_root = src.join("../tests").canonicalize().expect("expected tests dir to be ../tests from e2e_tests.rs");
    let lox_path = src.join("../target/debug/lox");
    let lox_exe = lox_path.canonicalize().expect("couldn't find lox exe @ {}");

    for test in read_dir(tests_root.clone()).unwrap() {
        let name = test.unwrap().file_name();
        let name_str = name.to_str().unwrap();
        if !name_str.contains("test") {
            continue;
        }
        let number = name_str.split("test").nth(1).unwrap().split(".lox").nth(0).unwrap();

        let res = Command::new(lox_exe.clone())
            .args(&[tests_root.join(name_str)])
            .output()
            .unwrap();

        let expected = read_to_string(tests_root.join(format!("out{}.txt", number.clone())))
            .expect(&format!("Expected a file called out{}.txt to exist", number.clone()));

        let mut stdout = String::from_utf8(res.stdout).unwrap();
        let mut stderr = String::from_utf8(res.stderr).unwrap();


        stdout.pop(); // rm \n
        println!("test {}", number);
        println!("Actual: {}", stdout);
        println!("Expected: {}", expected);
        if stderr.len() != 0 {
            eprintln!("Stderr: {}", stderr);
        }
        assert_eq!(expected, stdout);
    }
}