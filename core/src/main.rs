pub mod calc;
pub mod ast; //::calc;

#[test]
fn calculator4() {
    assert_eq!(&format!("{:?}", calc::parse_Expr("a - 22 * 44 + 66").unwrap()),
               "((a - (22 * 44)) + 66)");
}

#[cfg(not(test))]
fn main() {
  assert_eq!(1, 1);
}
