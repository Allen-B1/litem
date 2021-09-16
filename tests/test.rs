
use litem::template;

#[template("tests/test.html", escape="html")]
pub struct TestTmpl {
    hello: String,
    nums: Vec<u32>,
    raw: String,
}

#[test]
fn test() {
    let rendered = TestTmpl {
        hello: "litem! <script>alert('hacked!')</script>".to_string(),
        nums: vec![5, 2, 77, 34, 6823],
        raw: "<b>this is bold!</b>".to_string()
    }.render_string().unwrap();
    println!("{}", rendered);
}