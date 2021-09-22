
#[litem::template("tests/clicker.html", escape="html")]
pub struct ClickerTmpl<'a> {
    num: u32,
    text: &'a str,
}

#[litem::template("tests/test.html", escape="html")]
pub struct TestTmpl<'a> {
    hello: &'a str,
    nums: &'a [u32],
    raw: &'a str,
}

#[test]
fn test() {
    let rendered = TestTmpl {
        hello: "litem! <script>alert('hacked!')</script>",
        nums: &[5, 2, 77, 34, 6823],
        raw: "<b>this is bold!</b>"
    }.render_string().unwrap();
    println!("{}", rendered);
}