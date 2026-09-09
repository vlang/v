// issue #11379: this used to panic in the checker for `-b js_browser`.
fn main() {
	JS.print('hello vtic', 32, 32)
}
