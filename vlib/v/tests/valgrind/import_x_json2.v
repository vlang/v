import x.json2

fn main() {
	x := '[[],[],[]]'
	println(json2.raw_decode(x)?)
}
