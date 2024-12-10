import x.json2

fn main() {
	x := '[[],[],[]]'
	println(json2.decode[json2.Any](x)!)
}
