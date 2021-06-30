struct Foo {}
type Bar = Foo

fn test_alias_str() {
	bar := Bar{}
	println(bar)
}
