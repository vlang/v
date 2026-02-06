type Any = string | int

fn (m map[string]Any) to_toml() string {
	mut t := ''
	return t
}

fn test_main() {
	mut doc := map[string]Any{}
	_ := encode(doc)
}

fn encode[T](typ T) string {
	$for method in T.methods {
		$if method.name == 'to_toml' {
			return typ.$method()
		}
	}
	mp := encode_struct[T](typ)
	return mp.to_toml()
}

fn encode_struct[T](typ T) map[string]Any {
	mut mp := map[string]Any{}
	return mp
}
