@[abc]
struct Another {
	a []int
	b u8
	c u32
}

fn (f Another) test() {}

enum Abc {
	a
	b
	c
}

fn test_main() {
	mut c := 0
	$for f in Abc.values {
		dump(f)
		dump(f.value)
		c += 1
		assert typeof(f).name == 'EnumData'
	}
	assert c == 3

	$for f in Another.fields {
		dump(f)
		dump(f.name)
		c += 1
	}
	assert c == 6

	$for f in Another.methods {
		dump(f)
		dump(f.name)
		c += 1
		assert typeof(f).name == 'FunctionData'
	}
	assert c == 7

	$for f in Another.attributes {
		dump(f)
		dump(f.name)
		c += 1
		assert typeof(f).name == 'VAttribute'
	}
	assert c == 8
}
