struct Test {
	a int
	b []int
	c map[int]string
	d []?int
}

fn test_main() {
	mut i := 1
	$for f in Test.fields {
		type_name := typeof(f.$(f.name)).name
		match f.typ {
			int { assert i == 1, '1. ${f.name} is ${type_name}' }
			[]int { assert i == 2, '2. ${f.name} is ${type_name}' }
			map[int]string { assert i == 3, '3. ${f.name} is ${type_name}' }
			[]?int { assert i == 4, '4. ${f.name} is ${type_name}' }
			else {}
		}
		i++
	}
}
