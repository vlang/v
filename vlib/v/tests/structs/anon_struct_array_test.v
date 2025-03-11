module main

fn test_main() {
	tsts := [struct {
		name: 'Vlang'
		age:  20
	}]
	for tst in tsts {
		assert '${tst}' == "struct {
    name: 'Vlang'
    age: 20
}"
		assert tst.age == 20
		assert tst.name == 'Vlang'
	}
}
