struct Placeholder {
    name string
}

struct FnStruct {
mut:
    array_of_fn []fn(int, &Placeholder, string)bool
}

fn test_fn_array_direct_call() {
    mut fs := FnStruct{}
    fs.array_of_fn << fn(x int, y &Placeholder, z string) bool {
        return false
    }

    println(fs.array_of_fn[0](1, &Placeholder{name: 'Bob'}, 'Builder'))
    assert fs.array_of_fn[0](1, &Placeholder{name: 'Bob'}, 'Builder') == false
}

fn test_fn_map_direct_call() {
	a := {
		'aaa': fn()string {return 'aaa'},
		'bbb': fn()string {return 'bbb'},
	}
	println(a['aaa']())
    println(a['bbb']())
    assert a['aaa']() == 'aaa'
    assert a['bbb']() == 'bbb'
}
