import x.json2

fn test_json2_any_free() {
	c := json2.Any(123)
	unsafe { c.free() }
	assert true
}

fn test_json2_any_free_recursive_variants() {
	arr := json2.Any([json2.Any('abc')])
	obj := json2.Any({
		'key': json2.Any('value')
	})
	unsafe {
		arr.free()
		obj.free()
	}
	assert true
}
