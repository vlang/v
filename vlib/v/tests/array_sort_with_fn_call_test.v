struct Info {
mut:
	fields []string
}

fn test_sort_with_fn_call() {
	mut info := Info{
		fields: ['aaa(', 'b(']
	}
	info.fields.sort(a.before('(').len < b.before('(').len)
	println(info.fields)
	assert info.fields == ['b(', 'aaa(']
}
