struct Page {
    contents int
}

fn test_array_append_short_struct() {
	mut pages := []Page{}
	pages << {
		contents: 3
	}
	println(pages)
	assert pages == [Page{contents: 3}]
}

struct Container {
pub mut:
	name string
}

fn test_array_insert_or_prepend_short_struct() {
	mut a := []Container{}
	a.prepend({name: 'a'})
	a.insert(0, {name: 'b'})
	println(a)
	assert a == [Container{name: 'b'}, Container{name: 'a'}]
}
