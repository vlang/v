struct Page {
    contents int
}

fn test_array_append_short_struct() {
	mut pages := []Page{}
	pages << {
		contents: 3
	}
	println(pages)
	assert pages == [Page{ contents: 3}]
}
