import net.http

fn test_short_enum_syntax_across_module() {
	header := http.new_header_from_map({
		.content_type: 'application/json'
	})

	println(header)
	assert true
}
