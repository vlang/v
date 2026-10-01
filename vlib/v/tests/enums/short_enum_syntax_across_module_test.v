import net.http

fn cookie_same_site(cookie http.Cookie) http.SameSite {
	return cookie.same_site
}

fn test_short_enum_syntax_across_module() {
	header := http.new_header_from_map({
		.content_type: 'application/json'
	})

	println(header)
	assert true
}

fn test_short_enum_syntax_in_collapsed_struct_call_across_module() {
	assert cookie_same_site(same_site: .same_site_lax_mode) == .same_site_lax_mode
}
