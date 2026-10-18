// A `match`/`if` whose branches all yield `none` is valid where an option is
// expected, such as the value of `return` in a `?T` function.
enum AllNoneSheetKind {
	style_element
	link_element
	user_agent
}

fn all_none_match_source(kind AllNoneSheetKind, url ?string) ?string {
	return match kind {
		.style_element {
			none
		}
		.link_element {
			u := url or { return none }
			if u == 'found.css' {
				return 'body {}'
			}
			none
		}
		.user_agent {
			none
		}
	}
}

fn all_none_if_source(flag bool) ?string {
	return if flag { none } else { none }
}

fn test_all_none_match_in_option_return() {
	assert all_none_match_source(.style_element, 'found.css') == none
	assert all_none_match_source(.link_element, 'found.css') or { '' } == 'body {}'
	assert all_none_match_source(.link_element, 'other.css') == none
	assert all_none_match_source(.link_element, none) == none
	assert all_none_match_source(.user_agent, 'found.css') == none
}

fn test_all_none_if_in_option_return() {
	assert all_none_if_source(true) == none
	assert all_none_if_source(false) == none
}
