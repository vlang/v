// A `x == none` check does not narrow `x` in the condition of the `else if`
// that follows it, so an `if y := x {` guard there unwraps the option `x`.
struct Element {
	name string
}

fn find_element(name string) ?&Element {
	if name == '' {
		return none
	}
	return &Element{name}
}

fn context_name(name string) string {
	mut context_element := find_element(name)
	if context_element == none {
		context_element = &Element{'body'}
	} else if element := context_element {
		if element.name == 'html' {
			context_element = &Element{'html body'}
		}
	}
	element := context_element or { return 'none' }
	return element.name
}

fn guarded_after_chain(value ?int, skip bool) int {
	if value == none {
		return -1
	} else if skip {
		return 0
	} else if v := value {
		return v * 2
	}
	return -2
}

fn test_if_guard_in_else_if_after_none_check() {
	assert context_name('') == 'body'
	assert context_name('html') == 'html body'
	assert context_name('div') == 'div'
}

fn test_if_guard_in_later_else_if_after_none_check() {
	assert guarded_after_chain(none, false) == -1
	assert guarded_after_chain(21, true) == 0
	assert guarded_after_chain(21, false) == 42
}
