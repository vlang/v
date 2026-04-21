module json2

@[inline]
fn unquote_attr_value(value string) string {
	mut unquoted := value.trim_space()
	if unquoted.len > 1 {
		if (unquoted[0] == `'` && unquoted[unquoted.len - 1] == `'`)
			|| (unquoted[0] == `"` && unquoted[unquoted.len - 1] == `"`) {
			unquoted = unquoted[1..unquoted.len - 1]
		}
	}
	return unquoted
}

@[inline]
fn json_attr_value(attr string) ?string {
	if !attr.starts_with('json:') {
		return none
	}
	return unquote_attr_value(attr[5..])
}

fn json_attr_value_range(attr string) ?(int, int) {
	if !attr.starts_with('json:') {
		return none
	}
	mut start := 5
	for start < attr.len && attr[start] in [` `, `\t`, `\n`, `\r`] {
		start++
	}
	mut end := attr.len
	for end > start && attr[end - 1] in [` `, `\t`, `\n`, `\r`] {
		end--
	}
	if end - start > 1 {
		if (attr[start] == `'` && attr[end - 1] == `'`)
			|| (attr[start] == `"` && attr[end - 1] == `"`) {
			start++
			end--
		}
	}
	return start, end
}

@[inline]
fn enum_uses_json_as_number[T]() bool {
	$for attr in T.attributes {
		if attr.name == 'json_as_number' {
			return true
		}
	}
	return false
}
