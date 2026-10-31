enum AppendMode {
	first
	second
}

struct AppendHolder {
mut:
	modes []AppendMode
}

fn test_enum_append_conditional_context() {
	mut holder := AppendHolder{}
	for yes in [true, false] {
		holder.modes << if yes { .first } else { .second }
		holder.modes << (match yes {
			true { .first }
			false { .second }
		})
		holder.modes << if yes {
			value := AppendMode.first
			value
		} else {
			.second
		}
	}
	assert holder.modes == [.first, .first, .first, .second, .second, .second]
}
