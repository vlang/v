interface FieldNamed {
	name() string
}

struct FieldLabel {
	text string
}

fn (label &FieldLabel) name() string {
	return label.text
}

struct FieldWrapper {
	inner FieldNamed
}

fn (wrapper &FieldWrapper) name() string {
	return wrapper.inner.name()
}

fn format_field(value FieldNamed) string {
	return match value {
		FieldWrapper { 'chan ${value.inner.name()}{}' }
		else { value.name() }
	}
}

fn same_field(left FieldNamed, right FieldNamed) bool {
	if left is FieldWrapper {
		if right is FieldWrapper {
			return left.inner.name() == right.inner.name()
		}
	}
	return false
}

fn test_smartcast_interface_field_method_receiver() {
	first := FieldNamed(FieldWrapper{ inner: FieldLabel{'int'} })
	second := FieldNamed(FieldWrapper{ inner: FieldLabel{'string'} })
	assert format_field(first) == 'chan int{}'
	assert same_field(first, first)
	assert !same_field(first, second)
	assert format_field(FieldLabel{'bool'}) == 'bool'
}
