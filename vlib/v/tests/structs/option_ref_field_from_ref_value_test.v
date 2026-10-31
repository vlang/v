import imported_defaults_dep

// A `?&dep.StyleValue` field set from a `&dep.StyleValue` value must wrap the
// pointer. When the current module has its own type with the same short name
// (`StyleValue` below), the value kept its plain pointer type, and cgen's
// short-name pointer/value match dereferenced it into the option field:
// `.style_value = *(value)`.

struct Keyword {
	k int
}

struct Length {
	l int
}

type StyleValue = Keyword | Length

enum Important {
	no
	yes
}

struct Property {
mut:
	important Important                         = .no
	value     &imported_defaults_dep.StyleValue = unsafe { nil }
}

struct CSSProperty {
mut:
	name        string
	important   bool
	style_value ?&imported_defaults_dep.StyleValue
}

fn new_css_property(value &imported_defaults_dep.StyleValue, important Important) CSSProperty {
	return CSSProperty{
		name:        'color'
		important:   important == .yes
		style_value: unsafe { value }
	}
}

fn css_properties(properties []Property) []CSSProperty {
	mut result := []CSSProperty{}
	for property in properties {
		result << CSSProperty{
			name:        'width'
			important:   property.important == .yes
			style_value: unsafe { property.value }
		}
	}
	return result
}

fn test_ref_value_in_option_ref_field_with_same_named_local_type() {
	value := &imported_defaults_dep.StyleValue{
		n: 5
	}
	property := new_css_property(value, .yes)
	assert property.important
	style_value := property.style_value or { panic('missing style value') }
	assert style_value == value
	assert style_value.n == 5
	local := StyleValue(Keyword{
		k: 1
	})
	assert local is Keyword
}

fn test_ref_field_in_option_ref_field_with_same_named_local_type() {
	value := &imported_defaults_dep.StyleValue{
		n: 7
	}
	properties := css_properties([Property{
		important: .yes
		value:     value
	}])
	assert properties.len == 1
	assert properties[0].important
	style_value := properties[0].style_value or { panic('missing style value') }
	assert style_value == value
	assert style_value.n == 7
}
