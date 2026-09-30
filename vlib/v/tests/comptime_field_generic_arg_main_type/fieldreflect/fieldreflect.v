module fieldreflect

// Cell deliberately shares its short name with a type declared by the caller.
struct Cell {
	column int
}

// local_cell_count keeps the module's own `Cell` in use.
pub fn local_cell_count() int {
	cells := [Cell{
		column: 1
	}]
	return cells.len
}

// type_name returns the name of the inferred `T`.
pub fn type_name[T](_ T) string {
	return typeof[T]().name
}

// public_fields returns the names of the public fields of `T`.
pub fn public_fields[T](_ T) []string {
	mut names := []string{}
	$for field in T.fields {
		$if field.is_pub {
			names << field.name
		}
	}
	return names
}

// element_fields returns the public field names of the struct element type `E`.
pub fn element_fields[E](_ []E) []string {
	$if E is $struct {
		return public_fields[E](E{})
	} $else {
		return []string{}
	}
}

// element_type_name returns the name of the inferred element type `E`.
pub fn element_type_name[E](_ []E) string {
	return typeof[E]().name
}

// array_field_element_fields infers `E` from each array field of `value`.
pub fn array_field_element_fields[T](value T) []string {
	mut out := []string{}
	$for field in T.fields {
		$if field.typ is $array {
			out << element_fields(value.$(field.name))
		}
	}
	return out
}

// array_field_element_names infers `E` from each array field of `value`.
pub fn array_field_element_names[T](value T) []string {
	mut out := []string{}
	$for field in T.fields {
		$if field.typ is $array {
			out << element_type_name(value.$(field.name))
		}
	}
	return out
}

// struct_field_type_names infers `T` from each struct field of `value`.
pub fn struct_field_type_names[T](value T) []string {
	mut out := []string{}
	$for field in T.fields {
		$if field.typ is $struct {
			out << type_name(value.$(field.name))
		}
	}
	return out
}

struct Shape {
	kind    string
	element &Shape = unsafe { nil }
mut:
	fields map[string]Shape
}

fn (s Shape) str() string {
	if s.kind == 'list' {
		return '[' + (*s.element).str() + ']'
	}
	if s.kind == 'object' {
		mut keys := s.fields.keys()
		keys.sort()
		mut parts := []string{}
		for key in keys {
			field := s.fields[key] or { Shape{} }
			parts << '${key}:${field.str()}'
		}
		return '{' + parts.join(',') + '}'
	}
	return s.kind
}

fn element_shape[E](_ []E) Shape {
	$if E is $struct {
		return shape_of_value[E](E{})
	} $else {
		return shape_of_value[E]($zero(E))
	}
}

fn shape_of_value[T](value T) Shape {
	$if T is string {
		return Shape{
			kind: 'string'
		}
	} $else $if T is $int {
		return Shape{
			kind: 'number'
		}
	} $else $if T is $array {
		// `value` is forwarded as `[]Cell` from a nested specialization.
		element := element_shape(value)
		return Shape{
			kind:    'list'
			element: &element
		}
	} $else $if T is $struct {
		mut fields := map[string]Shape{}
		$for field in T.fields {
			$if field.is_pub {
				fields[field.name] = shape_of_value(value.$(field.name))
			}
		}
		return Shape{
			kind:   'object'
			fields: fields
		}
	} $else {
		return Shape{}
	}
}

// shape_of describes the public structure of `value`, recursing through generic
// specializations inferred from its fields and array elements.
pub fn shape_of[T](value T) string {
	return shape_of_value[T](value).str()
}

fn values_of_value[T](value T) []string {
	$if T is string {
		return [value]
	} $else $if T is $int {
		return [value.str()]
	} $else $if T is $array {
		mut out := []string{}
		for item in value {
			out << values_of_value(item)
		}
		return out
	} $else $if T is $struct {
		mut out := []string{}
		$for field in T.fields {
			$if field.is_pub {
				out << values_of_value(value.$(field.name))
			}
		}
		return out
	} $else {
		return []string{}
	}
}

// values_of flattens the public scalar values of `value`.
pub fn values_of[T](value T) []string {
	return values_of_value[T](value)
}
