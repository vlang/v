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

// field_type_names infers `T` from every field of `value`.
pub fn field_type_names[T](value T) []string {
	mut out := []string{}
	$for field in T.fields {
		out << type_name(value.$(field.name))
	}
	return out
}

// values_of flattens the public scalar values of `value`, recursing through the
// specializations inferred from its fields and array elements.
pub fn values_of[T](value T) []string {
	$if T is string {
		return [value]
	} $else $if T is $int {
		return [value.str()]
	} $else $if T is $array {
		mut out := []string{}
		for item in value {
			out << values_of(item)
		}
		return out
	} $else $if T is $struct {
		mut out := []string{}
		$for field in T.fields {
			$if field.is_pub {
				out << values_of(value.$(field.name))
			}
		}
		return out
	} $else {
		return []string{}
	}
}
