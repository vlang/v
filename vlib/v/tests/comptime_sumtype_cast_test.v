pub type Any = bool | int | map[string]Any | string

struct StructType {
mut:
	val string
}

fn test_main() {
	struct_to_map_string()
}

fn map_from[T](t T) {
	$if T is $struct {
		$for field in T.fields {
			assert Any('string').str() == "Any('string')"
			assert t.$(field.name).str() == 'true'
			a := t.$(field.name)
			assert Any(a).str() == "Any('true')"
			assert Any(t.$(field.name)).str() == "Any('true')"
		}
	}
}

fn struct_to_map_string() {
	struct_type := StructType{
		val: 'true'
	}
	map_from(struct_type)
}
