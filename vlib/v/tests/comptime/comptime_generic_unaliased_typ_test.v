struct Struct {
	a int
}

type StructAlias = Struct

fn test_main() {
	mut a := StructAlias(Struct{})
	assert alias_first(a) == 'alias'
	assert struct_first(a) == 'struct'
}

fn alias_first[T](val T) string {
	$if T is $alias {
		return 'alias'
	} $else $if T is $struct {
		return 'struct'
	} $else {
		return 'else'
	}
}

fn struct_first[T](val T) string {
	$if T is $struct {
		return 'struct'
	} $else $if T is $alias {
		$if T.unaliased_typ is $struct {
			return 'struct'
		} $else {
			return 'alias'
		}
	} $else {
		return 'else'
	}
}
