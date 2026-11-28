// vtest vflags: -autofree -gc none
// https://github.com/vlang/v/issues/32939: the compiler-provided `clone()` of an
// `IClone` struct must type as the struct in a struct update (`...x.clone()`).

struct Table implements IClone {
	name    string
	columns []string
}

fn test_struct_update_from_default_clone() {
	table := Table{
		name:    'users'
		columns: ['id', 'name']
	}
	copy_all := Table{
		...table.clone()
	}
	assert copy_all.name == 'users'
	assert copy_all.columns == ['id', 'name']
	renamed := Table{
		...table.clone()
		name: 'people'
	}
	assert renamed.name == 'people'
	assert renamed.columns == ['id', 'name']
	assert table.name == 'users'
}
