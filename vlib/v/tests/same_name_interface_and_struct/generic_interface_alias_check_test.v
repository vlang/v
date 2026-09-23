import os

const vexe = os.quoted_path(@VEXE)
const generic_interface_alias_project = os.join_path(os.dir(@FILE), 'generic_interface_alias_project')

// Inferring `T` from a `csv.Reader[T]` parameter, where `csv` is an import alias
// of a module with a generic `Reader` interface, must resolve the alias instead of
// matching the loaded `encoding.csv.Reader` struct. The project is only
// type-checked: generating code for generic interfaces from other modules is a
// separate problem.
fn test_generic_interface_through_alias_named_like_a_loaded_struct_checks_cleanly() {
	res := os.execute('${vexe} -check ${os.quoted_path(generic_interface_alias_project)}')
	assert res.exit_code == 0, res.output
}
