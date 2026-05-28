// Redeclaring an existing C type alias should be accepted. Before the
// parser was setting TypeSymbol.language=.c for C aliases, the table's
// "Allow C type aliases to override existing C types" override never fired
// and the second declaration failed with `cannot register alias`.

type C.RedeclareU16Alias = u16
type C.RedeclareU16Alias = u16

fn test_c_alias_redeclaration_compiles() {
	assert true
}
