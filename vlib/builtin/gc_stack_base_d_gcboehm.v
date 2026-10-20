module builtin

// C.GC_stack_base is declared in a `.v` file, not beside the other Boehm bindings in
// builtin_d_gcboehm.c.v: once a program includes any system header, a C struct declared in a
// `.c.v` file is taken to be defined by a header and its layout is not emitted. The generated
// C never includes gc.h, so nothing else defines this one, and `C.GC_stack_base{}` became an
// incomplete type.
pub struct C.GC_stack_base {
	mem_base voidptr
	// reg_base voidptr
}
