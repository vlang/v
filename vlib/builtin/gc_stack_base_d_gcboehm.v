module builtin

// C.GC_stack_base is declared in a `.v` file, not beside the other Boehm bindings in
// builtin_d_gcboehm.c.v: once a program includes any system header, a C struct declared in a
// `.c.v` file is taken to be defined by a header and its layout is not emitted. The generated
// C never includes gc.h, so nothing else defined this one, and `C.GC_stack_base{}` became an
// incomplete type. Where a program does include gc.h, cgen leaves the layout to it
// (c_struct_header_guards).
pub struct C.GC_stack_base {
	mem_base voidptr
	// reg_base voidptr
}
