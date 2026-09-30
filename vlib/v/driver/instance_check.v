module driver

// ProgramInstanceCheck is the check of the instances of the program's generics
// that a child of a build runs beside the transform (see
// start_program_instance_check): the fields, methods and operators that the
// types of an instance lack, which a check reports too.
struct ProgramInstanceCheck {
	pid    int = -1
	output i32 = -1 // the read end of what the child prints
}
