module driver

import v.flat
import v.types

// start_program_instance_check needs fork(): on Windows a build leaves the
// instances of the program's generics to the C compiler, as before.
fn start_program_instance_check(mut a flat.FlatAst, mut tc types.TypeChecker, is_checker_fixture bool, fatal_errors bool, message_limit int, skip_notices bool) ProgramInstanceCheck {
	return ProgramInstanceCheck{}
}

fn (check ProgramInstanceCheck) finish() int {
	return -1
}
