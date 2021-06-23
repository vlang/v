module submodule

/*
This submodule just imports its sibling submodules.
Note that they are NOT under 'submodule' itself,
but are in its parent mod1 , and mod1 has a 'v.mod' file.
*/
import v.tests.project_with_modules_having_submodules.mod1.mod11
import v.tests.project_with_modules_having_submodules.mod1.mod12
import v.tests.project_with_modules_having_submodules.mod1.mod13
import v.tests.project_with_modules_having_submodules.mod1.mod14

pub fn f() int {
	return 1000 + mod11.f() + mod12.f() + mod13.f() + mod14.f()
}
