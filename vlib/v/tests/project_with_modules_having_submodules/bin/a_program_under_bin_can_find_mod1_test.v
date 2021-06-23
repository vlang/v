import v.tests.project_with_modules_having_submodules.mod1.submodule as m

fn test_mod1_can_still_be_found_through_parent_project_vmod() {
	assert 1051 == m.f()
}

/*
NB: this main program is under bin/ , but it still
can find mod1, because the parent project has v.mod,
so v module lookup for this program will find mod1 through
relation to the parent v.mod file
*/
