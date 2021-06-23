import v.tests.project_with_modules_having_submodules.mod1
import v.tests.project_with_modules_having_submodules.mod1.submodule

fn test_mod1() {
	assert 1 == mod1.f()
}

fn test_mod1_submodule_can_find_and_use_all_its_sibling_submodules() {
	assert 1051 == submodule.f()
}
