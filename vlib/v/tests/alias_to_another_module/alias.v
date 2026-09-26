module alias_to_another_module

import another_module

pub type MyAlias = another_module.SomeStruct

pub fn (m MyAlias) alias_method() int {
	return 42
}
