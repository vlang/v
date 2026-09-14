module main

import iface_mod
import sum_mod

fn test_an_interface_keeps_its_own_module_when_another_module_has_a_sum_type_of_the_same_name() {
	holder := iface_mod.Holder{
		label: 'hi'
	}
	value := holder.get() or { panic('expected a value') }
	assert value is string
	assert sum_mod.describe(sum_mod.Any('hi')) == 'string'
	assert sum_mod.describe(sum_mod.Any(1)) == 'int'
}
