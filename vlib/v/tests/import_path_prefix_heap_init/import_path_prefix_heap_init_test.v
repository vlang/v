module main

import tsc.other

fn test_module_path_with_import_alias_prefix_heap_initializer() {
	assert other.make() == 3
}
