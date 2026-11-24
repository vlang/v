module c

import os

fn test_export_prefers_its_own_file_abi_and_keeps_first_fallback() ! {
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'export_local_abi_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'api.h'), 'int local_abi_increment(int value);\nint local_abi_consumer(void);\n')!
	os.write_file(os.join_path(root, 'consumer.c'), '#include "api.h"
#include <limits.h>
int local_abi_consumer(void) {
    return local_abi_increment(41) != 42 || local_abi_increment(-1234) != -1233 ||
        local_abi_increment(INT_MAX - 1) != INT_MAX;
}
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main\nfn C.local_abi_consumer() i32\nfn main() { assert C.local_abi_consumer() == 0 }\n')!
	exported := "@[export: 'local_abi_increment']\nfn implementation(value int) int { return value + 1 }\n"
	exact := 'fn C.local_abi_increment(value i32) i32\n'
	broad := 'fn C.local_abi_increment(value i64) i64\n'
	preamble := 'module main\n#include "@DIR/api.h"\n#flag @DIR/consumer.c\n'
	for local_first in [false, true] {
		for declaration_first in [false, true] {
			local_name := if local_first { 'a_local.c.v' } else { 'z_local.c.v' }
			other_name := if local_first { 'z_other.c.v' } else { 'a_other.c.v' }
			local := if declaration_first { exact + exported } else { exported + exact }
			os.write_file(os.join_path(root, local_name), preamble + local)!
			os.write_file(os.join_path(root, other_name), 'module main\n' + broad)!
			built := os.exec([@VEXE, '-cc', cc, '-gc', 'none', '-nocache', '-no-retry-compilation',
				'run', root])
			assert built.exit_code == 0, built.output
			os.rm(os.join_path(root, local_name))!
			os.rm(os.join_path(root, other_name))!
		}
	}
	// Without a declaration beside the V definition, retain the existing first
	// declaration in that module, even when a broader declaration follows it.
	os.write_file(os.join_path(root, 'a_exact.c.v'), 'module main\n' + exact)!
	os.write_file(os.join_path(root, 'b_other.c.v'), 'module main\n' + broad)!
	os.write_file(os.join_path(root, 'z_export.c.v'), preamble + exported)!
	built := os.exec([@VEXE, '-cc', cc, '-gc', 'none', '-nocache', '-no-retry-compilation', 'run',
		root])
	assert built.exit_code == 0, built.output
}
