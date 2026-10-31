module driver

import os
import v.cmdexec
import v.gen.c as cgen

// cross_libc_headers_output_env names the file that a child run writes the
// header-based libc variant of a portable compiler snapshot to.
const cross_libc_headers_output_env = 'V3_CROSS_LIBC_HEADERS_OUTPUT'

// add_cross_libc_headers_variant makes the portable compiler snapshot at `path`
// (vc/v.c, from `v -cross cmd/v`) serve 32-bit hosts as well. That snapshot spells
// out every libc declaration it needs, with the layouts of 64-bit targets. On
// 32-bit targets the layouts differ between C libraries (glibc and musl disagree
// on `time_t`, `off_t` and `struct stat`), and only the system headers know them.
// A second run of this compiler writes the same snapshot with the system headers,
// and the two are merged, so that the C compiler picks the headers on 32-bit targets.
fn add_cross_libc_headers_variant(path string) ! {
	headers_path := '${path}.headers.c'
	mut env := map[string]string{}
	env[cross_libc_headers_output_env] = os.abs_path(headers_path)
	result := cmdexec.run_in_merged_with_env(os.executable(), os.args[1..], os.getwd(),
		env)
	if result.exit_code != 0 {
		os.rm(headers_path) or {}
		return error('cannot generate the header-based libc variant of ${path}:\n${result.output}')
	}
	headerless := os.read_file(path)!
	headers := os.read_file(headers_path) or {
		os.rm(headers_path) or {}
		return err
	}
	os.rm(headers_path) or {}
	os.write_file(path, cgen.merge_libc_variants(headerless, headers))!
}
