module modulecache

import os

fn test_file_metadata_helper_uses_generated_u64_abi() {
	header_path := os.join_path(@VEXEROOT, 'vlib', 'v3', 'modulecache', 'file_metadata.c')
	header := os.read_file(header_path) or { panic(err) }
	signature := 'static int v3_modulecache_file_metadata(const char *path, u64 *device, u64 *inode,
	u64 *size, u64 *mtime_seconds, u64 *mtime_nanoseconds,
	u64 *ctime_seconds, u64 *ctime_nanoseconds)'
	assert header.count(signature) == 2
	assert !header.contains('uint64_t *')

	cc := os.find_abs_path_of_executable('cc') or { return }
	temp_dir := os.join_path(os.temp_dir(), 'v3_modulecache_file_metadata_${os.getpid()}')
	os.mkdir_all(temp_dir) or { panic(err) }
	defer {
		os.rmdir_all(temp_dir) or {}
	}

	header_include := header_path.replace('\\', '/')
	aliases := ['typedef unsigned long long u64;', 'typedef uint64_t u64;']
	branches := ['', '#undef __APPLE__\n#undef __linux__']
	for alias_index, alias in aliases {
		for branch_index, branch in branches {
			source_path := os.join_path(temp_dir, 'abi_${alias_index}_${branch_index}.c')
			source := '#include <stdint.h>
${alias}
${branch}
#include "${header_include}"

int main(void) {
	u64 device = 0;
	u64 inode = 0;
	u64 size = 0;
	u64 mtime_seconds = 0;
	u64 mtime_nanoseconds = 0;
	u64 ctime_seconds = 0;
	u64 ctime_nanoseconds = 0;
	return v3_modulecache_file_metadata("", &device, &inode, &size, &mtime_seconds,
		&mtime_nanoseconds, &ctime_seconds, &ctime_nanoseconds);
}
'
			os.write_file(source_path, source) or { panic(err) }
			result :=
				os.execute('${os.quoted_path(cc)} -D_DEFAULT_SOURCE -std=c99 -fsyntax-only -Werror=incompatible-pointer-types ${os.quoted_path(source_path)}')
			assert result.exit_code == 0, result.output
		}
	}
}
