module modulecache

import os

fn test_gnu_extension_typedefs_and_declarations_remain_replicable() ! {
	source := '__extension__ typedef struct {
    long long quot;
    long long rem;
} GnuDiv;
__extension__ /* a declaration marker */ __extension__
typedef unsigned long long GnuBuffer[22];
__extension__ extern long long gnu_result(GnuDiv value);
'
	header := declaration_header(source)
	assert header == source
	assert c_source_is_replicable(source)
	types := c_source_type_declarations(source)
	assert types.contains('GnuDiv')
	assert types.contains('GnuBuffer')
	assert !types.contains('gnu_result')
	cc := os.find_abs_path_of_executable('cc') or { return }
	dir := os.join_path(os.vtmp_dir(), 'gnu_extension_declarations_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	path := os.join_path(dir, 'declarations.c')
	os.write_file(path, header + 'GnuDiv value;\nGnuBuffer buffer;\n')!
	checked := os.exec([cc, '-std=c99', '-pedantic-errors', '-fsyntax-only', path])
	assert checked.exit_code == 0, checked.output
}

fn test_gnu_extension_definitions_and_static_state_are_not_replicable() {
	for source in ['__extension__ int exported(void) { return 1; }\n', '__extension__ int state = 1;\n',
		'__extension__ static int state;\n',
		'__extension__ static inline int counter(void) { static int n; return ++n; }\n'] {
		assert !c_source_is_replicable(source), source
	}
}

fn test_gnu_extension_identifier_prefix_is_preserved() {
	source := 'typedef int __extension__type;\n__extension__type value = 1;\n'
	header := declaration_header(source)
	assert header.contains('extern __extension__type value;')
	assert !c_source_is_replicable(source)
}
