module c

import os

fn test_object_exports_initialize_with_or_without_a_v_main() {
	$if !linux && !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'object_runtime_entry_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for compiler in ['clang', 'gcc'] {
		cc := os.find_abs_path_of_executable(compiler) or { continue }
		version := os.exec([cc, '--version'])
		assert version.exit_code == 0, version.output
		is_clang := version.output.to_lower().contains('clang')
		for has_main in [false, true] {
			dir := os.join_path(root, '${compiler}_${has_main}')
			os.mkdir_all(dir)!
			header := os.join_path(dir, 'host.h')
			os.write_file(header, 'int CheckExports(void);\n')!
			entry := if has_main {
				'fn main() {\n if C.CheckExports() != 0 { exit(5) }\n}\n'
			} else {
				''
			}
			// With Clang, -o .o keeps V main private and the native consumer owns
			// entry. With GCC, exercise V main as entry too: its first native call
			// comes back through an export and must not run init a second time.
			v_owns_entry := has_main && !is_clang
			os.write_file(os.join_path(dir, 'lib.c.v'), "@[translated]
module main

#include \"${header}\"

struct Callbacks {
	callback fn () i32
}

__global callbacks = Callbacks{
	callback: answer
}
__global init_count i32

fn init() {
	init_count++
}

fn answer() i32 {
	return 41 + init_count
}

@[markused]
@[export: 'ReadCallbacks']
fn read_callbacks() &Callbacks {
	return &callbacks
}

@[markused]
@[export: 'ReadInitCount']
fn read_init_count() i32 {
	return init_count
}

fn C.CheckExports() i32

${entry}")!
			os.write_file(os.join_path(dir, 'host.c'), '#include <stdio.h>
struct callbacks { int (*callback)(void); };
extern struct callbacks *ReadCallbacks(void);
extern int ReadInitCount(void);

int CheckExports(void) {
    struct callbacks *table = ReadCallbacks();
    if (table->callback == NULL) {
        fprintf(stderr, "callback initializer did not run; init_count=%d\\n", ReadInitCount());
        return 1;
    }
    if (table->callback() != 42 || ReadInitCount() != 1) return 2;
    if (ReadCallbacks()->callback() != 42 || ReadInitCount() != 1) return 3;
    puts("object initialized once: 42");
    return 0;
}

#ifndef V_OWNS_ENTRY
int main(void) { return CheckExports(); }
#endif
')!
			object := os.join_path(dir, 'lib.o')
			compile := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-gc', 'none',
				'-cc', cc, '-o', object, os.join_path(dir, 'lib.c.v')])
			assert compile.exit_code == 0, '${compiler}, has_main=${has_main}: ${compile.output}'
			host := os.join_path(dir, 'host')
			mut link_args := [cc, '-o', host, os.join_path(dir, 'host.c'), object, '-lm', '-lpthread']
			if v_owns_entry {
				link_args << '-DV_OWNS_ENTRY'
			}
			link := os.exec(link_args)
			assert link.exit_code == 0, '${compiler}, has_main=${has_main}: ${link.output}'
			run := os.exec([host])
			assert run.exit_code == 0, '${compiler}, has_main=${has_main}: ${run.output}'
			assert run.output == 'object initialized once: 42\n', run.output
		}
	}
}
