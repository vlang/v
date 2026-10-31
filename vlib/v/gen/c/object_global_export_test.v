module c

import os

fn object_export_has_symbol(output string, name string) bool {
	for line in output.split_into_lines() {
		fields := line.fields()
		if fields.len > 0 && fields.last() in [name, '_${name}'] {
			return true
		}
	}
	return false
}

fn test_exported_globals_link_from_native_object_consumers() {
	$if !linux && !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'object_global_exports_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for compiler in ['clang', 'gcc'] {
		cc := os.find_abs_path_of_executable(compiler) or { continue }
		version := os.exec([cc, '--version'])
		assert version.exit_code == 0, version.output
		for export_last in [false, true] {
			dir := os.join_path(root, '${compiler}_${export_last}')
			os.mkdir_all(dir)!
			private_after := '__global private_after = i32(19)\n'
			last_export := "@[export: 'public_zero_array']\n__global zero_array_storage [2]i32\n"
			// Exercise both a private declaration after exports and an exported
			// fixed-array declaration whose early continue ends the global loop.
			tail := if export_last {
				private_after + last_export
			} else {
				last_export + private_after
			}
			os.write_file(os.join_path(dir, 'lib.v'), "@[translated]
module main

__global private_before = i32(13)

@[export: 'public_scalar']
__global scalar_storage = i32(7)

@[c_extern]
__global native_counter i32

__global private_between = i32(17)

@[export: 'public_array']
__global array_storage = [i32(3), i32(5)]!

@[export]
__global public_zero i32

${tail}
@[markused]
@[export: 'ReadExports']
fn read_exported() i32 {
	return scalar_storage + array_storage[0] + array_storage[1] + public_zero +
		zero_array_storage[0] + zero_array_storage[1] + native_counter +
		private_before + private_between + private_after
}
")!
			os.write_file(os.join_path(dir, 'host.c'), 'extern int public_scalar;
extern int public_array[2];
extern int public_zero;
extern int public_zero_array[2];
extern int ReadExports(void);
int native_counter = 31;

int main(void) {
    if (public_scalar != 7 || public_array[0] != 3 || public_array[1] != 5) return 1;
    if (public_zero != 0 || public_zero_array[0] != 0 || public_zero_array[1] != 0) return 2;
    public_scalar = 11;
    public_array[0] = 23;
    public_array[1] = 29;
    public_zero = 37;
    public_zero_array[0] = 41;
    public_zero_array[1] = 43;
    native_counter = 47;
    /* The first V call must observe native writes, including static arrays. */
    return ReadExports() == 280 ? 0 : 3;
}
')!
			object := os.join_path(dir, 'lib.o')
			compile := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-gc', 'none',
				'-cc', cc, '-o', object, os.join_path(dir, 'lib.v')])
			assert compile.exit_code == 0, '${compiler}, export_last=${export_last}: ${compile.output}'
			host := os.join_path(dir, 'host')
			link := os.exec([cc, '-o', host, os.join_path(dir, 'host.c'), object, '-lm', '-lpthread'])
			assert link.exit_code == 0, '${compiler}, export_last=${export_last}: ${link.output}'
			run := os.exec([host])
			assert run.exit_code == 0, '${compiler}, export_last=${export_last}: ${run.output}'
			if version.output.to_lower().contains('clang') {
				// Clang's object-file scope must still hide ordinary V globals and
				// function bodies, including after the final exported declaration.
				symbols := os.exec(['nm', '-g', object])
				assert symbols.exit_code == 0, symbols.output
				for name in ['public_scalar', 'public_array', 'public_zero', 'public_zero_array',
					'ReadExports'] {
					assert object_export_has_symbol(symbols.output, name), symbols.output
				}
				for name in ['private_before', 'private_between', 'private_after', 'read_exported'] {
					assert !object_export_has_symbol(symbols.output, name), symbols.output
				}
			}
		}
	}
}
