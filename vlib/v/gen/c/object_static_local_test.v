module c

import os

fn test_profiled_darwin_objects_keep_native_clock_linkage() {
	$if !linux && !macos {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'object_profile_clock_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'clock.v')
	output := os.join_path(root, 'clock.c')
	// Profiling must supply its native timer dependencies without an explicit
	// time import. Generate Darwin C on Linux and macOS hosts that have Clang.
	os.write_file(source, "@[export: 'clock_value']\nfn clock_value() int { return 42 }\n")!
	generated := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none',
		'-os', 'macos', '-is_o', '-profile', os.join_path(root, 'profile'), '-o', output, source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(output)!
	// Preserve the emitted native declarations and linkage scopes. Compile this
	// slice against separate native definitions so no Darwin SDK is needed.
	mut declarations := []string{}
	for line in code.split_into_lines() {
		is_clock := (line.starts_with('u64 ') || line.starts_with('i32 ')
			|| line.starts_with('extern ')) && line.ends_with(';')
			&& (line.contains('mach_absolute_time(') || line.contains('mach_timebase_info('))
		if is_clock || line.starts_with('#pragma clang attribute') {
			if line == '#pragma clang attribute pop' {
				declarations << 'u64 read_clock(void) { mach_timebase_info_data_t info; mach_timebase_info(&info); return mach_absolute_time() * info.numer / info.denom; }'
			}
			declarations << line
		}
	}
	selected := declarations.join('\n')
	assert selected.contains('mach_absolute_time(void);'), selected
	assert selected.contains('mach_timebase_info(mach_timebase_info_data_t*);'), selected
	assert selected.contains('#pragma clang attribute push'), selected
	assert selected.contains('#pragma clang attribute pop'), selected
	types := 'typedef unsigned long long u64;\ntypedef int i32;\ntypedef struct { unsigned int numer, denom; } mach_timebase_info_data_t;\n'
	probe := os.join_path(root, 'probe.c')
	os.write_file(probe, types + selected + '\nint main(void) { return read_clock() == 42 ? 0 : 1; }\n')!
	native := os.join_path(root, 'native.c')
	os.write_file(native, types + 'u64 mach_absolute_time(void) { return 21; }\nint mach_timebase_info(mach_timebase_info_data_t* info) { info->numer = 2; info->denom = 1; return 0; }\n')!
	executable := os.join_path(root, 'probe')
	compiled := os.exec([cc, '-std=c99', '-Werror', '-o', executable, probe, native])
	assert compiled.exit_code == 0, compiled.output
	ran := os.exec([executable])
	assert ran.exit_code == 0, ran.output
}

fn test_clang_objects_keep_static_locals_and_private_storage_separate() {
	$if !linux && !macos {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'object_static_locals_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for profile in [false, true] {
		dir := os.join_path(root, 'profile_${profile}')
		os.mkdir_all(dir)!
		mut objects := []string{}
		for name in ['first', 'second'] {
			source := os.join_path(dir, '${name}.v')
			os.write_file(source, "@[translated]
module main

__global private_bias = i32(3)

@[export: '${name}_public']
__global exported_value = i32(7)

const private_message = 'four'
const private_values = [i32(7), i32(11)]!
const private_runtime = seed()

fn seed() i32 {
	return 31
}

fn bump() i32 {
	mut static state := i32(10)
	state++
	return state
}

fn other_bump() i32 {
	mut static state := i32(100)
	state++
	return state
}

fn lazy_bump() i32 {
	mut static state := [i32(31)]
	state[0]++
	return state[0]
}

@[export: '${name}_next']
fn next() i32 {
	return bump() + other_bump() + lazy_bump() + private_bias + exported_value +
		i32(private_message.len) + private_values[0] + private_runtime
}
")!
			object := os.join_path(dir, '${name}.o')
			mut args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none',
				'-cc', cc, '-o', object]
			if profile {
				args << ['-profile', os.join_path(dir, '${name}.profile')]
			}
			args << source
			compiled := os.exec(args)
			assert compiled.exit_code == 0, compiled.output
			objects << object
		}
		host := os.join_path(dir, 'host.c')
		os.write_file(host, 'extern int first_public, second_public;
extern int first_next(void), second_next(void);
int main(void) {
    if (first_public != 7 || second_public != 7) return 1;
    first_public = 19;
    second_public = 23;
    if (first_next() != 208) return 2;
    if (first_next() != 211) return 3;
    if (second_next() != 212) return 4;
    if (first_next() != 214) return 5;
    if (second_next() != 215) return 6;
    return 0;
}
')!
		executable := os.join_path(dir, 'host')
		linked := os.exec([cc, '-o', executable, host, objects[0], objects[1], '-lm', '-lpthread'])
		assert linked.exit_code == 0, 'profile=${profile}: ${linked.output}'
		run := os.exec([executable])
		assert run.exit_code == 0, 'profile=${profile}: ${run.output}'
		for object in objects {
			symbols := os.exec(['nm', '-g', object])
			assert symbols.exit_code == 0, symbols.output
			for line in symbols.output.split_into_lines() {
				fields := line.fields()
				if fields.len == 0 {
					continue
				}
				symbol := fields.last().trim_left('_')
				assert symbol !in ['private_bias', 'private_message', 'private_values',
					'private_runtime', 'seed', 'bump', 'other_bump', 'lazy_bump', 'next'], symbols.output
			}
		}
	}
}
