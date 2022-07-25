#!/usr/bin/env -S v

module main

import os
import vab.vxt
import vab.android.ndk

fn main() {
	assert ndk.found()
	assert vxt.found()

	work_dir := os.join_path(os.temp_dir(), 'android_cross_compile_test')
	os.rm(work_dir) or {}
	os.mkdir_all(work_dir) or { panic(err) }
	vexe := vxt.vexe()

	examples_dir := os.join_path(vxt.home(), 'examples')
	v_example := os.join_path(examples_dir, 'toml.v')

	ndk_version := ndk.default_version()

	sysroot_path := ndk.sysroot_path(ndk_version) or { panic(err) }
	include_path := os.join_path(sysroot_path, 'usr', 'include')
	android_include_path := os.join_path(include_path, 'android')

	//'-I"$include_path"'
	cflags := ['-I"$android_include_path"', '-Wno-unused-value', '-Wno-implicit-function-declaration',
		'-Wno-int-conversion']
	for arch in ndk.supported_archs {
		for level in ['min', 'max'] {
			compiler_api := match level {
				'min' {
					ndk.compiler_min_api(.c, ndk_version, arch) or { panic(err) }
				}
				'max' {
					ndk.compiler_max_api(.c, ndk_version, arch) or { panic(err) }
				}
				else {
					panic('invalid min/max level')
				}
			}

			os.setenv('VCROSS_COMPILER_NAME', compiler_api, true)
			c_file := os.join_path(work_dir, arch + '-' + level + '.c')
			o_file := os.join_path(work_dir, arch + '-' + level + '.o')

			// x.v -> x.c
			v_compile_cmd := '$vexe -o $c_file -os android -gc none $v_example'
			vres := os.execute(v_compile_cmd)
			if vres.exit_code != 0 {
				panic('"$v_compile_cmd" failed: $vres.output')
			}
			assert os.exists(c_file)

			// x.c -> x.o
			compile_cmd := '$compiler_api ${cflags.join(' ')} -c $c_file -o $o_file'
			cres := os.execute(compile_cmd)
			if cres.exit_code != 0 {
				panic('"$compile_cmd" failed: $cres.output')
			}
			assert os.exists(o_file)
			compiler_exe_name := os.file_name(compiler_api)
			println('Compiled examples/toml.v successfully for ($level) $arch $compiler_exe_name')
		}
	}
}
