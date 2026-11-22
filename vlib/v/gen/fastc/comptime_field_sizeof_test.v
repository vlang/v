module fastc

import os
import v.cmdexec

// In a `$for field in T.fields` loop, `sizeof(field.typ)` measures the field type.
fn test_fastc_comptime_field_type_sizes() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_comptime_field_type_sizes_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

fn auto_hash_array_has_unique_representation(_ []u8) bool {
	return true
}

struct P {
	a u32
	b u32
}

fn auto_hash_has_unique_representation[K](key K) bool {
	$if K is $option {
		return false
	} $else $if K.unaliased_typ is $int || K.unaliased_typ is $enum || K.unaliased_typ is $pointer
		|| K.unaliased_typ is $function {
		return true
	} $else $if K.unaliased_typ is $array_fixed {
		return auto_hash_array_has_unique_representation(key[..])
	} $else $if K.unaliased_typ is $struct {
		$for method in K.methods {
			$if method.name == 'source_auto_hash_bytes' { return false }
		}
		mut sum_size := uint(0)
		$for field in K.fields {
			if !auto_hash_has_unique_representation(key.$(field.name)) { return false }
			sum_size += sizeof(field.typ)
		}
		return sizeof(K) == sum_size
	} $else {
		return false
	}
}

fn main() {
	println(auto_hash_has_unique_representation(u32(3)))
	println(auto_hash_has_unique_representation(P{}))
	println(auto_hash_has_unique_representation(?int(none)))
}
") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true\ntrue\nfalse', run.output
}
