import os

fn test_json_builtin_named_fields_from_cached_import() ! {
	$if windows {
		return
	}
	os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'builtin_named_fields_cache_${os.getpid()}')
	modules := os.join_path(root, 'modules')
	module_dir := os.join_path(modules, 'namedfields')
	os.mkdir_all(module_dir)!
	cache := os.join_path(root, 'cache')
	variables := ['VFLAGS', 'VOSARGS', 'V3CACHE', 'VTMP', 'VMODULES', 'V3_CACHE_FORCE_SOURCE',
		'V3_CACHE_DISABLE_INCREMENTAL', 'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE']
	previous := os.environ()
	defer {
		for name in variables {
			if name in previous { os.setenv(name, previous[name], true) } else { os.unsetenv(name) }
		}
		os.rmdir_all(root) or {}
	}
	// Select cache use explicitly; a parent test runner may pass -nocache in VFLAGS.
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')
	os.unsetenv('V3_CACHE_FORCE_SOURCE')
	os.setenv('V3CACHE', cache, true)
	os.setenv('VTMP', cache, true)
	os.setenv('VMODULES', modules, true)
	os.setenv('V3_CACHE_DISABLE_INCREMENTAL', '1', true)
	os.setenv('V3_CACHE_DISABLE_PROGRAM_EXECUTABLE', '1', true)
	os.write_file(os.join_path(module_dir, 'namedfields.v'), 'module namedfields
pub struct Fields {
pub:
	i64 i64 @[json: "number"]
	string string
	bool bool
}
pub fn value() Fields { return Fields{ i64: 42, string: "text", bool: true } }
pub struct Embedded {
	Fields
pub:
	extra string
}
')!
	source := os.join_path(root, 'main.v')
	program := 'import json2
import namedfields
fn main() {
	value := namedfields.value()
	println(json2.encode(value))
	println(json2.encode(namedfields.Embedded{ Fields: value, extra: "more" }))
	\$for field in namedfields.Fields.fields { assert !field.is_embed }
}
'
	expected := '{"number":42,"string":"text","bool":true}\n' +
		'{"number":42,"string":"text","bool":true,"extra":"more"}'
	output := os.join_path(root, 'program')
	for mode in ['uncached', 'cold', 'warm'] {
		// Changing the entry forces a new program build while imported modules stay valid.
		os.write_file(source, program + '\n// ' + mode + '\n')!
		mut args := [@VEXE, '-memory-limit', '2048M', '-gc', 'none', '-cc', 'cc', '-show-timings']
		if mode == 'uncached' { args << '-nocache' }
		args << ['-o', output, source]
		build := os.exec(args)
		assert build.exit_code == 0, '${mode}: ${build.output}'
		if mode == 'cold' {
			assert build.output.contains('Caching module namedfields...'), build.output
			assert os.walk_ext(cache, '.vh').any(os.file_name(it).starts_with('namedfields_'))
		} else if mode == 'warm' {
			assert !build.output.contains('Caching module '), build.output
			// Only the entry should be parsed from source; imports must use cached headers.
			mut parsed := -1
			for line in build.output.split_into_lines() {
				if line.contains('parsed .v files') {
					parsed = line.all_after('parsed .v files').trim_space().all_before(' ').int()
				}
			}
			assert parsed == 1, build.output
		}
		run := os.exec([output])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == expected, '${mode}: ${run.output}'
	}
}
