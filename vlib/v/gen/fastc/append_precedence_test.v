module fastc

import os
import v.cmdexec
import v.pref

const append_precedence_source = 'module main

struct Buffer {
mut:
	lines []string
	definitions map[string][]string
}

struct Reader {
mut:
	buffer Buffer
	parsed string
}

fn load_source(value string) !string {
	if value == "" {
		return error("missing source")
	}
	return value
}

fn (mut reader Reader) parse_source(value string) {
	reader.parsed = value + "!"
}

fn (mut reader Reader) append_source(value string) {
	reader.buffer.lines << load_source(value) or {
		reader.parsed = "missing"
		return
	}
	reader.parse_source(reader.buffer.lines.last())
}

fn main() {
	mut reader := Reader{}
	lines := ["first", "second"]
	reader.buffer.definitions["owner"] << lines.join("\\n") + "\\n"
	reader.buffer.definitions["owner"] << "third" + "\\n"
	assert reader.buffer.definitions["owner"] == ["first\\nsecond\\n", "third\\n"]
	name := "module.method"
	dot := 6
	reader.buffer.definitions[name[..dot]] << name + "!"
	reader.buffer.definitions[name[dot + 1..]] << name
	assert reader.buffer.definitions["module"] == ["module.method!"]
	assert reader.buffer.definitions["method"] == ["module.method"]
	mut values := map[string][]int{}
	number := 4
	values["answer"] << number + 2 * 3
	values["answer"] << number * 2 + 1
	assert values["answer"] == [10, 9]
	large := int(1) << 40
	values["wide"] << large + number
	assert values["wide"] == [large + number]
	reader.append_source("source")
	assert reader.buffer.lines == ["source"]
	assert reader.parsed == "source!"
	reader.append_source("")
	assert reader.buffer.lines == ["source"]
	assert reader.parsed == "missing"
	println("append precedence works")
}
'

fn test_fastc_append_precedence_and_sliced_map_keys_runtime() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_append_precedence_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), append_precedence_source) or { panic(err) }
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
	assert run.output.trim_space() == 'append precedence works', run.output
}

fn test_fastc_map_append_rhs_is_lowered_as_one_expression() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := generate('module main

fn main() {
	mut definitions := map[string][]string{}
	owner := "owner"
	value := "line"
	definitions[owner] << value + "\\n"
	name := "module.method"
	dot := 6
	definitions[name[..dot]] << value
	definitions[name[dot + 1..]] << value
	mut values := map[string][]int{}
	values[owner] << 4 + 2 * 3
}
', 'append_rhs_precedence.v', prefs) or { panic(err) }
	body := source.all_after_last('int ${fastc_c_main_name(prefs)}(')
	assert body.contains('string __vf_append_value = (builtin__string_plus('), body
	assert body.contains('${fastc_output_c_type('int')} __vf_append_value = ('), body
	assert !body.contains('<<'), body
	assert body.count('builtin__array_push(') == 4, body
}
