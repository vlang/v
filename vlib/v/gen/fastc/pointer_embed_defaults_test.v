module fastc

import os
import v.cmdexec
import v.pref
import v.scanner
import v.token

fn test_fastc_self_qualified_types_use_the_canonical_module_path() {
	prefs := pref.new_preferences()
	source := '[]types.Type'
	file := token.File.unindexed('type.v', source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	first := scan.scan()
	typ, next := fastc_scan_type_bytes(mut scan, first, 'type.v', 'v.types',
		map[string]string{}, {
			'v.types.Type': true
		}, false) or { panic(err) }
	assert typ == fastc_array_c_type('v__types__Type')
	assert next in [.eof, .semicolon]
}

// Promoted fields copy pointer embeds before applying overrides to shared defaults.
fn test_fastc_pointer_embeds_preserve_defaults_and_copy_on_override() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_pointer_embed_defaults_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Number { value int }
struct Payload {
    count int = 7
    label string = "default"
    params []int = [1, 2]
    link &Number = default_number
}
type PayloadRef = &Payload
const shared_payload = &Payload{count: 11}
const default_number = &Number{value: 41}
struct Wrapped { PayloadRef = shared_payload }
struct Nested { Wrapped }
struct Collection { items []main.Payload }
struct Direct { count int }
type Variant = Wrapped | Direct
fn (value Wrapped) read() int { return value.count }
fn (value Direct) read() int { return value.count }
fn (value Variant) read() int {
    return match value {
        Wrapped { value.count }
        Direct { value.count }
    }
}
fn grouped_read(value Variant) int {
    return match value {
        Wrapped, Direct { value.read() }
    }
}
fn grouped_count(value Variant) int {
    return match value {
        Wrapped, Direct { value.count }
    }
}
fn guarded_count(value Variant) int {
    if value is Wrapped { return value.count }
    return 0
}
fn guarded_loop(value Variant) int {
    mut current := value
    for current is Wrapped { return current.count }
    return 0
}
fn larger(value Wrapped, values []int) bool {
    return values.len > value.params.len || (values.len == value.params.len && value.label.len == 0)
}
fn main() {
    assert larger(Wrapped{}, [1, 2, 3])
    assert !larger(Wrapped{}, [1])
    collection := Collection{items: [Payload{count: 19}]}
    assert collection.items[0].count == 19
    empty := Wrapped{}
    assert empty.count == 11
    assert empty.label == "default"
    assert empty.link.value == 41
    changed := Wrapped{count: 23, label: "changed"}
    assert changed.count == 23
    assert changed.label == "changed"
    assert empty.count == 11
    assert empty.label == "default"
    assert shared_payload.count == 11
    nested := Nested{count: 31}
    assert nested.count == 31
    assert nested.label == "default"
    assert shared_payload.count == 11
    assert changed.PayloadRef.count == 23
    assert changed == changed
    other := Wrapped{count: 23, label: "changed"}
    assert changed != other
    assert guarded_count(changed) == 23
    assert guarded_loop(changed) == 23
    values := [changed]
    assert values[0].count == 23
    assert grouped_count(changed) == 23
    assert grouped_count(Direct{count: 37}) == 37
    assert grouped_read(changed) == 23
    assert grouped_read(Direct{count: 37}) == 37
    println("ok")
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}

fn test_fastc_root_member_rewrite_distinguishes_comparison_from_arrow() {
	assert fastc_replace_c_root_identifier('left>value.count', 'value.count', 'promoted') == 'left>promoted'
	assert fastc_replace_c_root_identifier('left>>value.count', 'value.count', 'promoted') == 'left>>promoted'
	assert fastc_replace_c_root_identifier('left->value.count', 'value.count', 'promoted') == 'left->value.count'
	assert fastc_replace_c_root_identifier('left.value.count', 'value.count', 'promoted') == 'left.value.count'
}

fn test_fastc_rendered_constant_dependencies_ignore_literals_and_members() {
	mut dependencies := ['main.direct']
	fastc_rendered_constant_dependencies('({ box.value = (main__later); box.main__fake = "main__fake"; box->main__fake; })', {
		'main__later': 'main.later'
		'main__fake':  'main.fake'
	}, mut dependencies)
	assert dependencies == ['main.direct', 'main.later']
}
