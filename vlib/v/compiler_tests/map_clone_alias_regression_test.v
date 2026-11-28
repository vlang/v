import os

fn test_map_clone_entry_exception_preserves_alias_diagnostics() {
	root := os.join_path(os.temp_dir(), 'v_map_clone_alias_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	header := 'struct Row {
mut:
	cells []int
}

fn borrowed(values map[string]Row) map[string]Row {
	return values
}
'
	values := {
		'nested':             '
fn main() {
	scope := {"old": Row{cells: [1]}}
	mut copy := scope.clone()
	copy["new"] = Row{cells: [2]}
	copy["old"].cells[0] = 9
}
'
		'rebound':            '
fn main() {
	scope := {"old": Row{cells: [1]}}
	mut copy := scope.clone()
	copy = borrowed(scope)
	copy["new"] = Row{cells: [2]}
}
'
		'loop_rebound':       '
fn main() {
	scope := {"old": Row{cells: [1]}}
	mut copy := scope.clone()
	for i in 0 .. 2 {
		copy["new"] = Row{cells: [2]}
		if i == 0 {
			copy = borrowed(scope)
		}
	}
}
'
		'mutable_escape':     '
fn replace_map(mut dst map[string]Row, src map[string]Row) {
	dst = borrowed(src)
}

fn main() {
	scope := {"old": Row{cells: [1]}}
	mut copy := scope.clone()
	replace_map(mut copy, scope)
	copy["new"] = Row{cells: [2]}
}
'
		'user_clone':         '
struct Holder {
	values map[string]Row
}

fn (holder Holder) clone() map[string]Row {
	return holder.values
}

fn main() {
	holder := Holder{values: {"old": Row{cells: [1]}}}
	mut copy := holder.clone()
	copy["new"] = Row{cells: [2]}
}
'
		'closure_rebind':     '
fn main() {
	scope := {"old": Row{cells: [1]}}
	mut copy := scope.clone()
	fill := fn [mut copy] () {
		copy["closure"] = Row{cells: [3]}
	}
	fill()
	copy = borrowed(scope)
	copy["new"] = Row{cells: [2]}
}
'
		'shadowed_recursion': '
fn reset(mut dst map[string]Row, src map[string]Row) {
	dst = borrowed(src)
}

fn recursive_name(mut dst map[string]Row, src map[string]Row) {
	recursive_name := reset
	recursive_name(mut dst, src)
}

fn main() {
	scope := {"old": Row{cells: [1]}}
	mut copy := scope.clone()
	recursive_name(mut copy, scope)
	copy["old"] = Row{cells: [42]}
}
'
	}
	fields := {
		'field_branch':             '
struct Ctx {
mut:
	values map[string]Row
}

fn update(ctx Ctx, fresh bool) {
	mut copy := ctx
	if fresh {
		copy.values = ctx.values.clone()
	}
	copy.values["new"] = Row{cells: [2]}
}

fn main() {
	update(Ctx{}, true)
}
'
		'field_loop':               '
struct Ctx {
mut:
	values map[string]Row
}

fn update(ctx Ctx) {
	mut copy := ctx
	copy.values = ctx.values.clone()
	for i in 0 .. 2 {
		copy.values["new"] = Row{cells: [2]}
		if i == 0 {
			copy = ctx
		}
	}
}

fn main() {
	update(Ctx{})
}
'
		'field_alias':              '
struct Ctx {
mut:
	values map[string]Row
}

fn update(ctx Ctx) {
	mut copy := ctx
	mut p := &copy
	copy.values = ctx.values.clone()
	p.values = borrowed(ctx.values)
	copy.values["new"] = Row{cells: [2]}
}

fn main() {
	update(Ctx{})
}
'
		'field_prior_alias_escape': '
struct Ctx {
mut:
	values map[string]Row
}

fn reset(mut dst Ctx, src Ctx) {
	dst = src
}

fn update(ctx Ctx) {
	mut copy := ctx
	mut alias := &copy
	copy.values = ctx.values.clone()
	reset(mut alias, ctx)
	copy.values["old"] = Row{cells: [42]}
}

fn main() {
	update(Ctx{values: {"old": Row{cells: [1]}}})
}
'
		'field_escape':             '
struct Ctx {
mut:
	values map[string]Row
}

fn reset(mut dst Ctx, src Ctx) {
	dst = src
}

fn update(ctx Ctx) {
	mut copy := ctx
	copy.values = ctx.values.clone()
	reset(mut copy, ctx)
	copy.values["new"] = Row{cells: [2]}
}

fn main() {
	update(Ctx{})
}
'
	}
	check_map_clone_alias_batch(os.join_path(root, 'values'), header, values, 'copy')
	check_map_clone_alias_batch(os.join_path(root, 'fields'), header, fields, 'copy.values')
}

fn check_map_clone_alias_batch(root string, header string, cases map[string]string, alias string) {
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for name, body in cases {
		dir := os.join_path(root, name)
		os.mkdir_all(dir) or { panic(err) }
		source := 'module ${name}\n\n' + header + body.replace_once('\nfn main()', '\npub fn exercise()')
		os.write_file(os.join_path(dir, '${name}.v'), source) or { panic(err) }
		imports << 'import ${name}'
		calls << '\t${name}.exercise()'
	}
	main := os.join_path(root, 'main.v')
	os.write_file(main, 'module main\n\n${imports.join('\n')}\n\nfn main() {\n${calls.join('\n')}\n}\n') or {
		panic(err)
	}
	result := os.exec([@VEXE, '-nocache', '-b', 'c', '-o', os.join_path(root, 'program'), main])
	assert result.exit_code != 0, result.output
	mut errors := map[string][]string{}
	for line in result.output.split_into_lines() {
		location, message := line.split_once(': error: ') or { continue }
		file := os.file_name(location.all_before_last(':').all_before_last(':'))
		errors[file] << message
	}
	for name, _ in cases {
		target := if name == 'nested' { 'copy["old"].cells' } else { alias }
		expected := '`${target}` aliases mutable data from an immutable value'
		assert errors['${name}.v'].any(it.starts_with(expected)), '${name}: ${result.output}'
	}
}
