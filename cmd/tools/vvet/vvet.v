// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import os
import os.cmdline
import v.pref
import v.parser
import v.flat
import v.help
import v.token
import term
import arrays

// Files are visited sequentially. Keep diagnostics and analysis state directly
// reachable from this GC-managed object while later files are parsed.
@[heap]
struct Vet {
mut:
	opt            Options
	errors         []VetError
	warns          []VetError
	notices        []VetError
	file           string
	mod            string
	filtered_lines FilteredLines
	analyze        VetAnalyze
	regex_vars     map[string]bool
	string_vars    map[string]bool
	a              &flat.FlatAst = unsafe { nil }
	source         string
}

struct Options {
	is_force            bool
	is_werror           bool
	is_verbose          bool
	show_warnings       bool
	use_color           bool
	doc_private_fns_too bool
	fn_sizing           bool
	repeated_code       bool
	fn_inlining         bool
mut:
	is_vfmt_off bool
}

const term_colors = term.can_show_color_on_stderr()
const clean_seq = ['[', '', ']', '', ' ', '']
const exclude_dirs = ['test', 'slow_test', 'testdata']

fn main() {
	vet_options := cmdline.options_after(os.args, ['vet'])
	mut vt := Vet{
		opt: Options{
			is_werror:           '-W' in vet_options
			is_verbose:          '-verbose' in vet_options || '-v' in vet_options
			show_warnings:       '-hide-warnings' !in vet_options && '-w' !in vet_options
			doc_private_fns_too: '-p' in vet_options
			use_color:           '-color' in vet_options
				|| (term_colors && '-nocolor' !in vet_options)
			repeated_code:       '-r' in vet_options
			fn_sizing:           '-F' in vet_options
			fn_inlining:         '-I' in vet_options
		}
	}
	mut paths := cmdline.only_non_options(vet_options)
	vtmp := os.getenv('VTMP')
	if vtmp != '' {
		// `v test-cleancode` passes also `-o tmpfolder` as well as all options in VFLAGS
		paths = paths.filter(!it.starts_with(vtmp))
	}
	if paths.len == 0 || '-help' in vet_options || '--help' in vet_options {
		help.print_and_exit('vet')
	}
	for path in paths {
		if !os.exists(path) {
			eprintln('File/folder ${path} does not exist')
			continue
		}
		if os.is_file(path) {
			vt.vet_file(path)
		}
		if os.is_dir(path) {
			vt.vprintln("vetting folder: '${path}' ...")
			overwrite_exclude := exclude_dirs.any(path.contains(it))
			os.walk(path, fn [mut vt, overwrite_exclude] (p string) {
				if p.ends_with('.v') || p.ends_with('.vv') {
					if !overwrite_exclude {
						for d in exclude_dirs {
							if p.contains(d) {
								return
							}
						}
					}
					vt.vet_file(p)
				}
			})
		}
	}
	vt.vet_code_analyze()
	vfmt_err_count := vt.errors.filter(it.fix == .vfmt).len
	for n in vt.notices {
		eprintln(vt.e2string(n))
	}
	if vt.opt.show_warnings {
		for w in vt.warns {
			eprintln(vt.e2string(w))
		}
	}
	for err in vt.errors {
		eprintln(vt.e2string(err))
	}
	if vfmt_err_count > 0 {
		filtered_out := arrays.distinct(vt.errors.map(it.file_path))
		eprintln('Note: You can run `v fmt -w ${filtered_out.join(' ')}` to fix these errors automatically')
	}
	if vt.errors.len > 0 {
		exit(1)
	}
}

// vet_file vets the file read from `path`.
fn (mut vt Vet) vet_file(path string) {
	vt.file = path
	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	vt.vprintln("vetting file '${path}'...")
	vt.source = os.read_file(path) or { return }
	mut p := parser.Parser.new(prefs)
	vt.a = p.parse_file(path)
	vt.mod = 'main'
	vt.filtered_lines = {
		.space_indent:   map[int]bool{}
		.trailing_space: map[int]bool{}
	}
	for comment in vt.a.comments {
		start, end := vt.span_lines(comment.pos)
		is_multi := vt.source[int(comment.pos.offset)..int(comment.pos.end)].starts_with('/*')
		vt.filtered_lines.comments(is_multi, start, end)
	}
	vt.regex_vars = map[string]bool{}
	vt.string_vars = map[string]bool{}
	for raw_id in vt.a.file_node_ids {
		id := flat.NodeId(raw_id)
		node := vt.a.node(id)
		if node.kind != .file || node.children_count == 0 {
			continue
		}
		for child in vt.a.children_of(node) {
			if vt.a.node(child).kind == .module_decl {
				vt.mod = vt.a.node(child).value
			}
			vt.visit(child)
		}
	}
	source_lines := os.read_lines(vt.file) or { []string{} }
	for ln, line in source_lines {
		vt.vet_line(source_lines, line, ln)
	}
}

// vet_line vets the contents of `line` from `vet.file`.
fn (mut vt Vet) vet_line(lines []string, line string, lnumber int) {
	if line == '' {
		return
	}
	vt.vet_fn_documentation(lines, line, lnumber)
	vt.vet_space_usage(line, lnumber)
}

fn (mut vt Vet) vet_space_usage(line string, lnumber int) {
	if line.starts_with('// vfmt off') {
		vt.opt.is_vfmt_off = true
	} else if line.starts_with('// vfmt on') {
		vt.opt.is_vfmt_off = false
	}
	if vt.opt.is_vfmt_off {
		return
	}
	if lnumber !in vt.filtered_lines[.space_indent] {
		if line.starts_with(' ') {
			vt.error('Looks like you are using spaces for indentation.', lnumber, .vfmt)
		}
	}
	if lnumber !in vt.filtered_lines[.trailing_space] {
		if line.ends_with(' ') {
			vt.error('Looks like you have trailing whitespace.', lnumber, .unknown)
		}
	}
}

fn collect_tags(line string) []string {
	mut cleaned := line.all_before('/')
	cleaned = cleaned.replace_each(clean_seq)
	return cleaned.split(',')
}

fn ident_fn_name(line string) string {
	mut fn_idx := line.index(' fn ') or { return '' }
	if line.len < fn_idx + 5 {
		return ''
	}
	mut tokens := line[fn_idx + 4..].split(' ')
	// Skip struct identifier
	if tokens.first().starts_with('(') {
		fn_idx = line.index(')') or { return '' }
		tokens = line[fn_idx..].split(' ')
		if tokens.len > 1 {
			tokens = [tokens[1]]
		}
	}
	if tokens.len > 0 {
		function_name_with_generic_parameters := tokens[0].all_before('(')
		return function_name_with_generic_parameters.all_before('[')
	}
	return ''
}

// vet_fn_documentation ensures that functions are documented
fn (mut vt Vet) vet_fn_documentation(lines []string, line string, lnumber int) {
	if line.starts_with('fn C.') {
		return
	}
	is_pub_fn := line.starts_with('pub fn ')
	is_fn := is_pub_fn || line.starts_with('fn ')
	if !is_fn {
		return
	}
	if line.starts_with('fn main') {
		return
	}
	if !(is_pub_fn || vt.opt.doc_private_fns_too) {
		return
	}
	// Scan function declarations for missing documentation
	mut line_above := lines[lnumber - 1] or { return }
	mut tags := []string{}
	if !line_above.starts_with('//') {
		mut grab := true
		for j := lnumber - 1; j >= 0; j-- {
			prev_line := lines[j]
			if prev_line.contains('}') { // We've looked back to the above scope, stop here
				break
			} else if prev_line.starts_with('@[') {
				tags << collect_tags(prev_line)
				continue
			} else if prev_line.starts_with('//') { // Single-line comment
				grab = false
				break
			}
		}
		if grab {
			clean_line := line.all_before_last('{').trim(' ')
			vt.warn('Function documentation seems to be missing for "${clean_line}".', lnumber, .doc)
		}
	} else {
		fn_name := ident_fn_name(line)
		mut grab := true
		for j := lnumber - 1; j >= 0; j-- {
			mut prev_prev_line := ''
			if j - 1 >= 0 {
				prev_prev_line = lines[j - 1]
			}
			prev_line := lines[j]

			if prev_line.starts_with('//') {
				if prev_line.starts_with('// ${fn_name} ') {
					grab = false
					break
				} else if prev_line.starts_with('// ${fn_name}')
					&& !prev_prev_line.starts_with('//') {
					grab = false
					clean_line := line.all_before_last('{').trim(' ')
					vt.warn('The documentation for "${clean_line}" seems incomplete.', lnumber, .doc)
					break
				}

				continue
			}

			if prev_line.contains('}') { // We've looked back to the above scope, stop here
				break
			} else if prev_line.starts_with('@[') {
				tags << collect_tags(prev_line)
				continue
			}
		}
		if grab {
			clean_line := line.all_before_last('{').trim(' ')
			vt.warn('A function name is missing from the documentation of "${clean_line}".', lnumber, .doc)
		}
	}
}

fn (mut vt Vet) visit(id flat.NodeId) {
	node := vt.a.node(id)
	if node.kind == .fn_decl {
		old_fn := vt.analyze.cur_fn
		old_regex_vars := vt.regex_vars.clone()
		old_string_vars := vt.string_vars.clone()
		vt.analyze.cur_fn = if vt.mod == 'builtin' { node.value } else { '${vt.mod}.${node.value}' }
		vt.regex_vars = map[string]bool{}
		vt.string_vars = map[string]bool{}
		for child in vt.a.children_of(node) {
			parameter := vt.a.node(child)
			if parameter.kind == .param && parameter.typ == 'string' {
				vt.string_vars[parameter.value] = true
			}
		}
		for child in vt.a.children_of(node) {
			vt.visit(child)
		}
		if vt.opt.fn_sizing {
			vt.analyze.long_or_empty_fn(mut vt, id)
		}
		if vt.opt.fn_inlining {
			vt.analyze.potential_non_inlined(mut vt, id)
		}
		vt.analyze.cur_fn = old_fn
		vt.regex_vars = old_regex_vars
		vt.string_vars = old_string_vars
		return
	}
	match node.kind {
		.decl_assign, .assign {
			vt.track_assign(node)
			vt.analyze.assignment(&vt, node)
		}
		.const_field {
			if child := vt.first_child(node) {
				value := vt.a.node(child)
				if value.kind == .array_literal && !vt.node_source(value).ends_with(']!') {
					vt.notice('Use a fixed array instead of a dynamic one', vt.node_line(value) - 1, .unknown)
				}
			}
		}
		.string_literal, .string_interp, .array_literal, .array_init {
			start, end := vt.span_lines(node.pos)
			vt.filtered_lines.assigns(start, end)
			vt.analyze.expression(&vt, node)
		}
		.infix {
			vt.vet_empty_str(node)
			vt.analyze.expression(&vt, node)
		}
		.in_expr {
			vt.vet_in_condition(node)
		}
		.call {
			vt.vet_confusing_regex(node)
			vt.analyze.expression(&vt, node)
		}
		.selector, .index, .as_expr {
			vt.analyze.expression(&vt, node)
		}
		else {}
	}
	if node.kind == .for_stmt && node.value == 'c_style' && node.children_count >= 4 {
		children := vt.a.children_of(node)
		vt.visit(children[0])
		vt.visit(children[1])
		for child in children[3..] {
			vt.visit(child)
		}
		vt.visit(children[2])
		return
	}
	for child in vt.a.children_of(node) {
		vt.visit(child)
	}
}

fn (vt &Vet) first_child(node &flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	return vt.a.child(node, 0)
}

fn (vt &Vet) node_source(node &flat.Node) string {
	start := int(node.pos.offset)
	end := int(node.pos.end)
	if start < 0 || end <= start || end > vt.source.len {
		return ''
	}
	return vt.source[start..end]
}

fn (vt &Vet) node_line(node &flat.Node) int {
	position := vt.a.source_position(node.pos) or { return 1 }
	return position.line
}

fn (vt &Vet) span_lines(pos token.Pos) (int, int) {
	file := vt.a.source_files[pos.id] or { return 0, 0 }
	start := file.position_at(pos.offset).line - 1
	end := file.position_at(pos.end).line - 1
	return start, end
}

fn (mut vt Vet) track_assign(node &flat.Node) {
	if node.children_count < 2 {
		return
	}
	left := vt.a.child_node(node, 0)
	right := vt.a.child_node(node, node.children_count - 1)
	if left.kind != .ident {
		return
	}
	if vt.is_regex_value(right) {
		vt.regex_vars[left.value] = true
	} else {
		vt.regex_vars.delete(left.value)
	}
	if right.kind in [.string_literal, .string_interp] {
		vt.string_vars[left.value] = true
	} else {
		vt.string_vars.delete(left.value)
	}
}

fn (mut vt Vet) vet_confusing_regex(call &flat.Node) {
	if call.children_count < 2 || !vt.is_regex_pattern_call(call) {
		return
	}
	pattern := vt.a.child_node(call, 1)
	if pattern.kind != .string_literal {
		return
	}
	snippet, suggestion := confusing_regex_branch(pattern.value) or { return }
	vt.warn('Confusing regex `|` in `${snippet}`: V regex applies `|` to adjacent tokens, not whole branches. Use `${suggestion}` if you intended alternation.', vt.node_line(pattern) - 1, .unknown)
}

fn (vt &Vet) is_regex_pattern_call(call &flat.Node) bool {
	callee := vt.a.child_node(call, 0)
	if callee.kind == .ident {
		return vt.mod == 'regex' && callee.value in ['regex_opt', 'regex_base']
	}
	if callee.kind != .selector {
		return false
	}
	receiver := vt.a.child_node(callee, 0)
	if receiver.kind == .ident && receiver.value == 'regex'
		&& callee.value in ['regex_opt', 'regex_base'] {
		return true
	}
	return callee.value == 'compile_opt' && vt.is_regex_value(receiver)
}

fn (vt &Vet) is_regex_value(node &flat.Node) bool {
	if node.kind == .ident {
		return node.value in vt.regex_vars
	}
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	callee := vt.a.child_node(node, 0)
	if callee.kind == .ident {
		return vt.mod == 'regex' && callee.value in ['new', 'regex_opt', 'regex_base']
	}
	if callee.kind != .selector || callee.value !in ['new', 'regex_opt', 'regex_base'] {
		return false
	}
	receiver := vt.a.child_node(callee, 0)
	return receiver.kind == .ident && receiver.value == 'regex'
}

fn confusing_regex_branch(pattern string) ?(string, string) {
	mut escaped := false
	mut in_char_class := false
	for i := 0; i < pattern.len; i++ {
		ch := pattern[i]
		if escaped {
			escaped = false
			continue
		}
		if ch == `\\` {
			escaped = true
			continue
		}
		if in_char_class {
			if ch == `]` {
				in_char_class = false
			}
			continue
		}
		if ch == `[` {
			in_char_class = true
			continue
		}
		if ch != `|` || i == 0 || i + 1 >= pattern.len {
			continue
		}
		if !is_regex_plain_letter(pattern[i - 1]) || !is_regex_plain_letter(pattern[i + 1]) {
			continue
		}
		if !((i >= 2 && is_regex_plain_letter(pattern[i - 2]))
			|| (i + 2 < pattern.len && is_regex_plain_letter(pattern[i + 2]))) {
			continue
		}
		mut left_start := i - 1
		for left_start > 0 && is_regex_plain_letter(pattern[left_start - 1]) {
			left_start--
		}
		mut right_end := i + 1
		for right_end + 1 < pattern.len && is_regex_plain_letter(pattern[right_end + 1]) {
			right_end++
		}
		left := pattern[left_start..i]
		right := pattern[i + 1..right_end + 1]
		return pattern[left_start..right_end + 1], '(${left})|(${right})'
	}
	return none
}

fn is_regex_plain_letter(ch u8) bool {
	return ch.is_letter()
}

fn (mut vt Vet) vet_empty_str(expr &flat.Node) {
	if expr.children_count != 2 {
		return
	}
	left := vt.a.child_node(expr, 0)
	right := vt.a.child_node(expr, 1)
	op := flat_op_string(expr.op)
	if name := vt.string_len_name(left) {
		if right.kind == .int_literal && right.value == '0' && expr.op != .lt {
			replacement_op := if expr.op == .gt { '!=' } else { op }
			vt.notice("Use `${name} ${replacement_op} ''` instead of `${name}.len ${op} 0`", vt.node_line(expr) - 1, .unknown)
		} else if right.kind == .int_literal && right.value == '1' && expr.op == .lt {
			vt.notice("Use `${name} == ''` instead of `${name}.len ${op} 1`", vt.node_line(expr) - 1, .unknown)
		}
		return
	}
	if name := vt.string_len_name(right) {
		if left.kind == .int_literal && left.value == '0' && expr.op != .gt {
			replacement_op := if expr.op == .lt { '!=' } else { op }
			vt.notice("Use `'' ${replacement_op} ${name}` instead of `0 ${op} ${name}.len`", vt.node_line(expr) - 1, .unknown)
		} else if left.kind == .int_literal && left.value == '1' && expr.op == .gt {
			vt.notice("Use `'' == ${name}` instead of `1 ${op} ${name}.len`", vt.node_line(expr) - 1, .unknown)
		}
	}
}

fn (vt &Vet) string_len_name(node &flat.Node) ?string {
	if node.kind != .selector || node.value != 'len' || node.children_count == 0 {
		return none
	}
	receiver := vt.a.child_node(node, 0)
	if receiver.kind == .ident && receiver.value in vt.string_vars {
		return receiver.value
	}
	return none
}

fn flat_op_string(op flat.Op) string {
	return match op {
		.lt { '<' }
		.gt { '>' }
		.eq { '==' }
		.ne { '!=' }
		else { '${op}' }
	}
}

fn (vt &Vet) vprintln(s string) {
	if !vt.opt.is_verbose {
		return
	}
	println(s)
}

fn (mut vt Vet) vet_in_condition(expr &flat.Node) {
	if expr.children_count != 2 {
		return
	}
	left := vt.a.child_node(expr, 0)
	right := vt.a.child_node(expr, 1)
	if right.kind != .array_literal || right.children_count != 1 {
		return
	}
	left_source := vt.node_source(left)
	right_source := vt.node_source(vt.a.child_node(right, 0))
	is_not_in := vt.node_source(expr).contains('!in')
	op := if is_not_in { '!in' } else { 'in' }
	eq := if is_not_in { '!=' } else { '==' }
	vt.error('Use `${left_source} ${eq} ${right_source}` instead of `${left_source} ${op} [${right_source}]`', vt.node_line(expr) - 1, .vfmt)
}
