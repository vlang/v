// Copyright (c) 2025 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import arrays
import os
import v.flat

const indexexpr_cutoff = os.getenv_opt('VET_INDEXEXPR_CUTOFF') or { '10' }.int()
const infixexpr_cutoff = os.getenv_opt('VET_INFIXEXPR_CUTOFF') or { '10' }.int()
const selectorexpr_cutoff = os.getenv_opt('VET_SELECTOREXPR_CUTOFF') or { '10' }.int()
const callexpr_cutoff = os.getenv_opt('VET_CALLEXPR_CUTOFF') or { '10' }.int()
const stringinterliteral_cutoff = os.getenv_opt('STRINGINTERLITERAL_CUTOFF') or { '10' }.int()
const stringliteral_cutoff = os.getenv_opt('STRINGLITERAL_CUTOFF') or { '10' }.int()
const ascast_cutoff = os.getenv_opt('ASCAST_CUTOFF') or { '10' }.int()
const stringconcat_cutoff = os.getenv_opt('STRINGCONCAT_CUTOFF') or { '10' }.int()

const fns_call_cutoff = os.getenv_opt('VET_FNS_CALL_CUTOFF') or { '10' }.int()
const short_fns_cutoff = os.getenv_opt('VET_SHORT_FNS_CUTOFF') or { '3' }.int()
const stringliteral_min_size = os.getenv_opt('VET_STRINGLITERAL_MIN_SIZE') or { '20' }.int()
const long_fns_cutoff = os.getenv_opt('VET_LONG_FNS_CUTOFF') or { '300' }.int()

struct VetAnalyze {
mut:
	repeated_expr_cutoff     shared map[string]int
	repeated_expr            shared map[string]map[string]map[string][]int
	potential_non_inlined    shared map[string]map[string]int
	call_counter             shared map[string]int
	unqualified_call_counter shared map[string]int
	declared_fns             shared map[string]bool
	cur_fn                   string
}

fn (mut va VetAnalyze) assignment(vet &Vet, node &flat.Node) {
	if node.kind != .assign || node.op != .plus_assign || node.children_count < 2 {
		return
	}
	right := vet.a.child_node(node, node.children_count - 1)
	if right.kind !in [.string_literal, .string_interp] {
		return
	}
	left := vet.a.child_node(node, 0)
	expr := '${vet.node_source(left)} += ${vet.node_source(right)}'
	va.save_expr(stringconcat_cutoff, expr, vet.file, vet.node_line(left))
}

fn (mut va VetAnalyze) save_expr(cutoff int, expr string, file string, line int) {
	if expr == '' {
		return
	}
	lock va.repeated_expr {
		if va.cur_fn !in va.repeated_expr {
			va.repeated_expr[va.cur_fn] = map[string]map[string][]int{}
		}
		if expr !in va.repeated_expr[va.cur_fn] {
			va.repeated_expr[va.cur_fn][expr] = map[string][]int{}
		}
		if file !in va.repeated_expr[va.cur_fn][expr] {
			va.repeated_expr[va.cur_fn][expr][file] = []int{}
		}
		va.repeated_expr[va.cur_fn][expr][file] << line
	}
	lock va.repeated_expr_cutoff {
		va.repeated_expr_cutoff[expr] = cutoff
	}
}

fn (mut va VetAnalyze) expression(vet &Vet, node &flat.Node) {
	expr := vet.node_source(node)
	match node.kind {
		.infix { va.save_expr(infixexpr_cutoff, expr, vet.file, vet.node_line(node)) }
		.index { va.save_expr(indexexpr_cutoff, expr, vet.file, vet.node_line(node)) }
		.selector {
			receiver := vet.a.child_node(node, 0)
			if receiver.kind != .ident {
				va.save_expr(selectorexpr_cutoff, expr, vet.file, vet.node_line(node))
			}
		}
		.call {
			va.count_call(vet, node)
			va.save_expr(callexpr_cutoff, expr, vet.file, vet.node_line(node))
		}
		.as_expr { va.save_expr(ascast_cutoff, expr, vet.file, vet.node_line(node)) }
		.string_literal {
			if node.value.len > stringliteral_min_size {
				va.save_expr(stringliteral_cutoff, expr, vet.file, vet.node_line(node))
			}
		}
		.string_interp {
			va.save_expr(stringinterliteral_cutoff, expr, vet.file, vet.node_line(node))
		}
		else {}
	}
}

fn (mut va VetAnalyze) count_call(vet &Vet, call &flat.Node) {
	if call.children_count == 0 {
		return
	}
	callee := vet.a.child_node(call, 0)
	if callee.kind == .ident {
		qualified := '${vet.mod}.${callee.value}'
		lock va.call_counter {
			va.call_counter[qualified]++
		}
		lock va.unqualified_call_counter {
			va.unqualified_call_counter[qualified]++
		}
		return
	}
	if callee.kind == .selector {
		lock va.call_counter {
			va.call_counter[callee.value]++
		}
	}
}

fn (mut va VetAnalyze) long_or_empty_fn(mut vet Vet, id flat.NodeId) {
	node := vet.a.node(id)
	mut has_body := false
	for child in vet.a.children_of(node) {
		if vet.a.node(child).kind != .param {
			has_body = true
			break
		}
	}
	start_line := vet.node_line(node)
	end_offset := vet.a.formatter_node_ends[int(id)] or { int(node.pos.end) }
	file := vet.a.source_files[node.pos.id] or { return }
	end_line := file.position_at(end_offset).line
	nr_lines := end_line - start_line - 1
	if nr_lines > long_fns_cutoff {
		vet.notice('Long function - ${nr_lines} lines long.', start_line - 1, .long_fns)
	} else if !has_body {
		vet.notice('Empty function.', start_line - 1, .empty_fn)
	}
}

fn (mut va VetAnalyze) potential_non_inlined(mut vet Vet, id flat.NodeId) {
	node := vet.a.node(id)
	fn_key := va.cur_fn
	lock va.declared_fns {
		va.declared_fns[fn_key] = true
	}
	start_line := vet.node_line(node)
	end_offset := vet.a.formatter_node_ends[int(id)] or { int(node.pos.end) }
	file := vet.a.source_files[node.pos.id] or { return }
	nr_lines := file.position_at(end_offset).line - start_line - 1
	if nr_lines >= short_fns_cutoff || vet.has_attribute_before(node, 'inline') {
		return
	}
	lock va.potential_non_inlined {
		if fn_key !in va.potential_non_inlined {
			va.potential_non_inlined[fn_key] = map[string]int{}
		}
		va.potential_non_inlined[fn_key][vet.file] = start_line
	}
}

fn (vet &Vet) has_attribute_before(node &flat.Node, name string) bool {
	line := vet.node_line(node)
	lines := vet.source.split_into_lines()
	for i := line - 2; i >= 0; i-- {
		text := lines[i].trim_space()
		if text == '' {
			continue
		}
		if text.starts_with('@[') {
			return text.trim('@[]').split(',').any(it.trim_space() == name)
		}
		break
	}
	return false
}

fn (mut va VetAnalyze) vet_repeated_code(mut vet Vet) {
	rlock va.repeated_expr {
		for fn_name, ref_expr in va.repeated_expr {
			scope_name := if fn_name == '' { 'global scope' } else { 'function scope (${fn_name})' }
			for expr, info in ref_expr {
				occurrences := arrays.sum(info.values().map(it.len)) or { 0 }
				if occurrences < va.repeated_expr_cutoff[expr] {
					continue
				}
				for file, lines in info {
					for i, line in lines {
						vet.notice_with_file(file, '${expr} occurs ${i + 1}/${occurrences} times in ${scope_name}.', line - 1, .repeated_code)
					}
				}
			}
		}
	}
}

fn (mut va VetAnalyze) vet_inlining_fn(mut vet Vet) {
	mut declared_fns := map[string]bool{}
	rlock va.declared_fns {
		declared_fns = va.declared_fns.clone()
	}
	mut unqualified_calls := map[string]int{}
	rlock va.unqualified_call_counter {
		unqualified_calls = va.unqualified_call_counter.clone()
	}
	mut builtin_calls := map[string]int{}
	for caller_fn_name, count in unqualified_calls {
		caller_mod := caller_fn_name.all_before_last('.')
		if caller_mod == 'builtin' || caller_fn_name !in declared_fns {
			builtin_calls[caller_fn_name.all_after_last('.')] += count
		}
	}
	for fn_name, info in va.potential_non_inlined {
		for file, line in info {
			calls := if fn_name.contains('.') {
				va.call_counter[fn_name] or { 0 }
			} else {
				builtin_calls[fn_name] or { 0 }
			}
			if calls < fns_call_cutoff {
				continue
			}
			vet.notice_with_file(file, '${fn_name.all_after('.')} fn might be inlined (possibly called at least ${calls} times)', line - 1, .inline_fn)
		}
	}
}

fn (mut vt Vet) vet_code_analyze() {
	if vt.opt.repeated_code {
		vt.analyze.vet_repeated_code(mut vt)
	}
	if vt.opt.fn_inlining {
		vt.analyze.vet_inlining_fn(mut vt)
	}
}
