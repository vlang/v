module parser

import os
import runtime
import v3.flat
import v3.scanner
import v3.token
import v3.workers

// Parallel file parsing. The input file list is split into contiguous,
// size-balanced chunks; each worker parses its chunk into a private Parser +
// FlatAst on its own thread, and the master folds the results back in chunk
// order, shifting node ids and children offsets by the merge position. A
// file's nodes only ever reference nodes of the same file, and chunks are
// contiguous slices of the input list merged in input order, so the merged
// nodes/children arrays are laid out exactly as a serial parse of the same
// list — the phases downstream see an identical AST either way.

const min_parallel_parse_files = 4
const min_parallel_parse_bytes = 131072
const max_parallel_parse_jobs = 8
const comptime_const_prepass_alias_prefix = '\x00v3-comptime-alias:'

struct ComptimeConstPrepassToken {
	tok token.Token
	lit string
}

struct ComptimeConstPrepassDecl {
	key   string
	value string
}

struct ComptimeConstPrepassChunk {
mut:
	decls []ComptimeConstPrepassDecl
}

$if !windows {
	// ParseChunkArgs is the payload handed to each worker thread.
	struct ParseChunkArgs {
		worker        voidptr // &Parser
		paths_ptr     voidptr // &[]string
		starts_ptr    voidptr // &[]int (worker-local starts; the master shifts them on merge)
		prepass_chunk voidptr // &ComptimeConstPrepassChunk
		start         int
		end           int
		chunk_bytes   int
		scope_enabled bool
	mut:
		scope voidptr
	}

	// parse_chunk_thread parses one worker's contiguous range of files into the
	// worker's private FlatAst, recording each file's worker-local first node id
	// into its own preallocated slot of the shared starts array.
	fn parse_chunk_thread(arg voidptr) voidptr {
		mut a := unsafe { &ParseChunkArgs(arg) }
		mut w := unsafe { &Parser(a.worker) }
		a.scope = parser_worker_scope_begin(a.scope_enabled)
		w.reserve_for_source(a.chunk_bytes)
		paths := unsafe { &[]string(a.paths_ptr) }
		mut starts := unsafe { &[]int(a.starts_ptr) }
		for i in a.start .. a.end {
			unsafe {
				(*starts)[i] = w.a.nodes.len
			}
			w.parse_into((*paths)[i])
		}
		parser_worker_scope_leave(a.scope)
		return unsafe { nil }
	}

	fn precollect_const_chunk_thread(arg voidptr) voidptr {
		mut a := unsafe { &ParseChunkArgs(arg) }
		a.scope = parser_worker_scope_begin(a.scope_enabled)
		mut w := unsafe { &Parser(a.worker) }
		paths := unsafe { &[]string(a.paths_ptr) }
		mut chunk := unsafe { &ComptimeConstPrepassChunk(a.prepass_chunk) }
		unsafe {
			w.precollect_parallel_comptime_consts(*paths, a.start, a.end, mut chunk.decls)
		}
		parser_worker_scope_leave(a.scope)
		return unsafe { nil }
	}
}

fn clone_comptime_const_prepass_decls(values []ComptimeConstPrepassDecl) []ComptimeConstPrepassDecl {
	mut cloned := []ComptimeConstPrepassDecl{cap: values.len}
	for value in values {
		cloned << ComptimeConstPrepassDecl{
			key:   value.key.clone()
			value: value.value.clone()
		}
	}
	return cloned
}

fn parser_worker_scope_begin(enabled bool) voidptr {
	$if prealloc {
		if enabled {
			return unsafe { prealloc_scope_begin() }
		}
	}
	return unsafe { nil }
}

fn parser_worker_scope_leave(scope voidptr) {
	$if prealloc {
		if scope != unsafe { nil } {
			unsafe { prealloc_scope_leave(scope) }
		}
	}
}

fn parser_worker_scope_free(scope voidptr) {
	$if prealloc {
		if scope != unsafe { nil } {
			unsafe { prealloc_scope_free_after(scope) }
		}
	}
}

// parse_files_dispatch parses paths in order, appending to p.a exactly like a
// serial parse_into loop, across worker threads when there is enough work.
// Returns each file's first node id in p.a and whether threads were used.
pub fn (mut p Parser) parse_files_dispatch(paths []string, allow_parallel bool) ([]int, bool) {
	$if windows {
		return p.parse_files_with_starts(paths), false
	} $else {
		if !allow_parallel || paths.len < min_parallel_parse_files {
			return p.parse_files_with_starts(paths), false
		}
		mut sizes := []i64{cap: paths.len}
		mut total_bytes := i64(0)
		for path in paths {
			size := i64(os.file_size(path))
			sizes << size
			total_bytes += size
		}
		if isnil(p.a.worker_pool) {
			p.a.worker_pool = workers.new(runtime.nr_jobs() - 1)
		}
		n_jobs := parse_job_count(p.a.worker_pool.size() + 1, paths.len)
		if n_jobs <= 1 || total_bytes < min_parallel_parse_bytes {
			return p.parse_files_with_starts(paths), false
		}
		bounds := parse_chunk_bounds(sizes, n_jobs)
		thread_count := n_jobs - 1
		dispatch_file_id_start := p.next_file_id
		mut starts := []int{len: paths.len}
		mut prepass_chunks := []&ComptimeConstPrepassChunk{cap: n_jobs}
		for _ in 0 .. n_jobs {
			prepass_chunks << &ComptimeConstPrepassChunk{}
		}
		// Worker parsers are cheap to build (no AST clone: parse output is
		// per-file independent), so all of them are created up front. Each
		// pre-reserves for its chunk's source bytes to avoid growth doubling.
		mut parser_workers := []&Parser{cap: thread_count}
		mut worker_chunk_bytes := []int{len: thread_count}
		for ci in 0 .. thread_count {
			mut w := Parser.new(p.prefs)
			w.next_file_id = dispatch_file_id_start + bounds[ci + 1]
			mut chunk_bytes := i64(0)
			for i in bounds[ci + 1] .. bounds[ci + 2] {
				chunk_bytes += sizes[i]
			}
			worker_chunk_bytes[ci] = int(chunk_bytes)
			parser_workers << w
		}
		mut args := []ParseChunkArgs{cap: n_jobs}
		args << ParseChunkArgs{
			worker:        voidptr(p)
			paths_ptr:     unsafe { voidptr(&paths) }
			starts_ptr:    unsafe { voidptr(&starts) }
			prepass_chunk: voidptr(prepass_chunks[0])
			start:         bounds[0]
			end:           bounds[1]
			chunk_bytes:   0
			scope_enabled: false
		}
		for ci in 0 .. thread_count {
			args << ParseChunkArgs{
				worker:        voidptr(parser_workers[ci])
				paths_ptr:     unsafe { voidptr(&paths) }
				starts_ptr:    unsafe { voidptr(&starts) }
				prepass_chunk: voidptr(prepass_chunks[ci + 1])
				start:         bounds[ci + 1]
				end:           bounds[ci + 2]
				chunk_bytes:   worker_chunk_bytes[ci]
				scope_enabled: true
			}
		}
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
		// Collect ordered foldable-const declarations in parallel. Once every
		// chunk is scanned, replay the declarations in input order to make a
		// prefix snapshot for each worker: later chunks inherit earlier consts,
		// while no chunk can see declarations from its own or a later range.
		mut prepass_tasks := []workers.Task{cap: n_jobs}
		for ci in 0 .. n_jobs {
			helper_idx := ci - 1
			prepass_tasks << workers.Task{
				run:        precollect_const_chunk_thread
				arg:        unsafe { voidptr(&args[ci]) }
				force_sync: ci == 0 || fail == 'parser:all' || fail == 'parser:${helper_idx}'
			}
		}
		p.a.worker_pool.run(prepass_tasks)
		for ci in 1 .. n_jobs {
			if args[ci].scope != unsafe { nil } {
				prepass_chunks[ci].decls =
					clone_comptime_const_prepass_decls(prepass_chunks[ci].decls)
				parser_worker_scope_free(args[ci].scope)
				args[ci].scope = unsafe { nil }
			}
		}
		mut prefix_values := p.comptime_const_values.clone()
		for chunk_idx in 0 .. n_jobs {
			if chunk_idx > 0 {
				parser_workers[chunk_idx - 1].comptime_const_values = prefix_values.clone()
			}
			apply_parallel_comptime_const_decls(mut prefix_values, prepass_chunks[chunk_idx].decls)
		}
		mut tasks := []workers.Task{cap: n_jobs}
		for ci in 0 .. n_jobs {
			helper_idx := ci - 1
			tasks << workers.Task{
				run:        parse_chunk_thread
				arg:        unsafe { voidptr(&args[ci]) }
				force_sync: ci == 0 || fail == 'parser:all' || fail == 'parser:${helper_idx}'
			}
		}
		any_started := p.a.worker_pool.run(tasks)
		// Merge each helper in fixed chunk order (input file order),
		// so node numbering stays deterministic and byte-identical to serial.
		for ci in 0 .. thread_count {
			p.merge_parsed_worker(mut parser_workers[ci], mut starts, bounds[ci + 1],
				bounds[ci + 2], args[ci + 1].scope)
			parser_worker_scope_free(args[ci + 1].scope)
		}
		p.next_file_id = dispatch_file_id_start + paths.len
		return starts, any_started
	}
}

fn (mut p Parser) precollect_parallel_comptime_consts(paths []string, start int, end int, mut decls []ComptimeConstPrepassDecl) {
	mut values := p.comptime_const_values.clone()
	for path in paths[start..end] {
		// The full parse records the structured I/O diagnostic. The prepass only
		// supplies ordered const snapshots and must not consume a failed read.
		src := read_source_file_raw(path) or { continue }
		if src.len == 0 {
			continue
		}
		mut file_set := token.FileSet.new()
		file := file_set.add_file(path, src.len)
		mut s := scanner.new_scanner(p.prefs, .normal)
		s.init(file, src)
		mut module_name := ''
		module_name = p.precollect_parallel_comptime_scope(mut s, src, path, module_name, false, mut
			values, mut decls)
	}
}

// Follow declaration-level comptime branches so only consts from the selected branch enter
// the ordered worker-prefix snapshots.
fn (mut p Parser) precollect_parallel_comptime_scope(mut s scanner.Scanner, src string, path string, module_name string, stop_at_rcbr bool, mut values map[string]string, mut decls []ComptimeConstPrepassDecl) string {
	mut current_module := module_name
	mut brace_depth := 0
	mut has_pending_attrs := false
	mut pending_decl_disabled := false
	for {
		tok := s.scan()
		if tok == .eof {
			return current_module
		}
		if tok == .lcbr {
			brace_depth++
			continue
		}
		if tok == .rcbr {
			if brace_depth > 0 {
				brace_depth--
				continue
			}
			if stop_at_rcbr {
				return current_module
			}
			continue
		}
		if brace_depth != 0 {
			continue
		}
		if tok == .dollar {
			saved_s := s
			comptime_kind := s.scan()
			if comptime_kind == .key_if {
				current_module = p.precollect_parallel_comptime_if(mut s, src, path,
					current_module, mut values, mut decls)
				continue
			}
			if comptime_kind == .key_match {
				current_module = p.precollect_parallel_comptime_match(mut s, src, path,
					current_module, mut values, mut decls)
				continue
			}
			s = saved_s
			continue
		}
		if tok == .attribute || tok == .lsbr {
			has_pending_attrs = true
			if !p.parallel_decl_attr_enabled(mut s, src, path, current_module) {
				pending_decl_disabled = true
			}
			continue
		}
		if has_pending_attrs {
			if tok == .semicolon || tok == .key_pub {
				continue
			}
			disabled := pending_decl_disabled
			has_pending_attrs = false
			pending_decl_disabled = false
			if tok == .key_const {
				if !disabled {
					p.precollect_parallel_const_decl_and_apply(mut s, current_module, mut values, mut
						decls)
				}
				continue
			}
		}
		if tok == .key_module {
			if s.scan() == .name {
				current_module = s.lit
			}
			continue
		}
		if tok == .key_const {
			p.precollect_parallel_const_decl_and_apply(mut s, current_module, mut values, mut decls)
		}
	}
	return current_module
}

fn (mut p Parser) precollect_parallel_const_decl_and_apply(mut s scanner.Scanner, module_name string, mut values map[string]string, mut decls []ComptimeConstPrepassDecl) {
	start := decls.len
	p.precollect_parallel_const_decl(mut s, module_name, mut decls)
	if decls.len > start {
		apply_parallel_comptime_const_decls(mut values, decls[start..])
	}
}

fn (mut p Parser) precollect_parallel_comptime_if(mut s scanner.Scanner, src string, path string, module_name string, mut values map[string]string, mut decls []ComptimeConstPrepassDecl) string {
	mut current_module := module_name
	mut any_taken := false
	mut has_condition := true
	for {
		mut is_enabled := true
		if has_condition {
			is_enabled = p.parallel_comptime_branch_enabled(mut s, src, path, current_module,
				values)
		}
		take_branch := !any_taken && is_enabled
		if take_branch {
			current_module = p.precollect_parallel_comptime_scope(mut s, src, path, current_module,
				true, mut values, mut decls)
			any_taken = true
		} else {
			skip_parallel_comptime_block(mut s)
		}

		saved_s := s
		mut tok := s.scan()
		for tok == .semicolon {
			tok = s.scan()
		}
		if tok != .dollar || s.scan() != .key_else {
			s = saved_s
			return current_module
		}
		tok = s.scan()
		for tok == .semicolon {
			tok = s.scan()
		}
		if tok == .dollar {
			if s.scan() != .key_if {
				return current_module
			}
			has_condition = true
			continue
		}
		if tok == .key_if {
			has_condition = true
			continue
		}
		if tok != .lcbr {
			return current_module
		}
		has_condition = false
	}
	return current_module
}

fn (mut p Parser) parallel_comptime_branch_enabled(mut s scanner.Scanner, src string, path string, module_name string, values map[string]string) bool {
	mut cond := ''
	mut cond_start := s.offset
	mut prev := ''
	for {
		tok := s.scan()
		if tok == .eof {
			return false
		}
		if tok == .lcbr {
			break
		}
		if tok == .semicolon {
			continue
		}
		if cond.len == 0 {
			cond_start = s.pos
		}
		mut piece := parallel_comptime_prepass_token_text(tok, s, src)
		piece = p.resolve_parallel_comptime_prepass_at_token(piece, s.pos, src, path, module_name)
		if cond.len > 0 && comptime_cond_needs_space(prev, piece) {
			cond += ' '
		}
		cond += piece
		prev = piece
	}
	resolved := p.resolve_parallel_comptime_prepass_text(cond, cond_start, src, path, module_name,
		values, true)
	return p.eval_comptime_cond(resolved)
}

fn (mut p Parser) precollect_parallel_comptime_match(mut s scanner.Scanner, src string, path string, module_name string, mut values map[string]string, mut decls []ComptimeConstPrepassDecl) string {
	mut current_module := module_name
	mut subject := ''
	mut subject_start := s.offset
	mut subject_is_literal := false
	mut subject_has_pseudo := false
	mut prev := ''
	for {
		tok := s.scan()
		if tok == .eof {
			return current_module
		}
		if tok == .lcbr {
			break
		}
		if tok == .semicolon {
			continue
		}
		mut piece := parallel_comptime_prepass_token_text(tok, s, src)
		if piece.starts_with('@') {
			subject_has_pseudo = true
		}
		piece = p.resolve_parallel_comptime_prepass_at_token(piece, s.pos, src, path,
			current_module)
		if subject.len == 0 {
			subject_start = s.pos
			subject_is_literal = tok in [.string, .char, .number, .key_true, .key_false]
		} else if comptime_cond_needs_space(prev, piece) {
			subject += ' '
		}
		subject += piece
		prev = piece
	}
	resolved_subject := p.resolve_parallel_comptime_prepass_text(subject, subject_start, src, path,
		current_module, values, false)
	subject_known := subject_is_literal || subject_has_pseudo || resolved_subject != subject
	mut matched := false
	for {
		mut tok := s.scan()
		for tok == .semicolon {
			tok = s.scan()
		}
		if tok == .eof || tok == .rcbr {
			return current_module
		}

		mut is_else := tok == .key_else
		if tok == .dollar {
			saved_s := s
			if s.scan() == .key_else {
				is_else = true
				tok = s.scan()
				for tok == .semicolon {
					tok = s.scan()
				}
			} else {
				s = saved_s
			}
		} else if is_else {
			tok = s.scan()
			for tok == .semicolon {
				tok = s.scan()
			}
		}

		mut pattern_matches := false
		if !is_else {
			mut pattern := ''
			mut pattern_start := s.pos
			mut pattern_prev := ''
			mut nested_depth := 0
			for {
				if tok == .lcbr && nested_depth == 0 {
					if pattern.len > 0 {
						resolved_pattern := p.resolve_parallel_comptime_prepass_text(pattern,
							pattern_start, src, path, current_module, values, false)
						if comptime_cond_value(resolved_pattern) == comptime_cond_value(resolved_subject) {
							pattern_matches = true
						}
					}
					break
				}
				if tok == .comma && nested_depth == 0 {
					resolved_pattern := p.resolve_parallel_comptime_prepass_text(pattern,
						pattern_start, src, path, current_module, values, false)
					if comptime_cond_value(resolved_pattern) == comptime_cond_value(resolved_subject) {
						pattern_matches = true
					}
					pattern = ''
					pattern_prev = ''
					tok = s.scan()
					pattern_start = s.pos
					continue
				}
				mut piece := parallel_comptime_prepass_token_text(tok, s, src)
				piece = p.resolve_parallel_comptime_prepass_at_token(piece, s.pos, src, path,
					current_module)
				if pattern.len > 0 && comptime_cond_needs_space(pattern_prev, piece) {
					pattern += ' '
				}
				pattern += piece
				pattern_prev = piece
				if tok == .lpar || tok == .lsbr {
					nested_depth++
				} else if (tok == .rpar || tok == .rsbr) && nested_depth > 0 {
					nested_depth--
				}
				tok = s.scan()
				if tok == .eof {
					return current_module
				}
			}
		} else if tok != .lcbr {
			return current_module
		}

		take_arm := subject_known && !matched && (is_else || pattern_matches)
		if take_arm {
			current_module = p.precollect_parallel_comptime_scope(mut s, src, path, current_module,
				true, mut values, mut decls)
			matched = true
		} else {
			skip_parallel_comptime_block(mut s)
		}
	}
	return current_module
}

fn (mut p Parser) resolve_parallel_comptime_prepass_text(text string, pos int, src string, path string, module_name string, values map[string]string, preserve_flags bool) string {
	mut resolver := p.new_parallel_comptime_prepass_resolver(src, path, module_name)
	resolver.tok_pos = pos
	resolver.comptime_const_values = values.clone()
	return resolver.resolve_comptime_cached_values(resolver.resolve_comptime_at_values(text),
		preserve_flags)
}

fn (mut p Parser) resolve_parallel_comptime_prepass_at_token(text string, pos int, src string, path string, module_name string) string {
	if !text.starts_with('@') {
		return text
	}
	mut resolver := p.new_parallel_comptime_prepass_resolver(src, path, module_name)
	return resolver.resolve_comptime_at_values_at(text, pos)
}

fn (p &Parser) new_parallel_comptime_prepass_resolver(src string, path string, module_name string) &Parser {
	mut resolver := Parser.new(p.prefs)
	resolver.cur_file = path
	resolver.cur_module = module_name
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, src.len)
	file.index_lines(src)
	resolver.s.init(file, src)
	return resolver
}

fn parallel_comptime_prepass_token_text(tok token.Token, s &scanner.Scanner, src string) string {
	if tok == .string || tok == .char {
		return comptime_cond_string_token_text(s.lit)
	}
	if s.pos >= 0 && s.offset <= src.len && s.pos < s.offset {
		return src[s.pos..s.offset]
	}
	return s.lit
}

fn skip_parallel_comptime_block(mut s scanner.Scanner) {
	mut depth := 1
	for depth > 0 {
		tok := s.scan()
		if tok == .eof {
			return
		}
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			depth--
		}
	}
}

fn (mut p Parser) parallel_decl_attr_enabled(mut s scanner.Scanner, src string, path string, module_name string) bool {
	first := s.scan()
	if first != .key_if {
		mut bracket_depth := if first == .lsbr || first == .attribute { 1 } else { 0 }
		if first == .rsbr || first == .eof {
			return true
		}
		for {
			tok := s.scan()
			if tok == .eof {
				return true
			}
			if tok == .lsbr || tok == .attribute {
				bracket_depth++
			} else if tok == .rsbr {
				if bracket_depth == 0 {
					return true
				}
				bracket_depth--
			}
		}
	}
	mut cond := ''
	mut cond_start := s.offset
	mut bracket_depth := 0
	mut prev := ''
	for {
		tok := s.scan()
		if tok == .eof {
			break
		}
		if tok == .lsbr || tok == .attribute {
			bracket_depth++
		} else if tok == .rsbr {
			if bracket_depth == 0 {
				break
			}
			bracket_depth--
		}
		if tok == .semicolon {
			continue
		}
		mut piece := parallel_comptime_prepass_token_text(tok, s, src)
		piece = p.resolve_parallel_comptime_prepass_at_token(piece, s.pos, src, path, module_name)
		if cond.len == 0 {
			cond_start = s.pos
		} else if comptime_cond_needs_space(prev, piece) {
			cond += ' '
		}
		cond += piece
		prev = piece
	}
	if !cond.contains('@') {
		return p.eval_comptime_cond(cond)
	}
	mut resolver := p.new_parallel_comptime_prepass_resolver(src, path, module_name)
	resolver.tok_pos = cond_start
	return resolver.eval_comptime_cond(resolver.resolve_comptime_at_values(cond))
}

fn (mut p Parser) precollect_parallel_const_decl(mut s scanner.Scanner, module_name string, mut decls []ComptimeConstPrepassDecl) {
	mut tok := s.scan()
	grouped := tok == .lpar
	if grouped {
		tok = s.scan()
	}
	for tok != .eof {
		for tok == .semicolon {
			tok = s.scan()
		}
		if grouped && tok == .rpar {
			return
		}
		if tok != .name {
			return
		}
		name := s.lit
		tok = s.scan()
		if tok != .assign {
			return
		}
		mut value_tokens := []ComptimeConstPrepassToken{}
		mut paren_depth := 0
		mut bracket_depth := 0
		mut brace_depth := 0
		mut closed_group := false
		for {
			before_tok := s
			tok = s.scan()
			if tok == .eof {
				break
			}
			if tok == .semicolon && paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 {
				break
			}
			if tok == .rcbr && paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 {
				s = before_tok
				break
			}
			if tok == .rpar {
				if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 && grouped {
					closed_group = true
					break
				}
				if paren_depth > 0 {
					paren_depth--
				}
			} else if tok == .lpar {
				paren_depth++
			} else if tok == .lsbr {
				bracket_depth++
			} else if tok == .rsbr && bracket_depth > 0 {
				bracket_depth--
			} else if tok == .lcbr {
				brace_depth++
			} else if tok == .rcbr && brace_depth > 0 {
				brace_depth--
			}
			value_tokens << ComptimeConstPrepassToken{
				tok: tok
				lit: s.lit
			}
		}
		if value := parallel_comptime_const_value(value_tokens) {
			decls << ComptimeConstPrepassDecl{
				key:   comptime_const_value_key(module_name, name)
				value: value
			}
		}
		if !grouped || closed_group || tok == .eof {
			return
		}
		tok = s.scan()
	}
}

fn parallel_comptime_const_value(tokens []ComptimeConstPrepassToken) ?string {
	mut start := 0
	mut end := tokens.len
	for end - start >= 2 && tokens[start].tok == .lpar && tokens[end - 1].tok == .rpar {
		start++
		end--
	}
	if end - start != 1 {
		return none
	}
	t := tokens[start]
	return match t.tok {
		.number {
			t.lit
		}
		.key_true {
			'true'
		}
		.key_false {
			'false'
		}
		.char {
			'`${t.lit}`'
		}
		.string {
			comptime_cond_quoted_string(strip_quotes(t.lit))
		}
		.name {
			comptime_const_prepass_alias_prefix + t.lit
		}
		else {
			none
		}
	}
}

fn apply_parallel_comptime_const_decls(mut values map[string]string, decls []ComptimeConstPrepassDecl) {
	for decl in decls {
		if !decl.value.starts_with(comptime_const_prepass_alias_prefix) {
			values[decl.key] = decl.value
			continue
		}
		alias_name := decl.value[comptime_const_prepass_alias_prefix.len..]
		module_name := decl.key.all_before('\n')
		module_key := comptime_const_value_key(module_name, alias_name)
		builtin_key := comptime_const_value_key('builtin', alias_name)
		if resolved := values[module_key] {
			values[decl.key] = resolved
		} else if resolved := values[builtin_key] {
			values[decl.key] = resolved
		}
	}
}

// merge_parsed_worker appends a finished worker's parsed output to the master
// AST. Worker node ids and children offsets are 0-based (the worker started
// from an empty FlatAst), so every node-id reference moves by the master's
// node count and every children_start by the master's children count at merge
// time. Negative child slots (flat.empty_node sentinels) are left untouched.
fn (mut p Parser) merge_parsed_worker(mut w Parser, mut starts []int, chunk_start int, chunk_end int, worker_scope voidptr) {
	node_shift := p.a.nodes.len
	child_shift := i32(p.a.children.len)
	new_children := w.a.children.len
	if new_children > 0 {
		old_len := p.a.children.len
		unsafe {
			p.a.children.grow_len(new_children)
			vmemcpy(&p.a.children[old_len], &w.a.children[0],
				new_children * int(sizeof(flat.NodeId)))
			// Ownership of the copied elements moved to the master.
			w.a.children.len = 0
		}
		for k in old_len .. p.a.children.len {
			cid := p.a.children[k]
			if int(cid) >= 0 {
				p.a.children[k] = flat.NodeId(int(cid) + node_shift)
			}
		}
	}
	new_nodes := w.a.nodes.len
	if new_nodes > 0 {
		for k in 0 .. new_nodes {
			if w.a.nodes[k].children_count != 0 {
				w.a.nodes[k].children_start += child_shift
			}
		}
		nodes_old_len := p.a.nodes.len
		unsafe {
			p.a.nodes.grow_len(new_nodes)
			vmemcpy(&p.a.nodes[nodes_old_len], &w.a.nodes[0], new_nodes * int(sizeof(flat.Node)))
			// flat.Node owns strings/slices. Zero the source length after the raw
			// move so only the master owns and eventually releases those payloads.
			w.a.nodes.len = 0
		}
		for k in nodes_old_len .. p.a.nodes.len {
			// Declaration attributes are linked by an internal directive whose value embeds the
			// worker-local declaration id. Relocate that id just like child NodeId references.
			if p.a.nodes[k].kind == .directive && p.a.nodes[k].value.starts_with('@attributes:') {
				local_id := p.a.nodes[k].value['@attributes:'.len..].int()
				unsafe {
					mut node := &p.a.nodes[k]
					node.value = '@attributes:${local_id + node_shift}'
				}
			}
		}
		// Worker text tables are private. Rebind every moved payload to the
		// master's compilation-wide canonical text table before the worker dies.
		p.a.intern_node_texts_from(nodes_old_len)
	}
	// Per-file region starts move by the merge offset.
	for i in chunk_start .. chunk_end {
		starts[i] += node_shift
	}
	// The worker validated its exports against its own files only; revalidate
	// them here against disabled fns accumulated from earlier chunks, exactly
	// like the serial parse where register_pending_export sees every previously
	// parsed file. The worker's own disabled marks are folded in afterwards, so
	// a disable in a later file keeps earlier exports — matching the serial
	// order-dependent behavior.
	for rec in w.export_records {
		if rec.name in p.a.disabled_fns || rec.qname in p.a.disabled_fns {
			continue
		}
		_, qname := p.a.intern_text(rec.qname)
		_, value := p.a.intern_text(rec.value)
		p.a.export_fn_names[qname] = value
	}
	for name, disabled in w.a.disabled_fns {
		if disabled {
			_, canonical := p.a.intern_text(name)
			p.a.disabled_fns[canonical] = true
		}
	}
	for name, is_noreturn in w.a.noreturn_fns {
		if is_noreturn {
			_, canonical := p.a.intern_text(name)
			p.a.noreturn_fns[canonical] = true
		}
	}
	for diagnostic in w.diagnostics {
		p.append_diagnostic(Diagnostic{
			file:    diagnostic.file.clone()
			pos:     diagnostic.pos
			line:    diagnostic.line
			column:  diagnostic.column
			message: diagnostic.message.clone()
		})
	}
	if worker_scope == unsafe { nil } {
		for source in w.a.source_buffers {
			p.a.source_buffers << source
		}
		unsafe {
			// The string headers moved to the master AST. The worker must no longer
			// release the storage referenced by them.
			w.a.source_buffers.len = 0
		}
		for file_id, file in w.a.source_files {
			p.a.source_files[file_id] = file
		}
	} else {
		// Node and metadata text has already been promoted into the master text
		// table. Rebuild only the compact line indexes needed by diagnostics and
		// let the much larger worker source buffers die with the task arena.
		mut file_ids := w.a.source_files.keys()
		file_ids.sort()
		for source_idx, file_id in file_ids {
			file := w.a.source_files[file_id] or { continue }
			if source_idx >= w.a.source_buffers.len {
				continue
			}
			source := w.a.source_buffers[source_idx]
			mut file_set := token.FileSet.new()
			mut stored_file := file_set.add_file(file.name.clone(), file.size)
			stored_file.index_lines(source)
			p.a.source_files[file_id] = stored_file
		}
	}
	for key, value in w.comptime_const_values {
		if key !in p.comptime_const_values {
			p.comptime_const_values[key.clone()] = value.clone()
		}
	}
	p.parsed_v_files += w.parsed_v_files
	for path in w.parsed_v_file_paths {
		p.parsed_v_file_paths << path.clone()
	}
	p.parsed_v_header_files += w.parsed_v_header_files
	for path in w.parsed_v_header_file_paths {
		p.parsed_v_header_file_paths << path.clone()
	}
}

// parse_job_count caps the worker count by the runtime job count, a fixed
// ceiling and the file count.
fn parse_job_count(n_runtime_jobs int, n_files int) int {
	if n_runtime_jobs <= 0 || n_files <= 0 {
		return 0
	}
	mut n := n_runtime_jobs
	if n > max_parallel_parse_jobs {
		n = max_parallel_parse_jobs
	}
	if n > n_files {
		n = n_files
	}
	return n
}

// parse_chunk_bounds splits the file list into n contiguous ranges of roughly
// equal source byte count (parse time tracks source size closely). Contiguity
// in input order is required so the ordered merge reproduces the serial layout.
fn parse_chunk_bounds(sizes []i64, n int) []int {
	mut total := i64(0)
	for size in sizes {
		total += size + 1
	}
	mut bounds := []int{cap: n + 1}
	bounds << 0
	mut acc := i64(0)
	mut chunk := 1
	for i in 0 .. sizes.len {
		acc += sizes[i] + 1
		if chunk < n && acc >= total * i64(chunk) / i64(n) {
			bounds << i + 1
			chunk++
		}
	}
	for bounds.len < n + 1 {
		bounds << sizes.len
	}
	return bounds
}
