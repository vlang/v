module types

import hash
import strings
import v.flat
import v.token

// An incremental check is the check of a diagnostics server's child that checks
// again only the function bodies whose code changed since the last check of the
// program that recorded what its bodies reported (see incremental_record), and
// takes what the others reported from that record. The code of a body is its
// region: the lines from the one its declaration starts on to the one where the
// next declaration of its file starts. A region of the same text, in a program
// that declares the same, reports the same, moved with its region.
//
// Everything is checked anew when the record cannot tell: another set of files,
// a changed declaration, or no record. And a body is checked anew when its
// region names its own place (`@LINE`) or reads another file (`$tmpl`), when it
// reported something outside its region, and when another body of its file goes
// by the same name.
//
// What the rest of the check reads of the bodies left out is put back from the
// record too: the functions they name as values (the unused declarations), the
// functions they call (the unused imports), and their warnings about unhandled
// Results. What needs their types, markused and the instances of generic
// functions, checks them first (see complete_incremental_check), as a question
// of the editor does.

const incremental_record_header = 'v-incremental-check 2'
const incremental_seed = u64(0x9e3779b97f4a7c15)

// incremental_min_left_out is the fewest nodes that the bodies a check leaves
// out hold by default (see start_incremental_check): leaving out less saves
// less than putting back what they reported and checking them later costs.
pub const incremental_min_left_out = 2048

// The list a diagnostic of a body goes back to.
const incremental_errors = 0
const incremental_notices = 1
const incremental_pending = 2
const incremental_unhandled = 3 // the warnings of warn_unhandled_result_calls

// IncrementalCheck is what an incremental check keeps (see
// start_incremental_check).
pub struct IncrementalCheck {
mut:
	own_files    map[string]bool // the files the child parsed, not the server
	verify       bool            // a completion compares what it finds with what was put back
	min_left_out int             // the fewest nodes the bodies left out hold
	earlier      IncrementalRecord
	has_earlier  bool
	declarations u64
	files        string
	file_index   map[string]int        // the place of each of its files in `files`
	selected     bool                  // incremental_select ran
	functions    []IncrementalFunction // every body of the check, in its order
	skipped      []int                 // the indexes in functions of the bodies left out
	completed    bool                  // they were checked since (see complete_incremental_check)
	completing   bool                  // that check began
	next         int                   // the first of them it did not check yet
	// What that check changes, and puts back as it was.
	saved_errors  int
	saved_notices int
	saved_pending int
	saved_file    string
	saved_module  string
	saved_capture bool
	saved_trusted bool
	captured      map[int]IncrementalCaptured // what each checked body reported, by fn_idx
	call_names    map[int]string              // the calls of the bodies left out, by node
	fn_values     []int                       // the nodes of those bodies named as functions, put back
	trace         []string
}

// IncrementalFunction is a body of an incremental check, and its region.
struct IncrementalFunction {
	item     CheckWorkItem
	key      string
	file_id  int
	start    int
	end      int
	hash     u64
	reusable bool // its region names no place of its own
mut:
	earlier int = -1 // its entry in the earlier record, when it is left out
}

// IncrementalCaptured is what the check of a body reported.
struct IncrementalCaptured {
mut:
	errors  []TypeError
	notices []TypeError
	pending []PendingIerrorError
}

// IncrementalItemMark is where the diagnostics of a checked body end in the
// lists of the fork that checked it (see incremental_capture).
struct IncrementalItemMark {
	fn_idx  int
	errors  int
	notices int
	pending int
}

// IncrementalRecord is what a check recorded for the next one, read back:
// an entry for each body, whose details are read when they are needed (see
// details).
struct IncrementalRecord {
mut:
	text         string
	declarations u64
	files        string
	entries      []IncrementalStored
	by_key       map[string]int
}

// IncrementalStored is the entry of a body in a record read back: its lines in
// the text of the record, the first one and those of its details after it.
struct IncrementalStored {
	key           string
	hash          u64
	length        int
	range_len     int
	reusable      bool
	start         int
	details_start int
	end           int
}

// IncrementalEntry is the entry of a body that a record is written with.
struct IncrementalEntry {
mut:
	key       string
	hash      u64
	length    int
	range_len int
	reusable  bool
	details   IncrementalDetails
}

// IncrementalDetails are what a body reported, and what the rest of the check
// read of it, placed relative to its region.
struct IncrementalDetails {
mut:
	diagnostics []IncrementalDiagnostic
	calls       []IncrementalName
	fn_values   []IncrementalName
}

// IncrementalDiagnostic is a diagnostic of a body: `back` counts its node back
// from the node of the function, and `offset` and `end` its place from the
// start of the region.
struct IncrementalDiagnostic {
	list     int
	kind     int
	back     int
	offset   int
	end      int
	meta     u16
	order    int
	severity string
	msg      string
	fn_qname string
	details  []string
}

// IncrementalName is a name the check gave a node of a body.
struct IncrementalName {
	back int
	name string
}

// start_incremental_check has the check of a diagnostics server's child note
// what each body reports, for the next check of the program, and leave out the
// bodies whose regions `record`, what an earlier check noted, holds unchanged.
// `own` are the files the child parsed: those of the program, not those the
// server prepared for every child. With `verify`, a check of the bodies left
// out (see complete_incremental_check) compares what they report with what
// was put back. The check leaves out no body unless those it would leave out
// hold `min_left_out` nodes.
pub fn (mut tc TypeChecker) start_incremental_check(record string, own []string, verify bool, min_left_out int) {
	mut state := &IncrementalCheck{
		verify:       verify
		min_left_out: min_left_out
	}
	for name in own {
		state.own_files[name] = true
	}
	if earlier := decode_incremental_record(record) {
		state.earlier = earlier
		state.has_earlier = true
	}
	tc.incremental = state
}

// take_incremental_trace returns what an incremental check decided since it was
// last asked, a line for each decision.
pub fn (mut tc TypeChecker) take_incremental_trace() []string {
	if isnil(tc.incremental) || tc.incremental.trace.len == 0 {
		return []string{}
	}
	mut state := tc.incremental
	lines := state.trace.clone()
	state.trace.clear()
	return lines
}

// incremental_select returns the items whose bodies an incremental check
// checks: all of them without a record that holds the same files and
// declarations, and otherwise those whose regions the record does not hold
// unchanged. The check notes what each body it checks reports.
fn (mut tc TypeChecker) incremental_select(items []CheckWorkItem) []CheckWorkItem {
	if isnil(tc.incremental) || tc.incremental.selected {
		return items
	}
	mut state := tc.incremental
	// A program with less than that in all is checked as ever: what the check
	// notes for the next one would cost more than it can save.
	mut total := 0
	for item in items {
		total += item.fn_idx - item.range_lo + 1
	}
	if total < state.min_left_out {
		state.trace << 'incremental: every body checked (${total} nodes in all)'
		return items
	}
	state.selected = true
	tc.capture_items = true
	state.declarations, state.files = tc.incremental_declarations(state.own_files)
	for i, name in state.files.split_into_lines() {
		if name !in state.file_index {
			state.file_index[name] = i
		}
	}
	state.functions = tc.incremental_functions(items, state.file_index)
	reason := if state.files == '' {
		'no file of the program'
	} else if !state.has_earlier {
		'no earlier check'
	} else if state.earlier.files != state.files {
		'other files'
	} else if state.earlier.declarations != state.declarations {
		'declarations changed'
	} else {
		''
	}
	if reason != '' {
		state.trace << 'incremental: every body checked (${reason})'
		return items
	}
	mut names := map[string]int{}
	for f in state.functions {
		names[f.key]++
	}
	mut selected := []CheckWorkItem{cap: 8}
	mut left_out := 0
	for i, f in state.functions {
		if f.reusable && names[f.key] == 1 {
			if at := state.earlier.by_key[f.key] {
				entry := state.earlier.entries[at]
				if entry.reusable && entry.hash == f.hash && entry.length == f.end - f.start
					&& entry.range_len == f.item.fn_idx - f.item.range_lo {
					state.functions[i].earlier = at
					state.skipped << i
					left_out += f.item.fn_idx - f.item.range_lo + 1
					continue
				}
			}
		}
		selected << f.item
	}
	if left_out < state.min_left_out {
		for i in state.skipped {
			state.functions[i].earlier = -1
		}
		state.skipped.clear()
		state.trace << 'incremental: every body checked (${left_out} nodes to leave out)'
		return items
	}
	state.trace << 'incremental: ${selected.len} of ${items.len} bodies checked'
	return selected
}

// incremental_skipping reports whether the check leaves out bodies.
fn (tc &TypeChecker) incremental_skipping() bool {
	return !isnil(tc.incremental) && tc.incremental.skipped.len > 0
}

// incremental_declarations returns a sum of what the files `own` declare, as
// the bodies of their functions see it, and the names of those files in order.
// Neither the places of the declarations nor the bodies of the functions count.
fn (tc &TypeChecker) incremental_declarations(own map[string]bool) (u64, string) {
	mut sum := incremental_seed
	mut files := strings.new_builder(256)
	mut b := strings.new_builder(4096)
	mut in_own := false
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .file {
			in_own = own[node.value]
			if in_own {
				files.write_string(node.value)
				files.write_u8(`\n`)
			}
			continue
		}
		// The attributes of a declaration name its node: they count with it.
		if !in_own || (node.kind == .directive && node.value.starts_with('@attributes:')) {
			continue
		}
		b.go_back_to(0)
		if node.kind == .fn_decl {
			// Its children are its parameters, then the statements of its body.
			mut params := []flat.NodeId{}
			for i in 0 .. node.children_count {
				child_id := tc.a.child(&node, i)
				if tc.valid_node_id(child_id) && tc.a.node(child_id).kind == .param {
					params << child_id
				}
			}
			incremental_write_node(mut b, &node, params.len)
			for param in params {
				tc.incremental_write_tree(mut b, param)
			}
		} else {
			tc.incremental_write_tree(mut b, flat.NodeId(idx))
		}
		for attribute in tc.declaration_attributes[idx] or { []string{} } {
			b.write_string(attribute)
			b.write_u8(0)
		}
		sum = hash.wyhash_c(b.data, u64(b.len), sum)
	}
	return sum, files.str()
}

// incremental_write_tree writes the nodes of the tree `root` into `b`, without
// their places.
fn (tc &TypeChecker) incremental_write_tree(mut b strings.Builder, root flat.NodeId) {
	mut stack := [root]
	for stack.len > 0 {
		id := stack.pop()
		if !tc.valid_node_id(id) {
			b.write_u8(1)
			continue
		}
		node := tc.a.node(id)
		incremental_write_node(mut b, node, node.children_count)
		for i := node.children_count - 1; i >= 0; i-- {
			stack << tc.a.child(node, i)
		}
	}
}

// incremental_write_node writes into `b` what the node `node` holds, with
// `children` children that count, but its place and where its children are.
fn incremental_write_node(mut b strings.Builder, node &flat.Node, children int) {
	b.write_decimal(i64(int(node.kind)))
	b.write_u8(` `)
	b.write_decimal(i64(int(node.op)))
	b.write_u8(` `)
	b.write_decimal(i64(node.flags))
	b.write_u8(if node.is_mut { `m` } else { `-` })
	b.write_decimal(i64(children))
	b.write_u8(0)
	b.write_string(node.typ)
	b.write_u8(0)
	b.write_string(node.value)
	b.write_u8(0)
	if node.payload != 0 {
		payload := flat.node_payload_at(node.payload)
		if !isnil(payload) {
			for param in payload.generic_params {
				b.write_string(param)
				b.write_u8(1)
			}
			b.write_u8(2)
			for constraint in payload.generic_constraints {
				b.write_string(constraint)
				b.write_u8(1)
			}
		}
	}
	b.write_u8(3)
}

// incremental_functions notes the region of the body of each item. A body goes
// by the place of its file in `file_index` and its name.
fn (tc &TypeChecker) incremental_functions(items []CheckWorkItem, file_index map[string]int) []IncrementalFunction {
	// The starts of the lines where the declarations of each file of a body
	// start.
	mut starts := map[int][]int{}
	for item in items {
		starts[int(tc.a.nodes[item.fn_idx].pos.id)] = []int{}
	}
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .file || !node.pos.is_valid() || int(node.pos.id) !in starts {
			continue
		}
		file := tc.a.source_files[node.pos.id] or { continue }
		source := tc.source_texts_by_file[file.name] or { continue }
		starts[int(node.pos.id)] << incremental_line_start(source, int(node.pos.offset))
	}
	for _, mut file_starts in starts {
		file_starts.sort()
	}
	mut functions := []IncrementalFunction{cap: items.len}
	for item in items {
		node := tc.a.nodes[item.fn_idx]
		file_id := int(node.pos.id)
		key := '${file_index[item.file] or { -1 }}\t${node.value}'
		file := tc.a.source_files[file_id] or {
			functions << IncrementalFunction{
				item: item
				key:  key
			}
			continue
		}
		source := tc.source_texts_by_file[file.name] or {
			functions << IncrementalFunction{
				item: item
				key:  key
			}
			continue
		}
		if !node.pos.is_valid() || int(node.pos.offset) > source.len {
			functions << IncrementalFunction{
				item: item
				key:  key
			}
			continue
		}
		start := incremental_line_start(source, int(node.pos.offset))
		end := incremental_next_start(starts[file_id] or { []int{} }, start, source.len)
		region := source[start..end]
		functions << IncrementalFunction{
			item:     item
			key:      key
			file_id:  file_id
			start:    start
			end:      end
			hash:     hash.sum64_string(region, incremental_seed)
			reusable: !incremental_region_names_its_place(region)
		}
	}
	return functions
}

// incremental_line_start returns where the line of `offset` starts.
fn incremental_line_start(source string, offset int) int {
	mut start := int_min(offset, source.len)
	for start > 0 && source[start - 1] != `\n` {
		start--
	}
	return start
}

// incremental_next_start returns the first of the sorted `starts` after
// `start`, or `end` without one.
fn incremental_next_start(starts []int, start int, end int) int {
	mut lo := 0
	mut hi := starts.len
	for lo < hi {
		mid := (lo + hi) / 2
		if starts[mid] <= start {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	return if lo < starts.len { starts[lo] } else { end }
}

// incremental_region_names_its_place reports whether the code `region` names
// where it is, as `@LINE` does, or reads another file, as `$tmpl` does: the
// same text elsewhere may report otherwise.
fn incremental_region_names_its_place(region string) bool {
	for i, c in region {
		if c == `@` {
			if incremental_word_at(region, i + 1, 'LINE') || incremental_word_at(region, i + 1, 'COLUMN')
				|| incremental_word_at(region, i + 1, 'LOCATION')
				|| incremental_word_at(region, i + 1, 'FILE_LINE') {
				return true
			}
		} else if c == `$` {
			if incremental_word_at(region, i + 1, 'tmpl')
				|| incremental_word_at(region, i + 1, 'embed_file') {
				return true
			}
		}
	}
	return false
}

// incremental_word_at reports whether `word` is in `s` at `at`.
fn incremental_word_at(s string, at int, word string) bool {
	if at + word.len > s.len {
		return false
	}
	for j in 0 .. word.len {
		if s[at + j] != word[j] {
			return false
		}
	}
	return true
}

// incremental_capture notes what each body that the fork `w` checked reported:
// the diagnostics between its mark and the one before (see
// check_fn_items_serial).
fn (mut tc TypeChecker) incremental_capture(w &TypeChecker, scoped bool) {
	mut state := tc.incremental
	mut errors_at := 0
	mut notices_at := 0
	mut pending_at := 0
	for mark in w.item_marks {
		mut captured := IncrementalCaptured{}
		for err in w.errors[errors_at..mark.errors] {
			captured.errors << if scoped { clone_parallel_type_error(err) } else { err }
		}
		for notice in w.notices[notices_at..mark.notices] {
			captured.notices << if scoped { clone_parallel_type_error(notice) } else { notice }
		}
		for pending in w.pending_ierror_errors[pending_at..mark.pending] {
			captured.pending << if scoped {
				PendingIerrorError{
					err:      clone_parallel_type_error(pending.err)
					fn_qname: pending.fn_qname.clone()
				}
			} else {
				pending
			}
		}
		state.captured[mark.fn_idx] = captured
		errors_at = mark.errors
		notices_at = mark.notices
		pending_at = mark.pending
	}
}

// check_incremental_items checks the bodies of `items`, which incremental_select
// left, and puts back what the others reported (see incremental_put_back).
fn (mut tc TypeChecker) check_incremental_items(items []CheckWorkItem) {
	errors_start := tc.errors.len
	notices_start := tc.notices.len
	pending_start := tc.pending_ierror_errors.len
	tc.check_scoped_batches(items, scoped_check_serial_batches)
	tc.incremental_put_back(errors_start, notices_start, pending_start)
}

// incremental_put_back puts what the bodies left out reported in the lists of
// the check, between those of the bodies it checked, as a check of every body
// lists them: those after `errors_start`, `notices_start` and `pending_start`.
// And it puts back the functions they name as values, and the calls they make.
fn (mut tc TypeChecker) incremental_put_back(errors_start int, notices_start int, pending_start int) {
	mut state := tc.incremental
	mut errors := []TypeError{}
	mut notices := []TypeError{}
	mut pending := []PendingIerrorError{}
	mut checked_errors := 0
	mut checked_notices := 0
	mut checked_pending := 0
	mut skipped_at := 0
	saved_file := tc.cur_file
	for i, f in state.functions {
		if skipped_at < state.skipped.len && state.skipped[skipped_at] == i {
			skipped_at++
			entry := state.earlier.details(f.earlier)
			tc.cur_file = f.item.file
			for d in entry.diagnostics {
				match d.list {
					incremental_errors {
						errors << tc.incremental_diagnostic(f, d)
					}
					incremental_notices {
						notices << tc.incremental_diagnostic(f, d)
					}
					incremental_pending {
						pending << PendingIerrorError{
							err:      tc.incremental_diagnostic(f, d)
							fn_qname: d.fn_qname
						}
					}
					else {}
				}
			}
			for value in entry.fn_values {
				tc.set_resolved_fn_value(f.item.fn_idx - value.back, value.name)
				state.fn_values << f.item.fn_idx - value.back
			}
			for call in entry.calls {
				state.call_names[f.item.fn_idx - call.back] = call.name
			}
			continue
		}
		captured := state.captured[f.item.fn_idx] or { IncrementalCaptured{} }
		errors << captured.errors
		notices << captured.notices
		pending << captured.pending
		checked_errors += captured.errors.len
		checked_notices += captured.notices.len
		checked_pending += captured.pending.len
	}
	tc.cur_file = saved_file
	// The lists end with what the checked bodies reported, in their order: that
	// part becomes what every body reported. Were it otherwise, what was put back
	// comes after it.
	if tc.errors.len - errors_start == checked_errors
		&& tc.notices.len - notices_start == checked_notices
		&& tc.pending_ierror_errors.len - pending_start == checked_pending {
		tc.errors.trim(errors_start)
		tc.notices.trim(notices_start)
		tc.pending_ierror_errors.trim(pending_start)
		tc.errors << errors
		tc.notices << notices
		tc.pending_ierror_errors << pending
		return
	}
	state.trace << 'incremental: the checked bodies reported out of order'
	skipped_at = 0
	for i, f in state.functions {
		if skipped_at >= state.skipped.len || state.skipped[skipped_at] != i {
			continue
		}
		skipped_at++
		entry := state.earlier.details(f.earlier)
		tc.cur_file = f.item.file
		for d in entry.diagnostics {
			match d.list {
				incremental_errors {
					tc.errors << tc.incremental_diagnostic(f, d)
				}
				incremental_notices {
					tc.notices << tc.incremental_diagnostic(f, d)
				}
				incremental_pending {
					tc.pending_ierror_errors << PendingIerrorError{
						err:      tc.incremental_diagnostic(f, d)
						fn_qname: d.fn_qname
					}
				}
				else {}
			}
		}
	}
	tc.cur_file = saved_file
}

// incremental_diagnostic returns the diagnostic `d` of the body `f`, in its
// region now.
fn (tc &TypeChecker) incremental_diagnostic(f IncrementalFunction, d IncrementalDiagnostic) TypeError {
	pos := token.Pos{
		offset: i32(f.start + d.offset)
		end:    i32(f.start + d.end)
		id:     i32(f.file_id)
		meta:   d.meta
	}
	base := tc.make_type_error_at(unsafe { TypeErrorKind(d.kind) }, d.msg, flat.NodeId(f.item.fn_idx - d.back),
		pos)
	return TypeError{
		...base
		details:          d.details
		severity:         d.severity
		diagnostic_order: d.order
	}
}

// incremental_call_name returns the function that the call `idx` of a body the
// check left out calls, or ''.
fn (tc &TypeChecker) incremental_call_name(idx int) string {
	if isnil(tc.incremental) || tc.incremental.completed {
		return ''
	}
	return tc.incremental.call_names[idx] or { '' }
}

// put_back_incremental_unhandled puts in tc.notices the warnings about unhandled
// Results of the bodies the check left out, which warn_unhandled_result_calls
// cannot find there without their types, and returns where its warnings start.
pub fn (mut tc TypeChecker) put_back_incremental_unhandled() int {
	start := tc.notices.len
	if !tc.incremental_skipping() || tc.incremental.completed {
		return start
	}
	state := tc.incremental
	saved_file := tc.cur_file
	for i in state.skipped {
		f := state.functions[i]
		tc.cur_file = f.item.file
		for d in state.earlier.details(f.earlier).diagnostics {
			if d.list == incremental_unhandled {
				tc.notices << tc.incremental_diagnostic(f, d)
			}
		}
	}
	tc.cur_file = saved_file
	return start
}

// sort_incremental_unhandled orders the warnings about unhandled Results from
// `start` on as warn_unhandled_result_calls finds them, by their calls: those
// put back come first.
pub fn (mut tc TypeChecker) sort_incremental_unhandled(start int) {
	if !tc.incremental_skipping() || tc.notices.len - start < 2 {
		return
	}
	mut warnings := tc.notices[start..].clone()
	warnings.sort_with_compare(fn (a &TypeError, b &TypeError) int {
		return int(a.node) - int(b.node)
	})
	tc.notices.trim(start)
	tc.notices << warnings
}

// complete_incremental_check checks the bodies the check left out, for what
// needs their types: markused, the instances of generic functions, and the
// questions of the editor. What they report was put back already: it is left
// out, and with `verify`, compared with what was put back.
pub fn (mut tc TypeChecker) complete_incremental_check() {
	for tc.complete_incremental_check_step(max_int) {
	}
}

// complete_incremental_check_step checks up to `limit` more of the bodies the
// check left out (see complete_incremental_check), and reports whether some
// remain: a child of a diagnostics server checks them a few at a time while
// it waits for a question.
pub fn (mut tc TypeChecker) complete_incremental_check_step(limit int) bool {
	if !tc.incremental_skipping() || tc.incremental.completed {
		return false
	}
	mut state := tc.incremental
	if !state.completing {
		state.completing = true
		// What was put back: the check finds it again.
		for idx in state.fn_values {
			tc.clear_resolved_fn_value(flat.NodeId(idx))
		}
		state.saved_errors = tc.errors.len
		state.saved_notices = tc.notices.len
		state.saved_pending = tc.pending_ierror_errors.len
		state.saved_file = tc.cur_file
		state.saved_module = tc.cur_module
		state.saved_capture = tc.capture_items
		state.saved_trusted = tc.direct_parent_index_trusted
		tc.capture_items = state.verify
		// As the check found them: the index of the parents holds for the tree the
		// check checked, unless nodes came since, and the rest of the check, which
		// changes the tree, builds it again before it completes the check.
		if !state.saved_trusted {
			tc.reuse_direct_parent_index_for_unchanged_ast(tc.a)
		}
		tc.resolution_type_mode = false
		tc.install_type_cache_overlay()
	}
	end := if limit >= state.skipped.len - state.next {
		state.skipped.len
	} else {
		state.next + limit
	}
	mut items := []CheckWorkItem{cap: end - state.next}
	for i in state.skipped[state.next..end] {
		items << state.functions[i].item
	}
	state.next = end
	tc.check_scoped_batches(items, scoped_check_serial_batches)
	if state.next < state.skipped.len {
		return true
	}
	tc.restore_type_cache_base()
	tc.direct_parent_index_trusted = state.saved_trusted
	tc.resolution_type_mode = true
	tc.capture_items = state.saved_capture
	tc.cur_file = state.saved_file
	tc.cur_module = state.saved_module
	state.completed = true
	if state.verify {
		tc.verify_incremental_check()
	}
	tc.errors.trim(state.saved_errors)
	tc.notices.trim(state.saved_notices)
	tc.pending_ierror_errors.trim(state.saved_pending)
	return false
}

// verify_incremental_check compares what the bodies left out reported once
// checked with what was put back for them, and notes each difference in the
// trace.
fn (mut tc TypeChecker) verify_incremental_check() {
	mut state := tc.incremental
	for i in state.skipped {
		f := state.functions[i]
		captured := state.captured[f.item.fn_idx] or {
			state.trace << 'incremental: ${f.key}: not checked again'
			continue
		}
		mut found := []string{}
		for err in captured.errors {
			found << incremental_verify_line(incremental_errors, err)
		}
		for notice in captured.notices {
			found << incremental_verify_line(incremental_notices, notice)
		}
		for pending in captured.pending {
			found << incremental_verify_line(incremental_pending, pending.err)
		}
		mut put_back := []string{}
		for d in state.earlier.details(f.earlier).diagnostics {
			if d.list != incremental_unhandled {
				put_back << incremental_verify_line(d.list, tc.incremental_diagnostic(f, d))
			}
		}
		if found != put_back {
			state.trace << 'incremental: ${f.key}: put back ${put_back}, found ${found}'
		}
	}
}

fn incremental_verify_line(list int, err TypeError) string {
	return '${list} ${int(err.node)} ${err.pos.offset}-${err.pos.end} ${err.severity} ${err.msg} ${err.details}'
}

// incremental_record returns what the check found in each body, for the next
// incremental check of the program, or '' when it noted nothing (see
// incremental_select). `unhandled_start` is where the warnings of
// warn_unhandled_result_calls start in tc.notices.
pub fn (tc &TypeChecker) incremental_record(unhandled_start int) string {
	if isnil(tc.incremental) || !tc.incremental.selected {
		return ''
	}
	state := tc.incremental
	// The bodies by their nodes, to find the one around a node.
	mut by_node := []int{len: state.functions.len, init: index}
	by_node.sort_with_compare(fn [state] (a &int, b &int) int {
		return state.functions[*a].item.fn_idx - state.functions[*b].item.fn_idx
	})
	mut unhandled := map[int][]TypeError{}
	for notice in tc.notices[int_min(unhandled_start, tc.notices.len)..] {
		i := state.function_around(by_node, int(notice.node))
		if i >= 0 {
			unhandled[i] << notice
		}
	}
	mut fn_values := map[int][]IncrementalName{}
	for idx, name in tc.sparse_resolved_fn_values {
		i := state.function_around(by_node, idx)
		if i >= 0 {
			fn_values[i] << IncrementalName{
				back: state.functions[i].item.fn_idx - idx
				name: name
			}
		}
	}
	selective := tc.incremental_selective_import_files()
	mut b := strings.new_builder(4096 + state.functions.len * 64)
	b.writeln(incremental_record_header)
	b.writeln('declarations\t${state.declarations.hex()}')
	b.writeln('files\t${incremental_escape(state.files)}')
	mut skipped_at := 0
	for i, f in state.functions {
		if skipped_at < state.skipped.len && state.skipped[skipped_at] == i {
			skipped_at++
			// Its lines, as they were.
			stored := state.earlier.entries[f.earlier]
			unsafe { b.write_ptr(state.earlier.text.str + stored.start, stored.end - stored.start) }
			continue
		}
		mut entry := IncrementalEntry{
			key:       f.key
			hash:      f.hash
			length:    f.end - f.start
			range_len: f.item.fn_idx - f.item.range_lo
			reusable:  f.reusable && f.item.fn_idx in state.captured
		}
		captured := state.captured[f.item.fn_idx] or { IncrementalCaptured{} }
		for err in captured.errors {
			entry.add(f, incremental_errors, err, '')
		}
		for notice in captured.notices {
			entry.add(f, incremental_notices, notice, '')
		}
		for pending in captured.pending {
			entry.add(f, incremental_pending, pending.err, pending.fn_qname)
		}
		for warning in unhandled[i] or { []TypeError{} } {
			entry.add(f, incremental_unhandled, warning, '')
		}
		entry.details.fn_values = fn_values[i] or { []IncrementalName{} }
		// Only the unused imports of a file with selective imports read calls.
		if selective[f.file_id] {
			for idx in f.item.range_lo .. f.item.fn_idx + 1 {
				if name := tc.resolved_call_name(flat.NodeId(idx)) {
					entry.details.calls << IncrementalName{
						back: f.item.fn_idx - idx
						name: name
					}
				}
			}
		}
		write_incremental_entry(mut b, entry)
	}
	b.writeln('end\t${state.functions.len}')
	return b.str()
}

// function_around returns the index in functions of the body whose nodes hold
// `idx`, or -1. `by_node` are the indexes of the functions by their nodes.
fn (state &IncrementalCheck) function_around(by_node []int, idx int) int {
	mut lo := 0
	mut hi := by_node.len
	for lo < hi {
		mid := (lo + hi) / 2
		if state.functions[by_node[mid]].item.fn_idx < idx {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	if lo < by_node.len && state.functions[by_node[lo]].item.range_lo <= idx {
		return by_node[lo]
	}
	return -1
}

// incremental_selective_import_files returns the files that import names of a
// module, as `import os { join_path }` does.
fn (tc &TypeChecker) incremental_selective_import_files() map[int]bool {
	mut files := map[int]bool{}
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind != .import_decl {
			continue
		}
		for i in 0 .. node.children_count {
			if tc.a.child_node(&node, i).kind == .ident {
				files[int(node.pos.id)] = true
				break
			}
		}
	}
	return files
}

// add notes the diagnostic `err` of the body `f`, or that the body cannot be
// taken from the record when the diagnostic is not in its region.
fn (mut entry IncrementalEntry) add(f IncrementalFunction, list int, err TypeError, fn_qname string) {
	node := int(err.node)
	if err.file != f.item.file || int(err.pos.id) != f.file_id || int(err.pos.offset) < f.start
		|| int(err.pos.end) > f.end || err.pos.end < err.pos.offset || node < f.item.range_lo
		|| node > f.item.fn_idx || err.details.any(it.contains('.v:')) {
		entry.reusable = false
		return
	}
	entry.details.diagnostics << IncrementalDiagnostic{
		list:     list
		kind:     int(err.kind)
		back:     f.item.fn_idx - node
		offset:   int(err.pos.offset) - f.start
		end:      int(err.pos.end) - f.start
		meta:     err.pos.meta
		order:    err.diagnostic_order
		severity: err.severity
		msg:      err.msg
		fn_qname: fn_qname
		details:  err.details
	}
}

fn write_incremental_entry(mut b strings.Builder, entry IncrementalEntry) {
	// A key is the place of a file and the name of a function, which holds no
	// tab, newline or backslash.
	b.writeln('f\t${entry.key}\t${entry.hash.hex()}\t${entry.length}\t${entry.range_len}\t${int(entry.reusable)}')
	for d in entry.details.diagnostics {
		b.write_string('d\t${d.list}\t${d.kind}\t${d.back}\t${d.offset}\t${d.end}\t${d.meta}\t${d.order}\t${incremental_escape(d.severity)}\t${incremental_escape(d.msg)}\t${incremental_escape(d.fn_qname)}\t${d.details.len}')
		for detail in d.details {
			b.write_u8(`\t`)
			b.write_string(incremental_escape(detail))
		}
		b.write_u8(`\n`)
	}
	for call in entry.details.calls {
		b.writeln('c\t${call.back}\t${incremental_escape(call.name)}')
	}
	for value in entry.details.fn_values {
		b.writeln('v\t${value.back}\t${incremental_escape(value.name)}')
	}
}

// decode_incremental_record reads what incremental_record wrote, or none when
// `text` is not all of it. It reads the first line of each entry: the lines of
// its details are read when they are needed (see details).
fn decode_incremental_record(text string) ?IncrementalRecord {
	if !text.starts_with(incremental_record_header + '\n') {
		return none
	}
	mut record := IncrementalRecord{
		text: text
	}
	mut at := incremental_record_header.len + 1
	mut ended := false
	for at < text.len {
		line_end := incremental_line_end(text, at)
		// An entry and its details start with a letter and a tab.
		kind := if at + 1 < line_end && text[at + 1] == `\t` { text[at] } else { u8(0) }
		match kind {
			`f` {
				if record.entries.len > 0 {
					record.entries[record.entries.len - 1] = IncrementalStored{
						...record.entries[record.entries.len - 1]
						end: at
					}
				}
				// f <file> <name> <hash> <length> <range> <reusable>
				mut fields := [7]int{}
				mut field_ends := [7]int{}
				mut field := 0
				mut field_start := at
				for i in at .. line_end + 1 {
					if i == line_end || text[i] == `\t` {
						if field >= 7 {
							return none
						}
						fields[field] = field_start
						field_ends[field] = i
						field++
						field_start = i + 1
					}
				}
				if field != 7 {
					return none
				}
				key := text[fields[1]..field_ends[2]]
				record.by_key[key] = record.entries.len
				record.entries << IncrementalStored{
					key:           key
					hash:          incremental_hex(text, fields[3], field_ends[3]) or { return none }
					length:        incremental_int(text, fields[4], field_ends[4])
					range_len:     incremental_int(text, fields[5], field_ends[5])
					reusable:      text[fields[6]] == `1`
					start:         at
					details_start: line_end + 1
					end:           line_end + 1
				}
			}
			`d`, `c`, `v` {
				if record.entries.len == 0 {
					return none
				}
			}
			else {
				line := text[at..line_end]
				if line.starts_with('declarations\t') {
					record.declarations = incremental_hex(line, 'declarations\t'.len, line.len) or {
						return none
					}
				} else if line.starts_with('files\t') {
					record.files = incremental_unescape(line['files\t'.len..])
				} else if line.starts_with('end\t') {
					if record.entries.len > 0 {
						record.entries[record.entries.len - 1] = IncrementalStored{
							...record.entries[record.entries.len - 1]
							end: at
						}
					}
					ended = line['end\t'.len..].int() == record.entries.len
					break
				} else {
					return none
				}
			}
		}
		at = line_end + 1
	}
	return if ended { record } else { none }
}

// details returns the details of the entry `i`.
fn (record &IncrementalRecord) details(i int) IncrementalDetails {
	stored := record.entries[i]
	mut details := IncrementalDetails{}
	if stored.details_start >= stored.end {
		return details
	}
	for line in record.text[stored.details_start..stored.end].split('\n') {
		if line == '' {
			continue
		}
		fields := line.split('\t')
		match fields[0] {
			'd' {
				if fields.len < 12 || fields.len != 12 + fields[11].int() {
					continue
				}
				mut texts := []string{cap: fields.len - 12}
				for detail in fields[12..] {
					texts << incremental_unescape(detail)
				}
				details.diagnostics << IncrementalDiagnostic{
					list:     fields[1].int()
					kind:     fields[2].int()
					back:     fields[3].int()
					offset:   fields[4].int()
					end:      fields[5].int()
					meta:     u16(fields[6].int())
					order:    fields[7].int()
					severity: incremental_unescape(fields[8])
					msg:      incremental_unescape(fields[9])
					fn_qname: incremental_unescape(fields[10])
					details:  texts
				}
			}
			'c', 'v' {
				if fields.len != 3 {
					continue
				}
				name := IncrementalName{
					back: fields[1].int()
					name: incremental_unescape(fields[2])
				}
				if fields[0] == 'c' {
					details.calls << name
				} else {
					details.fn_values << name
				}
			}
			else {}
		}
	}
	return details
}

// incremental_line_end returns where the line of `text` from `at` ends.
fn incremental_line_end(text string, at int) int {
	mut end := at
	for end < text.len && text[end] != `\n` {
		end++
	}
	return end
}

// incremental_int reads the decimal number of `text` from `start` to `end`.
fn incremental_int(text string, start int, end int) int {
	mut n := 0
	mut negative := false
	for i in start .. end {
		c := text[i]
		if c == `-` && i == start {
			negative = true
		} else if c >= `0` && c <= `9` {
			n = n * 10 + int(c - `0`)
		}
	}
	return if negative { -n } else { n }
}

// incremental_hex reads the hexadecimal number of `text` from `start` to `end`.
fn incremental_hex(text string, start int, end int) ?u64 {
	if end <= start || end - start > 16 {
		return none
	}
	mut n := u64(0)
	for i in start .. end {
		c := text[i]
		digit := if c >= `0` && c <= `9` {
			u64(c - `0`)
		} else if c >= `a` && c <= `f` {
			u64(c - `a` + 10)
		} else {
			return none
		}
		n = n << 4 | digit
	}
	return n
}

// incremental_escape writes `s` on a field of a line of a record.
fn incremental_escape(s string) string {
	if !s.contains_any('\\\t\n') {
		return s
	}
	mut b := strings.new_builder(s.len + 8)
	for c in s {
		match c {
			`\\` { b.write_string('\\\\') }
			`\t` { b.write_string('\\t') }
			`\n` { b.write_string('\\n') }
			else { b.write_u8(c) }
		}
	}
	return b.str()
}

// incremental_unescape reads what incremental_escape wrote.
fn incremental_unescape(s string) string {
	if !s.contains('\\') {
		return s
	}
	mut b := strings.new_builder(s.len)
	mut i := 0
	for i < s.len {
		c := s[i]
		if c == `\\` && i + 1 < s.len {
			next := s[i + 1]
			if next == `t` {
				b.write_u8(`\t`)
			} else if next == `n` {
				b.write_u8(`\n`)
			} else {
				b.write_u8(next)
			}
			i += 2
			continue
		}
		b.write_u8(c)
		i++
	}
	return b.str()
}
