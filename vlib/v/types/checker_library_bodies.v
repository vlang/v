module types

import v.flat
import v.gen.c.naming

// A build compiles only the functions that the program reaches, and a library has
// many that it does not: most of what `import os` brings is never called. The
// checker therefore leaves the bodies of the standard library functions that the
// program cannot reach unchecked.
//
// What a program reaches is only known for certain after the check: markused
// resolves calls with the types that the check finds. Before it, the checker takes
// every function that the program can name (library_fns_reachable_by_name), which
// is more than it reaches. Should markused find a function that no name led to,
// the driver has its body checked then, and runs markused again
// (check_reached_library_bodies).
//
// The bodies of the program itself, of the modules of its project, of the runtime
// modules that a program has for one construct (library_runtime_modules), and of
// every generic function are always checked, as are all the declarations.

// skip_unreachable_library_bodies leaves the bodies of the functions in
// `library_files` that the program cannot name unchecked, and returns how many
// library functions it can. `seeded_fns` are the functions that markused keeps
// without a call that leads to them. Without `follow_names` it leaves out every
// body that is not checked whatever the program names, which leaves them all to
// check_reached_library_bodies: a test of that path.
pub fn (mut tc TypeChecker) skip_unreachable_library_bodies(library_files map[string]bool, seeded_fns []string, follow_names bool) int {
	tc.library_files = library_files.clone()
	tc.reachable_library_fns = tc.library_fns_reachable_by_name(seeded_fns, follow_names)
	tc.skips_library_bodies = library_files.len > 0
	return tc.reachable_library_fns.len
}

// library_fn_implicit_names are the methods that the compiler calls without the
// program naming them: to print a value, to free it, to iterate over it, to
// report an error.
const library_fn_implicit_names = ['str', 'free', 'next', 'msg', 'code', 'init', 'cleanup', 'main']

// library_runtime_modules are the modules that a program only has for a construct
// that the compiler lowers to calls of their functions: a closure, a channel, a
// `shared` value, an embedded file, a checked overflow, a debugger statement. No
// name in the program leads to those calls, so their bodies are all checked.
//
// `builtin`, `strings` and `strconv` are in every program, which calls a part of
// them. Of those, the check takes what the program names, and what markused keeps
// on its own for the arrays, maps, strings and errors of a program: `seeded_fns`.
const library_runtime_modules = ['closure', 'sync', 'stdatomic', 'embed_file', 'overflow', 'debug']

struct LibraryFnBody {
	fn_idx   int
	range_lo int
	module   string
}

// library_fns_reachable_by_name returns the functions of tc.library_files that the
// rest of the program can reach, judged by name alone: a body reaches every
// function whose name, without its module or receiver, it mentions anywhere. That
// needs no types, so it can run before the check, and it errs on the side of more
// functions: `x.len` reaches every method named `len`. The functions of
// `seeded_fns` are reached without a name that leads to them.
fn (mut tc TypeChecker) library_fns_reachable_by_name(seeded_fns []string, follow_names bool) map[string]bool {
	mut reachable := map[string]bool{}
	mut seeded_names := map[string]bool{}
	if follow_names {
		for name in seeded_fns {
			// `array.push`, `array__push` and `strconv__format_int` name functions
			// that are declared as `push` and `format_int`.
			seeded_names[name.all_after_last('.').all_after_last('__')] = true
		}
	}
	mut bodies := []LibraryFnBody{cap: 4096}
	mut by_name := map[string][]int{}
	mut reached := []bool{cap: 4096}
	mut queue := []int{cap: 4096}
	// The ranges of the nodes that are checked whatever the program reaches.
	mut root_ranges := []LibraryFnBody{cap: 1024}
	saved_file := tc.cur_file
	saved_module := tc.cur_module
	tc.cur_module = ''
	tc.cur_file = ''
	mut is_library := false
	mut prev_tl := -1
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		range_lo := prev_tl + 1
		prev_tl = i
		match node.kind {
			.file {
				tc.enter_file(node.value)
				is_library = tc.library_files[node.value]
				// The statements of a program without a `main` function.
				root_ranges << LibraryFnBody{
					fn_idx:   i
					range_lo: range_lo
				}
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.fn_decl {
				short_name := node.value.all_after_last('.')
				is_root := !is_library || tc.cur_module in library_runtime_modules
					|| short_name in library_fn_implicit_names || seeded_names[short_name]
					|| !library_fn_name_is_identifier(short_name)
					|| tc.infer_decl_generic_param_names(node).len > 0
					|| tc.library_fn_is_marked_root(i)
				if is_root {
					root_ranges << LibraryFnBody{
						fn_idx:   i
						range_lo: range_lo
					}
					if is_library {
						reachable[node.value] = true
						reachable[checker_qualified_fn_name(tc.cur_module, node.value)] = true
					}
					continue
				}
				by_name[short_name] << bodies.len
				bodies << LibraryFnBody{
					fn_idx:   i
					range_lo: range_lo
					module:   tc.cur_module
				}
				reached << false
			}
			else {
				// Constants, globals, field defaults and top-level statements.
				root_ranges << LibraryFnBody{
					fn_idx:   i
					range_lo: range_lo
				}
			}
		}
	}
	tc.cur_file = saved_file
	tc.cur_module = saved_module
	if bodies.len == 0 || !follow_names {
		return reachable
	}
	for root in root_ranges {
		tc.reach_library_fns_named_in(root, by_name, mut reached, mut queue)
	}
	for queue.len > 0 {
		body := bodies[queue.pop()]
		tc.reach_library_fns_named_in(body, by_name, mut reached, mut queue)
	}
	for i, body in bodies {
		if reached[i] {
			name := tc.a.nodes[body.fn_idx].value
			reachable[name] = true
			reachable[checker_qualified_fn_name(body.module, name)] = true
		}
	}
	return reachable
}

// reach_library_fns_named_in marks the library functions that the nodes of `body`
// name as reached, and queues them for their own bodies.
fn (tc &TypeChecker) reach_library_fns_named_in(body LibraryFnBody, by_name map[string][]int, mut reached []bool, mut queue []int) {
	for i in body.range_lo .. body.fn_idx + 1 {
		value := tc.a.nodes[i].value
		if value.len == 0 {
			continue
		}
		short_name := value.all_after_last('.')
		candidates := by_name[short_name] or { continue }
		for candidate in candidates {
			if !reached[candidate] {
				reached[candidate] = true
				queue << candidate
			}
		}
	}
}

// library_fn_name_is_identifier reports whether `name` is a plain name rather than
// an operator, which the compiler calls for the operator's uses.
fn library_fn_name_is_identifier(name string) bool {
	if name.len == 0 {
		return false
	}
	for c in name {
		if !(c.is_letter() || c.is_digit() || c == `_`) {
			return false
		}
	}
	return true
}

// library_fn_is_marked_root reports whether the function declaration at `fn_idx`
// has an attribute that keeps it whatever calls it.
fn (tc &TypeChecker) library_fn_is_marked_root(fn_idx int) bool {
	attr_idx := fn_idx + 1
	if attr_idx >= tc.a.nodes.len {
		return false
	}
	attr := tc.a.nodes[attr_idx]
	if attr.kind != .directive || !attr.value.starts_with('@attributes:') {
		return false
	}
	for name in attr.generic_params() {
		if name == 'markused' || name == 'export' || name.starts_with('export:') {
			return true
		}
	}
	return false
}

// skips_library_body reports whether the check leaves out the body of the function
// declaration `node`, which is in the current file and module.
fn (tc &TypeChecker) skips_library_body(node flat.Node) bool {
	if !tc.skips_library_bodies || !tc.library_files[tc.cur_file] {
		return false
	}
	if tc.reachable_library_fns[node.value]
		|| tc.reachable_library_fns[checker_qualified_fn_name(tc.cur_module, node.value)] {
		return false
	}
	// A generic body is checked for its type parameters, whoever instantiates it.
	return tc.infer_decl_generic_param_names(node).len == 0
}

// skipped_library_bodies returns how many bodies the check left out so far.
pub fn (mut tc TypeChecker) skipped_library_bodies() int {
	return tc.library_body_items(map[string]bool{}, false).len
}

// library_body_items returns the bodies that the check left out: those that `used`
// names, or all of them without `only_used`.
fn (mut tc TypeChecker) library_body_items(used map[string]bool, only_used bool) []CheckWorkItem {
	mut items := []CheckWorkItem{}
	if !tc.skips_library_bodies {
		return items
	}
	saved_file := tc.cur_file
	saved_module := tc.cur_module
	tc.cur_module = ''
	tc.cur_file = ''
	mut prev_tl := -1
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.fn_decl {
				if tc.skips_library_body(node)
					&& (!only_used || library_fn_is_used(used, tc.cur_module, node.value)) {
					cost := i - prev_tl
					items << CheckWorkItem{
						fn_idx:   i
						range_lo: prev_tl + 1
						file:     tc.cur_file
						module:   tc.cur_module
						cost:     cost
						rank:     i64(cost) * 1_000_000_000 - i64(i)
					}
				}
			}
			else {}
		}
		prev_tl = i
	}
	tc.cur_file = saved_file
	tc.cur_module = saved_module
	return items
}

// library_fn_is_used reports whether `used`, the functions that markused keeps, has
// the function `name` of `module`: under its V name, or under the name that it has
// in the generated C, which is how markused keeps some functions of the runtime
// and how the code generator looks them up too.
fn library_fn_is_used(used map[string]bool, module string, name string) bool {
	qualified := checker_qualified_fn_name(module, name)
	if used[name] || used[qualified] {
		return true
	}
	c_name := naming.c_name(name)
	if c_name != name && used[c_name] {
		return true
	}
	c_qualified := naming.c_name(qualified)
	return c_qualified != qualified && c_qualified != c_name && used[c_qualified]
}

// check_reached_library_bodies checks the bodies that the check left out and that
// `used`, the functions that markused found since, names. It returns how many
// there were: their types can lead markused to more functions. `parallel` is
// what check_semantics_opt was asked for: a check that had to be serial stays
// so, and starts no worker.
pub fn (mut tc TypeChecker) check_reached_library_bodies(used map[string]bool, parallel bool) int {
	items := tc.library_body_items(used, true)
	if items.len == 0 {
		return 0
	}
	for item in items {
		name := tc.a.nodes[item.fn_idx].value
		tc.reachable_library_fns[name] = true
		tc.reachable_library_fns[checker_qualified_fn_name(item.module, name)] = true
	}
	saved_file := tc.cur_file
	saved_module := tc.cur_module
	saved_trusted := tc.direct_parent_index_trusted
	if !saved_trusted {
		tc.reuse_direct_parent_index_for_unchanged_ast(tc.a)
	}
	tc.resolution_type_mode = false
	tc.install_type_cache_overlay()
	// As the check of the other bodies: an invalid IError return is only reported
	// for the functions that the selected files call.
	tc.defer_ierror_gating = tc.diagnostic_files.len > 0
	if parallel {
		tc.run_parallel_check(items, true)
	} else if tc.scope_parallel_check_workers {
		tc.check_scoped_batches(items, scoped_check_serial_batches)
	} else {
		tc.check_fn_items_serial(items)
	}
	if tc.defer_ierror_gating {
		if tc.pending_ierror_errors.len > 0 {
			tc.collect_selected_file_called_fns()
		}
		tc.filter_pending_ierror_errors()
		tc.defer_ierror_gating = false
	}
	tc.sort_parallel_check_errors()
	tc.restore_type_cache_base()
	tc.direct_parent_index_trusted = saved_trusted
	tc.resolution_type_mode = true
	tc.cur_file = saved_file
	tc.cur_module = saved_module
	return items.len
}
