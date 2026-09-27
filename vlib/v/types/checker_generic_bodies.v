module types

import v.flat

// A check and a build check the body of a generic function whose type
// parameters all have a constraint as the body of any other function: with
// each type parameter as a type that its constraint admits, the interface
// itself, as any type that implements it, or each type of a set in turn. What
// is wrong with one of them is wrong with the function: `e = x` for `x T` with
// `T Number` and `e int`, as `f64` is not an `int`. The body of a generic
// function with a type parameter without a constraint is left to its
// instances, as before: V checks those.

// check_generic_fn_body checks the body of the generic function `node`, whose
// type parameters are `params`, when they all have a constraint. The body is
// checked by forks of the checker, as workers of the parallel check with no
// range of their own: all they learn stays in their private caches, and only
// their errors come back. The checks against the constraints report what a
// member or an operator of a type parameter does wrong
// (checker_generic_constraints.v): a statement with one of those errors gets
// no other.
fn (mut tc TypeChecker) check_generic_fn_body(node flat.Node, fn_idx int, params map[string]bool) {
	if params.len == 0 {
		return
	}
	constraints := tc.generic_constraints_of(node)
	for name, _ in params {
		if name !in constraints {
			return
		}
	}
	// With its type parameters open, the checker says of a statement that does
	// not depend on them what it says in every instance.
	open := tc.check_generic_fn_body_as(node, fn_idx, map[string]string{})
	dependent := tc.type_param_dependent_names(node, params)
	mut statements := map[int]bool{}
	for notice in open.notices {
		if !tc.diagnostic_depends_on_type_params(notice.node, fn_idx, dependent, params, mut
			statements)
		{
			tc.notices << notice
		}
	}
	texts := tc.type_param_instance_texts(params, constraints) or {
		// A constraint that names a type parameter, `[T Comparable[T]]`, gives
		// no type to check the body with: its statements that do not depend on
		// the type parameters are what can be told.
		for open_error in open.errors {
			if !tc.diagnostic_depends_on_type_params(open_error.node, fn_idx, dependent,
				params, mut statements)
			{
				tc.errors << open_error
			}
		}
		return
	}
	reported := tc.statements_with_errors(fn_idx)
	mut independent := map[string]bool{}
	for err in open.errors {
		independent[type_error_key(err)] = true
	}
	mut positions := map[string]bool{}
	for combination in type_param_combinations(texts, 32) {
		instance := tc.check_generic_fn_body_as(node, fn_idx, combination)
		for err in instance.errors {
			statement := tc.enclosing_body_statement(err.node, fn_idx) or { continue }
			if reported[int(statement)] {
				continue
			}
			position := '${err.pos.id}:${err.pos.offset}:${err.pos.end}'
			if position in positions {
				continue
			}
			positions[position] = true
			if type_error_key(err) in independent {
				tc.errors << err
			} else {
				tc.errors << TypeError{
					...err
					msg: '${err.msg}: ${type_param_instance_reason(combination, constraints)}'
				}
			}
		}
	}
}

// GenericBodyDiagnostics are what a check of a generic body reports.
struct GenericBodyDiagnostics {
	errors  []TypeError
	notices []TypeError
}

// check_generic_fn_body_as checks the body of the generic function `node` in a
// fork of the checker, with each type parameter that `texts` names as the type
// written there, or with its type parameters open when it names none.
fn (tc &TypeChecker) check_generic_fn_body_as(node flat.Node, fn_idx int, texts map[string]string) GenericBodyDiagnostics {
	mut w := tc.fork_for_parallel_check()
	w.fn_context.node_id = fn_idx
	w.fn_context.concrete_generic_receiver_specialization =
		tc.fn_context.concrete_generic_receiver_specialization
	w.cur_fn_node_id = fn_idx
	if texts.len > 0 {
		// A function without type parameters, whose types name the ones given.
		w.type_param_texts = texts.clone()
		w.type_cache.parse_enabled = false
		w.fn_context.generic_params = []string{}
		return_text := if node.typ.ends_with('?') && !node.typ.starts_with('?') {
			node.typ.trim_right('?')
		} else {
			node.typ
		}
		w.cur_fn_ret_type = w.parse_type(return_text)
		w.fn_context.return_type = w.cur_fn_ret_type
	} else {
		w.fn_context.generic_params = tc.fn_context.generic_params.clone()
		w.fn_context.return_type = tc.fn_context.return_type
		w.cur_fn_ret_type = tc.cur_fn_ret_type
	}
	w.index_local_decl_rhs(flat.NodeId(fn_idx))
	$if ownership ? {
		w.ownership_begin_fn(node)
	}
	w.push_scope()
	for i in 0 .. node.children_count {
		param_id := tc.a.child(&node, i)
		w.insert_fn_param_binding(param_id, tc.a.node(param_id))
	}
	w.insert_implicit_veb_ctx(node)
	w.check_fn_body(node)
	w.pop_scope()
	$if ownership ? {
		w.ownership_end_fn()
	}
	return GenericBodyDiagnostics{
		errors:  w.errors
		notices: w.notices
	}
}

// parse_type_as_instance parses `typ` in a fork that checks a generic body with
// its type parameters as types that their constraints admit (see
// check_generic_fn_body_as): each of them is its type there, and no parse comes
// from the cache or goes to it, which holds the types of other functions.
fn (tc &TypeChecker) parse_type_as_instance(typ string) Type {
	mut names := []string{cap: tc.type_param_texts.len}
	mut args := []string{cap: tc.type_param_texts.len}
	mut name_set := map[string]bool{}
	for name, text in tc.type_param_texts {
		if tc.type_params_expanding[name] {
			continue
		}
		names << name
		args << text
		name_set[name] = true
	}
	if names.len == 0 || !type_text_names_any(typ, name_set) {
		_, result := tc.intern_type(tc.parse_type_uncached(typ))
		return result
	}
	// A type parameter whose type names it, `T` of `[T Comparable[T]]`, is open
	// inside that type: `T` is `Comparable[T]`, not `Comparable[Comparable[...]]`.
	// Only this fork parses with these texts, on one thread.
	mut fork := unsafe { tc }
	mut expanding := []string{}
	for i, name in names {
		mut own := map[string]bool{}
		own[name] = true
		if type_text_names_any(args[i], own) {
			fork.type_params_expanding[name] = true
			expanding << name
		}
	}
	_, result := tc.intern_type(tc.parse_type_uncached(subst_generic_text(typ, args, names)))
	for name in expanding {
		fork.type_params_expanding.delete(name)
	}
	return result
}

// instance_type_text is the type that `text` stands for in a `$if` of a generic
// body checked with types for its type parameters (see check_generic_fn_body_as):
// the type of a type parameter, and the type of a value that the `$if` tests,
// `x` in `$if x is f64`, a parameter declared with a type parameter or a local.
// Anything else is `text`.
fn (tc &TypeChecker) instance_type_text(text string) string {
	if tc.type_param_texts.len == 0 {
		return text
	}
	if instance := tc.type_param_texts[text] {
		return instance
	}
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if tc.valid_node_id(fn_id) {
		if param := tc.fn_param_type_param(tc.a.node(fn_id), text, tc.type_param_texts.keys()) {
			return tc.type_param_texts[param] or { text }
		}
	}
	if typ := tc.cur_scope.lookup(text) {
		if typ !is Unknown {
			return typ.name()
		}
	}
	return text
}

// ComptimeInTerm is a term `x in [int, $float]` or `T !in [f64]` of a `$if`.
struct ComptimeInTerm {
	left    string
	negated bool
	items   []string
}

// comptime_in_term reads the term `cond` of a `$if` as an `in` or a `!in` of a
// name, or none: the parser writes the list right after `in`, `T in[int]`.
fn comptime_in_term(cond string) ?ComptimeInTerm {
	if cond.len == 0 {
		return none
	}
	end := comptime_condition_name_end(cond, 0)
	if end == 0 {
		return none
	}
	mut rest := cond[end..].trim_space()
	negated := rest.starts_with('!in')
	if negated {
		rest = rest[3..]
	} else if rest.starts_with('in') {
		rest = rest[2..]
	} else {
		return none
	}
	if rest.len == 0 || rest[0] !in [` `, `\t`, `[`] {
		return none
	}
	rest = rest.trim_space()
	if !rest.starts_with('[') || !rest.ends_with(']') {
		return none
	}
	return ComptimeInTerm{
		left:    cond[..end]
		negated: negated
		items:   split_params(rest[1..rest.len - 1])
	}
}

// instance_comptime_in_value decides the term `term` of a `$if` in a generic
// body checked with types for its type parameters (see check_generic_fn_body_as)
// as `is` decides one: whether the type of its name is one of its list. None
// when a type of the list cannot be told apart.
fn (tc &TypeChecker) instance_comptime_in_value(term ComptimeInTerm) ?bool {
	mut matched := false
	for item in term.items {
		if tc.comptime_type_matches(term.left, item)? {
			matched = true
			break
		}
	}
	return if term.negated { !matched } else { matched }
}

// type_param_instance_texts gives each type parameter of `params` the texts of
// the types its constraint admits for a check of the body: the interface, or
// the types of the set. An interface may name its own type parameter,
// `[T Comparable[T]]` (see parse_type_as_instance). None when a type names
// another type parameter.
fn (tc &TypeChecker) type_param_instance_texts(params map[string]bool, constraints map[string]GenericConstraint) ?map[string][]string {
	mut texts := map[string][]string{}
	for name, _ in params {
		constraint := constraints[name] or { return none }
		mut options := []string{}
		if constraint.is_interface {
			options << constraint.iface.name
		} else {
			for typ in constraint.types {
				options << typ.name()
			}
		}
		if options.len == 0 {
			return none
		}
		mut others := params.clone()
		if constraint.is_interface {
			others.delete(name)
		}
		for option in options {
			if option.len == 0 || type_text_names_any(option, others) {
				return none
			}
		}
		texts[name] = options
	}
	return texts
}

// type_param_combinations gives each type parameter of `texts` one of its
// types, in every combination, or, past `limit`, one type parameter at a time
// through all of its types while the others keep their first.
fn type_param_combinations(texts map[string][]string, limit int) []map[string]string {
	mut combinations := [map[string]string{}]
	mut count := 1
	for _, options in texts {
		count *= options.len
	}
	if count <= limit {
		for name, options in texts {
			mut next := []map[string]string{cap: combinations.len * options.len}
			for combination in combinations {
				for option in options {
					mut with := combination.clone()
					with[name] = option
					next << with
				}
			}
			combinations = next.clone()
		}
		return combinations
	}
	mut first := map[string]string{}
	for name, options in texts {
		first[name] = options[0]
	}
	combinations = [first]
	for name, options in texts {
		for option in options[1..] {
			mut with := first.clone()
			with[name] = option
			combinations << with
		}
	}
	return combinations
}

// type_param_instance_reason says why an error of a check with the types
// `combination` is an error of the generic function: what each type parameter
// can be.
fn type_param_instance_reason(combination map[string]string, constraints map[string]GenericConstraint) string {
	mut parts := []string{}
	for name, text in combination {
		constraint := constraints[name] or { continue }
		if constraint.is_interface {
			parts << '`${name}` is any type that implements `${constraint.name}`'
		} else {
			parts << 'when `${name}` is `${text.all_after_last('.')}`, in its constraint `${constraint.name}`'
		}
	}
	return parts.join(', and ')
}

// type_error_key tells two errors apart by what they say and where.
fn type_error_key(err TypeError) string {
	return '${err.pos.id}:${err.pos.offset}:${err.pos.end}:${err.msg}'
}

// statements_with_errors returns the statements of the body of the function at
// `fn_idx` that an error was already reported in.
fn (tc &TypeChecker) statements_with_errors(fn_idx int) map[int]bool {
	first := tc.first_node_below(flat.NodeId(fn_idx))
	mut statements := map[int]bool{}
	for err in tc.errors {
		if int(err.node) < first || int(err.node) > fn_idx {
			continue
		}
		if statement := tc.enclosing_body_statement(err.node, fn_idx) {
			statements[int(statement)] = true
		}
	}
	return statements
}

// first_node_below returns the lowest id among `id` and the nodes below it: in
// the flat tree a node comes after the nodes below it.
fn (tc &TypeChecker) first_node_below(id flat.NodeId) int {
	mut first := int(id)
	mut stack := [id]
	for stack.len > 0 {
		current := stack.pop()
		if !tc.valid_node_id(current) {
			continue
		}
		if int(current) < first {
			first = int(current)
		}
		node := tc.a.node(current)
		for i in 0 .. node.children_count {
			stack << tc.a.child(node, i)
		}
	}
	return first
}

// type_param_dependent_names returns the names whose values depend on the type
// parameters `params` of the generic function `fn_node`: the parameters
// themselves, the parameters of the function whose types name one, and the
// locals declared from a value that depends on them, as `ys := xs.filter(..)`
// for `xs []T`. V has no shadowing, so a name stands for one binding in the
// whole body.
fn (tc &TypeChecker) type_param_dependent_names(fn_node flat.Node, params map[string]bool) map[string]bool {
	mut names := params.clone()
	mut roots := []flat.NodeId{}
	for i in 0 .. fn_node.children_count {
		child_id := tc.a.child(&fn_node, i)
		child := tc.a.node(child_id)
		if child.kind == .param {
			if child.value.len > 0 && type_text_names_any(child.typ, params) {
				names[child.value] = true
			}
		} else {
			roots << child_id
		}
	}
	// A declaration can take its value from one that comes later in the walk,
	// in a loop: the walk goes on while it finds new names.
	for _ in 0 .. 64 {
		mut changed := false
		mut stack := roots.clone()
		for stack.len > 0 {
			id := stack.pop()
			if !tc.valid_node_id(id) {
				continue
			}
			node := tc.a.node(id)
			match node.kind {
				.decl_assign {
					if tc.any_child_depends(node, 0, names, params) {
						changed = tc.add_declared_names(node, 0, node.children_count, mut
							names) || changed
					}
				}
				.for_in_stmt {
					// `for k, v in xs {`: the names of the loop, then what it walks.
					if node.children_count > 2 && tc.any_child_depends(node, 2, names, params) {
						changed = tc.add_declared_names(node, 0, 2, mut names) || changed
					}
				}
				.fn_literal, .lambda_expr {
					for i in 0 .. node.children_count {
						param := tc.a.child_node(node, i)
						if param.kind == .param && param.value.len > 0 && param.value !in names
							&& type_text_names_any(param.typ, params) {
							names[param.value] = true
							changed = true
						}
					}
				}
				else {}
			}
			for i in 0 .. node.children_count {
				stack << tc.a.child(node, i)
			}
		}
		if !changed {
			break
		}
	}
	return names
}

// any_child_depends reports whether a child of `node` from `first` on, but a
// block, depends on `names` or `params` (see subtree_depends_on_type_params).
fn (tc &TypeChecker) any_child_depends(node &flat.Node, first int, names map[string]bool, params map[string]bool) bool {
	for i in first .. node.children_count {
		child_id := tc.a.child(node, i)
		if tc.valid_node_id(child_id) && tc.a.node(child_id).kind != .block
			&& tc.subtree_depends_on_type_params(child_id, names, params) {
			return true
		}
	}
	return false
}

// add_declared_names adds the names the children of `node` from `first` to
// `end` declare, and reports whether one was new.
fn (tc &TypeChecker) add_declared_names(node &flat.Node, first int, end int, mut names map[string]bool) bool {
	mut added := false
	for i in first .. int_min(end, node.children_count) {
		child := tc.a.child_node(node, i)
		if child.kind == .ident && child.value.len > 0 && child.value != '_'
			&& child.value !in names {
			names[child.value] = true
			added = true
		}
	}
	return added
}

// diagnostic_depends_on_type_params reports whether the diagnostic at `id`, in
// the body of the function `fn_idx`, is about a statement that depends on its
// type parameters: one that names them, or a name in `names`. A diagnostic
// outside a statement of the body depends on them too: it is not said.
fn (tc &TypeChecker) diagnostic_depends_on_type_params(id flat.NodeId, fn_idx int, names map[string]bool, params map[string]bool, mut statements map[int]bool) bool {
	statement := tc.enclosing_body_statement(id, fn_idx) or { return true }
	if depends := statements[int(statement)] {
		return depends
	}
	depends := tc.subtree_depends_on_type_params(statement, names, params)
	statements[int(statement)] = depends
	return depends
}

// enclosing_body_statement returns the statement of the body of the function
// `fn_idx`, or of a block in it, that holds `id`.
fn (tc &TypeChecker) enclosing_body_statement(id flat.NodeId, fn_idx int) ?flat.NodeId {
	mut current := id
	for _ in 0 .. 4096 {
		if !tc.valid_node_id(current) || int(current) == fn_idx {
			return none
		}
		parent := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent) {
			return none
		}
		if int(parent) == fn_idx {
			// A parameter is not a statement of the body.
			if tc.a.node(current).kind == .param {
				return none
			}
			return current
		}
		if tc.a.node(parent).kind == .block {
			return current
		}
		current = parent
	}
	return none
}

// subtree_depends_on_type_params reports whether the node `id`, or a node below
// it, names a type parameter of `params` or a name of `names`: an identifier,
// or the text of a type, `T{}`, `[]T{}`, `x as T`, `$if T is f64 {`.
fn (tc &TypeChecker) subtree_depends_on_type_params(id flat.NodeId, names map[string]bool, params map[string]bool) bool {
	mut stack := [id]
	for stack.len > 0 {
		current := stack.pop()
		if !tc.valid_node_id(current) {
			continue
		}
		node := tc.a.node(current)
		if node.kind == .ident && node.value in names {
			return true
		}
		if type_text_names_any(node.typ, params) {
			return true
		}
		if node.kind !in [.string_literal, .char_literal, .int_literal, .float_literal]
			&& type_text_names_any(node.value, params) {
			return true
		}
		for i in 0 .. node.children_count {
			stack << tc.a.child(node, i)
		}
	}
	return false
}

// type_text_names_any reports whether `text` has a name of `names` among the
// names it is written with: `T`, `[]T`, `map[string]T`, `Box[T]`, `fn (T) bool`.
fn type_text_names_any(text string, names map[string]bool) bool {
	mut start := -1
	for i := 0; i <= text.len; i++ {
		is_name_byte := i < text.len && (text[i].is_letter() || text[i].is_digit() || text[i] == `_`)
		if is_name_byte {
			if start < 0 {
				start = i
			}
			continue
		}
		if start >= 0 {
			if text[start..i] in names {
				return true
			}
			start = -1
		}
	}
	return false
}
