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
	texts := tc.type_param_instance_texts(node, params, constraints) or {
		// A constraint that gives no type to check the body with: its
		// statements that do not depend on the type parameters are what can be
		// told.
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
	// The first error at each place, and the types that each type parameter had
	// in every check that found it there.
	mut at_position := map[string]int{}
	mut found := []GenericBodyError{}
	for combination in tc.generic_body_combinations(node, texts, constraints) {
		instance := tc.check_generic_fn_body_as(node, fn_idx, closed_type_param_texts(combination))
		for err in instance.errors {
			statement := tc.enclosing_body_statement(err.node, fn_idx) or { continue }
			if reported[int(statement)] {
				continue
			}
			position := '${err.pos.id}:${err.pos.offset}:${err.pos.end}'
			if i := at_position[position] {
				if found[i].err.msg == err.msg {
					found[i].add_types(combination)
				}
				continue
			}
			at_position[position] = found.len
			mut first := GenericBodyError{
				err: err
			}
			first.add_types(combination)
			found << first
		}
	}
	for f in found {
		if type_error_key(f.err) in independent {
			tc.errors << f.err
			continue
		}
		msg := fold_type_param_expansions(f.err.msg, constraints)
		reason := type_param_occurrence_reason(f.types, texts, constraints)
		tc.errors << TypeError{
			...f.err
			msg: if reason == '' { msg } else { '${msg}: ${reason}' }
		}
	}
}

// GenericBodyError is an error that the checks of a generic body found at one
// place, with the types that each type parameter had in the checks that found
// it there with the same words.
struct GenericBodyError {
	err TypeError
mut:
	types map[string][]string
}

// add_types notes the types of `combination`, a check that found the error.
fn (mut e GenericBodyError) add_types(combination map[string]string) {
	for name, text in combination {
		mut taken := e.types[name] or { []string{} }
		if text !in taken {
			taken << text
			e.types[name] = taken
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
	w := tc.checked_generic_fn_body(node, fn_idx, texts, false)
	return GenericBodyDiagnostics{
		errors:  w.errors
		notices: w.notices
	}
}

// checked_generic_fn_body returns the fork of the checker that checked the body
// of the generic function `node` (see check_generic_fn_body_as); with
// `keep_placeholders`, it keeps the types that are a type parameter itself too
// (see placeholder_types).
fn (tc &TypeChecker) checked_generic_fn_body(node flat.Node, fn_idx int, texts map[string]string, keep_placeholders bool) &TypeChecker {
	mut w := tc.fork_for_parallel_check()
	w.keep_placeholder_types = keep_placeholders
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
	return w
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
	// A type parameter that the texts put in still name, `T` of
	// `[T Comparable[T]]` (see closed_type_param_texts), is open inside them: `T`
	// is `Comparable[T]`, not `Comparable[Comparable[...]]`. Only this fork
	// parses with these texts, on one thread.
	mut fork := unsafe { tc }
	mut expanding := []string{}
	for name, _ in tc.type_param_texts {
		mut one := map[string]bool{}
		one[name] = true
		if !tc.type_params_expanding[name] && args.any(type_text_names_any(it, one)) {
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
		items:   split_params(rest[1..rest.len - 1]).map(it.trim_space())
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
// the types its constraint admits for a check of the body of `node`: the types
// of the set; or the interface, which stands for any type that implements it,
// and each type that implements it that a `$if` of the body tests the type
// parameter against, `$if x is User`, whose branch the interface does not take.
// A type may name a type parameter, its own, `[T Comparable[T]]`, or another,
// `[C Container[T], T Named]` (see closed_type_param_texts).
fn (tc &TypeChecker) type_param_instance_texts(node flat.Node, params map[string]bool, constraints map[string]GenericConstraint) ?map[string][]string {
	mut names := []string{}
	for name, _ in params {
		names << name
	}
	mut texts := map[string][]string{}
	for name, _ in params {
		constraint := constraints[name] or { return none }
		mut options := []string{}
		if constraint.is_interface {
			options << constraint.iface.name
			for tested in tc.comptime_tested_types(node, name, names) {
				if tested !in options
					&& tc.generic_constraint_accepts(constraint, tc.parse_type(tested)) {
					options << tested
				}
			}
		} else {
			for typ in constraint.types {
				options << typ.name()
			}
		}
		if options.len == 0 || options.any(it.len == 0) {
			return none
		}
		texts[name] = options
	}
	return texts
}

// comptime_tested_types returns the types that a `$if` of the body of the
// generic function `node` tests its type parameter `param` against: itself,
// `$if T is User`, or through a parameter declared with it, `$if x is User`,
// `$if x in [User, Admin]`, or through a local whose value comes from any type
// parameter, `y := x` and `$if y is User`; `!is` and `!in` too, whose `$else`
// is those types. `names` are the type parameters of `node`.
fn (tc &TypeChecker) comptime_tested_types(node flat.Node, param string, names []string) []string {
	mut found := []string{}
	for cond in tc.comptime_conditions_on(node, param, names) {
		for alternative in cond.split('||') {
			for part in alternative.split('&&') {
				mut term := part.trim_space()
				for term.starts_with('(') && term.ends_with(')') {
					term = term[1..term.len - 1].trim_space()
				}
				if in_term := comptime_in_term(term) {
					if in_term.left == param {
						for item in in_term.items {
							if !item.starts_with('$') && item !in found {
								found << item
							}
						}
					}
					continue
				}
				for op in [' !is ', ' is '] {
					idx := term.index(op) or { continue }
					left := term[..idx].trim_space()
					right := term[idx + op.len..].trim_space()
					if left == param && right.len > 0 && !right.starts_with('$')
						&& right !in found {
						found << right
					}
					break
				}
			}
		}
	}
	return found
}

// comptime_conditions_on returns the conditions of the `$if`s of the body of
// the generic function `node`, each value that they test written as its type
// parameter, `$if x is User` as `$if T is User`: the one of `names`, the type
// parameters of `node`, that a parameter is declared with, and `param` for a
// local whose value comes from any of them, `y := x`.
fn (tc &TypeChecker) comptime_conditions_on(node flat.Node, param string, names []string) []string {
	mut name_set := map[string]bool{}
	for name in names {
		name_set[name] = true
	}
	// The locals whose values come from the type parameters, `y := x`.
	dependent := tc.type_param_dependent_names(node, name_set)
	mut conditions := []string{}
	mut stack := []flat.NodeId{}
	for i in 0 .. node.children_count {
		stack << tc.a.child(&node, i)
	}
	for stack.len > 0 {
		id := stack.pop()
		if !tc.valid_node_id(id) {
			continue
		}
		current := tc.a.node(id)
		if current.kind == .comptime_if {
			mut tested := tc.comptime_tested_params(current.value, node, names, false)
			for tested_name in comptime_condition_tested_names(current.value) {
				if tested_name !in tested && tested_name !in name_set
					&& tested_name in dependent {
					tested[tested_name] = param
				}
			}
			conditions << comptime_condition_on_type_params(current.value, tested)
		}
		for i in 0 .. current.children_count {
			stack << tc.a.child(current, i)
		}
	}
	return conditions
}

// comptime_condition_terms returns the terms of the condition `cond` of `$if`
// that test `name`, `T is f64`, `T !is $float` or `T in [f32, f64]`, however
// the condition joins them.
fn comptime_condition_terms(cond string, name string) []string {
	mut terms := []string{}
	mut i := 0
	for i < cond.len {
		end := comptime_condition_name_end(cond, i)
		if end == i {
			i++
			continue
		}
		if cond[i..end] != name || !comptime_test_at(cond, end) {
			i = end
			continue
		}
		// What it is tested against ends at the `)`, `&&` or `||` that closes the
		// term: a list and a type argument have brackets of their own.
		mut j := end
		mut depth := 0
		for j < cond.len {
			c := cond[j]
			if c in [`[`, `(`] {
				depth++
			} else if c in [`]`, `)`] {
				if depth == 0 {
					break
				}
				depth--
			} else if depth == 0 && j + 1 < cond.len && c in [`&`, `|`] && cond[j + 1] == c {
				break
			}
			j++
		}
		terms << cond[i..j].trim_space()
		i = j
	}
	return terms
}

// type_param_branch_classes gives each type parameter of `texts` its types in
// groups, one for each way that the `$if`s of the body of `node` can go with
// it: the terms of their conditions on it, `T is f64`, `x in [f32, f64]` or
// `T !is $float`, split its types into groups that no term tells apart. A group
// for each type parameter takes a way through every `$if`, its `$else` too.
fn (tc &TypeChecker) type_param_branch_classes(node flat.Node, texts map[string][]string, constraints map[string]GenericConstraint) map[string][][]string {
	names := texts.keys()
	mut classes := map[string][][]string{}
	for name, options in texts {
		constraint := constraints[name] or {
			classes[name] = [options.clone()]
			continue
		}
		mut option_names := []string{cap: options.len}
		for option in options {
			option_names << tc.parse_type(option).name()
		}
		// The terms that each type meets, a mark for each term.
		mut marks := []string{len: options.len}
		for cond in tc.comptime_conditions_on(node, name, names) {
			for term in comptime_condition_terms(cond, name) {
				met := tc.constraint_condition_term_types(term, name, constraint) or { continue }
				for i, option_name in option_names {
					marks[i] += if met.any(it.name() == option_name) { '1' } else { '0' }
				}
			}
		}
		mut group_of := map[string]int{}
		mut groups := [][]string{}
		for i, option in options {
			if at := group_of[marks[i]] {
				groups[at] << option
			} else {
				group_of[marks[i]] = groups.len
				groups << [option]
			}
		}
		classes[name] = groups
	}
	return classes
}

// generic_body_instance_budget is how many checks of a generic body with types
// for its type parameters check_generic_fn_body makes before it stops checking
// every combination of their types (see generic_body_combinations): each check
// costs what a check of a function with that body does.
const generic_body_instance_budget = 256

// generic_body_branch_budget is how many more checks the ways that the `$if`s of
// a generic body can go take, together, when not every combination is checked
// (see type_param_way_combinations).
const generic_body_branch_budget = 4 * generic_body_instance_budget

// generic_body_combinations gives the combinations of the types of `texts` to
// check the body of `node` with: those of type_param_combinations and, when
// they are not all of them, those of each way that the `$if`s of the body can
// go (see type_param_way_combinations). A branch that `$if`s on three type
// parameters lead to needs three types at once, which the combinations of every
// two of them may not give; and in it, the other type parameters need their
// types as well.
fn (tc &TypeChecker) generic_body_combinations(node flat.Node, texts map[string][]string, constraints map[string]GenericConstraint) []map[string]string {
	mut combinations := type_param_combinations(texts, generic_body_instance_budget)
	mut count := 1
	for _, options in texts {
		count *= options.len
		if count > combinations.len {
			break
		}
	}
	if count <= combinations.len {
		return combinations
	}
	names := texts.keys()
	mut seen := map[string]bool{}
	for combination in combinations {
		seen[type_param_combination_key(names, combination)] = true
	}
	classes := tc.type_param_branch_classes(node, texts, constraints)
	for combination in type_param_way_combinations(names, classes, generic_body_instance_budget,
		generic_body_branch_budget) {
		key := type_param_combination_key(names, combination)
		if key !in seen {
			seen[key] = true
			combinations << combination
		}
	}
	return combinations
}

// type_param_combination_key is `combination` as a text, with its types in the
// order of `names`.
fn type_param_combination_key(names []string, combination map[string]string) string {
	mut key := []string{cap: names.len}
	for name in names {
		key << combination[name]
	}
	return key.join('\n')
}

// type_param_way_combinations gives the combinations of the types of each way
// that the `$if`s can go, a group of `classes` for each type parameter of
// `names`: those of type_param_combinations over the groups of each way, while
// they come to at most `branch_budget` all together; else each type of each
// group one at a time, in each way. Past `budget` ways, the first type of each
// group stands for it, which takes every way but checks no other type.
fn type_param_way_combinations(names []string, classes map[string][][]string, budget int, branch_budget int) []map[string]string {
	mut sizes := []int{cap: names.len}
	mut ways := 1
	for name in names {
		sizes << classes[name].len
		if ways <= budget {
			ways *= classes[name].len
		}
	}
	if ways > budget {
		mut firsts := map[string][]string{}
		for name in names {
			mut first := []string{cap: classes[name].len}
			for group in classes[name] {
				first << group[0]
			}
			firsts[name] = first
		}
		return type_param_combinations(firsts, budget)
	}
	mut way_texts := []map[string][]string{}
	for way in all_index_rows(sizes) {
		mut texts := map[string][]string{}
		for i, name in names {
			texts[name] = classes[name][way[i]]
		}
		way_texts << texts
	}
	mut combinations := []map[string]string{}
	for texts in way_texts {
		combinations << type_param_combinations(texts, budget)
		if combinations.len > branch_budget {
			break
		}
	}
	if combinations.len <= branch_budget {
		return combinations
	}
	combinations = []map[string]string{}
	for texts in way_texts {
		mut way_sizes := []int{cap: names.len}
		for name in names {
			way_sizes << texts[name].len
		}
		combinations << index_rows_combinations(names, texts, one_at_a_time_index_rows(way_sizes))
	}
	return combinations
}

// type_param_combinations gives each type parameter of `texts` one of its
// types, in every combination while they are at most `budget`; past it, in
// combinations where every two type parameters meet with every two of their
// types (see pairwise_index_rows), what an operation between two of them
// needs; and when even those are past it, one type parameter at a time through
// all of its types while the others keep their first.
fn type_param_combinations(texts map[string][]string, budget int) []map[string]string {
	mut names := []string{cap: texts.len}
	mut sizes := []int{cap: texts.len}
	mut count := 1
	for name, options in texts {
		names << name
		sizes << options.len
		if count <= budget {
			count *= options.len
		}
	}
	mut rows := [][]int{}
	if count <= budget || names.len < 2 {
		rows = all_index_rows(sizes)
	} else {
		rows = pairwise_index_rows(sizes)
		if rows.len > budget {
			rows = one_at_a_time_index_rows(sizes)
		}
	}
	return index_rows_combinations(names, texts, rows)
}

// index_rows_combinations turns each row of `rows`, an index into the types of
// each type parameter of `names` in `texts`, into a combination.
fn index_rows_combinations(names []string, texts map[string][]string, rows [][]int) []map[string]string {
	mut combinations := []map[string]string{cap: rows.len}
	for row in rows {
		mut combination := map[string]string{}
		for i, name in names {
			combination[name] = texts[name][row[i]]
		}
		combinations << combination
	}
	return combinations
}

// all_index_rows returns every row of indexes into lists of `sizes` values, the
// first list the slowest to change.
fn all_index_rows(sizes []int) [][]int {
	mut rows := [][]int{}
	mut row := []int{len: sizes.len}
	mut done := false
	for !done {
		rows << row.clone()
		done = true
		for i := sizes.len - 1; i >= 0; i-- {
			row[i]++
			if row[i] < sizes[i] {
				done = false
				break
			}
			row[i] = 0
		}
	}
	return rows
}

// pairwise_index_rows returns rows of indexes into two or more lists of `sizes`
// values where every two lists meet with every two of their values, in far
// fewer rows than all of them: the rows of all the pairs of the first two
// lists, to which each next list adds, row by row, the value that meets the
// most values of the others that it has yet to meet, and then rows for what it
// still has to meet (the IPO strategy of combinatorial testing).
fn pairwise_index_rows(sizes []int) [][]int {
	mut rows := [][]int{}
	for a in 0 .. sizes[0] {
		for b in 0 .. sizes[1] {
			rows << [a, b]
		}
	}
	for i in 2 .. sizes.len {
		// unmet[j][a * sizes[i] + b]: value `a` of list `j` has yet to meet
		// value `b` of list `i`.
		mut unmet := [][]bool{}
		for j in 0 .. i {
			unmet << []bool{len: sizes[j] * sizes[i], init: true}
		}
		for mut row in rows {
			mut best := 0
			mut best_count := -1
			for b in 0 .. sizes[i] {
				mut count := 0
				for j in 0 .. i {
					if unmet[j][row[j] * sizes[i] + b] {
						count++
					}
				}
				if count > best_count {
					best = b
					best_count = count
				}
			}
			row << best
			for j in 0 .. i {
				unmet[j][row[j] * sizes[i] + best] = false
			}
		}
		// A new row leaves each list open, -1, until a pair takes it.
		first_new := rows.len
		for j in 0 .. i {
			for a in 0 .. sizes[j] {
				for b in 0 .. sizes[i] {
					if !unmet[j][a * sizes[i] + b] {
						continue
					}
					mut placed := false
					for r in first_new .. rows.len {
						if rows[r][i] == b && rows[r][j] == -1 {
							rows[r][j] = a
							placed = true
							break
						}
					}
					if !placed {
						mut row := []int{len: i + 1, init: -1}
						row[j] = a
						row[i] = b
						rows << row
					}
					unmet[j][a * sizes[i] + b] = false
				}
			}
		}
		for r in first_new .. rows.len {
			for j in 0 .. i {
				if rows[r][j] == -1 {
					rows[r][j] = 0
				}
			}
		}
	}
	return rows
}

// one_at_a_time_index_rows returns the row of the first value of every list of
// `sizes` values, and a row for each other value of each list, where the others
// keep their first.
fn one_at_a_time_index_rows(sizes []int) [][]int {
	mut rows := [[]int{len: sizes.len}]
	for i, size in sizes {
		for value in 1 .. size {
			mut row := []int{len: sizes.len}
			row[i] = value
			rows << row
		}
	}
	return rows
}

// closed_type_param_texts gives each type parameter of `combination` its type
// with the types of the other type parameters put into it: `C` of
// `[C Container[T], T Named]` is `Container[Named]`. A type parameter that its
// own type reaches again, `T` of `[T Comparable[T]]`, stays in it, open (see
// parse_type_as_instance).
fn closed_type_param_texts(combination map[string]string) map[string]string {
	mut closed := map[string]string{}
	for name, text in combination {
		mut path := map[string]bool{}
		path[name] = true
		closed[name] = close_type_param_text(text, combination, mut path)
	}
	return closed
}

// close_type_param_text puts into `text` the types of the type parameters of
// `combination` that it names, but those on `path`, the ones being put in.
fn close_type_param_text(text string, combination map[string]string, mut path map[string]bool) string {
	mut names := []string{}
	mut args := []string{}
	for name, other in combination {
		mut one := map[string]bool{}
		one[name] = true
		if name in path || !type_text_names_any(text, one) {
			continue
		}
		path[name] = true
		names << name
		args << close_type_param_text(other, combination, mut path)
		path.delete(name)
	}
	if names.len == 0 {
		return text
	}
	return subst_generic_text(text, args, names)
}

// type_param_occurrence_reason says why an error that the checks found, with
// the types `types` for each type parameter, is an error of the generic
// function: what the type parameters that decide it were. One that had every
// type of `texts`, more than one, does not decide it, and is left out; one that
// has only its interface is any type that implements it.
fn type_param_occurrence_reason(types map[string][]string, texts map[string][]string, constraints map[string]GenericConstraint) string {
	mut parts := []string{}
	for name, options in texts {
		taken := types[name] or { continue }
		constraint := constraints[name] or { continue }
		if options.len > 1 && taken.len >= options.len {
			continue
		}
		if constraint.is_interface {
			implementers := taken.filter(it != constraint.iface.name)
			if implementers.len == 0 || implementers.len < taken.len {
				parts << '`${name}` is any type that implements `${constraint.name}`'
			} else {
				verb := if implementers.len == 1 { 'implements' } else { 'implement' }
				parts << 'when `${name}` is ${type_names_text(implementers, 'or')}, which ${verb} `${constraint.name}`'
			}
			continue
		}
		// The shorter side: the types it had, or the ones it did not.
		missing := options.filter(it !in taken)
		what := if missing.len > 0 && missing.len < taken.len {
			if missing.len == 1 {
				'not `${missing[0].all_after_last('.')}`'
			} else {
				'none of ${type_names_text(missing, 'and')}'
			}
		} else {
			type_names_text(taken, 'or')
		}
		parts << 'when `${name}` is ${what}, in its constraint `${constraint.name}`'
	}
	return parts.join(', and ')
}

// type_names_text writes `names`, types, as a list joined with `word` before
// the last one, `f32` or `f64`: past three, the first three and how many more.
fn type_names_text(names []string, word string) string {
	mut shown := []string{}
	for name in names#[..3] {
		shown << '`${name.all_after_last('.')}`'
	}
	if names.len > 3 {
		return '${shown.join(', ')} ${word} ${names.len - 3} more'
	}
	if shown.len == 1 {
		return shown[0]
	}
	return '${shown#[..-1].join(', ')} ${word} ${shown.last()}'
}

// fold_type_param_expansions writes, in the message `msg`, a type in backticks
// that nests the constraints of type parameters that name each other,
// `Wrapper[Container[Wrapper[C]]]` of `[C Container[T], T Wrapper[C]]`, as the
// type parameter that it unfolds, `T`. A type that nests none, `Comparable[T]`,
// stays.
fn fold_type_param_expansions(msg string, constraints map[string]GenericConstraint) string {
	mut names := map[string]bool{}
	for name, _ in constraints {
		names[name] = true
	}
	mut folds := map[string]string{}
	for name, constraint in constraints {
		if type_text_names_any(constraint.name, names) {
			folds[constraint.name] = name
		}
	}
	if folds.len == 0 || !msg.contains('`') {
		return msg
	}
	parts := msg.split('`')
	mut out := []string{cap: parts.len}
	for i, part in parts {
		if i % 2 == 0 {
			out << part
			continue
		}
		mut text := part
		mut steps := 0
		for _ in 0 .. 32 {
			mut changed := false
			for from, to in folds {
				if text.contains(from) {
					text = text.replace(from, to)
					steps++
					changed = true
				}
			}
			if !changed {
				break
			}
		}
		out << if steps > 1 && text in names { text } else { part }
	}
	return out.join('`')
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
