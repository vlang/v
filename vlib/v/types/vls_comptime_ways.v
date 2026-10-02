module types

// What each constrained type parameter of a generic body is where the editor
// asks, whatever the conditions of the `$if`s on the way there: tests of
// several type parameters, joined with `&&`, `||` and `!`, in parentheses. A
// type parameter can be one of its types there when some types of the others
// make every `$if` on the way go the way that leads there: in the branch of
// `$if a is f64 && b is f64 {` both are `f64`, in the `$else` of `$if a is f64
// || b is f64 {` neither is, and in `$else $if a is f64 {` after the first,
// `b` is not. The check of the body walks one `$if` at a time and leaves those
// on several type parameters to the checks of the combinations of their types
// (see constraint_comptime_branches and check_generic_fn_body), whose messages
// say what the others are.

// vls_comptime_way_budget is how many combinations of the groups of the types
// of the type parameters, and of the tests that may go either way, a question
// looks at. Past it, the `$if`s leave the type parameters as they are.
const vls_comptime_way_budget = 1 << 16

// ComptimeWay is a `$if` on the way to a node: its condition, with the values
// it tests written as their type parameters, and whether the way takes its
// first branch.
struct ComptimeWay {
	cond  string
	taken bool
}

// ComptimeCondNode is a node of a parsed `$if` condition: a test, or `!`, `&&`
// or `||` of `left` and `right`.
struct ComptimeCondNode {
	op    u8 // 0 for a test, `!`, `&` or `|`
	left  int
	right int
	test  int // a test: its index in ComptimeConds.tests
}

// ComptimeConds are the conditions of the `$if`s on a way, parsed into nodes
// that share their tests: `a is f64` in two of them is one test.
struct ComptimeConds {
mut:
	nodes []ComptimeCondNode
	tests []string
	text  string // the condition being parsed
	pos   int
}

// parse_cond parses `text`, a `$if` condition as the parser keeps it, `(a is
// f64) && ((b is f64) && (c is f64))`, `!(T in[f32, f64])`, or as it is
// written, and returns its root. none when it cannot read it.
fn (mut c ComptimeConds) parse_cond(text string) ?int {
	c.text = text
	c.pos = 0
	root := c.parse_or()?
	c.skip_blanks()
	if c.pos != c.text.len {
		return none
	}
	return root
}

fn (mut c ComptimeConds) parse_or() ?int {
	mut left := c.parse_and()?
	for c.eat('||') {
		right := c.parse_and()?
		left = c.add_node(ComptimeCondNode{
			op:    `|`
			left:  left
			right: right
		})
	}
	return left
}

fn (mut c ComptimeConds) parse_and() ?int {
	mut left := c.parse_unary()?
	for c.eat('&&') {
		right := c.parse_unary()?
		left = c.add_node(ComptimeCondNode{
			op:    `&`
			left:  left
			right: right
		})
	}
	return left
}

fn (mut c ComptimeConds) parse_unary() ?int {
	c.skip_blanks()
	if c.pos >= c.text.len {
		return none
	}
	if c.text[c.pos] == `!` {
		c.pos++
		inner := c.parse_unary()?
		return c.add_node(ComptimeCondNode{
			op:   `!`
			left: inner
		})
	}
	if c.text[c.pos] == `(` {
		c.pos++
		inner := c.parse_or()?
		c.skip_blanks()
		if c.pos >= c.text.len || c.text[c.pos] != `)` {
			return none
		}
		c.pos++
		return inner
	}
	return c.parse_test()
}

// parse_test reads a test up to the `&&`, `||` or `)` that ends it: a list,
// `[f32, f64]`, a call, `sizeof(T)`, and a string have brackets of their own.
fn (mut c ComptimeConds) parse_test() ?int {
	start := c.pos
	mut depth := 0
	mut quote := u8(0)
	for c.pos < c.text.len {
		ch := c.text[c.pos]
		if quote != 0 {
			if ch == `\\` {
				c.pos++
			} else if ch == quote {
				quote = 0
			}
		} else if ch in [`'`, `"`] {
			quote = ch
		} else if ch in [`(`, `[`] {
			depth++
		} else if ch in [`)`, `]`] {
			if depth == 0 {
				break
			}
			depth--
		} else if depth == 0 && ch in [`&`, `|`] && c.pos + 1 < c.text.len
			&& c.text[c.pos + 1] == ch {
			break
		}
		c.pos++
	}
	if quote != 0 || depth != 0 || c.pos > c.text.len {
		return none
	}
	text := c.text[start..c.pos].trim_space()
	if text.len == 0 {
		return none
	}
	mut test := c.tests.index(text)
	if test < 0 {
		test = c.tests.len
		c.tests << text
	}
	return c.add_node(ComptimeCondNode{
		test: test
	})
}

fn (mut c ComptimeConds) add_node(node ComptimeCondNode) int {
	c.nodes << node
	return c.nodes.len - 1
}

fn (mut c ComptimeConds) skip_blanks() {
	for c.pos < c.text.len && c.text[c.pos] in [` `, `\t`, `\n`, `\r`] {
		c.pos++
	}
}

// eat skips `op` and the blanks before it, if `op` comes next.
fn (mut c ComptimeConds) eat(op string) bool {
	c.skip_blanks()
	if c.text[c.pos..].starts_with(op) {
		c.pos += op.len
		return true
	}
	return false
}

// holds reports whether the node `id` holds when each test holds as `values`
// says.
fn (c &ComptimeConds) holds(id int, values []bool) bool {
	node := c.nodes[id]
	return match node.op {
		`!` { !c.holds(node.left, values) }
		`&` { c.holds(node.left, values) && c.holds(node.right, values) }
		`|` { c.holds(node.left, values) || c.holds(node.right, values) }
		else { values[node.test] }
	}
}

// ComptimeParamGroups are the types of a constrained type parameter that the
// tests of a way tell apart, in groups that pass the same tests.
struct ComptimeParamGroups {
	name string
mut:
	types []Type // its types: those of its set, or of an interface, the ones its tests name
	open  bool   // an interface: after `types`, any other type that implements it
	tests []int  // the tests of it
	// For each of its types, and the open one last, its group; for each group,
	// whether its types pass each of the tests of the way.
	group_of []int
	passes   [][]bool
}

// comptime_ways_constraints are `constraints` at the end of `ways`, the `$if`s on
// the way to a node from the outermost one: each type parameter keeps the
// types for which some types of the others take every `$if` the way that
// leads there. One of `names` without a constraint can be any type, as one
// whose constraint is an interface: `$if u is int {` makes it an `int`, and
// gives it a constraint without a name there. A test that cannot be followed,
// or that tests no type parameter, may go either way. A condition that cannot
// be read, or ways with too many combinations, leave the type parameters as
// they are.
fn (tc &TypeChecker) comptime_ways_constraints(ways []ComptimeWay, constraints map[string]GenericConstraint, names []string) map[string]GenericConstraint {
	mut testable := constraints.clone()
	for name in names {
		if name !in testable {
			testable[name] = GenericConstraint{
				is_interface: true
			}
		}
	}
	mut conds := ComptimeConds{}
	mut roots := []int{}
	mut taken := []bool{}
	for way in ways {
		root := conds.parse_cond(way.cond) or { continue }
		roots << root
		taken << way.taken
	}
	if roots.len == 0 {
		return constraints
	}
	mut groups := []ComptimeParamGroups{}
	mut either_way := []int{}
	mut types_of := [][]Type{len: conds.tests.len, init: []Type{}}
	mut negated := []bool{len: conds.tests.len}
	mut group_index := map[string]int{}
	for test_idx, test in conds.tests {
		name, tested_types, is_negated := tc.comptime_test_types(test, testable) or {
			either_way << test_idx
			continue
		}
		types_of[test_idx] = tested_types
		negated[test_idx] = is_negated
		if at := group_index[name] {
			groups[at].tests << test_idx
			continue
		}
		group_index[name] = groups.len
		groups << ComptimeParamGroups{
			name:  name
			tests: [test_idx]
		}
	}
	if either_way.len > 16 {
		return constraints
	}
	mut combinations := 1 << either_way.len
	for mut g in groups {
		constraint := testable[g.name]
		if constraint.is_interface {
			g.open = true
		} else {
			g.types = constraint.types.clone()
		}
		// `$if T is Admin` with `[T User]`: `Admin`, which embeds `User`; with an
		// interface, the types its tests name.
		for test_idx in g.tests {
			for t in types_of[test_idx] {
				if !g.types.any(it.name() == t.name()) {
					g.types << t
				}
			}
		}
		mut group_of_key := map[string]int{}
		count := if g.open { g.types.len + 1 } else { g.types.len }
		for i in 0 .. count {
			mut key := ''
			mut passes := []bool{len: conds.tests.len}
			for test_idx in g.tests {
				named := i < g.types.len && types_of[test_idx].any(it.name() == g.types[i].name())
				passes[test_idx] = named != negated[test_idx]
				key += if passes[test_idx] { '1' } else { '0' }
			}
			if at := group_of_key[key] {
				g.group_of << at
			} else {
				group_of_key[key] = g.passes.len
				g.group_of << g.passes.len
				g.passes << passes
			}
		}
		combinations *= g.passes.len
		if combinations > vls_comptime_way_budget {
			return constraints
		}
	}
	// The groups that some combination takes the way with.
	mut possible := [][]bool{len: groups.len}
	for k, g in groups {
		possible[k] = []bool{len: g.passes.len}
	}
	mut values := []bool{len: conds.tests.len}
	mut chosen := []int{len: groups.len}
	mut reached := false
	for combination in 0 .. combinations {
		mut rest := combination
		for k, g in groups {
			chosen[k] = rest % g.passes.len
			rest /= g.passes.len
			for test_idx in g.tests {
				values[test_idx] = g.passes[chosen[k]][test_idx]
			}
		}
		for bit, test_idx in either_way {
			values[test_idx] = (rest >> bit) & 1 == 1
		}
		mut takes := true
		for i, root in roots {
			if conds.holds(root, values) != taken[i] {
				takes = false
				break
			}
		}
		if !takes {
			continue
		}
		reached = true
		for k, group in chosen {
			possible[k][group] = true
		}
	}
	// A way that no types take: nothing to tell there.
	if !reached {
		return constraints
	}
	mut narrowed := constraints.clone()
	for k, g in groups {
		constraint := testable[g.name]
		if g.open && possible[k][g.group_of[g.types.len]] {
			continue
		}
		mut kept := []Type{}
		for i, t in g.types {
			if possible[k][g.group_of[i]] {
				kept << t
			}
		}
		if !constraint.is_interface && kept.len == constraint.types.len
			&& g.types.len == constraint.types.len {
			continue
		}
		narrowed[g.name] = GenericConstraint{
			name:          constraint.name
			types:         kept
			family_struct: constraint.family_struct
		}
	}
	return narrowed
}

// comptime_test_types tells which constrained type parameter of `constraints`
// the test `test` of a `$if` tests, the types it names or lets that type
// parameter have, and whether it holds for the types those do not name, as
// `!is` and `!in` on an interface do. none for a test of no constrained type
// parameter, of more than one, of one without types, or that says something of
// one that cannot be followed, as `sizeof(T) == 8`.
fn (tc &TypeChecker) comptime_test_types(test string, constraints map[string]GenericConstraint) ?(string, []Type, bool) {
	names := constraints.keys().filter(comptime_condition_names(test, it))
	if names.len != 1 {
		return none
	}
	name := names[0]
	constraint := constraints[name]
	// `[T Nope]`, which the check reports: no types to tell apart.
	if !constraint.is_interface && constraint.types.len == 0 {
		return none
	}
	if constraint.is_interface {
		// `T !is User`: an interface has no list of types to leave the others
		// of, so the test holds for the types that `T is User` does not name.
		if positive := comptime_positive_test(test, name) {
			return name, tc.constraint_condition_term_types(positive, name, constraint)?, true
		}
	}
	return name, tc.constraint_condition_term_types(test, name, constraint)?, false
}

// comptime_positive_test is `test`, `name !is X` or `name !in [X, Y]`, without
// its `!`; none for any other test.
fn comptime_positive_test(test string, name string) ?string {
	if !test.starts_with(name) {
		return none
	}
	rest := test[name.len..].trim_space()
	for op in ['!is', '!in'] {
		if !rest.starts_with(op) {
			continue
		}
		after := rest[op.len..]
		// `T !in[User]`: the parser writes the list right after `!in`.
		if after.len > 0 && (after[0] in [` `, `\t`] || (op == '!in' && after[0] == `[`)) {
			return '${name} ${op[1..]} ${after.trim_space()}'
		}
	}
	return none
}
