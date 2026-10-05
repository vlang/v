module semver

// * Private functions.
// `||` is matched without its surrounding spaces, because the grammar allows
// either side of it to be empty; each set is trimmed where it is read.
const comparator_sep = ' '
const comparator_set_sep = '||'
const hyphen_range_sep = ' - '
const x_range_symbols = 'Xx*'

enum Operator {
	gt
	lt
	ge
	le
	eq
}

struct Comparator {
	ver Version
	op  Operator
}

struct ComparatorSet {
	comparators []Comparator
}

struct Range {
	comparator_sets []ComparatorSet
}

struct InvalidComparatorFormatError {
	MessageError
}

fn (r Range) satisfies(ver Version) bool {
	return r.comparator_sets.any(it.satisfies(ver))
}

fn (set ComparatorSet) satisfies(ver Version) bool {
	for comp in set.comparators {
		if !comp.satisfies(ver) {
			return false
		}
	}
	// The grammar excludes a prerelease unless this set names one on the same
	// `[major, minor, patch]`. Without that, `^1.0.0` would admit `1.0.0-alpha`
	// and a resolver could never tell a pre-release from the release it precedes.
	if ver.prerelease.len > 0 && !set.admits_prerelease(ver) {
		return false
	}
	return true
}

fn (set ComparatorSet) admits_prerelease(ver Version) bool {
	for comp in set.comparators {
		if comp.ver.prerelease.len > 0 && comp.ver.major == ver.major && comp.ver.minor == ver.minor
			&& comp.ver.patch == ver.patch {
			return true
		}
	}
	return false
}

fn (c Comparator) satisfies(ver Version) bool {
	return match c.op {
		.gt { ver > c.ver }
		.lt { ver < c.ver }
		.ge { ver >= c.ver }
		.le { ver <= c.ver }
		.eq { ver == c.ver }
	}
}

fn parse_range(input string) !Range {
	// The grammar reads an empty range as `*`. Left to the comparator parser it
	// would land on the exact pin `=0.0.0`, so it is answered here instead.
	if input.trim_space().len == 0 {
		return Range{[ComparatorSet{[Comparator{Version{}, Operator.ge}]}]}
	}
	mut comparator_sets := []ComparatorSet{}
	for raw_comp_set in input.split(comparator_set_sep) {
		comp_set := raw_comp_set.trim_space()
		if comp_set.len == 0 {
			// `range ::= ... | ''`: an empty arm is the empty range, which is `*`
			comparator_sets << ComparatorSet{[Comparator{Version{}, Operator.ge}]}
			continue
		}
		comparator_sets << parse_comparator_set(comp_set) or { return err }
	}
	return Range{comparator_sets}
}

// parse_comparator_set reads the whitespace-separated operands of one `||` arm.
//
// The grammar puts no limit on how many there are, so `>=1.0.0 <2.0.0 <=3.0.0` is a
// legitimate range rather than a parse failure. Each operand is expanded on its
// own if it is a range form and read as a comparator otherwise, and the results are
// intersected. Deciding once for the whole arm instead — which is what this used to
// do — only works when the arm holds a single range, because `expand_comparator_set`
// expands exactly one: `3.X >0.0 >=2.4` was handed over whole and came back as the
// expansion of `3.X` alone.
fn parse_comparator_set(input string) !ComparatorSet {
	// A hyphen range is three whitespace-separated operands (`1.2.3`, `-`, `2.3.4`),
	// and `-` occurs nowhere else in the grammar, so the arm is one of those or it
	// is not.
	if input.contains(hyphen_range_sep) {
		return expand_hyphen(input) or {
			return &InvalidComparatorFormatError{
				msg: 'Invalid comparator set "${input}"'
			}
		}
	}

	raw_comparators := input.split(comparator_sep)
	mut comparators := []Comparator{}
	for raw_comp in raw_comparators {
		if raw_comp.len == 0 {
			continue
		}
		// Checked for every operand, not only for the ones that go on to be
		// expanded: `>2.1.` reaches the plain comparator path, and without this it
		// is answered instead of refused.
		if !is_grammar_operand(raw_comp) {
			return &InvalidComparatorFormatError{
				msg: 'Invalid comparator "${raw_comp}" in input "${input}"'
			}
		}
		if can_expand(raw_comp) {
			expanded := expand_comparator_set(raw_comp) or {
				return &InvalidComparatorFormatError{
					msg: 'Invalid comparator "${raw_comp}" in input "${input}"'
				}
			}
			comparators << expanded.comparators
			continue
		}
		c := parse_comparator(raw_comp) or {
			return &InvalidComparatorFormatError{
				msg: 'Invalid comparator "${raw_comp}" in input "${input}"'
			}
		}
		comparators << c
	}
	return ComparatorSet{comparators}
}

fn parse_comparator(input string) ?Comparator {
	op, raw_version := comparator_parts(input)
	version := coerce_version(raw_version) or { return none }
	return Comparator{version, op}
}

fn comparator_parts(input string) (Operator, string) {
	mut op := Operator.eq
	raw_version := match true {
		input.starts_with('>=') {
			op = .ge
			input[2..]
		}
		input.starts_with('<=') {
			op = .le
			input[2..]
		}
		input.starts_with('>') {
			op = .gt
			input[1..]
		}
		input.starts_with('<') {
			op = .lt
			input[1..]
		}
		input.starts_with('=') {
			input[1..]
		}
		else {
			input
		}
	}

	return op, raw_version
}

fn parse_xrange(input string) ?Version {
	parsed := parse(input)
	if has_prerelease(input) {
		if parsed.prerelease.len == 0 || parsed.raw_ints.len != 3 {
			return none
		}
		for identifier in parsed.prerelease.split('.') {
			if identifier.len == 0
				|| (identifier.len > 1 && identifier[0] == `0` && is_valid_number(identifier)) {
				return none
			}
		}
	}
	if input.contains('+') && (parsed.metadata.len == 0
		|| parsed.metadata.split('.').any(it.len == 0)) {
		return none
	}
	// Validate every component before a wildcard replaces the later components.
	if parsed.raw_ints.any(it !in ['x', 'X', '*'] && !is_valid_number(it)) {
		return none
	}
	mut wildcard_seen := false
	for component in parsed.raw_ints {
		if component in ['x', 'X', '*'] {
			wildcard_seen = true
		} else if wildcard_seen {
			return none
		}
	}
	mut raw_ver := parsed.complete()
	for typ in versions {
		if raw_ver.raw_ints[typ].index_any(x_range_symbols) == -1 {
			continue
		}
		match typ {
			ver_major {
				raw_ver.raw_ints[ver_major] = '0'
				raw_ver.raw_ints[ver_minor] = '0'
				raw_ver.raw_ints[ver_patch] = '0'
			}
			ver_minor {
				raw_ver.raw_ints[ver_minor] = '0'
				raw_ver.raw_ints[ver_patch] = '0'
			}
			ver_patch {
				raw_ver.raw_ints[ver_patch] = '0'
			}
			else {}
		}
	}
	version := raw_ver.validate() or { return none }
	if first_wildcard_index(input) < 3 {
		return Version{version.major, version.minor, version.patch, '', version.metadata}
	}
	return version
}

// numeric_core strips the prerelease and build metadata from a version-shaped
// string, leaving just the dotted numbers.
fn numeric_core(s string) string {
	return s.all_before('-').all_before('+')
}

// has_prerelease reports whether a version-shaped string carries a prerelease tag.
fn has_prerelease(s string) bool {
	return s.all_before('+').contains('-')
}

// is_grammar_operand reports whether an operand that is about to be expanded
// actually has a shape the grammar gives meaning to.
//
// `coerce_version` completes a short version into a plausible one, so an operand
// the grammar does not define would be answered rather than refused: `~1.4-Z`
// would complete to `1.4.0-Z` and become a real tilde range, and `0.4.` would
// complete to `0.4.0`. node-semver rejects both. Two rules do the refusing:
//
//   - a prerelease needs all three components, since `1.2-beta` is not a version
//     any more than `1-beta` is
//   - no component may be empty
//
// A `-` that is the hyphen range separator rather than a prerelease tag is not
// this function's business: `parse_comparator_set` routes those to expand_hyphen
// before they get here.
fn is_grammar_operand(operand string) bool {
	body := operand.all_before('+')
	core := numeric_core(body)
	if core.split('.').any(it.len == 0) {
		return false
	}
	if has_prerelease(body) {
		// Hyphens belong to prerelease identifiers after the first separator.
		// Only dots delimit identifiers, so `a-.b` and `a.-b` are both valid.
		if parts_of(core).len < 3 || body.all_after('-').split('.').any(it.len == 0) {
			return false
		}
	}
	return true
}

// parts_of splits a version-shaped string into its dotted components, without
// completing a short one.
fn parts_of(s string) []string {
	return numeric_core(s).split('.')
}

// first_wildcard_index reports which component of a range stands in for a number
// with a wildcard, or is missing: `1.x` is 1, `1.2` is 2 because the absent patch
// stands in for a number too. It returns 3 when every component is a real number.
//
// This is what tells `1.2.x` (`<1.3.0`) from `1.2.3` (`=1.2.3`), which parse() alone
// cannot: both complete to the same version.
fn first_wildcard_index(s string) int {
	for i, part in parts_of(s) {
		if i >= 3 {
			break
		}
		if part.index_any(x_range_symbols) > -1 {
			return i
		}
	}
	return parts_of(s).len
}

// has_real_component reports whether the component at `index` is a number rather
// than a wildcard or absent. `^0.0.3` bounds the patch; `^0.0` and `^0.0.x` do not.
fn has_real_component(s string, index int) bool {
	parts := parts_of(s)
	if index >= parts.len {
		return false
	}
	return is_valid_number(parts[index])
}

// is_bare_partial_version reports a range that is nothing but a version with fewer
// than three components. The grammar treats a short version as an x-range, so `1.2`
// means `1.2.x` rather than the exact pin `=1.2.0`.
fn is_bare_partial_version(input string) bool {
	if input.len == 0 || input.contains(comparator_sep) || has_prerelease(input) {
		return false
	}
	// An operator means this is a comparator rather than a bare version.
	if input[0] in [`>`, `<`, `=`, `~`, `^`] {
		return false
	}
	parts := parts_of(input)
	if parts.len > 2 {
		return false
	}
	return parts.all(it.len > 0 && is_valid_number(it))
}

// is_major_only reports a bare major version such as the `2` in `1.2.3 - 2`.
fn is_major_only(s string) bool {
	if has_prerelease(s) {
		return false
	}
	parts := parts_of(s)
	return parts.len == 1 && is_valid_number(parts[0])
}

// can_expand reports whether an operand is a shape expand_comparator_set knows how
// to expand, rather than a plain comparator.
fn can_expand(input string) bool {
	if input.len == 0 {
		return false
	}
	// Inspect each operand before judging a composite set.
	if input.contains(comparator_sep) && !input.contains(hyphen_range_sep) {
		return input.split(comparator_sep).any(can_expand(it))
	}
	if input[0] == `~` || input[0] == `^` || input.contains(hyphen_range_sep) {
		return true
	}
	// An operator in front of a shortened version is a rewrite rather than a plain
	// comparator, so it is stepped over before the shape is judged. Without this,
	// `>0` looks like a comparator and is answered as `>0.0.0` instead of `>=1.0.0`.
	operand := match input[0] {
		`<`, `>`, `=` {
			if input.len > 1 && input[1] == `=` {
				input[2..]
			} else {
				input[1..]
			}
		}
		else { input }
	}
	if operand.len == 0 {
		return false
	}
	// A prerelease identifier may be alphanumeric, so an `x` after the `-` belongs
	// to the tag and is not a wildcard: `1.4.0-125.12.x` is a plain comparator.
	if numeric_core(operand).index_any(x_range_symbols) > -1 {
		return true
	}
	return is_bare_partial_version(operand)
}

fn expand_comparator_set(input string) ?ComparatorSet {
	if input.contains(hyphen_range_sep) {
		return expand_hyphen(input)
	}
	if input.contains(comparator_sep) {
		mut comparators := []Comparator{}
		for raw_comp in input.split(comparator_sep) {
			if raw_comp.len == 0 {
				return none
			}
			if can_expand(raw_comp) {
				set := expand_comparator_set(raw_comp) or { return none }
				comparators << set.comparators
			} else {
				comparators << parse_comparator(raw_comp) or { return none }
			}
		}
		return ComparatorSet{comparators}
	}
	match input[0] {
		`~` { return expand_tilda(input[1..]) }
		`^` { return expand_caret(input[1..]) }
		`<`, `>`, `=` { return expand_operator_range(input) }
		else {}
	}
	return expand_xrange(input)
}

// expand_operator_range handles an operator in front of an incomplete version.
//
// The grammar does not read these as a comparator applied to a shortened version.
// It expands the version to its x-range first and applies the operator to that
// range, so:
//
//     >=1.2   is  >=1.2.0
//     <1.2    is  <1.2.0-0
//     <=1.2   is  <1.3.0-0
//     >1.2    is  >=1.3.0
//     =1.2    is  >=1.2.0 <1.3.0-0
//
// which is why `<=1.2` admits nothing below 0.0.0 while `<=1.2.0` does not: the
// first drops the whole `1.2` series and the second is an ordinary comparator.
//
// A version with all three components is not rewritten. `>=1.2.3` is `>=1.2.3`,
// prerelease included, which is what parse_comparator already does.
fn expand_operator_range(input string) ?ComparatorSet {
	mut raw_version := input[1..]
	// The `=` has to be read off before the operator is dispatched on. Matching on
	// input[0] alone sends `>=1.2` down the `>` branch, which drops the whole 1.2
	// series when it should keep it.
	mut inclusive := false
	if input.len > 1 && input[1] == `=` {
		inclusive = true
		raw_version = input[2..]
	}
	// `first_wildcard_index` counts a missing component as standing in for a
	// number, so it answers below 3 exactly for the versions the grammar shortens.
	if raw_version.len == 0 || first_wildcard_index(raw_version) > 2 {
		return none
	}
	expanded := expand_xrange(raw_version) or { return none }
	if expanded.comparators.len < 2 {
		// A wildcard major has no ceiling. Inclusive operators and equality keep
		// that open range, while strict operators cannot match any version.
		if inclusive || input[0] == `=` {
			return expanded
		}
		return ComparatorSet{[Comparator{Version{0, 0, 0, '0', ''}, Operator.lt}]}
	}
	floor := expanded.comparators[0].ver
	ceiling := expanded.comparators[1].ver
	// `floor - 0` is the exclusive lower bound the grammar writes for `<`, and
	// `ceiling` already carries its own `- 0`.
	floor_exclusive := Version{floor.major, floor.minor, floor.patch, '0', ''}
	ceiling_exclusive := Version{ceiling.major, ceiling.minor, ceiling.patch, '', ''}

	if input[0] == `=` {
		return expanded
	}
	if input[0] == `<` {
		return ComparatorSet{[Comparator{if inclusive {
			ceiling
		} else {
			floor_exclusive
		}, Operator.lt}]}
	}
	return ComparatorSet{[Comparator{if inclusive {
		floor
	} else {
		ceiling_exclusive
	}, Operator.ge}]}
}

fn expand_tilda(raw_version string) ?ComparatorSet {
	min_ver := coerce_version(raw_version) or { return none }
	// A written minor, including zero, limits a tilde range to the next minor.
	// The -0 ceiling excludes prereleases of the next series when ranges intersect.
	max_ver := if !has_real_component(raw_version, 1) {
		Version{min_ver.major + 1, 0, 0, '0', ''}
	} else {
		Version{min_ver.major, min_ver.minor + 1, 0, '0', ''}
	}
	return make_comparator_set_ge_lt(min_ver, max_ver)
}

// expand_caret builds `^`. The rule is that the left-most non-zero component is
// held still and the one before it is what may grow, and a component that the
// grammar lets stand in for a number is treated as a wildcard rather than as a 0:
// `^0.0.3` stops at 0.0.4, while `^0.0` and `^0.0.x` stop at 0.1.0, and `^0.x` stops
// at 1.0.0.
fn expand_caret(raw_version string) ?ComparatorSet {
	min_ver := coerce_version(raw_version) or { return none }
	return make_comparator_set_ge_lt(min_ver, caret_ceiling(raw_version, min_ver))
}

fn caret_ceiling(raw_version string, min_ver Version) Version {
	if min_ver.major > 0 {
		return Version{min_ver.major + 1, 0, 0, '', ''}
	}
	if !has_real_component(raw_version, 1) {
		// a wildcard minor: the whole major
		return Version{1, 0, 0, '', ''}
	}
	if min_ver.minor > 0 {
		return Version{0, min_ver.minor + 1, 0, '', ''}
	}
	if has_real_component(raw_version, 2) {
		return Version{0, 0, min_ver.patch + 1, '', ''}
	}
	// a wildcard or absent patch under 0.0
	return Version{0, 1, 0, '', ''}
}

fn expand_hyphen(raw_range string) ?ComparatorSet {
	raw_versions := raw_range.split(hyphen_range_sep)
	if raw_versions.len != 2 {
		return none
	}
	min_ver := coerce_version(raw_versions[0]) or { return none }
	if is_major_only(raw_versions[1]) {
		// `1.2.3 - 2` runs to the end of major 2, not to 2.0.0
		next := numeric_core(raw_versions[1]).int() + 1
		return make_comparator_set_ge_lt(min_ver, Version{next, 0, 0, '', ''})
	}
	if first_wildcard_index(raw_versions[1]) == 0 {
		// `1.2.3 - *` is open ended above, like `*` on its own
		return ComparatorSet{[Comparator{min_ver, Operator.ge}]}
	}
	// `2-beta` has a bare major under a prerelease tag, which `parts_of` reports as one
	// component, so it must not be taken for the `2.x` case.
	if first_wildcard_index(raw_versions[1]) == 1 && !has_prerelease(raw_versions[1]) {
		// `1.2 - 2.x` runs to the end of major 2 rather than to 2.0.0: a wildcard
		// minor means the whole series, the same reading `2 - 3.*` gets.
		next := numeric_core(raw_versions[1]).int() + 1
		return make_comparator_set_ge_lt(min_ver, Version{next, 0, 0, '', ''})
	}
	raw_max_ver := parse(raw_versions[1])
	if raw_max_ver.is_missing(ver_major) {
		return none
	}
	if raw_max_ver.is_missing(ver_minor) {
		max_ver := raw_max_ver.coerce() or { return none }.increment(.minor)
		return make_comparator_set_ge_lt(min_ver, max_ver)
	}
	max_ver := raw_max_ver.coerce() or { return none }
	return make_comparator_set_ge_le(min_ver, max_ver)
}

// expand_xrange builds `1.2.x`, `1.x`, `1`, `*` and their zero-major forms.
//
// The ceiling raises the component before the wildcard, on a zero major too:
// `0.x` stops at 1.0.0 and `0.1.x` stops at 0.2.0. Returning only the floor there
// is what made every zero-major x-range unbounded.
fn expand_xrange(raw_range string) ?ComparatorSet {
	min_ver := parse_xrange(raw_range) or { return none }
	wildcard := first_wildcard_index(raw_range)
	match wildcard {
		0 {
			// `*` stands in for the major itself, so there is no ceiling at all
			return ComparatorSet{[Comparator{Version{}, Operator.ge}]}
		}
		1 {
			upper := Version{min_ver.major + 1, 0, 0, '0', ''}
			return make_comparator_set_ge_lt(min_ver, upper)
		}
		2 {
			upper := Version{min_ver.major, min_ver.minor + 1, 0, '0', ''}
			return make_comparator_set_ge_lt(min_ver, upper)
		}
		else {
			// every component is a real number, so this is an exact pin
			return ComparatorSet{[Comparator{min_ver, Operator.eq}]}
		}
	}
}

// expand_xrange_comparator adjusts the bound at the first wildcard or missing
// component: >1.x starts at 2.0.0, while <=1.2.x stops before 1.3.0.
fn expand_xrange_comparator(raw_version string, op Operator) ?ComparatorSet {
	if op == .eq {
		return expand_xrange(raw_version)
	}
	min_ver := parse_xrange(raw_version) or { return none }
	wildcard := first_wildcard_index(raw_version)
	if wildcard >= 3 {
		return ComparatorSet{[Comparator{min_ver, op}]}
	}
	if wildcard == 0 {
		if op in [.gt, .lt] {
			// No release can be strictly above or below an unspecified major.
			return ComparatorSet{[Comparator{Version{0, 0, 0, '0', ''}, Operator.lt}]}
		}
		return ComparatorSet{[Comparator{Version{}, Operator.ge}]}
	}
	mut bound := min_ver
	mut bound_op := op
	if op in [.gt, .le] {
		bound = if wildcard == 1 {
			Version{min_ver.major + 1, 0, 0, '', ''}
		} else {
			Version{min_ver.major, min_ver.minor + 1, 0, '', ''}
		}
		bound_op = if op == .gt { Operator.ge } else { Operator.lt }
	}
	if bound_op == .lt {
		bound = Version{bound.major, bound.minor, bound.patch, '0', ''}
	}
	return ComparatorSet{[Comparator{bound, bound_op}]}
}

// make_comparator_set_ge_lt builds `>= min, < max`.
//
// The ceiling always carries a `-0`. node-semver writes every exclusive upper
// bound that way — `<1.3.0-0`, `<2.0.0-0`, `<0.0.4-0` — and it is not decoration:
// `0.0.4-canary < 0.0.4` is true, while `0.0.4-canary < 0.0.4-0` is not. Without
// it a range with a caret admits a prerelease of the version it was supposed to
// stop below, which is what `^0.0.3-2.2.10 ^0.0.4-8.1` did.
fn make_comparator_set_ge_lt(min Version, max Version) ComparatorSet {
	ceiling := Version{max.major, max.minor, max.patch, '0', ''}
	return ComparatorSet{[
		Comparator{min, Operator.ge},
		Comparator{ceiling, Operator.lt},
	]}
}

fn make_comparator_set_ge_le(min Version, max Version) ComparatorSet {
	return ComparatorSet{[
		Comparator{min, Operator.ge},
		Comparator{max, Operator.le},
	]}
}
