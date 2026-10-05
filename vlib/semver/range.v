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
		if can_expand(comp_set) {
			s := expand_comparator_set(comp_set) or {
				return &InvalidComparatorFormatError{
					msg: 'Invalid comparator set "${comp_set}"'
				}
			}
			comparator_sets << s
		} else {
			s := parse_comparator_set(comp_set) or { return err }
			comparator_sets << s
		}
	}
	return Range{comparator_sets}
}

// parse_comparator_set reads the comparators joined by whitespace in one `||` arm.
// The grammar puts no limit on how many there are, so `>=1.0.0 <2.0.0 <=3.0.0` is a
// legitimate range rather than a parse failure.
fn parse_comparator_set(input string) !ComparatorSet {
	raw_comparators := input.split(comparator_sep)
	mut comparators := []Comparator{}
	for raw_comp in raw_comparators {
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

	version := coerce_version(raw_version) or { return none }
	return Comparator{version, op}
}

fn parse_xrange(input string) ?Version {
	mut raw_ver := parse(input).complete()
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
	return raw_ver.validate()
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

fn can_expand(input string) bool {
	if input.len == 0 {
		return false
	}
	return input[0] == `~` || input[0] == `^` || input.contains(hyphen_range_sep)
		|| input.index_any(x_range_symbols) > -1 || is_bare_partial_version(input)
}

fn expand_comparator_set(input string) ?ComparatorSet {
	match input[0] {
		`~` { return expand_tilda(input[1..]) }
		`^` { return expand_caret(input[1..]) }
		else {}
	}

	if input.contains(hyphen_range_sep) {
		return expand_hyphen(input)
	}
	return expand_xrange(input)
}

fn expand_tilda(raw_version string) ?ComparatorSet {
	min_ver := coerce_version(raw_version) or { return none }
	// The ceiling carries no prerelease, so `~1.2.3-beta.2` stops below every
	// 1.3.0, and does not reach up to `1.3.0-alpha`.
	max_ver := if min_ver.minor == 0 && min_ver.patch == 0 {
		Version{min_ver.major + 1, 0, 0, '', ''}
	} else {
		Version{min_ver.major, min_ver.minor + 1, 0, '', ''}
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
			upper := Version{min_ver.major + 1, 0, 0, '', ''}
			return make_comparator_set_ge_lt(min_ver, upper)
		}
		2 {
			upper := Version{min_ver.major, min_ver.minor + 1, 0, '', ''}
			return make_comparator_set_ge_lt(min_ver, upper)
		}
		else {
			// every component is a real number, so this is an exact pin
			return ComparatorSet{[Comparator{min_ver, Operator.eq}]}
		}
	}
}

fn make_comparator_set_ge_lt(min Version, max Version) ComparatorSet {
	return ComparatorSet{[
		Comparator{min, Operator.ge},
		Comparator{max, Operator.lt},
	]}
}

fn make_comparator_set_ge_le(min Version, max Version) ComparatorSet {
	return ComparatorSet{[
		Comparator{min, Operator.ge},
		Comparator{max, Operator.le},
	]}
}
