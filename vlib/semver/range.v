module semver

// * Private functions.
const comparator_sep = ' '
const comparator_set_sep = ' || '
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
	return true
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
	raw_comparator_sets := input.split(comparator_set_sep)
	mut comparator_sets := []ComparatorSet{}
	for raw_comp_set in raw_comparator_sets {
		if can_expand(raw_comp_set) {
			s := expand_comparator_set(raw_comp_set) or { return err }
			comparator_sets << s
		} else {
			s := parse_comparator_set(raw_comp_set) or { return err }
			comparator_sets << s
		}
	}
	return Range{comparator_sets}
}

fn parse_comparator_set(input string) !ComparatorSet {
	raw_comparators := input.split(comparator_sep)
	if raw_comparators.len > 2 {
		return &InvalidComparatorFormatError{
			msg: 'Invalid format of comparator set for input "${input}"'
		}
	}
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

fn can_expand(input string) bool {
	return input[0] == `~` || input[0] == `^` || input.contains(hyphen_range_sep)
		|| input.index_any(x_range_symbols) > -1
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
	max_ver := if min_ver.minor == 0 && min_ver.patch == 0 {
		min_ver.increment(.major)
	} else {
		min_ver.increment(.minor)
	}
	return make_comparator_set_ge_lt(min_ver, max_ver)
}

fn expand_caret(raw_version string) ?ComparatorSet {
	min_ver := coerce_version(raw_version) or { return none }
	max_ver := if min_ver.major == 0 {
		min_ver.increment(.minor)
	} else {
		min_ver.increment(.major)
	}
	return make_comparator_set_ge_lt(min_ver, max_ver)
}

fn expand_hyphen(raw_range string) ?ComparatorSet {
	raw_versions := raw_range.split(hyphen_range_sep)
	if raw_versions.len != 2 {
		return none
	}
	min_ver := coerce_version(raw_versions[0]) or { return none }
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

fn expand_xrange(raw_range string) ?ComparatorSet {
	min_ver := parse_xrange(raw_range) or { return none }
	if min_ver.major == 0 {
		return ComparatorSet{[Comparator{min_ver, Operator.ge}]}
	}
	max_ver := if min_ver.minor == 0 {
		min_ver.increment(.major)
	} else {
		min_ver.increment(.minor)
	}
	return make_comparator_set_ge_lt(min_ver, max_ver)
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
