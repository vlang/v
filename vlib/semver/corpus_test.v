module main

// A corpus for `vlib/semver`, measured against the ranges grammar in
// https://github.com/npm/node-semver (the canonical implementation of
// https://semver.org). `semver_test.v` covers the happy path; this file covers the
// shape of the grammar and records where this implementation parts ways with it.
//
// Every expectation here was produced by running the case, not by reading the
// source, and every divergence is annotated with node-semver's documented answer
// so that fixing one is a deliberate act rather than an accident.
//
// Nothing in this file should be read as an endorsement of the divergent answers.
// They are pinned so that they are visible, reviewable, and hard to change
// silently.

import semver

struct Case {
	ver       string
	rng       string
	satisfies bool
	note      string
}

// where this implementation agrees with node-semver. These are the behaviours a
// resolver may rely on.
const conforming = [
	// caret, the >= 1.x family
	Case{'1.2.3', '^1.2.3', true, ''},
	Case{'1.9.9', '^1.2.3', true, 'minor growth is inside a caret'},
	Case{'1.2.2', '^1.2.3', false, 'below the floor'},
	Case{'2.0.0', '^1.2.3', false, 'major excluded'},
	Case{'1.0.0', '^1.0', true, ''},
	Case{'1.9.0', '^1.0', true, ''},
	Case{'2.0.0', '^1.0', false, ''},
	Case{'1.5.0', '^1', true, ''},
	Case{'2.0.0', '^1', false, ''},
	// caret, the 0.x family
	Case{'0.2.3', '^0.2.3', true, ''},
	Case{'0.2.9', '^0.2.3', true, ''},
	Case{'0.3.0', '^0.2.3', false, 'minor is the breaking axis under 0.x'},
	Case{'0.2.2', '^0.2.3', false, ''},
	Case{'0.0.3', '^0.0.3', true, ''},
	Case{'0.1.0', '^0.0.3', false, ''},
	Case{'0.0.1', '^0.0.1', true, ''},
	Case{'0.0.1', '^0.0.x', true, 'node-semver: ^0.0.x := >=0.0.0 <0.1.0-0'},
	Case{'0.0.9', '^0.0.x', true, ''},
	Case{'0.1.0', '^0.0.x', false, ''},
	Case{'0.0.1', '^0.0', true, 'node-semver: ^0.0 := >=0.0.0 <0.1.0-0'},
	// tilde
	Case{'1.2.3', '~1.2.3', true, ''},
	Case{'1.2.9', '~1.2.3', true, ''},
	Case{'1.3.0', '~1.2.3', false, ''},
	Case{'1.2.0', '~1.2', true, ''},
	Case{'1.3.0', '~1.2', false, ''},
	Case{'1.9.9', '~1', true, ''},
	Case{'2.0.0', '~1', false, ''},
	Case{'0.2.9', '~0.2.3', true, ''},
	Case{'0.3.0', '~0.2.3', false, ''},
	Case{'0.9.9', '~0', true, 'node-semver: ~0 := >=0.0.0 <1.0.0-0'},
	Case{'1.0.0', '~0', false, ''},
	// x-ranges
	Case{'1.2.3', '1.2.x', true, ''},
	Case{'1.3.0', '1.2.x', false, ''},
	Case{'1.3.0', '1.2', false, 'right answer, but reached as an exact pin'},
	Case{'1.9.9', '1.x', true, ''},
	Case{'2.0.0', '1.x', false, ''},
	Case{'0.2.5', '0.x', true, ''},
	Case{'0.9.9', '0.x', true, ''},
	Case{'9.9.9', '*', true, ''},
	Case{'0.0.1', '*', true, 'a non-prerelease is always inside *'},
	// hyphen ranges
	Case{'2.3.4', '2.3.4 - 2.3.5', true, ''},
	Case{'2.3.5', '2.3.4 - 2.3.5', true, 'the upper end is inclusive'},
	Case{'2.3.6', '2.3.4 - 2.3.5', false, ''},
	Case{'2.3.3', '2.3.4 - 2.3.5', false, ''},
	Case{'2.3.4', '2.2 - 2.3', true, ''},
	Case{'2.4.0', '2.2 - 2.3', false, ''},
	Case{'3.0.0', '2.2 - 2', false, 'right answer, but the expansion failed outright'},
	// disjunctions
	Case{'1.5.0', '^1.0.0 || ^3.0.0', true, ''},
	Case{'3.1.0', '^1.0.0 || ^3.0.0', true, ''},
	Case{'2.0.0', '^1.0.0 || ^3.0.0', false, ''},
	Case{'3.5.0', '^1 || ^3', true, ''},
	// primitive comparators
	Case{'1.5.0', '>=1.0.0 <2.0.0', true, ''},
	Case{'2.0.0', '>=1.0.0 <2.0.0', false, ''},
	Case{'0.9.0', '>=1.0.0 <2.0.0', false, ''},
	Case{'1.2.3', '>=1.2.3', true, ''},
	Case{'1.2.3', '<=1.2.3', true, ''},
	Case{'1.2.3', '>1.2.3', false, ''},
	Case{'1.2.3', '<1.2.3', false, ''},
	Case{'1.2.3', '=1.2.3', true, ''},
	Case{'1.2.3', '>=1.2.4', false, ''},
	Case{'1.2.3', '1.2.3', true, 'a bare version is an exact pin'},
	Case{'1.2.4', '1.2.3', false, ''},
	// build metadata takes no part in precedence
	Case{'1.0.0+build.1', '1.0.0', true, ''},
	Case{'1.0.0', '>=1.0.0', true, ''},
	// a prerelease is admitted when the range names one on the same tuple
	Case{'1.0.0-alpha', '^1.0.0-alpha', true, ''},
	Case{'1.0.0-alpha', '>=1.0.0-alpha <2.0.0', true, ''},
	Case{'1.0.0-alpha', '>=1.0.0-alpha', true, ''},
	Case{'2.0.0-beta.4', '^1.2.3-beta.2', false, 'a different tuple, so excluded'},
	Case{'1.2.3-beta.4', '^1.2.3-beta.2', true, ''},
	Case{'1.0.0', '<1.0.0-alpha', false, ''},
	// shapes taken from real usage in the V ecosystem
	Case{'0.1.47', '^0.1.47', true, ''},
	Case{'0.1.47', '^0.1.0', true, ''},
	Case{'0.5.2', '>=0.4.0 <0.6.0', true, ''},
	Case{'0.5.2', '~0.2', false, ''},
	Case{'0.4.3', '~0.4', true, ''},
]

// where this implementation parts ways with node-semver. `satisfies` is what this
// module does today; the note says what node-semver documents, and why.
//
// These are pinned rather than asserted as correct. A resolver built on top of
// this has to know about every row here.
const divergent = [
	Case{'0.0.4', '^0.0.3', true, 'node-semver: ^0.0.3 := >=0.0.3 <0.0.4-0, so false. ' +
		'range.v expand_caret always increments the minor when the major is 0, ' +
		'so ^0.0.3 admits the whole 0.0.x series.'},
	Case{'0.9.9', '^0.x', false, 'node-semver: ^0.x := >=0.0.0 <1.0.0-0, so true. ' +
		'Same cause: the ceiling lands on 0.1.0 instead of 1.0.0.'},
	Case{'0.9.9', '^0.0', false, 'node-semver: ^0.0 := >=0.0.0 <0.1.0-0, so true. ' +
		'The ceiling lands on 0.0.0.'},
	Case{'1.2.9', '1.2', false, 'node-semver: a partial version is an x-range, ' +
		'so 1.2 := 1.2.x := >=1.2.0 <1.3.0-0 and the answer is true. ' +
		'range.v can_expand only looks for an explicit x, X or *, so a bare 1.2 ' +
		'is parsed as the exact pin =1.2.0.'},
	Case{'1.9.9', '1', false, 'node-semver: 1 := 1.x.x := >=1.0.0 <2.0.0-0, so true. ' +
		'Same cause: 1 is read as =1.0.0.'},
	Case{'0.9.9', '0', false, 'node-semver: 0 := 0.x.x := >=0.0.0 <1.0.0-0, so true. ' +
		'Same cause.'},
	Case{'1.0.0', '0.x', true, 'node-semver: 0.x := >=0.0.0 <1.0.0-0, so false. ' +
		'range.v expand_xrange returns a bare >=0.0.0 with no ceiling whenever ' +
		'the major is 0, so this is unbounded.'},
	Case{'2.9.9', '2.2 - 2', false, 'node-semver: 1.2.3 - 2 := >=1.2.3 <3.0.0-0, so true. ' +
		'A major-only upper bound hits is_missing(ver_major) and expand_hyphen ' +
		'returns none, which makes the whole range unsatisfiable.'},
	Case{'2.9.9', '2 - 3', false, 'node-semver: 2 - 3 := >=2.0.0 <4.0.0-0, so true. ' +
		'Same cause.'},
	Case{'1.9.9', '1.2.3 - 2', false, 'node-semver: >=1.2.3 <3.0.0-0, so true. Same cause.'},
	Case{'1.0.0-alpha', '<1.0.0', false, 'node-semver: a prerelease sorts below its ' +
		'release, so true. compare.v compare_lt never looks at the prerelease; it ' +
		'falls through to a patch comparison of 0 against 0.'},
	Case{'1.0.0-beta', '>1.0.0-alpha', false, 'node-semver: alpha sorts below beta, so ' +
		'true. Same cause: prerelease identifiers are never compared.'},
	Case{'1.0.0-alpha.2', '>1.0.0-alpha.1', false, 'node-semver: numeric prerelease ' +
		'identifiers compare numerically, so true. Same cause.'},
	Case{'1.0.0-alpha', '^1.0.0', true, 'node-semver excludes a prerelease unless some ' +
		'comparator in the set carries one on the same [major, minor, patch], so false. ' +
		'There is no prerelease admission check at all.'},
	Case{'1.0.0-alpha', '>=0.9.0', true, 'node-semver: false, same rule.'},
	Case{'1.0.0-alpha', '*', true, 'node-semver: * := >=0.0.0 and admits non-prereleases ' +
		'only, so false. Same missing check.'},
	Case{'1.2.4-beta.2', '^1.2.3-beta.2', true, 'node-semver: only prereleases of the ' +
		'1.2.3 tuple are admitted, so false. This module admits any prerelease once ' +
		'the range mentions one anywhere.'},
	Case{'1.2.3', '', false, 'node-semver: "" := * := >=0.0.0, so true. An empty range ' +
		'is a parse failure here, which is indistinguishable from "no match".'},
	Case{'1.0.0', '^1||^3', false, 'node-semver: logical-or allows the surrounding ' +
		'spaces to be absent, so true. range.v splits on the literal " || " only.'},
	Case{'1.5.0', '>=1.0.0 <2.0.0 <=3.0.0', false, 'node-semver intersects comparator ' +
		'sets without a limit on how many, so true. range.v parse_comparator_set ' +
		'rejects anything with more than two comparators.'},
]

// ranges outside the grammar, which must not match anything.
const rejected = [
	Case{'1.0.0', '^a', false, 'a non-numeric version'},
	Case{'1.0.0', '~b', false, ''},
	Case{'1.0.0', 'a - c', false, ''},
	Case{'1.0.0', '>a', false, ''},
	Case{'1.0.0', 'a', false, ''},
	Case{'1.0.0', 'a.x', false, ''},
	Case{'1.0.0', 'not-a-range', false, ''},
	Case{'1.0.0', '   ', false, 'whitespace only'},
	Case{'1.0.0', '1.2.3 - ', false, 'the grammar needs a partial on both sides'},
	Case{'1.2.3', '1.2.3', true, 'sanity: a valid range in this table still matches'},
]

fn satisfies(ver string, rng string) bool {
	return semver.from(ver) or { panic('cannot parse version "${ver}"') }.satisfies(rng)
}

fn test_corpus_agrees_with_the_ranges_grammar() {
	for c in conforming {
		actual := satisfies(c.ver, c.rng)
		assert actual == c.satisfies, '${c.ver} vs "${c.rng}": want ${c.satisfies}, got ${actual}. ${c.note}'
	}
}

fn test_corpus_records_known_divergences() {
	for c in divergent {
		actual := satisfies(c.ver, c.rng)
		assert actual == c.satisfies, '${c.ver} against "${c.rng}": the answer recorded here is ' +
			'${c.satisfies} but it is now ${actual}. If this changed, someone fixed ' +
			'range.v or compare.v, and the note below is now out of date: ${c.note}'
	}
}

fn test_corpus_rejects_out_of_grammar_ranges() {
	for c in rejected {
		actual := satisfies(c.ver, c.rng)
		assert actual == c.satisfies, '${c.ver} vs "${c.rng}": want ${c.satisfies}, got ${actual}. ${c.note}'
	}
}

// A range this module cannot parse is reported as "does not satisfy", the same as a
// genuine miss. A caller cannot tell the two apart, so a typo in a constraint is
// indistinguishable from an unsatisfiable one. Pinned because it is the failure
// mode most likely to surprise a resolver.
fn test_unparseable_range_is_indistinguishable_from_no_match() {
	// The second range is a perfectly good range that node-semver accepts, and that
	// reads like a normal constraint rather than a typo. Here it silently becomes
	// "no version satisfies this", the same answer as a genuine miss.
	typo := satisfies('1.2.3', '>=1.0.0 <2.0.0')
	broken := satisfies('1.2.3', '>=1.0.0 <2.0.0 <=3.0.0')
	assert typo == true
	assert broken == false
}

// The ordering operators do not order prereleases at all: two versions that differ
// only in their prerelease tag come out as neither `<` nor `>`, and yet both `<=`
// and `>=` hold, because `Version` overloads only `==` and `<` and the compiler
// derives the rest. That is an inconsistent relation, and it is why the prerelease
// rows in the tables above cannot simply be fixed in the range layer.
fn test_prereleases_have_no_ordering() {
	alpha := semver.from('1.0.0-alpha') or { panic('bad alpha') }
	release := semver.from('1.0.0') or { panic('bad release') }
	assert !(alpha < release)
	assert !(release < alpha)
	assert alpha <= release
	assert release <= alpha
	assert alpha >= release
	assert release >= alpha
	assert alpha != release
}
