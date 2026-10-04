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
	Case{'0.0.9', '^0.0', true, ''},
	Case{'0.9.9', '^0.0', false, ''},
	Case{'0.0.1', '^0.x', true, 'node-semver: ^0.x := >=0.0.0 <1.0.0-0'},
	Case{'0.9.9', '^0.x', true, ''},
	Case{'1.0.0', '^0.x', false, ''},
	Case{'0.0.4', '^0.0.3', false, 'node-semver: ^0.0.3 := >=0.0.3 <0.0.4-0'},
	Case{'0.0.9', '^0.0.3', false, ''},
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
	Case{'1.2.9', '1.2', true, 'node-semver: a partial version is an x-range'},
	Case{'1.3.0', '1.2', false, ''},
	Case{'1.2.0', '1.2', true, ''},
	Case{'1.9.9', '1', true, 'node-semver: 1 := 1.x.x := >=1.0.0 <2.0.0-0'},
	Case{'1.0.0', '1', true, ''},
	Case{'2.0.0', '1', false, ''},
	Case{'0.9.9', '0', true, 'node-semver: 0 := 0.x.x := >=0.0.0 <1.0.0-0'},
	Case{'1.0.0', '0', false, ''},
	Case{'0.0.4', '0.0', true, 'node-semver: 0.0 := 0.0.x := >=0.0.0 <0.1.0-0'},
	Case{'0.1.0', '0.0', false, ''},
	Case{'1.9.9', '1.x', true, ''},
	Case{'2.0.0', '1.x', false, ''},
	Case{'0.2.5', '0.x', true, ''},
	Case{'0.9.9', '0.x', true, ''},
	Case{'1.0.0', '0.x', false, 'node-semver: 0.x := >=0.0.0 <1.0.0-0'},
	Case{'0.1.9', '0.1.x', true, ''},
	Case{'0.2.0', '0.1.x', false, 'node-semver: 0.1.x := >=0.1.0 <0.2.0-0'},
	Case{'5.0.0', '0.1.x', false, ''},
	Case{'0.0.9', '0.0.x', true, ''},
	Case{'0.1.0', '0.0.x', false, ''},
	Case{'9.9.9', '*', true, ''},
	Case{'0.0.1', '*', true, 'a non-prerelease is always inside *'},
	// hyphen ranges
	Case{'2.3.4', '2.3.4 - 2.3.5', true, ''},
	Case{'2.3.5', '2.3.4 - 2.3.5', true, 'the upper end is inclusive'},
	Case{'2.3.6', '2.3.4 - 2.3.5', false, ''},
	Case{'2.3.3', '2.3.4 - 2.3.5', false, ''},
	Case{'2.3.4', '2.2 - 2.3', true, ''},
	Case{'2.4.0', '2.2 - 2.3', false, ''},
	Case{'2.9.9', '2.2 - 2', true, 'node-semver: 1.2.3 - 2 := >=1.2.3 <3.0.0-0'},
	Case{'3.0.0', '2.2 - 2', false, ''},
	Case{'2.9.9', '2 - 3', true, 'node-semver: 2 - 3 := >=2.0.0 <4.0.0-0'},
	Case{'3.0.0', '2 - 3', true, 'inside major 3, because the upper bound is major 4'},
	Case{'4.0.0', '2 - 3', false, ''},
	Case{'1.9.9', '1.2.3 - 2', true, ''},
	Case{'3.0.0', '1.2.3 - 2', false, ''},
	Case{'1.5.0', '1.0.0 - *', true, 'an open ended upper bound'},
	// disjunctions
	Case{'1.5.0', '^1.0.0 || ^3.0.0', true, ''},
	Case{'3.1.0', '^1.0.0 || ^3.0.0', true, ''},
	Case{'2.0.0', '^1.0.0 || ^3.0.0', false, ''},
	Case{'3.5.0', '^1 || ^3', true, ''},
	Case{'1.0.0', '^1||^3', true, 'the spaces around || are optional'},
	Case{'3.5.0', '^1||^3', true, ''},
	Case{'2.0.0', '^1||^3', false, ''},
	Case{'1.5.0', '>=1.0.0 <2.0.0 || >=3.0.0', true, 'arms of unequal length'},
	Case{'5.0.0', '^1.0.0 ||', true, 'node-semver: an empty arm is the empty range, *'},
	Case{'5.0.0', '|| ^1.0.0', true, ''},
	Case{'1.2.3', '||', true, ''},
	// primitive comparators
	Case{'1.5.0', '>=1.0.0 <2.0.0', true, ''},
	Case{'2.0.0', '>=1.0.0 <2.0.0', false, ''},
	Case{'0.9.0', '>=1.0.0 <2.0.0', false, ''},
	Case{'1.5.0', '>=1.0.0 <2.0.0 <=3.0.0', true, 'no limit on the comparator count'},
	Case{'2.5.0', '>=1.0.0 <2.0.0 <=3.0.0', false, ''},
	// the empty range is `*`
	Case{'1.2.3', '', true, 'node-semver: "" := * := >=0.0.0'},
	Case{'0.0.1', '', true, ''},
	Case{'1.2.3', '   ', true, 'whitespace only trims to the empty range'},
	Case{'1.2.3', ' 1.2.3 ', true, 'surrounding spaces are trimmed'},
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
	Case{'1.0.0-alpha', '>=1.0.0-alpha <1.0.0', false, 'node-semver: the >=1.0.0-alpha ' +
		'comparator admits prereleases of 1.0.0, and a prerelease sorts below its ' +
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
	Case{'1.0.0', '1.2.3 - ', false, 'the grammar needs a partial on both sides'},
	Case{'1.2.5', '1.2-beta', false, 'a prerelease needs all three components'},
	Case{'2.5.0', '1.2.3 - 2-beta', false, ''},
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

// Version.satisfies answers with a bool and has no way to report a range it could
// not read, so a malformed constraint and a genuine miss look identical to the
// caller. That still bites ranges node-semver accepts but this module cannot read,
// such as a space between an operator and its version (`>= 1.0.0`); the limitation
// is a property of the signature, so it is pinned here.
fn test_an_unreadable_range_is_reported_as_no_match() {
	// each of these is outside the grammar, and each reports the same way
	for bad in ['>=', '>=1.0.0 <', 'not-a-range', '^a', '1.2.3 - '] {
		assert satisfies('1.2.3', bad) == false, bad
	}
	// node-semver reads this as `>=1.0.0`, so true; here it is unreadable, so false
	assert satisfies('1.5.0', '>= 1.0.0') == false
	// and a well formed range beside them still answers
	assert satisfies('1.2.3', '>=1.0.0 <2.0.0')
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
