import semver

fn test_tilde_with_an_explicit_zero_minor() {
	for major in 0 .. 3 {
		for suffix in ['.0', '.0.0', '.0.0-beta.1'] {
			range := '~${major}${suffix}'
			for version in ['${major}.0.0', '${major}.0.9'] {
				ver := semver.from(version) or { panic(err) }
				assert ver.satisfies(range), '${version} ${range}'
			}
			ceiling := semver.from('${major}.1.0') or { panic(err) }
			assert !ceiling.satisfies(range), range
		}
		bare_major := semver.from('${major}.9.9') or { panic(err) }
		assert bare_major.satisfies('~${major}')
		prerelease := semver.from('${major}.0.0-beta.2') or { panic(err) }
		assert prerelease.satisfies('~${major}.0.0-beta.1')
	}
}

struct ComparatorRangeCase {
	version string
	range   string
	match   bool
}

fn test_comparators_with_wildcard_or_missing_components() {
	// Boundaries from node-semver's comparator x-range expansion grammar.
	cases := [
		ComparatorRangeCase{'1.0.0', '>=1.x', true},
		ComparatorRangeCase{'99.0.0', '>=1.x', true},
		ComparatorRangeCase{'0.9.9', '>=1.x', false},
		ComparatorRangeCase{'0.0.0', '>=0.x', true},
		ComparatorRangeCase{'0.0.1', '<=0.x', true},
		ComparatorRangeCase{'1.0.0', '<=0.x', false},
		ComparatorRangeCase{'1.9.9', '>1.x', false},
		ComparatorRangeCase{'2.0.0', '>1.x', true},
		ComparatorRangeCase{'0.9.9', '<1.x', true},
		ComparatorRangeCase{'1.0.0', '<1.x', false},
		ComparatorRangeCase{'1.2.9', '=1.2.x', true},
		ComparatorRangeCase{'1.3.0', '=1.2.x', false},
		ComparatorRangeCase{'1.3.0', '>1.2.x', true},
		ComparatorRangeCase{'1.2.9', '>1.2.x', false},
		ComparatorRangeCase{'1.2.9', '<=1.2.x', true},
		ComparatorRangeCase{'1.3.0', '<=1.2.x', false},
		ComparatorRangeCase{'1.3.0', '>1.2', true},
		ComparatorRangeCase{'1.9.9', '<=1', true},
		ComparatorRangeCase{'1.0.0', '=1', true},
		ComparatorRangeCase{'2.0.0', '=1', false},
		ComparatorRangeCase{'1.5.0', '>=1.x <2.0.0', true},
		ComparatorRangeCase{'2.0.0', '>=1.x <2.0.0', false},
		ComparatorRangeCase{'2.0.0-beta.1', '>=2.0.0-beta.1 <=1.x', false},
		ComparatorRangeCase{'1.2.0-beta.1', '>=1.2.0-beta.1 <1.2.x', false},
		ComparatorRangeCase{'1.2.0-beta.1', '>=1.2.x', false},
		ComparatorRangeCase{'2.0.0-beta.1', '=1.x >=2.0.0-beta.1', false},
		ComparatorRangeCase{'1.1.0-beta.1', '~1.0.0 >=1.1.0-beta.1', false},
	]
	for c in cases {
		ver := semver.from(c.version) or { panic(err) }
		assert ver.satisfies(c.range) == c.match, '${c.version} ${c.range}'
	}
	for wildcard in ['x', 'X', '*'] {
		for version in ['0.0.0', '1.2.3', '99.0.0'] {
			ver := semver.from(version) or { panic(err) }
			assert ver.satisfies('>=${wildcard}')
			assert ver.satisfies('<=${wildcard}')
			assert ver.satisfies('=${wildcard}')
			assert !ver.satisfies('>${wildcard}')
			assert !ver.satisfies('<${wildcard}')
		}
	}
}
