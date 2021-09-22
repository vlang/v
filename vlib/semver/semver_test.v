import semver

struct TestVersion {
	raw        string
	major      int
	minor      int
	patch      int
	prerelease string
	metadata   string
}

struct TestRange {
	raw_version       string
	range_satisfied   string
	range_unsatisfied string
}

struct TestCoerce {
	invalid string
	valid   string
}

const (
	versions_to_test         = [
		TestVersion{'1.2.4', 1, 2, 4, '', ''},
		TestVersion{'1.2.4-prerelease-1', 1, 2, 4, 'prerelease-1', ''},
		TestVersion{'1.2.4+20191231', 1, 2, 4, '', '20191231'},
		TestVersion{'1.2.4-prerelease-1+20191231', 1, 2, 4, 'prerelease-1', '20191231'},
		TestVersion{'1.2.4+20191231-prerelease-1', 1, 2, 4, '', '20191231-prerelease-1'},
	]
	ranges_to_test           = [
		TestRange{'1.1.0', '1.1.0', '1.1.1'},
		TestRange{'1.1.0', '=1.1.0', '=1.1.1'},
		TestRange{'1.1.0', '>=1.0.0', '<1.1.0'},
		TestRange{'1.1.0', '>=1.0.0 <=1.1.0', '>=1.0.0 <1.1.0'},
		TestRange{'2.3.1', '>=1.0.0 <=1.1.0 || >2.0.0 <2.3.4', '>=1.0.0 <1.1.0'},
		TestRange{'2.3.1', '>=1.0.0 <=1.1.0 || >2.0.0 <2.3.4', '>=1.0.0 <1.1.0 || >4.0.0 <5.0.0'},
		TestRange{'2.3.1', '~2.3.0', '~2.4.0'},
		TestRange{'3.0.0', '~3.0.0', '~4.0.0'},
		TestRange{'2.3.1', '^2.0.0', '^2.4.0'},
		TestRange{'0.3.1', '^0.3.0', '^2.4.0'},
		TestRange{'0.0.4', '^0.0.1', '^0.1.0'},
		TestRange{'2.3.4', '^0.0.1 || ^2.3.0', '^3.1.0 || ^4.2.0'},
		TestRange{'2.3.4', '>2 || <3', '>3 || >4'},
		TestRange{'2.3.4', '2.3.4 - 2.3.5', '2.5.1 - 2.8.3'},
		TestRange{'2.3.4', '2.2 - 2.3', '2.4 - 2.8'},
		TestRange{'2.3.4', '2.3.x', '2.4.x'},
		TestRange{'2.3.4', '2.x', '3.x'},
		TestRange{'2.3.4', '*', '3.x'},
	]
	coerce_to_test           = [
		TestCoerce{'1.2.0.4', '1.2.0'},
		TestCoerce{'1.2.0', '1.2.0'},
		TestCoerce{'1.2', '1.2.0'},
		TestCoerce{'1', '1.0.0'},
		TestCoerce{'1-alpha', '1.0.0-alpha'},
		TestCoerce{'1+meta', '1.0.0+meta'},
		TestCoerce{'1-alpha+meta', '1.0.0-alpha+meta'},
	]
	invalid_versions_to_test = [
		'a.b.c',
		'1.2',
		'1.2.x',
		'1.2.3.4',
		'1.2.3-alpha@',
		'1.2.3+meta%',
	]
	invalid_ranges_to_test   = [
		'^a',
		'~b',
		'a - c',
		'>a',
		'a',
		'a.x',
	]
)

fn test_from() {
	for item in versions_to_test {
		ver := semver.from(item.raw) or {
			assert false
			return
		}
		assert ver.major == item.major
		assert ver.minor == item.minor
		assert ver.patch == item.patch
		assert ver.metadata == item.metadata
		assert ver.prerelease == item.prerelease
	}
	for ver in invalid_versions_to_test {
		semver.from(ver) or {
			assert true
			continue
		}
		assert false
	}
}

fn test_increment() {
	version1 := semver.build(1, 2, 3)
	version1_inc := version1.increment(.major)
	assert version1_inc.major == 2
	assert version1_inc.minor == 0
	assert version1_inc.patch == 0
	version2_inc := version1.increment(.minor)
	assert version2_inc.major == 1
	assert version2_inc.minor == 3
	assert version2_inc.patch == 0
	version3_inc := version1.increment(.patch)
	assert version3_inc.major == 1
	assert version3_inc.minor == 2
	assert version3_inc.patch == 4
}

fn test_compare() {
	first := semver.build(1, 0, 0)
	patch := semver.build(1, 0, 1)
	minor := semver.build(1, 2, 3)
	major := semver.build(2, 0, 0)
	assert first.le(first)
	assert first.ge(first)
	assert !first.lt(first)
	assert !first.gt(first)
	assert patch.ge(first)
	assert first.le(patch)
	assert !first.ge(patch)
	assert !patch.le(first)
	assert patch.gt(first)
	assert first.lt(patch)
	assert !first.gt(patch)
	assert !patch.lt(first)
	assert minor.gt(patch)
	assert patch.lt(minor)
	assert !patch.gt(minor)
	assert !minor.lt(patch)
	assert major.gt(minor)
	assert minor.lt(major)
	assert !minor.gt(major)
	assert !major.lt(minor)
}

fn test_satisfies() {
	for item in ranges_to_test {
		ver := semver.from(item.raw_version) or {
			assert false
			return
		}
		assert ver.satisfies(item.range_satisfied)
		assert !ver.satisfies(item.range_unsatisfied)
	}
}

fn test_satisfies_invalid() {
	ver := semver.from('1.0.0') or {
		assert false
		return
	}
	for item in invalid_ranges_to_test {
		assert ver.satisfies(item) == false
	}
}

fn test_coerce() {
	for item in coerce_to_test {
		valid := semver.from(item.valid) or {
			assert false
			return
		}
		fixed := semver.coerce(item.invalid) or {
			assert false
			return
		}
		assert fixed.eq(valid)
	}
}

fn test_coerce_invalid() {
	semver.coerce('a') or {
		assert true
		return
	}
	assert false
}

fn test_is_valid() {
	for item in versions_to_test {
		assert semver.is_valid(item.raw)
	}
	for item in invalid_versions_to_test {
		assert semver.is_valid(item) == false
	}
}
