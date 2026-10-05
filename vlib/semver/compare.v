module semver

// * Private functions.
@[inline]
fn version_satisfies(ver Version, input string) bool {
	range := parse_range(input) or { return false }
	return range.satisfies(ver)
}

fn compare_eq(v1 Version, v2 Version) bool {
	return v1.major == v2.major && v1.minor == v2.minor && v1.patch == v2.patch
		&& v1.prerelease == v2.prerelease
}

fn compare_lt(v1 Version, v2 Version) bool {
	return match true {
		v1.major > v2.major { false }
		v1.major < v2.major { true }
		v1.minor > v2.minor { false }
		v1.minor < v2.minor { true }
		v1.patch > v2.patch { false }
		v1.patch < v2.patch { true }
		else { compare_prerelease(v1.prerelease, v2.prerelease) < 0 }
	}
}

// compare_prerelease orders two prerelease tags field by field and answers -1,
// 0 or 1. A purely numeric identifier sorts below an alphanumeric one, and a
// version carrying a prerelease sorts below the same version without one, so
// `1.0.0-alpha < 1.0.0` and `2.0.0-0 < 2.0.0-beta`.
fn compare_prerelease(pre1 string, pre2 string) int {
	if pre1.len == 0 {
		return if pre2.len == 0 { 0 } else { 1 }
	}
	if pre2.len == 0 {
		return -1
	}
	ids1 := pre1.split('.')
	ids2 := pre2.split('.')
	for i in 0 .. ids1.len {
		// All the fields they share are equal, and one tag has more of them.
		if i >= ids2.len {
			return 1
		}
		if ids1[i] == ids2[i] {
			continue
		}
		c := compare_prerelease_identifier(ids1[i], ids2[i])
		if c != 0 {
			return c
		}
	}
	return if ids2.len > ids1.len { -1 } else { 0 }
}

// compare_prerelease_identifier orders one field of a prerelease tag. Numeric
// fields compare as numbers rather than as text, so `alpha.10` outranks
// `alpha.9`. They are compared by their digits rather than converted, since a
// field such as a timestamp does not fit in an `int`.
fn compare_prerelease_identifier(id1 string, id2 string) int {
	numeric1 := is_valid_number(id1)
	numeric2 := is_valid_number(id2)
	if numeric1 && numeric2 {
		num1 := id1.trim_left('0')
		num2 := id2.trim_left('0')
		if num1.len != num2.len {
			return if num1.len < num2.len { -1 } else { 1 }
		}
		return if num1 < num2 {
			-1
		} else if num1 > num2 {
			1
		} else {
			0
		}
	}
	if numeric1 != numeric2 {
		return if numeric1 { -1 } else { 1 }
	}
	return if id1 < id2 {
		-1
	} else if id1 > id2 {
		1
	} else {
		0
	}
}
