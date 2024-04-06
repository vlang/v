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

fn compare_gt(v1 Version, v2 Version) bool {
	return match true {
		v1.major < v2.major { false }
		v1.major > v2.major { true }
		v1.minor < v2.minor { false }
		v1.minor > v2.minor { true }
		else { v1.patch > v2.patch }
	}
}

fn compare_lt(v1 Version, v2 Version) bool {
	return match true {
		v1.major > v2.major { false }
		v1.major < v2.major { true }
		v1.minor > v2.minor { false }
		v1.minor < v2.minor { true }
		else { v1.patch < v2.patch }
	}
}

fn compare_ge(v1 Version, v2 Version) bool {
	return if compare_eq(v1, v2) { true } else { compare_gt(v1, v2) }
}

fn compare_le(v1 Version, v2 Version) bool {
	return if compare_eq(v1, v2) { true } else { compare_lt(v1, v2) }
}
