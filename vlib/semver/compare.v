module semver

// * Private functions.
[inline]
fn version_satisfies(ver Version, input string) bool {
	range := parse_range(input) or { return false }
	return range.satisfies(ver)
}

fn compare_eq(v1 Version, v2 Version) bool {
	return v1.major == v2.major &&
		v1.minor == v2.minor && v1.patch == v2.patch && v1.prerelease == v2.prerelease
}

fn compare_gt(v1 Version, v2 Version) bool {
	if v1.major < v2.major {
		return false
	}
	if v1.major > v2.major {
		return true
	}
	if v1.minor < v2.minor {
		return false
	}
	if v1.minor > v2.minor {
		return true
	}
	return v1.patch > v2.patch
}

fn compare_lt(v1 Version, v2 Version) bool {
	if v1.major > v2.major {
		return false
	}
	if v1.major < v2.major {
		return true
	}
	if v1.minor > v2.minor {
		return false
	}
	if v1.minor < v2.minor {
		return true
	}
	return v1.patch < v2.patch
}

fn compare_ge(v1 Version, v2 Version) bool {
	if compare_eq(v1, v2) {
		return true
	}
	return compare_gt(v1, v2)
}

fn compare_le(v1 Version, v2 Version) bool {
	if compare_eq(v1, v2) {
		return true
	}
	return compare_lt(v1, v2)
}
