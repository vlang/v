// * Documentation: https://docs.npmjs.com/misc/semver
module semver

// * Structures.
// Structure representing version in semver format.
pub struct Version {
pub:
	major      int
	minor      int
	patch      int
	prerelease string
	metadata   string
}

// Enum representing type of version increment.
pub enum Increment {
	major
	minor
	patch
}

// * Constructor.
// from returns Version structure parsed from input string.
pub fn from(input string) ?Version {
	if input.len == 0 {
		return error('Empty input')
	}
	raw_version := parse(input)
	version := raw_version.validate() or {
		return error('Invalid version format for input "$input"')
	}
	return version
}

// build returns Version structure with given major, minor and patch versions.
pub fn build(major int, minor int, patch int) Version {
	// TODO Check if versions are greater than zero.
	return Version{major, minor, patch, '', ''}
}

// * Transformation.
// increment returns Version structure with incremented values.
pub fn (ver Version) increment(typ Increment) Version {
	return increment_version(ver, typ)
}

// * Comparison.
pub fn (ver Version) satisfies(input string) bool {
	return version_satisfies(ver, input)
}

pub fn (v1 Version) eq(v2 Version) bool {
	return compare_eq(v1, v2)
}

pub fn (v1 Version) gt(v2 Version) bool {
	return compare_gt(v1, v2)
}

pub fn (v1 Version) lt(v2 Version) bool {
	return compare_lt(v1, v2)
}

pub fn (v1 Version) ge(v2 Version) bool {
	return compare_ge(v1, v2)
}

pub fn (v1 Version) le(v2 Version) bool {
	return compare_le(v1, v2)
}

// * Utilites.
pub fn coerce(input string) ?Version {
	ver := coerce_version(input) or { return error('Invalid version for input "$input"') }
	return ver
}

pub fn is_valid(input string) bool {
	return is_version_valid(input)
}
