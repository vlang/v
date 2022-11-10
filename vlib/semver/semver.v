// * Documentation: https://docs.npmjs.com/misc/semver
module semver

// * Structures.
// `Version` represents a semantic version in semver format.
pub struct Version {
pub:
	major      int
	minor      int
	patch      int
	prerelease string
	metadata   string
}

// Increment represents the different types of version increments.
pub enum Increment {
	major
	minor
	patch
}

struct EmptyInputError {
	Error
}

pub fn (err EmptyInputError) msg() string {
	return 'Empty input'
}

struct InvalidVersionFormatError {
	Error
	input string
}

pub fn (err InvalidVersionFormatError) msg() string {
	return 'Invalid version format for input "$err.input"'
}

// * Constructor.
// from returns a `Version` structure parsed from `input` `string`.
pub fn from(input string) ?Version {
	if input.len == 0 {
		return &EmptyInputError{}
	}
	raw_version := parse(input)
	version := raw_version.validate() or { return &InvalidVersionFormatError{
		input: input
	} }
	return version
}

// build returns a `Version` structure with given `major`, `minor` and `patch` versions.
pub fn build(major int, minor int, patch int) Version {
	// TODO Check if versions are greater than zero.
	return Version{major, minor, patch, '', ''}
}

// * Transformation.
// increment returns a `Version` structure with incremented values.
pub fn (ver Version) increment(typ Increment) Version {
	return increment_version(ver, typ)
}

// * Comparison.
// satisfies returns `true` if the `input` expression can be validated to `true`
// when run against this `Version`.
// Example: assert semver.build(1,0,0).satisfies('<=2.0.0') == true
// Example: assert semver.build(1,0,0).satisfies('>=2.0.0') == false
pub fn (ver Version) satisfies(input string) bool {
	return version_satisfies(ver, input)
}

// eq returns `true` if `v1` is equal to `v2`.
pub fn (v1 Version) eq(v2 Version) bool {
	return compare_eq(v1, v2)
}

// gt returns `true` if `v1` is greater than `v2`.
pub fn (v1 Version) gt(v2 Version) bool {
	return compare_gt(v1, v2)
}

// lt returns `true` if `v1` is less than `v2`.
pub fn (v1 Version) lt(v2 Version) bool {
	return compare_lt(v1, v2)
}

// ge returns `true` if `v1` is greater than or equal to `v2`.
pub fn (v1 Version) ge(v2 Version) bool {
	return compare_ge(v1, v2)
}

// le returns `true` if `v1` is less than or equal to `v2`.
pub fn (v1 Version) le(v2 Version) bool {
	return compare_le(v1, v2)
}

// str returns the `string` representation of the `Version`.
pub fn (ver Version) str() string {
	common_string := '${ver.major}.${ver.minor}.$ver.patch'

	prerelease_string := if ver.prerelease.len > 0 { '-$ver.prerelease' } else { '' }
	metadata_string := if ver.metadata.len > 0 { '+$ver.metadata' } else { '' }

	return '$common_string$prerelease_string$metadata_string'
}

// * Utilites.
// coerce converts the `input` version to a `Version` struct.
// coerce will strip any contents *after* the parsed version string:
/*
Example:
import semver
v := semver.coerce('1.3-RC1-b2') or { semver.Version{} }
assert v.satisfies('>1.0 <2.0') == true // 1.3.0
*/
pub fn coerce(input string) ?Version {
	return coerce_version(input)
}

// is_valid returns `true` if the `input` `string` can be converted to
// a  (semantic) `Version` struct.
pub fn is_valid(input string) bool {
	return is_version_valid(input)
}
