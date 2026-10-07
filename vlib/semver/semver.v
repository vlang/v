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
	return 'Invalid version format for input "${err.input}"'
}

// * Constructor.
// from returns a `Version` structure parsed from `input` `string`.
pub fn from(input string) !Version {
	if input.len == 0 {
		return &EmptyInputError{}
	}
	raw_version := parse(input)
	return raw_version.validate() or {
		return &InvalidVersionFormatError{
			input: input
		}
	}
}

// build returns a `Version` structure with given `major`, `minor` and `patch` versions.
pub fn build(major int, minor int, patch int) Version {
	// TODO: Check if versions are greater than zero.
	return Version{major, minor, patch, '', ''}
}

// increment returns a `Version` structure with incremented values.
pub fn (ver Version) increment(typ Increment) Version {
	return increment_version(ver, typ)
}

// satisfies returns `true` if the `input` expression can be validated to `true` when run against this `Version`.
// Example: assert semver.build(1,0,0).satisfies('<=2.0.0')
// Example: assert semver.build(1,0,0).satisfies('>=2.0.0') == false
pub fn (ver Version) satisfies(input string) bool {
	return version_satisfies(ver, input)
}

// satisfies_or_error is satisfies, but it reports an unparseable range as an error
// rather than as a miss, so a caller can tell a broken constraint from a genuine one.
// satisfies keeps returning false for that case, because changing it would break
// every caller at once and the ones that need the distinction can move over.
pub fn (ver Version) satisfies_or_error(input string) !bool {
	return version_satisfies_or_error(ver, input)
}

// is_valid_range reports whether input parses as a range at all. It exists so a
// caller can reject a broken constraint before it is used, rather than discovering
// the problem as a result that looks like a genuine miss.
pub fn is_valid_range(input string) bool {
	_ = parse_range(input) or { return false }
	return true
}

// == checks if `v1` is equal to `v2`.
pub fn (v1 Version) == (v2 Version) bool {
	return compare_eq(v1, v2)
}

// < checks if `v1` is less than `v2`.
pub fn (v1 Version) < (v2 Version) bool {
	return compare_lt(v1, v2)
}

// str returns the `string` representation of the `Version`.
pub fn (ver Version) str() string {
	common_string := '${ver.major}.${ver.minor}.${ver.patch}'

	prerelease_string := if ver.prerelease.len > 0 { '-${ver.prerelease}' } else { '' }
	metadata_string := if ver.metadata.len > 0 { '+${ver.metadata}' } else { '' }

	return '${common_string}${prerelease_string}${metadata_string}'
}

// * Utilities.
// coerce converts the `input` version to a `Version` struct.
// coerce will strip any contents *after* the parsed version string:
/*
Example:
import semver
v := semver.coerce('1.3-RC1-b2') or { semver.Version{} }
assert v.satisfies('>1.0 <2.0') == true // 1.3.0
*/
pub fn coerce(input string) !Version {
	return coerce_version(input)
}

// is_valid returns `true` if the `input` `string` can be converted to
// a  (semantic) `Version` struct.
pub fn is_valid(input string) bool {
	return is_version_valid(input)
}
