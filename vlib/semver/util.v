module semver

// * Private functions.
[inline]
fn is_version_valid(input string) bool {
	raw_ver := parse(input)
	return raw_ver.is_valid()
}

[inline]
fn coerce_version(input string) ?Version {
	raw_ver := parse(input)
	ver := raw_ver.coerce() or { return error('Invalid version for input "${input}"') }
	return ver
}

[inline]
fn increment_version(ver Version, typ Increment) Version {
	mut major := ver.major
	mut minor := ver.minor
	mut patch := ver.patch
	match typ {
		.major {
			major++
			minor = 0
			patch = 0
		}
		.minor {
			minor++
			patch = 0
		}
		.patch {
			patch++
		}
	}
	return Version{major, minor, patch, ver.prerelease, ver.metadata}
}

fn is_valid_string(input string) bool {
	for c in input {
		if !(c.is_letter() || c.is_digit() || c == `.` || c == `-`) {
			return false
		}
	}
	return true
}

fn is_valid_number(input string) bool {
	for c in input {
		if !c.is_digit() {
			return false
		}
	}
	return true
}
