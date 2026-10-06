module main

// Override is one `dependency_overrides` entry: force `name` to `version`
// regardless of what was asked for. This is the root project's last word, and it
// deliberately does not check whether other constraints allow the version — that
// is what an override is for.
pub struct Override {
pub:
	name    string
	version string
}

// parse_overrides reads the `dependency_overrides` entries of a manifest. Each
// entry is `name: version`. Malformed or duplicate entries are rejected.
pub fn parse_overrides(raw []string) ![]Override {
	mut result := []Override{}
	mut seen := map[string]bool{}
	for entry in raw {
		parts := entry.split(':')
		if parts.len != 2 {
			return error('invalid dependency override `${entry}`; expected `name: version`')
		}
		name := parts[0].trim_space()
		version := parts[1].trim_space()
		if name == '' || version == '' {
			return error('invalid dependency override `${entry}`; name and version must be nonempty')
		}
		if name.contains_any('/\\@') || name in ['.', '..'] {
			return error('invalid dependency override name `${name}`')
		}
		if name in seen {
			return error('duplicate dependency override for `${name}`')
		}
		seen[name] = true
		result << Override{
			name:    name
			version: version
		}
	}
	return result
}

// overridden_request applies a matching override before its sources are resolved.
// The effective request is also the lockfile request, so changing an override
// invalidates a previous lock entry.
pub fn overridden_request(request string, names []string, overrides []Override) string {
	for name in names {
		for o in overrides {
			if o.name == name {
				return lockfile_module_key(request) + at_version(o.version)
			}
		}
	}
	return request
}
