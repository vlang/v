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
// entry is `name: version`, pnpm-style. An entry that is not exactly that shape is
// skipped rather than guessed at, because a typo in an override should not silently
// install something else.
pub fn parse_overrides(raw []string) []Override {
	mut result := []Override{}
	for entry in raw {
		parts := entry.split(':')
		if parts.len != 2 {
			continue
		}
		name := parts[0].trim_space()
		version := parts[1].trim_space()
		if name == '' || version == '' {
			continue
		}
		result << Override{
			name:    name
			version: version
		}
	}
	return result
}

// apply_overrides replaces the version of every module an override names. It runs
// before `validate_range_destinations`, so an overridden module is checked at the
// version it will actually be installed at.
pub fn apply_overrides(mut modules map[string]Module, overrides []Override) {
	for o in overrides {
		for key, mut m in modules {
			if m.name == o.name {
				m.version = o.version
				m.version_range = if is_version_range(o.version) { o.version } else { '' }
				modules[key] = m
			}
		}
	}
}
