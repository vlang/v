module main

import semver

// Override is one `dependency_overrides` entry. The `requiring` field is the module
// that has to ask for the dependency for the override to apply; when it is empty the
// override applies everywhere. This is the root project's last word, and it
// deliberately does not check whether other constraints allow the version — that is
// what an override is for.
pub struct Override {
pub:
	name      string
	version   string
	requiring string
}

// parse_overrides reads the `dependency_overrides` entries of a manifest. Each entry
// is `[requiring>]name: version`, pnpm-style. An entry that is not that shape is
// skipped rather than guessed at, because a typo in an override should not silently
// install something else.
pub fn parse_overrides(raw []string) []Override {
	mut result := []Override{}
	for entry in raw {
		o := parse_override(entry) or { continue }
		result << o
	}
	return result
}

// parse_override reads one entry. The `requiring>name` form forces a version only
// where `requiring` asks for `name`; without it the override applies everywhere.
fn parse_override(entry string) !Override {
	parts := entry.split(':')
	if parts.len != 2 {
		return error('invalid override `${entry}`')
	}
	left := parts[0].trim_space()
	version := parts[1].trim_space()

	mut requiring := ''
	mut name := left
	if idx := left.index('>') {
		requiring = left[..idx].trim_space()
		name = left[idx + 1..].trim_space()
	}

	if name == '' || version == '' {
		return error('invalid override `${entry}`')
	}
	return Override{
		name:      name
		version:   version
		requiring: requiring
	}
}

// build_graph reads the dependencies of every module and returns a map from a module
// name to the names it depends on. This is the graph the selector form of an override
// needs: `vsl>c: 1.0.2` can only be applied where vsl actually asks for c.
fn build_graph(modules map[string]Module) map[string][]string {
	mut graph := map[string][]string{}
	for _, m in modules {
		mut deps := []string{}
		for dep in m.manifest.dependencies {
			name := dep.all_before('@').trim_space()
			if name != '' {
				deps << name
			}
		}
		graph[m.name] = deps
	}
	return graph
}

// apply_overrides replaces the version of every module an override names. It runs
// before `validate_range_destinations`, so an overridden module is checked at the
// version it will actually be installed at.
pub fn apply_overrides(mut modules map[string]Module, overrides []Override, graph map[string][]string) {
	for o in overrides {
		// A selector override only applies where the requiring module asks for the
		// dependency. Without this, `vsl>c: 1.0.2` would force c everywhere.
		if o.requiring != '' {
			deps := graph[o.requiring] or { continue }
			if o.name !in deps {
				continue
			}
		}
		for key, mut m in modules {
			if m.name == o.name {
				m.version = o.version
				m.version_range = if is_version_range(o.version) { o.version } else { '' }
				modules[key] = m
			}
		}
	}
}
