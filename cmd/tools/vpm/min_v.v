module main

import semver
import v.util.version
import v.vmod

// MinVersionError reports that a module needs a newer compiler than the one running.
// It embeds `Error` so it can be returned from a `fn ... !` as the error value
// itself, which is how the rest of VPM reports failures.
pub struct MinVersionError {
	Error
pub:
	module_name string
	installed   string
	required    string
}

// msg explains what happened, so the user reads it at install time rather than
// discovering the mismatch as a compile error inside someone else's module.
pub fn (e MinVersionError) msg() string {
	return 'module `${e.module_name}` requires V ${e.required} or newer, but this compiler is ${e.installed}.'
}

// min_v_violation returns why `manifest` cannot be installed by the running compiler,
// or none when it can. It is separated from check_min_v so the decision can be
// tested on its own instead of only through a failure.
pub fn min_v_violation(manifest vmod.Manifest, module_name string) ?MinVersionError {
	required := manifest.unknown['min_v'] or { return none }
	if required.len == 0 || required[0].trim_space() == '' {
		// An empty `min_v` is what a template renders and what a hand-edited manifest
		// leaves behind; it means "no requirement", so it must not refuse an install.
		return none
	}
	// A malformed min_v is the module author's bug, not the user's, and refusing is
	// better than installing something that cannot be checked once it is on disk.
	wanted := semver.from(required[0].trim_space()) or {
		return MinVersionError{
			module_name: module_name
			installed:   version.v_version
			required:    required[0]
		}
	}
	running := semver.from(version.v_version) or { return none }
	if wanted <= running {
		return none
	}
	return MinVersionError{
		module_name: module_name
		installed:   version.v_version
		required:    wanted.str()
	}
}

// check_min_v refuses a module whose `min_v` the running compiler does not meet.
// `min_v` is a single value, so it arrives through `Manifest.unknown` alongside any
// key the parser does not know.
pub fn check_min_v(manifest vmod.Manifest, module_name string) ! {
	if viol := min_v_violation(manifest, module_name) {
		return viol
	}
}
