module main

import semver
import v.util.version
import v.vmod

fn manifest_with_min_v(min_v string) vmod.Manifest {
	return vmod.decode("Module {\n\tname: 'example'\n\tmin_v: '${min_v}'\n}\n") or {
		panic('could not build a manifest with min_v: ' + err.msg())
	}
}

fn test_min_v_violation_accepts_a_manifest_without_one() {
	manifest := vmod.decode("Module {\n\tname: 'example'\n}\n") or { panic(err) }
	if viol := min_v_violation(manifest, 'example') {
		assert false, 'a manifest without min_v must be accepted, got ${viol}'
	}
}

fn test_min_v_violation_accepts_this_compiler_and_anything_older() {
	if viol := min_v_violation(manifest_with_min_v(version.v_version), 'example') {
		assert false, 'min_v equal to the running compiler must be accepted, got ${viol}'
	}
	if viol := min_v_violation(manifest_with_min_v('0.0.1'), 'example') {
		assert false, 'a min_v below the running compiler must be accepted, got ${viol}'
	}
}

fn test_min_v_violation_refuses_a_newer_compiler() {
	// A refusal here is the whole point of min_v, so assert on the error itself
	// rather than on the absence of an error.
	viol := min_v_violation(manifest_with_min_v('99.0.0'), 'example') or {
		assert false, 'min_v 99.0.0 must be refused by compiler ${version.v_version}, got none'
		return
	}
	assert viol.module_name == 'example'
	assert viol.required == '99.0.0'
	assert viol.installed == version.v_version
	msg := viol.msg()
	assert msg.contains('example'), msg
	assert msg.contains('99.0.0'), msg
	assert msg.contains(version.v_version), msg
}

fn test_min_v_violation_refuses_a_malformed_value() {
	// A module author who cannot spell their own requirement must not be installed:
	// the alternative is a module on disk that nothing can check later.
	if viol := min_v_violation(manifest_with_min_v('not-a-version'), 'example') {
		assert viol.required == 'not-a-version'
		return
	}
	assert false, 'a malformed min_v must be refused, got none'
}

fn test_min_v_violation_compares_numerically_not_lexically() {
	// 0.5.10 is newer than 0.5.2 even though the string sorts lower, so a compiler at
	// 0.5.2 has to be refused by min_v 0.5.10. Build the pair from the running
	// compiler's own patch level so the test asserts nothing about a fixed version.
	running := semver.from(version.v_version) or { panic(err) }
	newer := semver.build(running.major, running.minor, running.patch + 1)
	older := if running.patch > 0 {
		semver.build(running.major, running.minor, running.patch - 1)
	} else {
		semver.build(running.major, running.minor - 1, 0)
	}
	if viol := min_v_violation(manifest_with_min_v('${newer.major}.${newer.minor}.${newer.patch}'),
		'example')
	{
		assert viol.required == newer.str()
	} else {
		assert false, 'min_v ${newer.str()} must refuse compiler ${version.v_version}'
	}
	if viol := min_v_violation(manifest_with_min_v('${older.major}.${older.minor}.${older.patch}'),
		'example')
	{
		assert false, 'min_v ${older.str()} is below ${version.v_version} and must be accepted, got ${viol}'
	}
}

fn test_min_v_violation_refuses_a_prerelease_of_the_next_patch() {
	// A prerelease of a newer line is not something the current compiler satisfies.
	running := semver.from(version.v_version) or { panic(err) }
	newer := semver.build(running.major, running.minor, running.patch + 1)
	if viol := min_v_violation(manifest_with_min_v('${newer.major}.${newer.minor}.${newer.patch}-beta.1'),
		'example')
	{
		return
	}
	assert false, 'min_v ${newer.str()}-beta.1 must refuse compiler ${version.v_version}'
}

fn test_min_v_violation_ignores_an_empty_value() {
	// `min_v: ''` is what a template renders; refusing to install over it would be
	// worse than not checking.
	if viol := min_v_violation(manifest_with_min_v(''), 'example') {
		assert false, 'an empty min_v must be ignored, not refused, got ${viol}'
	}
}

fn test_check_min_v_raises_the_violation() {
	check_min_v(manifest_with_min_v('99.0.0'), 'example') or { return }
	assert false, 'check_min_v must refuse min_v 99.0.0, got no error'
}

fn test_check_min_v_is_silent_when_satisfied() {
	check_min_v(manifest_with_min_v(version.v_version), 'example') or {
		panic('check_min_v must accept min_v ${version.v_version}: ' + err.msg())
	}
}
