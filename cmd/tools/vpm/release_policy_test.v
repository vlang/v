module main

fn test_release_policy_compares_instants_and_validates_durations() {
	assert release_cutoff_unix('2026-06-02T00:00:00+02:00')! < release_cutoff_unix('2026-06-01T23:00:00Z')!
	assert release_cutoff_unix('2026-06-01')! == release_cutoff_unix('2026-06-01T00:00:00Z')!
	assert release_age_seconds('2d')! == 172800
	assert release_age_seconds('3h')! == 10800
	assert release_age_seconds('5m')! == 300
	assert release_age_seconds('4')! == 14400
	assert release_age_seconds('0h')! == 0
	for value in ['', '-1d', '1s', '1.5h', 'invalid', '9223372036854775807d'] {
		mut failed := false
		release_age_seconds(value) or { failed = true }
		assert failed, value
	}
	for value in ['', 'invalid', '2026-01-01Tinvalid'] {
		mut failed := false
		release_cutoff_unix(value) or { failed = true }
		assert failed, value
	}
}

fn test_registry_qualified_lock_keys_keep_registry_identity() {
	assert lockfile_module_key('pkg@registry-one:1.0.0') == 'pkg@registry-one:1.0.0'
	assert lockfile_module_key('pkg@registry-one:1.0.0') != lockfile_module_key('pkg@registry-two:1.0.0')
	assert lockfile_module_key('pkg@v1.0.0') == 'pkg'
	assert lockfile_module_key('git@example.com:owner/pkg.git@v1.0.0') == 'git@example.com:owner/pkg.git'
}
