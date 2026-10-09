module main

import os

fn testsuite_begin() {
	os.unsetenv('VPM_TOKEN')
	os.unsetenv('VPM_TOKEN_VPM_EXAMPLE_COM')
	os.unsetenv('VPM_TOKEN_MY_REGISTRY_IO')
}

fn testsuite_end() {
	os.unsetenv('VPM_TOKEN')
	os.unsetenv('VPM_TOKEN_VPM_EXAMPLE_COM')
	os.unsetenv('VPM_TOKEN_MY_REGISTRY_IO')
}

fn test_public_registry_needs_no_token() {
	assert registry_token('https://vpm.vlang.io/a_module') == ''
}

fn test_unqualified_vpm_token_covers_one_private_registry() {
	os.setenv('VPM_TOKEN', 'tok-all', true)
	assert registry_token('https://vpm.example.com/a_module') == 'tok-all'
}

// A scoped token must win over the unqualified one, because a machine holding
// tokens for several private registries must not send one registry's token to
// another.
fn test_scoped_token_wins_over_the_unqualified_one() {
	os.setenv('VPM_TOKEN', 'tok-all', true)
	os.setenv('VPM_TOKEN_VPM_EXAMPLE_COM', 'tok-example', true)
	assert registry_token('https://vpm.example.com/a_module') == 'tok-example'
	// ... and the other registry keeps the fallback, not example's token.
	assert registry_token('https://vpm.other.io/a_module') == 'tok-all'
}

// An environment name may not hold `.` or `-`, so both fold to `_`.
fn test_host_punctuation_folds_into_underscores() {
	os.setenv('VPM_TOKEN_MY_REGISTRY_IO', 'tok-my', true)
	assert registry_token('https://my-registry.io/a_module') == 'tok-my'
}

fn test_unparseable_url_asks_for_no_token() {
	assert registry_token('::::not a url::::') == ''
}

fn test_401_without_a_token_is_reported_as_an_auth_requirement() {
	os.unsetenv('VPM_TOKEN')
	os.unsetenv('VPM_TOKEN_VPM_EXAMPLE_COM')
	require_registry_token('https://vpm.example.com/a_module', 401) or {
		assert err.msg().contains('requires authentication'), err.msg()
		return
	}
	assert false, 'a 401 with no token configured should have raised an error'
}

fn test_401_with_a_token_is_reported_as_a_rejection() {
	os.unsetenv('VPM_TOKEN')
	os.setenv('VPM_TOKEN', 'tok-all', true)
	require_registry_token('https://vpm.example.com/a_module', 401) or {
		assert err.msg().contains('rejected the configured token'), err.msg()
		return
	}
	assert false, 'a 401 with a token configured should have raised an error'
}

fn test_a_non_401_status_is_not_treated_as_an_auth_failure() {
	require_registry_token('https://vpm.example.com/a_module', 404) or {
		assert false, 'a 404 must not be reported as an authentication problem'
	}
}
