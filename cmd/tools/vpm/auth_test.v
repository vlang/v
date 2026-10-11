module main

import os
import net.http

fn testsuite_begin() {
	os.unsetenv('VPM_TOKEN')
	os.unsetenv('VPM_REGISTRY')
	os.unsetenv('VPM_TOKEN_VPM_EXAMPLE_COM')
	os.unsetenv('VPM_TOKEN_MY_REGISTRY_IO')
}

fn testsuite_end() {
	os.unsetenv('VPM_TOKEN')
	os.unsetenv('VPM_REGISTRY')
	os.unsetenv('VPM_TOKEN_VPM_EXAMPLE_COM')
	os.unsetenv('VPM_TOKEN_MY_REGISTRY_IO')
}

fn test_public_registry_needs_no_token() {
	assert registry_token('https://vpm.vlang.io/a_module') == ''
}

fn test_unqualified_vpm_token_covers_one_private_registry() {
	os.setenv('VPM_TOKEN', 'tok-all', true)
	os.setenv('VPM_REGISTRY', 'https://vpm.example.com', true)
	assert registry_token('https://vpm.example.com/a_module') == 'tok-all'
}

// A scoped token must win over the unqualified one, because a machine holding
// tokens for several private registries must not send one registry's token to
// another.
fn test_scoped_token_wins_over_the_unqualified_one() {
	os.setenv('VPM_TOKEN', 'tok-all', true)
	os.setenv('VPM_REGISTRY', 'https://vpm.example.com', true)
	os.setenv('VPM_TOKEN_VPM_EXAMPLE_COM', 'tok-example', true)
	assert registry_token('https://vpm.example.com/a_module') == 'tok-example'
	// Other origins receive neither the configured registry's fallback nor its scoped token.
	assert registry_token('https://vpm.other.io/a_module') == ''
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
	os.setenv('VPM_REGISTRY', 'https://vpm.example.com', true)
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

fn test_authenticated_redirects_stay_within_the_registry_origin() {
	request := http.Request{
		url:    'https://vpm.example.com/package'
		header: http.new_header(key: .authorization, value: 'Bearer test-token')
	}
	vpm_registry_redirect(&request, 0, 'https://VPM.EXAMPLE.COM:443/other')!
	for destination in ['https://other.example.com/package', 'http://vpm.example.com/package',
		'https://vpm.example.com:8443/package'] {
		vpm_registry_redirect(&request, 0, destination) or { continue }
		assert false, 'authenticated redirect accepted `${destination}`'
	}
	public := http.Request{
		url: 'https://vpm.example.com/package'
	}
	vpm_registry_redirect(&public, 0, 'https://other.example.com/package')!
}

fn test_unqualified_token_is_restricted_to_configured_registry_origin() {
	os.unsetenv('VPM_TOKEN_VPM_EXAMPLE_COM')
	os.setenv('VPM_TOKEN', 'private-secret', true)
	os.setenv('VPM_REGISTRY', 'https://vpm.example.com/private', true)
	assert registry_token('https://VPM.EXAMPLE.COM:443/module') == 'private-secret'
	for url in ['https://vpm.vlang.io/api/packages/module', 'http://vpm.example.com/module',
		'https://vpm.example.com:8443/module', '/module', 'file:///module'] {
		assert registry_token(url) == '', url
	}
	os.unsetenv('VPM_REGISTRY')
	assert registry_token('https://vpm.example.com/module') == ''
}
