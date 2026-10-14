module main

import os
import net.http

fn testsuite_begin() {
	clear_auth_environment()
}

fn testsuite_end() {
	clear_auth_environment()
}

fn clear_auth_environment() {
	os.unsetenv(registry_url_env)
	os.unsetenv(registry_insecure_loopback_env)
	os.unsetenv('VPM_TOKEN')
	os.unsetenv('VPM_REGISTRY_TOKEN_HOST_76706D2E6578616D706C652E636F6D')
	os.unsetenv('VPM_REGISTRY_TOKEN_HOST_6D792D72656769737472792E696F')
	os.unsetenv('VPM_REGISTRY_TOKEN_HOST_6D792E72656769737472792E696F')
	os.unsetenv('VPM_TOKEN_VPM_EXAMPLE_COM')
	os.unsetenv('VPM_TOKEN_MY_REGISTRY_IO')
	os.unsetenv('VPM_TOKEN_HOST_6162')
	os.unsetenv('VPM_REGISTRY_TOKEN_HOST_6162')
}

fn test_public_registry_needs_no_token() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	os.setenv('VPM_TOKEN', 'private-token', true)
	assert registry_token('https://vpm.vlang.io/a_module') == ''
}

fn test_unqualified_vpm_token_covers_one_private_registry() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	os.setenv('VPM_TOKEN', 'tok-all', true)
	os.setenv(registry_url_env, 'https://vpm.example.com', true)
	assert registry_token('https://vpm.example.com/a_module') == 'tok-all'
}

fn test_unqualified_token_is_limited_to_the_configured_http_origin() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	os.setenv('VPM_TOKEN', 'private-token', true)
	os.setenv(registry_url_env, 'https://VPM.EXAMPLE.COM:443/registry/', true)
	for url in ['https://vpm.example.com/a', 'https://VPM.EXAMPLE.COM:443/b'] {
		assert registry_token(url) == 'private-token', url
	}
	for url in ['https://other.example.com/a', 'http://vpm.example.com/a',
		'https://vpm.example.com:8443/a', 'https://vpm.example.com@other.example.com/a',
		'ftp://vpm.example.com/a', '//vpm.example.com/a', '/relative', 'https:///a'] {
		assert registry_token(url) == '', url
	}
	os.setenv(registry_url_env, 'http://vpm.example.com', true)
	assert registry_token('http://VPM.EXAMPLE.COM:80/a') == 'private-token'
	assert registry_token('http://vpm.example.com:8080/a') == ''
	assert registry_token('https://vpm.example.com/a') == ''
}

fn test_unqualified_token_requires_a_valid_configured_registry() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	os.setenv('VPM_TOKEN', 'private-token', true)
	assert registry_token('https://vpm.example.com/a') == ''
	for configured in ['', '::::not a url::::', '/relative', 'https:///missing',
		'file://vpm.example.com/a', '//vpm.example.com'] {
		os.setenv(registry_url_env, configured, true)
		assert registry_token('https://vpm.example.com/a') == '', configured
	}
}

// A scoped token must win over the unqualified one, because a machine holding
// tokens for several private registries must not send one registry's token to
// another.
fn test_scoped_token_wins_over_the_unqualified_one() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	os.setenv('VPM_TOKEN', 'tok-all', true)
	os.setenv(registry_url_env, 'https://vpm.example.com', true)
	os.setenv('VPM_REGISTRY_TOKEN_HOST_76706D2E6578616D706C652E636F6D', 'tok-example', true)
	assert registry_token('https://vpm.example.com/a_module') == 'tok-example'
	assert registry_token('https://vpm.other.io/a_module') == ''
	os.unsetenv(registry_url_env)
	assert registry_token('https://VPM.EXAMPLE.COM/a_module') == 'tok-example'
}

fn test_empty_scoped_token_disables_the_unqualified_fallback() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	os.setenv(registry_url_env, 'https://vpm.example.com', true)
	os.setenv('VPM_TOKEN', 'private-token', true)
	os.setenv('VPM_REGISTRY_TOKEN_HOST_76706D2E6578616D706C652E636F6D', '', true)
	assert registry_token('https://vpm.example.com/a') == ''
}

fn test_host_token_keys_are_injective_after_hostname_case_folding() {
	assert registry_token_env_name('vpm.example.com') == 'VPM_REGISTRY_TOKEN_HOST_76706D2E6578616D706C652E636F6D'
	assert registry_token_env_name('VPM.EXAMPLE.COM') == 'VPM_REGISTRY_TOKEN_HOST_76706D2E6578616D706C652E636F6D'
	assert registry_token_env_name('my-registry.io') == 'VPM_REGISTRY_TOKEN_HOST_6D792D72656769737472792E696F'
	assert registry_token_env_name('my.registry.io') == 'VPM_REGISTRY_TOKEN_HOST_6D792E72656769737472792E696F'
	assert registry_token_env_name('café.example') == 'VPM_REGISTRY_TOKEN_HOST_636166C3A92E6578616D706C65'
	// Windows folds environment variable case: these keys still stay distinct.
	assert registry_token_env_name('my-registry.io').to_lower() != registry_token_env_name('my.registry.io').to_lower()
}

fn test_old_colliding_hosts_receive_only_their_own_encoded_token() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	os.setenv('VPM_TOKEN_MY_REGISTRY_IO', 'unsafe-legacy-token', true)
	assert registry_token('https://my-registry.io/a_module') == ''
	assert registry_token('https://my.registry.io/a_module') == ''
	os.setenv('VPM_REGISTRY_TOKEN_HOST_6D792D72656769737472792E696F', 'hyphen-token', true)
	assert registry_token('https://MY-REGISTRY.IO/a_module') == 'hyphen-token'
	assert registry_token('https://my.registry.io/a_module') == ''
	os.unsetenv('VPM_REGISTRY_TOKEN_HOST_6D792D72656769737472792E696F')
	os.setenv('VPM_REGISTRY_TOKEN_HOST_6D792E72656769737472792E696F', 'dot-token', true)
	assert registry_token('https://my-registry.io/a_module') == ''
	assert registry_token('https://MY.REGISTRY.IO/a_module') == 'dot-token'
	os.setenv('VPM_REGISTRY_TOKEN_HOST_6D792D72656769737472792E696F', 'hyphen-token', true)
	assert registry_token('https://my-registry.io/a_module') == 'hyphen-token'
	assert registry_token('https://my.registry.io/a_module') == 'dot-token'
}

fn test_legacy_tokens_do_not_change_the_exact_origin_global_fallback() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	os.setenv('VPM_TOKEN_VPM_EXAMPLE_COM', 'legacy-token', true)
	assert registry_token('https://vpm.example.com/a') == ''
	os.setenv('VPM_TOKEN', 'global-token', true)
	os.setenv(registry_url_env, 'https://vpm.example.com:443', true)
	assert registry_token('https://vpm.example.com/a') == 'global-token'
	assert registry_token('https://vpm.example.com:8443/a') == ''
	assert registry_token('http://vpm.example.com/a') == ''
	assert registry_token('https://other.example.com/a') == ''
}

fn test_new_token_namespace_cannot_consume_a_legacy_token_for_another_host() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	// Under the old punctuation mapping, host-6162 used this key. A new
	// encoding beneath VPM_TOKEN_HOST_ would misread it as host ab's token.
	os.setenv('VPM_TOKEN_HOST_6162', 'legacy-host-6162-token', true)
	assert registry_token_env_name('ab') == 'VPM_REGISTRY_TOKEN_HOST_6162'
	assert registry_token('https://ab/module') == ''
	assert registry_token('https://host-6162/module') == ''
	os.setenv('VPM_REGISTRY_TOKEN_HOST_6162', 'ab-token', true)
	assert registry_token('https://ab/module') == 'ab-token'
	assert registry_token('https://host-6162/module') == ''
}

fn test_unparseable_url_asks_for_no_token() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	os.setenv(registry_url_env, 'https://vpm.example.com', true)
	os.setenv('VPM_TOKEN', 'private-token', true)
	os.setenv('VPM_REGISTRY_TOKEN_HOST_76706D2E6578616D706C652E636F6D', 'scoped-token', true)
	assert registry_token('::::not a url::::') == ''
	assert registry_token('file://vpm.example.com/a') == ''
	assert registry_token('//vpm.example.com/a') == ''
}

fn test_401_without_a_token_is_reported_as_an_auth_requirement() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	require_registry_token('https://vpm.example.com/a_module', 401) or {
		assert err.msg().contains('requires authentication'), err.msg()
		assert err.msg().contains('VPM_REGISTRY_TOKEN_HOST_76706D2E6578616D706C652E636F6D'), err.msg()
		assert !err.msg().contains('VPM_TOKEN_VPM_EXAMPLE_COM'), err.msg()
		return
	}
	assert false, 'a 401 with no token configured should have raised an error'
}

fn test_401_with_a_token_is_reported_as_a_rejection() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	os.setenv(registry_url_env, 'https://vpm.example.com', true)
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

fn assert_registry_transport_rejected(url string) {
	mut rejected := false
	require_registry_secure_transport(url) or {
		assert err.msg().contains('require HTTPS'), err.msg()
		rejected = true
	}
	assert rejected, url
}

fn test_bearer_transport_requires_https_without_a_development_opt_in() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	require_registry_secure_transport('https://vpm.example.com/package')!
	for url in ['http://127.0.0.1/package', 'http://[::1]/package', 'http://localhost/package',
		'http://vpm.example.com/package', 'ftp://127.0.0.1/package'] {
		assert_registry_transport_rejected(url)
	}
}

fn test_development_http_exception_accepts_only_the_exact_ipv4_loopback_literal() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	os.setenv(registry_insecure_loopback_env, '1', true)
	for url in ['http://127.0.0.1/package', 'http://127.0.0.1:8080/package',
		'https://vpm.example.com/package'] {
		require_registry_secure_transport(url)!
	}
	for host in ['localhost', '127.0.0.2', '127.1', '2130706433', '0x7f000001', '0177.0.0.1',
		'127.0.0.1.example.com', '[::1]', '[::1]:8080', '[::ffff:127.0.0.1]', '[0:0:0:0:0:0:0:1]',
		'[::1%25lo0]', '0.0.0.0', '192.0.2.1', '[::]', '127.0.0.1@example.com'] {
		assert_registry_transport_rejected('http://${host}/package')
	}
	assert_registry_transport_rejected('ftp://127.0.0.1/package')
}

fn test_development_http_exception_requires_the_exact_opt_in_value() {
	clear_auth_environment()
	defer { clear_auth_environment() }
	for value in ['', '0', 'true', 'yes', '01', '1 '] {
		os.setenv(registry_insecure_loopback_env, value, true)
		assert_registry_transport_rejected('http://127.0.0.1/package')
		assert_registry_transport_rejected('http://[::1]/package')
	}
}
