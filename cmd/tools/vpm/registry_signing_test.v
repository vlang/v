module main

import crypto.ed25519
import json2
import os

const signing_root = os.join_path(os.vtmp_dir(), 'vpm_registry_signing_${os.getpid()}')
const signing_original_dir = os.getwd()
const signing_original_environment = os.environ()

fn signing_demo_info(name string, version string) ModuleInfo {
	return ModuleInfo{
		name:         name
		version:      version
		description:  'demo'
		license:      'MIT'
		dependencies: map[string]string{}
		checksum:     'sha256:deadbeef'
		published_at: '2024-01-01T00:00:00Z'
		features:     map[string][]string{}
	}
}

fn new_demo_registry() Registry {
	mut r := new_registry()
	r.add_module(signing_demo_info('alpha', '1.0.0'))
	return r
}

fn testsuite_begin() {
	os.mkdir_all(signing_root)!
	os.chdir(signing_root)!
	os.unsetenv('VPM_REGISTRY_KEY')
}

fn testsuite_end() {
	if previous := signing_original_environment[registry_key_env] {
		os.setenv(registry_key_env, previous, true)
	} else {
		os.unsetenv(registry_key_env)
	}
	os.chdir(signing_original_dir) or {}
	os.rmdir_all(signing_root) or {}
}

fn test_an_unsigned_registry_vouches_for_nothing() {
	os.unsetenv('VPM_REGISTRY_KEY')
	r := new_demo_registry()
	loaded := r.checked_signing_key()!
	assert loaded.key == none
	assert r.signature() == '', 'an unsigned registry produced a signature'
	assert r.public_key_hex() == '', 'an unsigned registry produced a public key'
}

fn test_invalid_environment_keys_do_not_fall_back_to_a_valid_key_file() {
	_, private := ed25519.generate_key()!
	os.mkdir_all(registry_dir())!
	os.write_file(registry_key_path(), private.seed().hex())!
	defer {
		os.rm(registry_key_path()) or {}
		os.unsetenv(registry_key_env)
	}
	registry := new_demo_registry()
	for raw in ['', ' \t\n', 'not-a-hexadecimal-private-seed', '0', '00'.repeat(31), '00'.repeat(33)] {
		$if windows {
			if raw == '' {
				// _wputenv deletes an empty value; the CLI child-environment test
				// supplies a real KEY= entry without this setter limitation.
				continue
			}
		}
		os.setenv(registry_key_env, raw, true)
		mut refused := false
		registry.checked_signing_key() or {
			assert err.msg().contains(registry_key_env), err.msg()
			if raw.len > 10 {
				assert !err.msg().contains(raw), 'a key error disclosed the configured seed'
			}
			refused = true
		}
		assert refused, 'invalid environment key was treated as unsigned or fell back to the key file'
	}
}

fn test_invalid_key_files_are_errors_rather_than_unsigned_registries() {
	os.unsetenv(registry_key_env)
	os.mkdir_all(registry_dir())!
	defer {
		os.rm(registry_key_path()) or {}
	}
	registry := new_demo_registry()
	for raw in ['', ' \t\n', 'not-a-hexadecimal-private-seed', '0', '00'.repeat(31), '00'.repeat(33)] {
		os.write_file(registry_key_path(), raw)!
		mut refused := false
		registry.checked_signing_key() or {
			assert err.msg().contains('signing key file'), err.msg()
			if raw.len > 10 {
				assert !err.msg().contains(raw), 'a key error disclosed the configured seed'
			}
			refused = true
		}
		assert refused, 'invalid key file was treated as an unsigned registry'
	}
}

fn test_signing_uses_the_registry_key_file_without_an_environment_key() {
	os.unsetenv(registry_key_env)
	public_key, private_key := ed25519.generate_key()!
	os.mkdir_all(registry_dir())!
	os.write_file(registry_key_path(), private_key.seed().hex())!
	defer {
		os.rm(registry_key_path()) or {}
	}
	registry := new_demo_registry()
	assert registry.public_key_hex() == public_key.hex()
	assert registry.verify_signature(public_key.hex(), registry.signature())
}

fn test_a_signed_registry_verifies_against_its_own_key() {
	os.unsetenv('VPM_REGISTRY_KEY')
	r := new_demo_registry()
	_public, private := ed25519.generate_key() or {
		assert false, 'key generation failed: ${err}'
		return
	}
	os.setenv('VPM_REGISTRY_KEY', private.seed().hex(), true)
	assert r.public_key_hex().len == ed25519.public_key_size * 2
	sig := r.signature()
	assert sig.len == ed25519.signature_size * 2
	assert r.verify_signature(r.public_key_hex(), sig), 'the holder of the key could not verify its own registry'
}

// A mirror that edits a checkout before serving it changes the canonical form,
// so the original signature no longer verifies against the pinned origin key.
fn test_editing_the_registry_after_signing_breaks_verification() {
	os.unsetenv('VPM_REGISTRY_KEY')
	r := new_demo_registry()
	_public, private := ed25519.generate_key() or {
		assert false, 'key generation failed: ${err}'
		return
	}
	os.setenv('VPM_REGISTRY_KEY', private.seed().hex(), true)
	sig := r.signature()
	public_key := r.public_key_hex()

	mut tampered := new_demo_registry()
	tampered.add_module(signing_demo_info('injected', '9.9.9'))
	assert !tampered.verify_signature(public_key, sig), 'a signature over an edited registry verified'
}

fn test_a_signature_from_another_key_does_not_verify() {
	os.unsetenv('VPM_REGISTRY_KEY')
	r := new_demo_registry()
	_public, private := ed25519.generate_key() or {
		assert false, 'key generation failed: ${err}'
		return
	}
	os.setenv('VPM_REGISTRY_KEY', private.seed().hex(), true)
	sig := r.signature()
	other_public, _other_private := ed25519.generate_key() or {
		assert false, 'key generation failed: ${err}'
		return
	}
	assert !r.verify_signature(other_public.hex(), sig), 'a signature verified under an unrelated key'
}

fn test_malformed_hex_is_rejected_rather_than_trusted() {
	r := new_demo_registry()
	assert !r.verify_signature('zzzz', 'zzzz'), 'malformed hex was accepted'
	assert !r.verify_signature('', ''), 'empty hex was accepted'
	// A correct-length but all-zero key is not a valid point on the curve, so
	// verification must fail rather than panic.
	assert !r.verify_signature('0'.repeat(ed25519.public_key_size * 2), '0'.repeat(ed25519.signature_size * 2)), 'an all-zero key and signature were accepted'
}

fn test_signatures_do_not_depend_on_nested_map_insertion_order() {
	_, private := ed25519.generate_key()!
	os.setenv('VPM_REGISTRY_KEY', private.seed().hex(), true)
	mut a := new_registry()
	mut b := new_registry()
	for name in ['alpha', 'beta'] {
		a.add_module(ModuleInfo{
			...signing_demo_info(name, '1.0.0')
			dependencies: {
				'first':  '^1'
				'second': '^2'
			}
			features:     {
				'first':  ['one']
				'second': ['two']
			}
		})
	}
	for name in ['beta', 'alpha'] {
		b.add_module(ModuleInfo{
			...signing_demo_info(name, '1.0.0')
			dependencies: {
				'second': '^2'
				'first':  '^1'
			}
			features:     {
				'second': ['two']
				'first':  ['one']
			}
		})
	}
	before := json2.encode(b.modules)
	assert a.canonical_json() == b.canonical_json()
	assert a.signature() == b.signature()
	assert b.verify_signature(a.public_key_hex(), a.signature())
	assert json2.encode(b.modules) == before
}

fn test_configuration_exposes_the_key_that_signs_the_registry() {
	_, private := ed25519.generate_key()!
	os.setenv('VPM_REGISTRY_KEY', private.seed().hex(), true)
	r := new_demo_registry()
	config := json2.decode[RegistryConfig](handle_request(r, 'GET', '/config.json',
		map[string]string{}))!
	assert config.public_key == r.public_key_hex()
	assert config.public_key != ''
}

// The advertised key identifies the signer but does not establish trust.
fn test_the_config_route_names_the_advertised_signing_key() {
	os.unsetenv('VPM_REGISTRY_KEY')
	r := new_demo_registry()
	vouching_public, private := ed25519.generate_key() or {
		assert false, 'key generation failed: ${err}'
		return
	}
	os.setenv('VPM_REGISTRY_KEY', private.seed().hex(), true)
	result := handle_request(r, 'GET', '/config.json', map[string]string{})
	assert result.contains(vouching_public.hex()), result
}

fn test_a_mirror_cannot_replace_an_independently_pinned_origin_key() {
	origin_public, origin_private := ed25519.generate_key()!
	os.setenv('VPM_REGISTRY_KEY', origin_private.seed().hex(), true)
	origin := new_demo_registry()
	// The verifier gets this key through a trusted channel, before contacting
	// either the origin or the mirror. It never replaces it from config.json.
	pinned_key_path := os.join_path(signing_root, 'pinned.pub')
	os.write_file(pinned_key_path, origin_public.hex() + '\n')!
	defer {
		os.rm(pinned_key_path) or {}
	}
	pinned_public_key := os.read_file(pinned_key_path)!.trim_space()
	assert origin.verify_signature(pinned_public_key, origin.signature())

	mirror_public, mirror_private := ed25519.generate_key()!
	os.setenv('VPM_REGISTRY_KEY', mirror_private.seed().hex(), true)
	mut mirror := new_demo_registry()
	mirror.add_module(signing_demo_info('injected', '9.9.9'))
	config := json2.decode[RegistryConfig](handle_request(mirror, 'GET', '/config.json',
		map[string]string{}))!
	signature := json2.decode[string](handle_request(mirror, 'GET', '/signature.sig',
		map[string]string{}))!
	assert config.public_key == mirror_public.hex()
	assert config.public_key != pinned_public_key
	// The substituted index is self-consistent under the attacker's key. This
	// is why trusting a key discovered from the mirror would accept the forgery.
	assert mirror.verify_signature(config.public_key, signature)
	assert !mirror.verify_signature(pinned_public_key, signature), 'a mirror replaced the independently pinned signing key'
}
