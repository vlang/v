module main

import crypto.ed25519
import json2
import os

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
	os.unsetenv('VPM_REGISTRY_KEY')
}

fn testsuite_end() {
	os.unsetenv('VPM_REGISTRY_KEY')
}

fn test_an_unsigned_registry_vouches_for_nothing() {
	r := new_demo_registry()
	assert r.signature() == '', 'an unsigned registry produced a signature'
	assert r.public_key_hex() == '', 'an unsigned registry produced a public key'
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
// so the original signature no longer verifies. That is the whole point of
// signing: the mirror has no key of its own to re-sign with.
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

fn test_the_config_route_names_the_vouching_key() {
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

// A key file readable by group or others lets any local user forge
// signatures, so it must not be loaded. Unix-only: Windows has no equivalent
// mode check and accepts the file there.
fn test_group_readable_signing_key_is_not_loaded() {
	$if windows {
		return
	}
	os.unsetenv('VPM_REGISTRY_KEY')
	original := os.getwd()
	root := os.join_path(os.vtmp_dir(), 'vpm_signing_key_perm')
	os.mkdir_all(os.join_path(root, '.vpm-registry')) or { panic(err) }
	os.chdir(root) or { panic(err) }
	defer {
		os.chdir(original) or {}
		os.rmdir_all(root) or {}
	}
	public, private := ed25519.generate_key() or {
		assert false, 'key generation failed: ${err}'
		return
	}
	key_file := os.join_path('.vpm-registry', 'signing.key')
	os.write_file(key_file, private.seed().hex()) or { panic(err) }
	r := new_registry()
	os.chmod(key_file, 0o644) or { panic(err) }
	assert r.public_key_hex() == '', 'a group-readable signing key was loaded'
	os.chmod(key_file, 0o600) or { panic(err) }
	assert r.public_key_hex() == public.hex(), 'a private signing key was refused'
}

// The permission gate above is Unix-only by design: Windows has no equivalent
// mode check, so the loader accepts the file there, while a missing file is
// refused everywhere. This pins that split on every platform, including the
// ones where the test above returns immediately.
fn test_key_file_gate_is_platform_split() {
	$if windows {
		assert registry_key_is_private('C:\\nonexistent-signing.key')
	} $else {
		assert !registry_key_is_private('/nonexistent-signing-key')
	}
}
