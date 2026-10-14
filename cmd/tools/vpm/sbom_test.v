module main

import json2
import os
import uuid

fn sbom_demo_info(name string, version string, license string, checksum string) ModuleInfo {
	return ModuleInfo{
		name:         name
		version:      version
		description:  'demo'
		license:      license
		dependencies: map[string]string{}
		checksum:     checksum
		published_at: '2024-01-01T00:00:00Z'
		features:     map[string][]string{}
	}
}

// SHA256 of the actual artifact bytes 'abc'; short illustrative hashes are invalid.
const sbom_test_digest = 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad'

const sbom_namespace = 'https://vpm.example.com/spdx'
const sbom_download_base = 'https://vpm.example.com/dl'

fn new_sbom_demo_registry() Registry {
	mut r := new_registry()
	r.add_module(sbom_demo_info('serde', '1.0.0', 'MIT', 'sha256:${sbom_test_digest}'))
	r.add_module(sbom_demo_info('my/lib', '2.0.0', 'Apache-2.0', 'sha256:${sbom_test_digest}'))
	return r
}

fn test_the_document_declares_the_spdx_version_it_conforms_to() {
	r := new_sbom_demo_registry()
	doc := r.spdx_document(sbom_namespace, sbom_download_base)
	assert doc.spdx_version == 'SPDX-2.3'
	assert doc.data_license == 'CC0-1.0'
	assert doc.spdxid == 'SPDXRef-DOCUMENT'
	assert doc.document_namespace == sbom_namespace
}

// Every module version is a package, so a registry holding two versions of one
// module produces two packages rather than one.
fn test_every_version_is_a_package() {
	mut r := new_registry()
	r.add_module(sbom_demo_info('serde', '1.0.0', 'MIT', 'sha256:${sbom_test_digest}'))
	r.add_module(sbom_demo_info('serde', '1.1.0', 'MIT', 'sha256:${sbom_test_digest}'))
	r.add_module(sbom_demo_info('other', '2.0.0', 'MIT', 'sha256:${sbom_test_digest}'))
	doc := r.spdx_document(sbom_namespace, sbom_download_base)
	assert doc.packages.len == 3
}

// The `sha256:` prefix is SPDX metadata syntax, not part of the digest, so it is
// stripped: a consumer comparing against a checksum database would never match
// otherwise.
fn test_the_checksum_drops_the_vpm_prefix() {
	r := new_sbom_demo_registry()
	doc := r.spdx_document(sbom_namespace, sbom_download_base)
	assert doc.packages[0].checksums.len == 1
	assert doc.packages[0].checksums[0].algorithm == 'SHA256'
	assert doc.packages[0].checksums[0].checksum_value == sbom_test_digest
}

// A module with no recorded checksum must not produce an empty digest entry.
fn test_an_absent_checksum_yields_no_entry() {
	mut r := new_registry()
	r.add_module(sbom_demo_info('serde', '1.0.0', 'MIT', ''))
	doc := r.spdx_document(sbom_namespace, sbom_download_base)
	assert doc.packages[0].checksums.len == 0
}

fn test_sbom_keeps_all_sha256_spellings_accepted_from_a_manual_index() {
	original_directory := os.getwd()
	directory := os.join_path(os.vtmp_dir(), 'vpm_sbom_manual_index_${os.getpid()}')
	os.mkdir_all(directory)!
	os.chdir(directory)!
	defer {
		os.chdir(original_directory) or { panic(err) }
		os.rmdir_all(directory) or { panic(err) }
	}
	// Publishing canonicalizes a digest. Import the recorded spellings directly
	// so this proves the manually imported index and artifact path also agree.
	for recorded in [sbom_test_digest, 'sha256:${sbom_test_digest}', 'SHA256:${sbom_test_digest}',
		'sha256:SHA256:${sbom_test_digest}'] {
		mut source := new_registry()
		source.add_module(sbom_demo_info('serde', '1.0.0', 'MIT', recorded))
		registry := import_json(source.export_json())!
		info := registry.get_info('serde', '1.0.0') or { panic('imported module is missing') }
		assert info.checksum == recorded
		archive := artifact_path('serde', '1.0.0')
		os.mkdir_all(os.dir(archive))!
		os.write_file(archive, 'abc')!
		response := artifact_request(registry, '/serde/@v/1.0.0.zip') or {
			panic('artifact integrity rejected recorded checksum ${recorded}')
		}
		assert response.body == 'abc'
		document := json2.decode[SpdxDocument](registry.spdx_json(sbom_namespace, sbom_download_base))!
		assert document.packages.len == 1
		checksums := document.packages[0].checksums
		assert checksums.len == 1
		assert checksums[0].algorithm == 'SHA256'
		assert checksums[0].checksum_value == sbom_test_digest
	}
}

fn test_sbom_does_not_advertise_digests_that_artifact_integrity_cannot_accept() {
	for recorded in ['', 'sha256:', 'SHA256:', 'abc123', 'sha256:abc123',
		'sha256:${sbom_test_digest}0', sbom_test_digest.to_upper(), 'Sha256:${sbom_test_digest}',
		'sha512:${sbom_test_digest}', 'ga7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad'] {
		mut registry := new_registry()
		registry.add_module(sbom_demo_info('serde', '1.0.0', 'MIT', recorded))
		document := registry.spdx_document(sbom_namespace, sbom_download_base)
		assert document.packages.len == 1
		assert document.packages[0].checksums.len == 0, recorded
	}
}

// SPDX element ids may not hold `/` or `:`, so a name like `my/lib` must be
// rewritten or the document is not parseable by an SPDX consumer.
fn test_an_id_with_illegal_characters_is_rewritten() {
	for name in ['serde', 'my/lib', 'a.b:c'] {
		id := spdx_id_for(name, '1.0.0')
		assert id.starts_with('SPDXRef-Package-')
		assert !id.contains_any('/.:')
	}
}

fn test_package_ids_distinguish_versions_and_colliding_name_spellings() {
	mut r := new_registry()
	for name in ['my/lib', 'my-lib'] {
		for version in ['1.0.0', '1.1.0'] {
			r.add_module(sbom_demo_info(name, version, 'MIT', ''))
		}
	}
	doc := r.spdx_document(sbom_namespace, sbom_download_base)
	mut ids := map[string]bool{}
	for package in doc.packages {
		assert package.spdxid !in ids
		ids[package.spdxid] = true
	}
	assert ids.len == 4
}

// PURL is the identifier other supply-chain tools join on.
fn test_purl_is_well_formed() {
	assert spdx_purl('serde', '1.0.0') == 'pkg:vpm/serde@1.0.0'
	// A `/` separates namespace from name in PURL, so a name containing one
	// must be percent-encoded or the identifier changes shape.
	assert spdx_purl('my/lib', '2.0.0') == 'pkg:vpm/my%2Flib@2.0.0'
}

fn test_purl_encodes_reserved_bytes_in_both_components() {
	cases := {
		"-._~!$&'()*+,;=@[]": '-._~%21%24%26%27%28%29%2A%2B%2C%3B%3D%40%5B%5D'
		'@/?#%':              '%40%2F%3F%23%25'
		'%2F':                '%252F'
		'a b+c':              'a%20b%2Bc'
		'café本':             'caf%C3%A9%E6%9C%AC'
		'build:one':          'build:one'
	}
	for component, expected in cases {
		assert spdx_purl(component, '1.0.0') == 'pkg:vpm/${expected}@1.0.0'
		assert spdx_purl('serde', component) == 'pkg:vpm/serde@${expected}'
	}
}

fn test_purl_delimiters_do_not_conflate_package_identities() {
	assert spdx_purl('acme@internal', '1.0.0') == 'pkg:vpm/acme%40internal@1.0.0'
	assert spdx_purl('acme', 'internal@1.0.0') == 'pkg:vpm/acme@internal%401.0.0'
	assert spdx_purl('acme@internal', '1.0.0') != spdx_purl('acme', 'internal@1.0.0')
	assert spdx_purl('my/lib', '1.0.0') != spdx_purl('my%2Flib', '1.0.0')
}

fn test_spdx_json_emits_encoded_purls_without_changing_package_metadata() {
	mut registry := new_registry()
	registry.add_module(sbom_demo_info('acme@internal', '1.0.0+build@tag', 'MIT', ''))
	document := json2.decode[SpdxDocument](registry.spdx_json(sbom_namespace, sbom_download_base))!
	assert document.packages.len == 1
	package := document.packages[0]
	assert package.name == 'acme@internal'
	assert package.version_info == '1.0.0+build@tag'
	assert package.external_refs.len == 1
	assert package.external_refs[0].reference_type == 'purl'
	assert package.external_refs[0].reference_locator == 'pkg:vpm/acme%40internal@1.0.0%2Bbuild%40tag'
}

fn test_cpe_is_well_formed() {
	plain := spdx_cpe('serde', '1.0.0') or { panic('plain components were omitted') }
	assert plain == 'cpe:2.3:a:vpm:serde:1.0.0:*:*:*:*:*:*:*'
	escaped := spdx_cpe('a:b', 'release:one') or { panic('literal colon was omitted') }
	assert escaped == 'cpe:2.3:a:vpm:a\\:b:release\\:one:*:*:*:*:*:*:*'
}

fn test_cpe_quotes_literal_punctuation_in_both_product_and_version() {
	// Fixed vectors follow NISTIR 7695 section 6.2, Figure 6-3: only ASCII
	// letters, digits, hyphen, period and underscore are unquoted literals.
	cases := {
		'a-z_1.0':                    'a-z_1.0'
		'*':                          '\\*'
		'?':                          '\\?'
		'a*b?':                       'a\\*b\\?'
		'\\':                         '\\\\'
		'a\\b':                       'a\\\\b'
		':':                          '\\:'
		'!"#$%&\'(),/:;<=>@[]^`{|}~': '\\!\\"\\#\\$\\%\\&\\\'\\(\\)\\,\\/\\:\\;\\<\\=\\>\\@\\[\\]\\^\\`\\{\\|\\}\\~'
	}
	for component, expected in cases {
		product := spdx_cpe(component, '1.0.0') or { panic('product was omitted: ${component}') }
		assert product == 'cpe:2.3:a:vpm:${expected}:1.0.0:*:*:*:*:*:*:*'
		version := spdx_cpe('serde', component) or { panic('version was omitted: ${component}') }
		assert version == 'cpe:2.3:a:vpm:serde:${expected}:*:*:*:*:*:*:*'
	}
	// A plus is representable as a product literal; the version omission is
	// a retained registry policy, not a claim that CPE has no plus escape.
	product_plus := spdx_cpe('serde+extra', '1.0.0') or { panic('product plus was omitted') }
	assert product_plus == 'cpe:2.3:a:vpm:serde\\+extra:1.0.0:*:*:*:*:*:*:*'
}

fn test_unrepresentable_cpe_components_keep_exact_metadata_and_purl() {
	// Whole '-' means CPE NA, not the literal module/version string. Quoting
	// it as '\-' is not permitted by the formatted-string grammar either.
	for component in ['', '-', 'with space', 'tab\there', 'line\nbreak', '\x00', '\x7f', 'café'] {
		for as_product in [true, false] {
			name := if as_product { component } else { 'serde' }
			version := if as_product { '1.0.0' } else { component }
			mut registry := new_registry()
			registry.add_module(sbom_demo_info(name, version, 'MIT', ''))
			document := json2.decode[SpdxDocument](registry.spdx_json(sbom_namespace, sbom_download_base))!
			package := document.packages[0]
			assert package.name == name
			assert package.version_info == version
			assert package.external_refs.len == 1
			assert package.external_refs[0].reference_type == 'purl'
			assert package.external_refs[0].reference_locator == spdx_purl(name, version)
		}
	}
}

fn test_spdx_json_preserves_escaped_cpe_literals_without_wildcards() {
	mut registry := new_registry()
	registry.add_module(sbom_demo_info('a:b\\*?', 'release:*?\\tag', 'MIT', ''))
	document := json2.decode[SpdxDocument](registry.spdx_json(sbom_namespace, sbom_download_base))!
	package := document.packages[0]
	assert package.name == 'a:b\\*?'
	assert package.version_info == 'release:*?\\tag'
	assert package.external_refs.len == 2
	cpe := package.external_refs[1]
	assert cpe.reference_type == 'cpe23Type'
	assert cpe.reference_locator == 'cpe:2.3:a:vpm:a\\:b\\\\\\*\\?:release\\:\\*\\?\\\\tag:*:*:*:*:*:*:*'
}

// Preserve the registry policy of omitting a CPE for build-metadata versions;
// PURL still identifies the exact version.
fn test_a_build_metadata_version_omits_the_cpe() {
	mut r := new_registry()
	r.add_module(sbom_demo_info('serde', '1.0.0+build.7', 'MIT', 'sha256:${sbom_test_digest}'))
	doc := r.spdx_document(sbom_namespace, sbom_download_base)
	assert doc.packages[0].external_refs.len == 1
	assert doc.packages[0].external_refs[0].reference_type == 'purl'
}

fn test_a_normal_version_carries_both_external_refs() {
	r := new_sbom_demo_registry()
	doc := r.spdx_document(sbom_namespace, sbom_download_base)
	assert doc.packages[0].external_refs.len == 2
	kinds := doc.packages[0].external_refs.map(it.reference_type)
	assert 'purl' in kinds
	assert 'cpe23Type' in kinds
}

fn test_the_download_location_points_at_the_artifact() {
	r := new_sbom_demo_registry()
	doc := r.spdx_document(sbom_namespace, sbom_download_base)
	assert doc.packages[0].download_location == '${sbom_download_base}/serde/1.0.0.zip'
}

fn test_the_json_round_trips_through_a_decoder() {
	r := new_sbom_demo_registry()
	body := r.spdx_json(sbom_namespace, sbom_download_base)
	doc := json2.decode[SpdxDocument](body) or {
		assert false, 'could not decode the SPDX document we just encoded: ${err}'
		return
	}
	assert doc.packages.len == 2
	assert doc.packages[0].checksums[0].checksum_value == sbom_test_digest
}

fn test_the_sbom_endpoint_answers_json() {
	r := new_sbom_demo_registry()
	body := handle_request(r, 'GET', '/sbom.spdx.json', map[string]string{})
	doc := json2.decode[SpdxDocument](body) or {
		assert false, 'could not decode the SBOM endpoint body: ${err}'
		return
	}
	assert doc.spdx_version == 'SPDX-2.3'
	assert doc.packages.len == 2
}

fn test_default_sbom_namespaces_identify_each_document_across_registry_instances() {
	mut namespaces := map[string]bool{}
	// Identical metadata and back-to-back requests must not share an identity,
	// even when their creation timestamps happen to fall in the same second.
	for registry in [new_sbom_demo_registry(), new_sbom_demo_registry()] {
		for _ in 0 .. 8 {
			body := handle_request(registry, 'GET', '/sbom.spdx.json', map[string]string{})
			document := json2.decode[SpdxDocument](body)!
			namespace := document.document_namespace
			assert namespace.starts_with('urn:uuid:'), namespace
			identifier := uuid.parse(namespace)!
			assert identifier[6] >> 4 == 4
			assert identifier[8] >> 6 == 2
			assert namespace !in namespaces, 'different SPDX documents shared a namespace'
			namespaces[namespace] = true
		}
	}
	assert namespaces.len == 16
}

fn test_sbom_endpoint_preserves_an_explicit_namespace() {
	registry := new_sbom_demo_registry()
	for namespace in ['https://example.com/spdx/custom-document',
		'urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6'] {
		body := handle_request(registry, 'GET', '/sbom.spdx.json', {
			'namespace': namespace
		})
		document := json2.decode[SpdxDocument](body)!
		assert document.document_namespace == namespace
	}
}

fn test_fresh_default_sbom_document_does_not_revalidate_a_previous_document() {
	registry := new_sbom_demo_registry()
	first := serve(registry, 'GET', '/sbom.spdx.json', map[string]string{}, map[string]string{})
	assert first.status_code == 200
	assert first.etag != ''
	second := serve(registry, 'GET', '/sbom.spdx.json', map[string]string{}, {
		'If-None-Match': first.etag
	})
	assert second.status_code == 200
	assert second.etag != first.etag
	first_document := json2.decode[SpdxDocument](first.body)!
	second_document := json2.decode[SpdxDocument](second.body)!
	assert first_document.document_namespace != second_document.document_namespace
}

fn test_spdx_json_uses_the_standard_field_names() {
	r := new_sbom_demo_registry()
	body := r.spdx_json(sbom_namespace, sbom_download_base)
	doc := json2.decode[map[string]json2.Any](body)!
	for key in ['SPDXID', 'spdxVersion', 'dataLicense', 'documentNamespace', 'creationInfo'] {
		assert key in doc, body
	}
	assert 'spdxid' !in doc
	assert 'spdx_version' !in doc
	packages := doc['packages']! as []json2.Any
	for value in packages {
		package := value as map[string]json2.Any
		for key in ['SPDXID', 'versionInfo', 'downloadLocation', 'licenseConcluded', 'licenseDeclared',
			'copyrightText', 'filesAnalyzed', 'externalRefs'] {
			assert key in package, body
		}
		assert !(package['filesAnalyzed']! as bool)
		checksums := package['checksums']! as []json2.Any
		assert 'checksumValue' in (checksums[0] as map[string]json2.Any)
		refs := package['externalRefs']! as []json2.Any
		for reference in refs {
			ref := reference as map[string]json2.Any
			for key in ['referenceCategory', 'referenceType', 'referenceLocator'] {
				assert key in ref, body
			}
		}
	}
}
