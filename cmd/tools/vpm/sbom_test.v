module main

import json2

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

const sbom_namespace = 'https://vpm.example.com/spdx'
const sbom_download_base = 'https://vpm.example.com/dl'

fn new_sbom_demo_registry() Registry {
	mut r := new_registry()
	r.add_module(sbom_demo_info('serde', '1.0.0', 'MIT', 'sha256:abc123'))
	r.add_module(sbom_demo_info('my/lib', '2.0.0', 'Apache-2.0', 'sha256:abc123'))
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
	r.add_module(sbom_demo_info('serde', '1.0.0', 'MIT', 'sha256:abc123'))
	r.add_module(sbom_demo_info('serde', '1.1.0', 'MIT', 'sha256:abc123'))
	r.add_module(sbom_demo_info('other', '2.0.0', 'MIT', 'sha256:abc123'))
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
	assert doc.packages[0].checksums[0].checksum_value == 'abc123'
}

// A module with no recorded checksum must not produce an empty digest entry.
fn test_an_absent_checksum_yields_no_entry() {
	mut r := new_registry()
	r.add_module(sbom_demo_info('serde', '1.0.0', 'MIT', ''))
	doc := r.spdx_document(sbom_namespace, sbom_download_base)
	assert doc.packages[0].checksums.len == 0
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

fn test_cpe_is_well_formed() {
	assert spdx_cpe('serde', '1.0.0') == 'cpe:2.3:a:vpm:serde:1.0.0:*:*:*:*:*:*:*'
	// CPE requires `:` be escaped in the product part.
	assert spdx_cpe('a:b', '1.0.0').contains('a\\:b')
}

// A build-metadata version cannot be expressed as a CPE, so it is omitted
// rather than emitted as something malformed.
fn test_a_build_metadata_version_omits_the_cpe() {
	mut r := new_registry()
	r.add_module(sbom_demo_info('serde', '1.0.0+build.7', 'MIT', 'sha256:abc123'))
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
	assert doc.packages[0].checksums[0].checksum_value == 'abc123'
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
