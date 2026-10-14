module main

// SPDX is the software bill of materials a registry can produce for a
// resolution. The field names follow SPDX 2.3 so that the output can be read by
// the tools that consume it, rather than by V alone.

import json2
import net.urllib
import time

// The SPDX specification this document declares itself against.
const spdx_version = 'SPDX-2.3'

// SPDX data are published under CC0-1.0, which frees a consumer from the
// document's own licensing terms while the packages it describes keep theirs.
const spdx_data_license = 'CC0-1.0'

// An SpdxChecksum is one digest over one package.
struct SpdxChecksum {
	algorithm      string
	checksum_value string @[json: 'checksumValue']
}

// An SpdxExternalRef points from a package at an identifier in another system,
// such as a CPE naming a vulnerability-tracking entry.
struct SpdxExternalRef {
	reference_category string @[json: 'referenceCategory']
	reference_type     string @[json: 'referenceType']
	reference_locator  string @[json: 'referenceLocator']
}

// An SpdxPackage is one module version in the bill of materials.
struct SpdxPackage {
	name              string
	spdxid            string @[json: 'SPDXID']
	version_info      string @[json: 'versionInfo']
	download_location string @[json: 'downloadLocation']
	license_concluded string @[json: 'licenseConcluded']
	license_declared  string @[json: 'licenseDeclared']
	copyright_text    string @[json: 'copyrightText']
	files_analyzed    bool   @[json: 'filesAnalyzed']
	checksums         []SpdxChecksum
	external_refs     []SpdxExternalRef @[json: 'externalRefs']
}

// An SpdxDocument is a complete bill of materials.
struct SpdxDocument {
	spdx_version       string @[json: 'spdxVersion']
	data_license       string @[json: 'dataLicense']
	spdxid             string @[json: 'SPDXID']
	name               string
	document_namespace string           @[json: 'documentNamespace']
	creation_info      SpdxCreationInfo @[json: 'creationInfo']
	packages           []SpdxPackage
}

struct SpdxCreationInfo {
	created  string
	creators []string
}

// spdx_id_for identifies one package version without conflating names or versions.
fn spdx_id_for(name string, version string) string {
	return 'SPDXRef-Package-${name.bytes().hex()}-${version.bytes().hex()}'
}

// spdx_purl is the package URL for a module version. PURL is the identifier
// other supply-chain tools use, so a consumer can join this bill of materials
// against its own without knowing V's registry layout.
fn spdx_purl(name string, version string) string {
	// PURL components preserve unreserved characters and colons. Query escaping
	// encodes other bytes; spaces must use %20 rather than form encoding's +.
	encoded_name := urllib.query_escape(name).replace('+', '%20').replace('%3A', ':')
	encoded_version := urllib.query_escape(version).replace('+', '%20').replace('%3A', ':')
	return 'pkg:vpm/${encoded_name}@${encoded_version}'
}

// spdx_cpe_component binds literal printable ASCII to the NISTIR 7695
// formatted-string syntax. Empty and whole '-' cannot name these literals;
// spaces, controls and non-ASCII characters are outside its component grammar.
fn spdx_cpe_component(value string) ?string {
	if value == '' || value == '-' {
		return none
	}
	mut escaped := []u8{}
	for c in value {
		if c <= 32 || c >= 127 {
			return none
		}
		if !c.is_alnum() && c !in [`-`, `.`, `_`] {
			escaped << `\\`
		}
		escaped << c
	}
	return escaped.bytestr()
}

// spdx_cpe emits literal product and version components, never wildcard
// interpretations. Keep the existing conservative omission of build metadata.
fn spdx_cpe(name string, version string) ?string {
	if version.contains('+') {
		return none
	}
	product := spdx_cpe_component(name) or { return none }
	product_version := spdx_cpe_component(version) or { return none }
	return 'cpe:2.3:a:vpm:${product}:${product_version}:*:*:*:*:*:*:*'
}

// spdx_sha256_checksum accepts the same prefix trimming as artifact integrity
// checks, and emits only the lowercase 64-hex digest those checks can accept.
fn spdx_sha256_checksum(recorded string) ?string {
	digest := recorded.trim_string_left('sha256:').trim_string_left('SHA256:')
	if digest.len != 64 {
		return none
	}
	for c in digest {
		if !c.is_digit() && (c < `a` || c > `f`) {
			return none
		}
	}
	return digest
}

// spdx_document renders the bill of materials for this registry's modules.
// `namespace` should be unique per document, conventionally a URL under the
// producing organisation's control, and `download_base` is the registry's
// artifact base, which becomes each package's download location.
pub fn (r &Registry) spdx_document(namespace string, download_base string) SpdxDocument {
	mut packages := []SpdxPackage{}
	for _, entry in r.modules {
		for v in entry.versions {
			mut checksums := []SpdxChecksum{}
			if digest := spdx_sha256_checksum(v.checksum) {
				checksums << SpdxChecksum{
					algorithm:      'SHA256'
					checksum_value: digest
				}
			}
			mut refs := []SpdxExternalRef{}
			refs << SpdxExternalRef{
				reference_category: 'PACKAGE-MANAGER'
				reference_type:     'purl'
				reference_locator:  spdx_purl(v.name, v.version)
			}
			if cpe := spdx_cpe(v.name, v.version) {
				refs << SpdxExternalRef{
					reference_category: 'SECURITY'
					reference_type:     'cpe23Type'
					reference_locator:  cpe
				}
			}
			packages << SpdxPackage{
				name:              v.name
				spdxid:            spdx_id_for(v.name, v.version)
				version_info:      v.version
				download_location: '${download_base}/${v.name}/${v.version}.zip'
				license_concluded: if v.license == '' { 'NOASSERTION' } else { v.license }
				license_declared:  if v.license == '' { 'NOASSERTION' } else { v.license }
				copyright_text:    'NOASSERTION'
				checksums:         checksums
				external_refs:     refs
			}
		}
	}
	return SpdxDocument{
		spdx_version:       spdx_version
		data_license:       spdx_data_license
		spdxid:             'SPDXRef-DOCUMENT'
		name:               'vpm-registry'
		document_namespace: namespace
		creation_info:      SpdxCreationInfo{
			created:  time.utc().format_rfc3339()
			creators: ['Tool: vpm']
		}
		packages:           packages
	}
}

// spdx_json renders the bill of materials as JSON.
pub fn (r &Registry) spdx_json(namespace string, download_base string) string {
	return json2.encode(r.spdx_document(namespace, download_base), prettify: true)
}
