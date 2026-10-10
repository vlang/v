module main

// SPDX is the software bill of materials a registry can produce for a
// resolution. The field names follow SPDX 2.3 so that the output can be read by
// the tools that consume it, rather than by V alone.

import json2
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
	// A PURL percent-encodes anything outside its allowed set; `/` is the
	// separator, so a name containing one would otherwise change the shape.
	encoded := name.replace('/', '%2F')
	return 'pkg:vpm/${encoded}@${version}'
}

// spdx_cpe is a CPE 2.3 name, which is how vulnerability databases refer to a
// package. CPE requires backslashes and colons be escaped in the product part.
fn spdx_cpe(name string, version string) string {
	escaped := name.replace('\\', '\\5c').replace(':', '\\:')
	return 'cpe:2.3:a:vpm:${escaped}:${version}:*:*:*:*:*:*:*'
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
			if v.checksum.starts_with('sha256:') {
				checksums << SpdxChecksum{
					algorithm:      'SHA256'
					checksum_value: v.checksum.all_after('sha256:')
				}
			}
			mut refs := []SpdxExternalRef{}
			refs << SpdxExternalRef{
				reference_category: 'PACKAGE-MANAGER'
				reference_type:     'purl'
				reference_locator:  spdx_purl(v.name, v.version)
			}
			// CPE names a product for vulnerability tracking; a version holding
			// a `+` in it is a build-metadata version that CPE cannot express.
			if !v.version.contains('+') {
				refs << SpdxExternalRef{
					reference_category: 'SECURITY'
					reference_type:     'cpe23Type'
					reference_locator:  spdx_cpe(v.name, v.version)
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
