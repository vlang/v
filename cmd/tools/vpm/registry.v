// Registry protocol implementation for VPM.
// Defines the API endpoints and metadata format for VPM registries.
module main

import crypto.ed25519
import crypto.sha256
import encoding.hex as hexcode
import json2
import os
import semver
import time

const registry_key_env = 'VPM_REGISTRY_KEY'
const registry_key_file_name = 'signing.key'
const registry_signature_file_name = 'signature.sig'

// RegistryConfig is the configuration document served at the registry root.
// It tells clients where to find artifacts and whether authentication is required.
pub struct RegistryConfig {
pub:
	// dl is the base URL for downloading module artifacts.
	dl string
	// api is the base URL for the registry API (publishing, search, etc.).
	api string
	// auth_required indicates whether all operations require authentication.
	auth_required bool
	// public_key is the hex-encoded ed25519 key that vouches for the metadata
	// served by this registry. A mirror cannot be forged against it, because it
	// holds no key of its own to sign with.
	public_key string
}

// ModuleInfo is the metadata for one version of a module.
pub struct ModuleInfo {
pub:
	name         string
	version      string
	description  string
	license      string
	dependencies map[string]string
	checksum     string
	published_at string
	features     map[string][]string
pub mut:
	yanked bool
}

// RegistryEntry represents one entry in the registry index.
pub struct RegistryEntry {
pub mut:
	name     string
	versions []ModuleInfo
}

// Registry is the in-memory representation of a registry's contents.
pub struct Registry {
pub mut:
	modules map[string]RegistryEntry
}

// new_registry creates an empty registry.
fn new_registry() Registry {
	return Registry{
		modules: map[string]RegistryEntry{}
	}
}

// add_module adds or updates a module in the registry.
pub fn (mut r Registry) add_module(info ModuleInfo) {
	mut entry := r.modules[info.name]
	entry.name = info.name
	entry.versions << info
	r.modules[info.name] = entry
}

// list_versions returns all versions of a module, sorted by semver descending.
pub fn (r &Registry) list_versions(name string) []string {
	entry := r.modules[name] or { return [] }
	mut versions := entry.versions.clone()
	versions.sort_with_compare(fn (a &ModuleInfo, b &ModuleInfo) int {
		va := semver.from(a.version) or { semver.Version{} }
		vb := semver.from(b.version) or { semver.Version{} }
		if va > vb {
			return -1
		}
		if va < vb {
			return 1
		}
		return 0
	})
	return versions.map(it.version)
}

// get_info returns the metadata for a specific version of a module.
pub fn (r &Registry) get_info(name string, version string) ?ModuleInfo {
	entry := r.modules[name] or { return none }
	for v in entry.versions {
		if v.version == version {
			return v
		}
	}
	return none
}

// get_latest returns the highest non-yanked version of a module.
pub fn (r &Registry) get_latest(name string) ?ModuleInfo {
	entry := r.modules[name] or { return none }
	mut candidates := entry.versions.filter(!it.yanked)
	if candidates.len == 0 {
		return none
	}
	candidates.sort_with_compare(fn (a &ModuleInfo, b &ModuleInfo) int {
		va := semver.from(a.version) or { semver.Version{} }
		vb := semver.from(b.version) or { semver.Version{} }
		if va > vb {
			return -1
		}
		if va < vb {
			return 1
		}
		return 0
	})
	return candidates[0]
}

// yank marks a version as yanked.
pub fn (mut r Registry) yank(name string, version string) bool {
	mut entry := r.modules[name] or { return false }
	for mut v in entry.versions {
		if v.version == version {
			v.yanked = true
			r.modules[name] = entry
			return true
		}
	}
	return false
}

// unyank unmarks a version as yanked.
pub fn (mut r Registry) unyank(name string, version string) bool {
	mut entry := r.modules[name] or { return false }
	for mut v in entry.versions {
		if v.version == version {
			v.yanked = false
			r.modules[name] = entry
			return true
		}
	}
	return false
}

// search returns modules whose name or description matches the query.
pub fn (r &Registry) search(query string) []RegistryEntry {
	mut results := []RegistryEntry{}
	for _, entry in r.modules {
		if entry.name.contains(query) {
			results << entry
		}
	}
	return results
}

// compute_checksum computes the SHA256 checksum of a file's contents.
fn compute_checksum(path string) string {
	content := os.read_file(path) or { return '' }
	sum := sha256.sum(content.bytes())
	mut hex := ''
	for b in sum {
		hex += b.hex()
	}
	return hex
}

// export_json exports the registry as JSON.
pub fn (r &Registry) export_json() string {
	return json2.encode(r, prettify: true)
}

// import_json imports a registry from JSON.
pub fn import_json(data string) !Registry {
	r := json2.decode[Registry](data) or {
		return error('failed to parse registry JSON: ${err.msg()}')
	}
	return r
}

// registry_dir returns the directory where registry data is stored.
fn registry_dir() string {
	return os.join_path(os.getwd(), '.vpm-registry')
}

// registry_index_path returns the path to the registry index file.
fn registry_index_path() string {
	return os.join_path(registry_dir(), 'index.json')
}

// registry_key_path returns the path to the registry's signing key, kept
// beside the index and never served. The environment variable takes
// precedence so a registry can be signed without writing a key to disk.
fn registry_key_path() string {
	return os.join_path(registry_dir(), registry_key_file_name)
}

// save persists the registry to disk.
pub fn (r &Registry) save() ! {
	dir := registry_dir()
	os.mkdir_all(dir) or {
		return error('failed to create registry directory: ${err.msg()}')
	}
	os.write_file(registry_index_path(), r.export_json()) or {
		return error('failed to write registry index: ${err.msg()}')
	}
}

// load_registry loads the registry from disk, or returns an empty registry.
pub fn load_registry() Registry {
	data := os.read_file(registry_index_path()) or { return new_registry() }
	return import_json(data) or { new_registry() }
}

// handle_request routes a registry API request to the appropriate handler.
// This is the entry point for the registry HTTP server.
pub fn handle_request(registry Registry, method string, path string, query map[string]string) string {
	parts := path.trim_left('/').split('/')
	if parts.len == 0 {
		return '{"error": "not found"}'
	}

	// GET /config.json
	if path == '/config.json' && method == 'GET' {
		return json2.encode(RegistryConfig{
			dl:            'https://example.com/downloads'
			api:           'https://example.com/api'
			auth_required: false
		})
	}

	// GET /<module>/@v/list
	if parts.len == 3 && parts[1] == '@v' && parts[2] == 'list' && method == 'GET' {
		versions := registry.list_versions(parts[0])
		return json2.encode(versions)
	}

	// GET /<module>/@v/<version>.info
	if parts.len == 3 && parts[1] == '@v' && parts[2].ends_with('.info') && method == 'GET' {
		version := parts[2].trim_string_right('.info')
		info := registry.get_info(parts[0], version) or {
			return '{"error": "not found"}'
		}
		return json2.encode(info)
	}

	// GET /<module>/@latest
	if parts.len == 2 && parts[1] == '@latest' && method == 'GET' {
		info := registry.get_latest(parts[0]) or {
			return '{"error": "not found"}'
		}
		return json2.encode(info)
	}
	// GET /api/search?q=<query>
	if path == '/api/search' && method == 'GET' {
		q := query['q'] or { '' }
		results := registry.search(q)
		return json2.encode(results)
	}

	// GET /api/modules/<module>
	if parts.len == 3 && parts[0] == 'api' && parts[1] == 'modules' && method == 'GET' {
		entry := registry.modules[parts[2]] or {
			return '{"error": "not found"}'
		}
		return json2.encode(entry)
	}

	// GET /signature.sig serves the ed25519 signature over the registry index.
	// A client that knows the public key can tell a mirror apart from the
	// origin, because a mirror has no key to sign with.
	if path == '/${registry_signature_file_name}' && method == 'GET' {
		return json2.encode(registry.signature())
	}

	return '{"error": "not found"}'
}

// signing_key loads the registry's private key seed, from `VPM_REGISTRY_KEY`
// when set and from the key file beside the index otherwise. Both hold hex.
fn (r &Registry) signing_key() ?ed25519.PrivateKey {
	raw := os.getenv_opt(registry_key_env) or { os.read_file(registry_key_path()) or { return none } }
	hex := raw.trim_space()
	if hex == '' {
		return none
	}
	seed := hexcode.decode(hex) or { return none }
	if seed.len != ed25519.seed_size {
		return none
	}
	return ed25519.new_key_from_seed(seed)
}

// public_key_hex returns the hex-encoded public key that vouches for this
// registry, or '' when it holds no signing key.
pub fn (r &Registry) public_key_hex() string {
	key := r.signing_key() or { return '' }
	return key.public_key().hex()
}

// canonical_json is the byte sequence the signature covers. It must be a pure
// function of the registry's contents -- sorted keys, and no wall-clock time --
// or the same registry would produce a different signature on each run.
fn (r &Registry) canonical_json() string {
	return r.export_json()
}

// signature signs the canonical form of the index and returns a hex-encoded
// ed25519 signature. An unsigned registry returns '', which a client reads as
// "this registry does not vouch for itself".
pub fn (r &Registry) signature() string {
	key := r.signing_key() or { return '' }
	sig := ed25519.sign(key, r.canonical_json().bytes()) or { return '' }
	return sig.hex()
}

// verify_signature reports whether `sig_hex` over this registry's canonical
// form was produced by the holder of `public_key_hex`. A registry whose
// contents were edited after signing fails, because the canonical form no
// longer matches what was signed.
pub fn (r &Registry) verify_signature(public_key_hex string, sig_hex string) bool {
	public_bytes := hexcode.decode(public_key_hex) or { return false }
	sig := hexcode.decode(sig_hex) or { return false }
	return ed25519.verify(public_bytes, r.canonical_json().bytes(), sig) or { false }
}
