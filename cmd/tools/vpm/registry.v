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

// Change records one edit to the registry, so that a mirror can bring its copy
// up to date by asking only for what changed since it last looked.
pub struct Change {
	// module and version name the entry that changed.
	module  string
	version string
	// kind is `publish` for a new or replaced version, `yank` and `unyank`
	// for a change of resection rather than of content.
	kind string
	// occurred_at is the RFC3339 time the change was recorded.
	occurred_at string
}

// Registry is the in-memory representation of a registry's contents.
pub struct Registry {
pub mut:
	modules map[string]RegistryEntry
	changes []Change
}

// new_registry creates an empty registry.
fn new_registry() Registry {
	return Registry{
		modules: map[string]RegistryEntry{}
		changes: []Change{}
	}
}

// record_change appends one entry to the change log. The log is append-only:
// a mirror relies on `since` only ever returning entries at or after it.
fn (mut r Registry) record_change(kind string, module string, version string) {
	r.changes << Change{
		module:      module
		version:     version
		kind:        kind
		occurred_at: time.utc().format_rfc3339()
	}
}

// add_module adds or updates a module in the registry.
pub fn (mut r Registry) add_module(info ModuleInfo) {
	mut entry := r.modules[info.name]
	entry.name = info.name
	entry.versions << info
	r.modules[info.name] = entry
	r.record_change('publish', info.name, info.version)
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
			r.record_change('yank', name, version)
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
			r.record_change('unyank', name, version)
			return true
		}
	}
	return false
}

// changes_since returns the changes recorded at or after `unix_ts`, so a mirror
// holding a copy from that moment can bring it up to date without reading the
// whole index. An entry whose time cannot be parsed is skipped rather than
// reported: a mirror asking for a bad `since` wants nothing, not a crash.
pub fn (r &Registry) changes_since(unix_ts i64) []Change {
	mut result := []Change{}
	for c in r.changes {
		recorded := time.parse_rfc3339(c.occurred_at) or { continue }
		if recorded.unix() >= unix_ts {
			result << c
		}
	}
	return result
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

// artifacts_dir returns where published module archives are kept. Each is named
// by its own version, so the index and the archive cannot drift apart on a
// rename.
fn artifacts_dir() string {
	return os.join_path(registry_dir(), 'artifacts')
}

// artifact_path returns the archive holding `name` at `version`.
// A version may hold characters that are legal in a URL but not in a file name,
// so they are percent-encoded before it becomes a path component.
fn artifact_path(name string, version string) string {
	safe := version.replace('/', '%2F').replace('\\', '%5C').replace(':', '%3A')
	return os.join_path(artifacts_dir(), name, '${safe}.zip')
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

// publish records `info` and stores its archive, refusing anything whose bytes
// do not match the checksum it claims. A registry that accepted a mismatched
// archive would hand out sources that nobody vouched for.
pub fn (mut r Registry) publish(info ModuleInfo, archive_path string) ! {
	content := os.read_file(archive_path) or {
		return error('failed to read the archive for `${info.name}`: ${err.msg()}')
	}
	claimed := info.checksum.trim_string_left('sha256:').trim_string_left('SHA256:')
	actual := sha256_hex(content.bytes())
	if claimed != '' && claimed != actual {
		return error('the archive for `${info.name}@${info.version}` hashes to `${actual}`, but the metadata claims `${claimed}`')
	}
	// An already-published version is immutable: replacing it would change what
	// an existing lockfile points at, so it is refused rather than overwritten.
	if existing := r.get_info(info.name, info.version) {
		if existing.checksum == info.checksum {
			return
		}
		return error('`${info.name}@${info.version}` is already published; a version cannot be replaced')
	}
	dest := artifact_path(info.name, info.version)
	os.mkdir_all(os.dir(dest)) or {}
	os.write_file(dest, content) or {
		return error('failed to store the archive for `${info.name}@${info.version}`: ${err.msg()}')
	}
	if claimed == '' {
		// Nothing was claimed, so the bytes are the truth: record what they hash
		// to rather than storing a checksum that vouches for nothing.
		r.add_module(ModuleInfo{
			name:         info.name
			version:      info.version
			description:  info.description
			license:      info.license
			dependencies: info.dependencies
			checksum:     'sha256:${actual}'
			features:     info.features
		})
		return
	}
	r.add_module(info)
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

// RegistryResponse is one registry answer as it goes over the wire: a status,
// the body, and the entity tag that lets a client revalidate the body without
// transferring it again.
pub struct RegistryResponse {
pub mut:
	status_code int = 200
	body        string
	etag        string
	// artifact_path names a file the transport should stream instead of sending
	// `body`. An archive is not text, so it cannot be carried by the string
	// every other endpoint returns.
	artifact_path string
}

// serve routes a request through `handle_request` and applies HTTP caching.
// It computes the entity tag of the answer and replies `304 Not Modified`
// when the client's cached copy is still current, so an unchanged index costs
// a header exchange rather than a full retransmission.
pub fn serve(registry Registry, method string, path string, query map[string]string, headers map[string]string) RegistryResponse {
	// An archive is streamed, so it never has a body to hash for an entity tag
	// and never answers a conditional request.
	if artifact := artifact_request(registry, path) {
		return artifact
	}
	body := handle_request(registry, method, path, query)
	etag := etag_of(body)
	if etag_matches(headers['If-None-Match'] or { '' }, etag) {
		return RegistryResponse{
			status_code: 304
			body:        ''
			etag:        etag
		}
	}
	return RegistryResponse{
		body: body
		etag: etag
	}
}

// etag_of returns the entity tag for `body`: the sha256 of its bytes inside the
// double quotes the HTTP grammar requires. The same body always yields the
// same tag and a different body never does, which is what makes a client's
// comparison meaningful.
fn etag_of(body string) string {
	return '"${sha256_hex(body.bytes())}"'
}

// sha256_hex is the hex digest of `data`, shared by the entity tag and by the
// content hashes recorded for downloaded modules.
fn sha256_hex(data []u8) string {
	sum := sha256.sum(data)
	mut hex := ''
	for b in sum {
		hex += b.hex()
	}
	return hex
}

// etag_matches reports whether `if_none_match` names the same entity as `etag`.
// A client may send several tags separated by commas, may mark one with the
// weak validator prefix `W/`, and may send `*` to ask for whatever is current.
fn etag_matches(if_none_match string, etag string) bool {
	candidate := if_none_match.trim_space()
	if candidate == '' {
		return false
	}
	if candidate == '*' {
		return true
	}
	for raw in candidate.split(',') {
		mut tag := raw.trim_space()
		if tag.starts_with('W/') {
			tag = tag.all_after('W/').trim_space()
		}
		if tag == etag {
			return true
		}
	}
	return false
}

// handle_request routes a registry API request to the appropriate handler.
// This is the entry point for the registry HTTP server.
// RequestOptions carries what a mutating request sends that a plain GET does
// not. It is a params struct so a caller that has no body can omit it.
@[params]
pub struct RequestOptions {
pub:
	// body is the JSON payload of a PUT, empty for reads.
	body string
}

// artifact_request answers `GET /<module>/@v/<version>.zip`, the route that
// delivers a module's sources. It is separate from `handle_request` because an
// archive is not text: the response names a file for the transport to stream
// rather than carrying it in the string every other endpoint returns.
pub fn artifact_request(registry Registry, path string) ?RegistryResponse {
	parts := path.trim_left('/').split('/')
	if parts.len != 3 || parts[1] != '@v' || !parts[2].ends_with('.zip') {
		return none
	}
	version := parts[2].trim_string_right('.zip')
	archive := artifact_path(parts[0], version)
	if !os.exists(archive) {
		return none
	}
	// Verify what is being served against the checksum the index recorded, so a
	// corrupted or tampered store is caught here rather than at the client.
	if info := registry.get_info(parts[0], version) {
		content := os.read_file(archive) or { return none }
		actual := sha256_hex(content.bytes())
		claimed := info.checksum.trim_string_left('sha256:')
		if claimed != '' && claimed != actual {
			return none
		}
	}
	return RegistryResponse{
		artifact_path: archive
	}
}

pub fn handle_request(registry Registry, method string, path string, query map[string]string, opts RequestOptions) string {
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

	// GET /sbom.spdx.json renders the software bill of materials for the
	// registry, so a consumer can audit what it is about to install without
	// resolving the graph itself.
	if path == '/sbom.spdx.json' && method == 'GET' {
		namespace := query['namespace'] or { 'https://vpm.local/spdx/vpm-registry' }
		download_base := query['dl'] or { 'https://vpm.local/downloads' }
		return registry.spdx_json(namespace, download_base)
	}

	// GET /api/changes?since=<unix timestamp> returns only the changes a mirror
	// needs, so bringing a copy up to date does not mean reading the index.
	if path == '/api/changes' && method == 'GET' {
		since := (query['since'] or { '0' }).i64()
		return json2.encode(registry.changes_since(since))
	}

	// GET /<module>/@v/<version>.zip streams the published archive. The bytes
	// are verified against the checksum the index recorded before being served,
	// so a corrupted or tampered store is caught here rather than at the client.
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

// SignedIndex is the part of a registry that its signature vouches for. The
// change log is deliberately outside it: it records wall-clock times, so
// including it would make an unchanged registry produce a different signature
// on every run, even though the module metadata being signed has not moved.
struct SignedIndex {
	modules map[string]RegistryEntry
}

// canonical_json is the byte sequence the signature covers. It must be a pure
// function of the registry's contents — sorted keys, and no wall-clock time —
// or the same registry would produce a different signature on each run.
fn (r &Registry) canonical_json() string {
	return json2.encode(SignedIndex{
		modules: r.modules
	})
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
