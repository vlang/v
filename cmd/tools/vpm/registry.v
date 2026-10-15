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
import uuid

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
	// auth_required is served in `/config.json` for a client to parse. This
	// registry authenticates nothing: the field is hardcoded to `false` and no
	// code reads it, so it describes no behaviour of this server.
	auth_required bool
	// public_key advertises the registry's hex-encoded ed25519 signing key.
	// This document is not a trust root: a mirror can replace both the key and
	// the signature. Verifiers must obtain their trusted key independently.
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

// compare_registry_versions orders semver-parser versions first, then opaque versions.
// SemVer precedence is descending; raw spelling breaks ties and orders opaque values.
fn compare_registry_versions(a &ModuleInfo, b &ModuleInfo) int {
	mut a_valid := true
	mut b_valid := true
	va := semver.from(a.version) or {
		a_valid = false
		semver.Version{}
	}
	vb := semver.from(b.version) or {
		b_valid = false
		semver.Version{}
	}
	if a_valid != b_valid {
		return if a_valid { -1 } else { 1 }
	}
	if a_valid {
		if va > vb {
			return -1
		}
		if va < vb {
			return 1
		}
	}
	if a.version > b.version {
		return -1
	}
	if a.version < b.version {
		return 1
	}
	return 0
}

// list_versions returns versions in descending SemVer precedence before opaque values.
// Opaque versions and equal-precedence SemVer spellings use descending lexical order.
pub fn (r &Registry) list_versions(name string) []string {
	entry := r.modules[name] or { return [] }
	mut versions := entry.versions.clone()
	versions.sort_with_compare(compare_registry_versions)
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

// get_latest returns the first non-yanked version in the same order as list_versions.
pub fn (r &Registry) get_latest(name string) ?ModuleInfo {
	entry := r.modules[name] or { return none }
	mut candidates := entry.versions.filter(!it.yanked)
	if candidates.len == 0 {
		return none
	}
	candidates.sort_with_compare(compare_registry_versions)
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

// import_json imports a registry from JSON, rejecting unroutable or inconsistent
// module identities before they become reachable through the registry API.
pub fn import_json(data string) !Registry {
	r := json2.decode[Registry](data) or {
		return error('failed to parse registry JSON: ${err.msg()}')
	}
	r.validate_registry_entries()!
	return r
}

fn (r &Registry) validate_registry_entries() ! {
	mut seen_names := map[string]string{}
	for name, entry in r.modules {
		if !registry_artifact_component(name) {
			return error('invalid registry module key `${name}`')
		}
		folded_name := name.to_lower_ascii()
		if previous := seen_names[folded_name] {
			return error('case-insensitive registry module collision between `${previous}` and `${name}`')
		}
		seen_names[folded_name] = name
		if !registry_artifact_component(entry.name) || entry.name != name {
			return error('registry entry name `${entry.name}` does not match module key `${name}`')
		}
		mut seen_versions := map[string]bool{}
		mut folded_versions := map[string]string{}
		for info in entry.versions {
			if !registry_artifact_component(info.name) || info.name != name {
				return error('registry version module name `${info.name}` does not match module key `${name}`')
			}
			if !registry_artifact_component(info.version) {
				return error('invalid registry version `${info.version}` for module `${name}`')
			}
			if info.version in seen_versions {
				return error('duplicate registry version `${info.version}` for module `${name}`')
			}
			folded_version := info.version.to_lower_ascii()
			if previous := folded_versions[folded_version] {
				return error('case-insensitive registry version collision between `${previous}` and `${info.version}` for module `${name}`')
			}
			seen_versions[info.version] = true
			folded_versions[folded_version] = info.version
		}
	}
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

// registry_artifact_component accepts a filesystem component whose spelling
// survives the HTTP client's raw path handling without URL escapes or delimiters.
fn registry_artifact_component(value string) bool {
	if value == '' || value in ['.', '..'] || value.ends_with('.') {
		return false
	}
	for c in value {
		if !c.is_alnum() && c !in [`-`, `.`, `_`, `~`, `!`, `$`, `&`, `'`, `(`, `)`, `*`, `+`,
			`,`, `;`, `=`, `@`, `[`, `]`] {
			return false
		}
	}
	return true
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

// Artifact paths retain their published spelling, so aliases must be refused
// before writing on filesystems that cannot distinguish ASCII letter case.
fn (r &Registry) check_artifact_casefold(name string, version string) ! {
	folded_name := name.to_lower_ascii()
	folded_version := version.to_lower_ascii()
	for existing_name, entry in r.modules {
		if existing_name.to_lower_ascii() != folded_name {
			continue
		}
		if existing_name != name {
			return error('case-insensitive registry module collision between `${existing_name}` and `${name}`')
		}
		for info in entry.versions {
			if info.version != version && info.version.to_lower_ascii() == folded_version {
				return error('case-insensitive registry version collision between `${info.version}` and `${version}` for module `${name}`')
			}
		}
	}
}

// publish records `info` and stores its archive, refusing anything whose bytes
// do not match the checksum it claims. A registry that accepted a mismatched
// archive would hand out sources that nobody vouched for.
pub fn (mut r Registry) publish(info ModuleInfo, archive_path string) ! {
	if !registry_artifact_component(info.name) || !registry_artifact_component(info.version) {
		return error('module name and version must be nonempty artifact path components')
	}
	r.check_artifact_casefold(info.name, info.version)!
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
		if existing.checksum.trim_string_left('sha256:').trim_string_left('SHA256:') == actual {
			// A retry replaces and synchronizes verified bytes even when they
			// already match: a previous rename may have failed its directory sync.
			// Published metadata, yank state, and change history remain unchanged.
			r.store_registry_artifact(info.name, info.version, content)!
			return
		}
		return error('`${info.name}@${info.version}` is already published; a version cannot be replaced')
	}
	r.store_registry_artifact(info.name, info.version, content)!
	// Record verified bytes with the spelling used by downloads and SPDX consumers.
	r.add_module(ModuleInfo{
		...info
		checksum: 'sha256:${actual}'
	})
}

fn (r &Registry) store_registry_artifact(name string, version string, content string) ! {
	dest := artifact_path(name, version)
	registry_mkdir_all(os.dir(dest)) or {
		return error('failed to create artifact directory: ${err.msg()}')
	}
	registry_store_file(dest, content) or {
		return error('failed to store the archive for `${name}@${version}`: ${err.msg()}')
	}
}

// save persists a complete registry through an atomic same-directory replacement.
// Invalid in-memory identities are rejected before any filesystem changes.
// A failed write, file synchronization, close, or rename leaves the previous
// index intact. A later directory synchronization error reports uncertain durability.
pub fn (r &Registry) save() ! {
	r.validate_registry_entries()!
	dir := registry_dir()
	registry_mkdir_all(dir) or {
		return error('failed to create registry directory: ${err.msg()}')
	}
	registry_store_file(registry_index_path(), r.export_json()) or {
		return error('failed to write registry index: ${err.msg()}')
	}
}

// load_registry loads the registry from disk, returning an empty registry only
// when its index does not exist. Existing unreadable or invalid indexes fail.
pub fn load_registry() !Registry {
	dir := registry_dir()
	os.lstat(dir) or {
		// Windows lstat can report a dangling directory link as missing.
		if os.is_not_exist(err) && !os.is_link(dir) {
			return new_registry()
		}
		return error('failed to inspect registry directory `${dir}`: ${err.msg()}')
	}
	dir_stat := os.stat(dir) or {
		return error('failed to inspect registry directory `${dir}`: ${err.msg()}')
	}
	if dir_stat.get_filetype() != .directory {
		return error('registry directory `${dir}` is not a directory')
	}
	path := registry_index_path()
	// lstat distinguishes a missing index from an existing dangling symlink.
	os.lstat(path) or {
		if os.is_not_exist(err) && !os.is_link(path) {
			return new_registry()
		}
		return error('failed to inspect registry index `${path}`: ${err.msg()}')
	}
	data := os.read_file(path) or {
		return error('failed to read registry index `${path}`: ${err.msg()}')
	}
	return import_json(data) or {
		return error('failed to load registry index `${path}`: ${err.msg()}')
	}
}

// RegistryResponse is one registry answer as it goes over the wire: a status,
// the body, and the entity tag that lets a client revalidate the body without
// transferring it again.
pub struct RegistryResponse {
pub mut:
	status_code int = 200
	body        string
	etag        string
	// artifact_path identifies a binary archive response. Its verified bytes
	// are already in body, so transports must not reopen this path.
	artifact_path string
}

// serve routes a request through `handle_request` and applies HTTP caching.
// It computes the entity tag of the answer and replies `304 Not Modified`
// when the client's cached copy is still current, so an unchanged index costs
// a header exchange rather than a full retransmission.
pub fn serve(registry Registry, method string, path string, query map[string]string, headers map[string]string) RegistryResponse {
	// An archive already carries verified bytes and bypasses metadata ETag hashing
	// and conditional responses.
	if method == 'GET' {
		if artifact := artifact_request(registry, path) {
			return artifact
		}
	}
	mut resp := route(registry, method, path, query)
	etag := etag_of(resp.body)
	if method == 'GET' && resp.status_code == 200
		&& etag_matches(headers['If-None-Match'] or { '' }, etag) {
		return RegistryResponse{
			status_code: 304
			body:        ''
			etag:        etag
		}
	}
	resp.etag = etag
	return resp
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
// archive carries binary bytes in body with artifact_path identifying the content type.
// Verification reads the file once, and the transport uses that exact buffer.
pub fn artifact_request(registry Registry, path string) ?RegistryResponse {
	parts := path.trim_left('/').split('/')
	if parts.len != 3 || parts[1] != '@v' || !parts[2].ends_with('.zip') {
		return none
	}
	version := parts[2].trim_string_right('.zip')
	if !registry_artifact_component(parts[0]) || !registry_artifact_component(version) {
		return none
	}
	archive := artifact_path(parts[0], version)
	if !os.exists(archive) {
		return none
	}
	content := read_registry_artifact(registry, parts[0], version, archive) or { return none }
	return RegistryResponse{
		body:          content
		artifact_path: archive
	}
}

// read_registry_artifact verifies the exact bytes returned to an archive transport.
fn read_registry_artifact(registry Registry, name string, version string, archive string) ?string {
	info := registry.get_info(name, version) or { return none }
	content := os.read_file(archive) or { return none }
	actual := sha256_hex(content.bytes())
	claimed := info.checksum.trim_string_left('sha256:').trim_string_left('SHA256:')
	if claimed == '' || claimed != actual {
		return none
	}
	return content
}

// not_found_body is what the router answers for anything it does not serve.
// `route` maps exactly this body to 404, so the two halves share one contract:
// a client tells "not found" from success by the status line rather than by
// parsing the body. Any other error the router can produce still needs a status
// of its own, and adding one means giving `route` the case to map it.
const not_found_body = '{"error": "not found"}'

// route answers one request through the router and gives it a status. The
// router itself returns text; this is where that text becomes an HTTP response.
pub fn route(registry Registry, method string, path string, query map[string]string, opts RequestOptions) RegistryResponse {
	body := handle_request(registry, method, path, query, opts)
	status_code := if body == not_found_body { 404 } else { 200 }
	return RegistryResponse{
		status_code: status_code
		body:        body
	}
}

// handle_request routes a registry API request to the appropriate handler.
// This is the entry point for the registry HTTP server.
pub fn handle_request(registry Registry, method string, path string, query map[string]string, opts RequestOptions) string {
	parts := path.trim_left('/').split('/')
	if parts.len == 0 {
		return not_found_body
	}

	// GET /config.json
	if path == '/config.json' && method == 'GET' {
		return json2.encode(RegistryConfig{
			dl:            'https://example.com/downloads'
			api:           'https://example.com/api'
			auth_required: false
			// Discovery only; clients must pin a key obtained independently.
			public_key:    registry.public_key_hex()
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
			return not_found_body
		}
		return json2.encode(info)
	}

	// GET /<module>/@latest
	if parts.len == 2 && parts[1] == '@latest' && method == 'GET' {
		info := registry.get_latest(parts[0]) or {
			return not_found_body
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
			return not_found_body
		}
		return json2.encode(entry)
	}

	// GET /sbom.spdx.json renders the software bill of materials for the
	// registry, so a consumer can audit what it is about to install without
	// resolving the graph itself.
	if path == '/sbom.spdx.json' && method == 'GET' {
		namespace := query['namespace'] or { 'urn:uuid:${uuid.new_v4().str()}' }
		download_base := query['dl'] or { 'https://vpm.local/downloads' }
		return registry.spdx_json(namespace, download_base)
	}

	// GET /api/changes?since=<unix timestamp> returns only the changes a mirror
	// needs, so bringing a copy up to date does not mean reading the index.
	if path == '/api/changes' && method == 'GET' {
		since := (query['since'] or { '0' }).i64()
		return json2.encode(registry.changes_since(since))
	}

	// GET /<module>/@v/<version>.zip returns buffered archive bytes verified
	// against the checksum recorded in the index. Transport sends those same
	// bytes, so a changed file cannot replace the verified response contents.
	// GET /signature.sig serves the ed25519 signature over the registry index.
	// A verifier must pin the origin's public key through a trusted external
	// channel. A mirror can sign substituted contents using its own key.
	if path == '/${registry_signature_file_name}' && method == 'GET' {
		return json2.encode(registry.signature())
	}

	return not_found_body
}

// This private wrapper distinguishes an unsigned registry from an invalid key
// without adding secret material to the serializable Registry structure.
struct RegistrySigningKey {
	key ?ed25519.PrivateKey
}

// checked_signing_key treats only an absent environment variable and a genuinely
// missing key file as unsigned. A configured value must contain a valid seed.
fn (r &Registry) checked_signing_key() !RegistrySigningKey {
	if raw := os.getenv_opt(registry_key_env) {
		return registry_signing_key_from_hex(raw, 'environment variable ${registry_key_env}')
	}
	path := registry_key_path()
	os.lstat(path) or {
		// Windows lstat can report a dangling directory link as missing.
		if os.is_not_exist(err) && !os.is_link(path) {
			return RegistrySigningKey{}
		}
		return error('failed to inspect registry signing key file `${path}`')
	}
	stat := os.stat(path) or {
		return error('failed to inspect registry signing key file `${path}`')
	}
	if stat.get_filetype() != .regular {
		return error('registry signing key file `${path}` is not a regular file')
	}
	raw := os.read_file(path) or {
		return error('failed to read registry signing key file `${path}`')
	}
	return registry_signing_key_from_hex(raw, 'registry signing key file `${path}`')
}

fn registry_signing_key_from_hex(raw string, source string) !RegistrySigningKey {
	hex := raw.trim_space()
	if hex == '' {
		return error('invalid ${source}: signing seed is empty')
	}
	seed := hexcode.decode(hex) or {
		return error('invalid ${source}: signing seed must be hexadecimal')
	}
	if seed.len != ed25519.seed_size {
		return error('invalid ${source}: signing seed must contain exactly ${ed25519.seed_size} bytes')
	}
	return RegistrySigningKey{
		key: ed25519.new_key_from_seed(seed)
	}
}

// The public string helpers retain their existing signatures. The CLI checks
// configuration before binding, while these helpers continue to load per call.
fn (r &Registry) signing_key() ?ed25519.PrivateKey {
	loaded := r.checked_signing_key() or { return none }
	return loaded.key
}

// public_key_hex returns the hex-encoded public key for the configured signing
// key, or '' when no signing key is configured.
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
	mut names := r.modules.keys()
	names.sort()
	mut modules := map[string]RegistryEntry{}
	for name in names {
		entry := r.modules[name] or { continue }
		mut versions := []ModuleInfo{}
		for info in entry.versions {
			mut dependency_names := info.dependencies.keys()
			dependency_names.sort()
			mut dependencies := map[string]string{}
			for dependency in dependency_names {
				dependencies[dependency] = info.dependencies[dependency]
			}
			mut feature_names := info.features.keys()
			feature_names.sort()
			mut features := map[string][]string{}
			for feature in feature_names {
				features[feature] = info.features[feature]
			}
			versions << ModuleInfo{
				...info
				dependencies: dependencies
				features:     features
			}
		}
		modules[name] = RegistryEntry{
			name:     entry.name
			versions: versions
		}
	}
	return json2.encode(SignedIndex{
		modules: modules
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
// form was produced by the holder of `trusted_public_key_hex`. The caller must
// supply a public key pinned through a trusted external channel, such as a trusted
// local file. Reading the key from the same registry's config does not establish
// trust: a malicious mirror can replace the index, signature and advertised key.
// This check is explicit; registry fetch and install helpers do not invoke it.
pub fn (r &Registry) verify_signature(trusted_public_key_hex string, sig_hex string) bool {
	public_bytes := hexcode.decode(trusted_public_key_hex) or { return false }
	sig := hexcode.decode(sig_hex) or { return false }
	return ed25519.verify(public_bytes, r.canonical_json().bytes(), sig) or { false }
}
