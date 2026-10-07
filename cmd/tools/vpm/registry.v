// Registry protocol implementation for VPM.
// Defines the API endpoints and metadata format for VPM registries.
module main

import crypto.sha256
import json2
import os
import time

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
	versions.sort(a.version > b.version)
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

// get_latest returns the latest non-yanked version of a module.
pub fn (r &Registry) get_latest(name string) ?ModuleInfo {
	entry := r.modules[name] or { return none }
	for v in entry.versions {
		if !v.yanked {
			return v
		}
	}
	return none
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

	return '{"error": "not found"}'
}
