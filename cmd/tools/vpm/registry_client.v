module main

import json2
import net.urllib

// RegistryModule is one version of a module as the registry protocol describes
// it: what `registry.v` encodes as `ModuleInfo` and serves from the `@latest` and
// `@v/<version>.info` routes.
//
// It is deliberately a type of its own rather than an alias of `ModuleInfo`, so
// that a field the server gains does not silently widen the client's API, and a
// field it renames or drops fails registry_client_test.v instead of decoding to
// a zero value that no caller can tell from a real module.
//
// The names below are the ones `json2.encode` writes for `ModuleInfo`. The tests
// encode both structs and compare the bytes, so the two halves of the protocol
// cannot drift apart while the suite is green.
pub struct RegistryModule {
pub:
	name         string
	version      string
	description  string
	license      string
	dependencies map[string]string
	checksum     string
	published_at string
	features     map[string][]string
	yanked       bool
}

// registry_answer_is_absent reports whether a registry's answer describes
// nothing. `registry.v` maps exactly `not_found_body` to 404 and has no other
// error body, so the status is the contract; the body is compared as well,
// because a registry reached through something that rewrites status lines
// answers the same body with a 200, and that must not be read as a module.
//
// Whether an answer is absent is a decision of its own rather than a branch in
// each fetch, because it is the one rule every caller depends on: an absent
// module has to reach the caller as an error, since "try the next registry" is
// what `get_mod_vpm_info` does on a 404 and a zero-valued struct is not that.
fn registry_answer_is_absent(status_code int, body string) bool {
	return status_code == 404 || body.trim_space() == not_found_body
}

// registry_get requests `path` from the registry at `url` and returns the body of
// the 200 answer. The request goes through `vpm_http_get`, which attaches the
// bearer token `registry_token` finds for the host, so a private registry is
// authenticated exactly as every other vpm request is.
fn registry_get(url string, path string) !string {
	resp := vpm_http_get(url + path) or {
		return error('the registry at `${url}` did not answer a request for `${path}`: ${err.msg()}')
	}
	if registry_answer_is_absent(resp.status_code, resp.body) {
		return error('the registry at `${url}` does not serve `${path}`.')
	}
	if resp.status_code != 200 {
		return error('the registry at `${url}` answered ${resp.status_code} for `${path}`.')
	}
	return resp.body
}

// decode_registry_module reads one module body. A body that is valid JSON but
// names no module is refused, because a registry that reports not-found in the
// body rather than in the status line produces exactly that, and decoding it
// would hand back a module with an empty name.
fn decode_registry_module(url string, name string, body string) !RegistryModule {
	info := json2.decode[RegistryModule](body) or {
		return error('the registry at `${url}` did not describe `${name}` as JSON: ${err.msg()}')
	}
	if info.name == '' {
		return error('the registry at `${url}` holds no metadata for `${name}`.')
	}
	return info
}

// decode_registry_versions reads the version list body. It is a private function
// of its own so the empty-list rule can be exercised without a registry.
fn decode_registry_versions(url string, name string, body string) ![]string {
	return json2.decode[[]string](body) or {
		return error('the registry at `${url}` did not list the versions of `${name}` as JSON: ${err.msg()}')
	}
}

// fetch_registry_versions returns the versions of `name` that the registry at
// `url` serves, in the order it lists them: semver, highest first.
//
// A module the registry has never heard of is not an error. Its route answers
// 200 with an empty array, so an unknown module comes back as an empty list
// rather than as a failure. A 404 means the registry does not serve this route
// at all, which is returned as an error, so a caller can tell a registry that
// holds no such module from one that speaks a different protocol.
pub fn fetch_registry_versions(url string, name string) ![]string {
	return decode_registry_versions(url, name, registry_get(url, '/${name}/@v/list')!)
}

// fetch_registry_latest returns the metadata for the highest version of `name`
// that the registry at `url` has not yanked.
//
// A 404, meaning either that the registry does not hold `name` or that it does
// not serve this route, is returned as an error naming the module. It is never
// returned as a zero-valued RegistryModule, because a caller resolving a module
// has to be able to go on to the next candidate registry.
pub fn fetch_registry_latest(url string, name string) !RegistryModule {
	return decode_registry_module(url, name, registry_get(url, '/${name}/@latest')!)
}

// fetch_registry_info returns the metadata for `name` at `version`, as served by
// `/<module>/@v/<version>.info`.
//
// A 404, meaning either that no such version is published or that the registry
// does not serve this route, is returned as an error naming the module and the
// version. It is never returned as a zero-valued RegistryModule, so a caller can
// tell a version that was never published from one whose metadata is empty.
pub fn fetch_registry_info(url string, name string, version string) !RegistryModule {
	return decode_registry_module(url, name, registry_get(url, '/${name}/@v/${version}.info')!)
}

// fetch_registry_config returns the configuration document the registry at `url`
// serves at its root, which is where it publishes the base its archives are
// served from. A registry that does not serve the document is an error: without
// it nothing says where the sources of a module are.
pub fn fetch_registry_config(url string) !RegistryConfig {
	return json2.decode[RegistryConfig](registry_get(url, '/config.json')!) or {
		return error('the registry at `${url}` did not describe its configuration as JSON: ${err.msg()}')
	}
}

// registry_archive_base returns the base url the registry at `url` serves its
// module archives from.
//
// `registry.v` still answers `/config.json` with the fixed
// `https://example.com/downloads`, and RFC 2606 reserves that host for
// documentation, so a base on it names no host that ever served a module. It is
// reported as an error rather than returned, because an install pointed at a
// placeholder would look for an archive that was never published, and no later
// step could tell that from one that had gone missing.
pub fn registry_archive_base(url string) !string {
	base := fetch_registry_config(url)!.dl.trim_space().trim_string_right('/')
	if base == '' {
		return error('the registry at `${url}` names no archive base in its configuration.')
	}
	parsed := urllib.parse(base) or {
		return error('the registry at `${url}` names the archive base `${base}`, which is not a url.')
	}
	host := parsed.hostname().to_lower()
	if registry_placeholder_host(host) {
		return error('the registry at `${url}` names the placeholder archive base `${base}`.')
	}
	return base
}

// registry_placeholder_host reports whether `host` is one of the RFC 2606
// documentation domains, or a subdomain of one. Those hosts exist so that an
// example can name a url without naming a server, which is the opposite of what
// a registry naming its archive base needs.
fn registry_placeholder_host(host string) bool {
	return host == '' || ['example.com', 'example.net', 'example.org'].any(host == it
		|| host.ends_with('.' + it))
}
