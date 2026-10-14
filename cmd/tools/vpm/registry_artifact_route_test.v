module main

import json2
import net.http
import os

const artifact_route_root = os.join_path(os.vtmp_dir(), 'vpm_artifact_routes_${os.getpid()}')
const artifact_route_original_dir = os.getwd()

fn testsuite_begin() {
	os.mkdir_all(artifact_route_root)!
	os.chdir(artifact_route_root)!
}

fn testsuite_end() {
	os.chdir(artifact_route_original_dir) or {}
	os.rmdir_all(artifact_route_root) or {}
}

fn test_published_raw_path_components_round_trip_through_http_metadata_and_archive_routes() {
	mut punctuation := "-._~!$&'()*+,;=@[]"
	$if windows {
		// Windows rejects '*' in filenames independently of HTTP routability.
		punctuation = punctuation.replace('*', '')
	}
	names_and_versions := [['module${punctuation}', 'release${punctuation}+build.7'],
		['CaseSensitive.Module+meta', '1.2.3-beta+build.9']]
	content := [u8(0x50), 0x4b, 0, 0xff, 0x80, 0x0a].bytestr()
	archive := os.join_path(artifact_route_root, 'sources.zip')
	os.write_file(archive, content)!
	mut registry := new_registry()
	for pair in names_and_versions {
		registry.publish(ModuleInfo{
			name:    pair[0]
			version: pair[1]
		}, archive)!
	}
	mut server := &http.Server{
		addr:                 '127.0.0.1:0'
		handler:              RegistryHandler{
			registry: registry
		}
		show_startup_message: false
	}
	spawn server.listen_and_serve()
	server.wait_till_running()!
	defer {
		server.close()
	}
	base := 'http://127.0.0.1:${server.listener.addr()!.port()!}'
	for pair in names_and_versions {
		info_response := http.get('${base}/${pair[0]}/@v/${pair[1]}.info')!
		assert info_response.status_code == 200, info_response.body
		info := json2.decode[ModuleInfo](info_response.body)!
		assert info.name == pair[0]
		assert info.version == pair[1]
		assert info.checksum == 'sha256:${sha256_hex(content.bytes())}'
		archive_response := http.get('${base}/${pair[0]}/@v/${pair[1]}.zip')!
		assert archive_response.status_code == 200, archive_response.body
		assert archive_response.body == content
		content_type := archive_response.header.get(.content_type) or { panic('missing content type') }
		assert content_type == 'application/zip'
	}
}
