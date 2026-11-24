module parser

import os
import v.pref

fn test_embed_file_materialization_requires_a_self_contained_payload() {
	root := os.join_path(os.vtmp_dir(), 'embed_materialization_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'binary.bin')
	payload := [u8(0), 1, 127, 128, 255, 0]
	os.write_file_array(path, payload) or { panic(err) }
	for mode in ['development', 'production', 'cross', 'compiler', 'android', 'freestanding'] {
		preferences := pref.Preferences{
			target:         pref.Target{ os: if mode == 'android' { 'android' } else { 'linux' }, arch: 'amd64' }
			is_prod:        mode == 'production'
			output_cross_c: mode == 'cross'
			building_v:     mode == 'compiler'
			user_defines:   if mode == 'freestanding' { ['freestanding'] } else { []string{} }
		}
		mut p := Parser.new(&preferences)
		if id := p.embed_file_uncompressed_data(path, 0) {
			assert mode != 'development', mode
			node := p.a.node(id)
			assert node.is_embed_payload(), mode
			assert node.value.bytes() == payload, mode
		} else {
			assert mode == 'development', mode
		}
	}
}

fn test_freestanding_empty_file_has_a_real_payload_node() {
	path := os.join_path(os.vtmp_dir(), 'embed_empty_${os.getpid()}.bin')
	os.write_file(path, '') or { panic(err) }
	defer { os.rm(path) or {} }
	preferences := pref.Preferences{ user_defines: ['freestanding'] }
	mut p := Parser.new(&preferences)
	id := p.embed_file_uncompressed_data(path, 0) or { panic('missing empty payload') }
	node := p.a.node(id)
	assert node.is_embed_payload()
	assert node.value.len == 0
}

fn test_required_embed_diagnoses_missing_or_directory_payloads() {
	root := os.join_path(os.vtmp_dir(), 'embed_missing_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	for path in [os.join_path(root, 'missing.bin'), root] {
		for required in [false, true] {
			preferences := pref.Preferences{
				user_defines: if required {
					['freestanding']
				} else {
					[]string{}
				}
			}
			mut p := Parser.new(&preferences)
			if _ := p.embed_file_uncompressed_data(path, 0) {
				assert false, 'a missing file or directory cannot be a payload'
			}
			assert p.diagnostics.len == if required { 1 } else { 0 }
			if required {
				assert p.diagnostics[0].message.contains('cannot embed file')
				assert p.diagnostics[0].message.contains('not a regular file')
			}
		}
	}
}

fn test_required_embed_read_failure_is_not_discarded() {
	$if !windows {
		path := os.join_path(os.vtmp_dir(), 'embed_unreadable_${os.getpid()}.bin')
		os.write_file(path, 'private') or { panic(err) }
		defer {
			os.chmod(path, 0o600) or {}
			os.rm(path) or {}
		}
		os.chmod(path, 0) or { panic(err) }
		mut inaccessible := false
		_ := os.read_bytes(path) or {
			inaccessible = true
			[]u8{}
		}
		// Root can still read mode-000 files. Check the actual read result
		// rather than treating those environments as a compiler regression.
		if !inaccessible {
			return
		}
		preferences := pref.Preferences{ user_defines: ['freestanding'] }
		mut p := Parser.new(&preferences)
		if _ := p.embed_file_uncompressed_data(path, 0) {
			assert false, 'an unreadable file cannot be a payload'
		}
		assert p.diagnostics.len == 1
		assert p.diagnostics[0].message.contains('cannot embed file')
		assert p.diagnostics[0].message.contains(path)
		assert !p.diagnostics[0].message.contains('not a regular file')
	}
}

fn test_required_embed_error_is_anchored_to_its_call() {
	root := os.join_path(os.vtmp_dir(), 'embed_error_span_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	call := '\$embed_file("missing.bin")'
	source := 'fn main() {\n\tdata := ${call}\n\tprintln(data.len)\n}\n'
	os.write_file(path, source) or { panic(err) }
	preferences := pref.Preferences{ user_defines: ['freestanding'] }
	mut p := Parser.new(&preferences)
	p.parse_file(path)
	assert p.diagnostics.len == 1, p.diagnostics.str()
	diagnostic := p.diagnostics[0]
	assert diagnostic.file == path
	assert diagnostic.line == 2
	assert source[diagnostic.pos.offset..diagnostic.pos.end] == call
	assert diagnostic.message.contains(os.join_path(root, 'missing.bin'))
}
