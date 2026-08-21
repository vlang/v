module tests

import crypto.sha1
import os
import crypto.sha256
import encoding.base64
import tccbin_automation.bin

fn C.tccbin_open_directory_no_follow(path &char) int
fn C.tccbin_openat_no_follow(parent_fd int, name &char, directory int) int
fn C.tccbin_open_directory_enumerator(directory_fd int) voidptr
fn C.tccbin_read_directory_entry(directory voidptr, buffer &char, capacity u64) int
fn C.tccbin_close_directory_enumerator(directory voidptr) int
fn C.tccbin_read_document(fd int, buffer voidptr, length u64) i64
fn C.tccbin_close_document(fd int) int
fn C.tccbin_windows_open_directory_path_no_follow(path &u16) voidptr
fn C.tccbin_windows_open_child_no_follow(parent voidptr, name &u16, directory int) voidptr
fn C.tccbin_windows_open_directory_enumerator(parent voidptr) voidptr
fn C.tccbin_windows_read_directory_entry(enumerator voidptr, buffer &u16, capacity u64) int
fn C.tccbin_windows_close_directory_enumerator(enumerator voidptr) int
fn C.tccbin_file_names_apply_batch_status(status i32, information u64, capacity u64,
	valid_bytes &u64, offset &u64, batch_ready &int, finished &int) int
fn C.tccbin_file_names_decode_record(batch &u8, valid_bytes u64, offset &u64,
	batch_ready &int, output &u16, output_capacity u64, output_length &u64) int
fn C.ReadFile(handle voidptr, buffer voidptr, bytes_to_read u32, bytes_read &u32,
	overlapped voidptr) bool
fn C.CloseHandle(handle voidptr) bool

fn t2c_put_u32_native(mut bytes []u8, offset int, value u32) {
	assert offset >= 0 && offset + 4 <= bytes.len
	bytes[offset] = u8(value)
	bytes[offset + 1] = u8(value >> 8)
	bytes[offset + 2] = u8(value >> 16)
	bytes[offset + 3] = u8(value >> 24)
}

fn t2c_file_names_record(name string, next u32, extent int) []u8 {
	minimum := 12 + name.len * 2
	assert extent >= minimum
	mut record := []u8{len: extent}
	t2c_put_u32_native(mut record, 0, next)
	t2c_put_u32_native(mut record, 4, 0)
	t2c_put_u32_native(mut record, 8, u32(name.len * 2))
	for index, character in name.bytes() {
		record[12 + index * 2] = character
	}
	return record
}

fn t2c_u16_ascii(buffer []u16, length int) string {
	assert length >= 0 && length <= buffer.len
	mut bytes := []u8{len: length}
	for index in 0 .. length {
		assert buffer[index] <= 0x7f
		bytes[index] = u8(buffer[index])
	}
	return bytes.bytestr()
}

fn t2c_file_names_decode_code(batch []u8, valid_bytes u64, output_capacity u64) int {
	assert valid_bytes <= u64(batch.len)
	mut offset := u64(0)
	mut batch_ready := 1
	mut output := []u16{len: 64}
	mut output_length := u64(0)
	return C.tccbin_file_names_decode_record(batch.data, valid_bytes, &offset, &batch_ready,
		output.data, output_capacity, &output_length)
}

fn test_file_names_raw_helpers_accept_batches_chains_skips_and_reload() {
	mut valid_bytes := u64(99)
	mut offset := u64(77)
	mut batch_ready := 0
	mut finished := 1
	single := t2c_file_names_record('alpha', 0, 22)
	assert C.tccbin_file_names_apply_batch_status(0, u64(single.len), 65_536, &valid_bytes,
		&offset, &batch_ready, &finished) == 1
	assert valid_bytes == u64(single.len)
	assert offset == 0
	assert batch_ready == 1
	assert finished == 0
	mut output := []u16{len: 32}
	mut output_length := u64(0)
	assert C.tccbin_file_names_decode_record(single.data, valid_bytes, &offset, &batch_ready,
		output.data, u64(output.len), &output_length) == 1
	assert output_length == 5
	assert t2c_u16_ascii(output, int(output_length)) == 'alpha'
	assert batch_ready == 0

	first := t2c_file_names_record('a', 16, 16)
	second := t2c_file_names_record('bb', 0, 16)
	mut chain := first.clone()
	chain << second
	assert C.tccbin_file_names_apply_batch_status(0, u64(chain.len), 65_536, &valid_bytes, &offset,
		&batch_ready, &finished) == 1
	assert C.tccbin_file_names_decode_record(chain.data, valid_bytes, &offset, &batch_ready,
		output.data, u64(output.len), &output_length) == 1
	assert t2c_u16_ascii(output, int(output_length)) == 'a'
	assert offset == 16
	assert batch_ready == 1
	assert C.tccbin_file_names_decode_record(chain.data, valid_bytes, &offset, &batch_ready,
		output.data, u64(output.len), &output_length) == 1
	assert t2c_u16_ascii(output, int(output_length)) == 'bb'
	assert batch_ready == 0

	dot := t2c_file_names_record('.', 16, 16)
	dotdot := t2c_file_names_record('..', 0, 16)
	mut skips := dot.clone()
	skips << dotdot
	assert C.tccbin_file_names_apply_batch_status(0, u64(skips.len), 65_536, &valid_bytes, &offset,
		&batch_ready, &finished) == 1
	assert C.tccbin_file_names_decode_record(skips.data, valid_bytes, &offset, &batch_ready,
		output.data, u64(output.len), &output_length) == 2
	assert offset == 16
	assert batch_ready == 1
	assert C.tccbin_file_names_decode_record(skips.data, valid_bytes, &offset, &batch_ready,
		output.data, u64(output.len), &output_length) == 2
	assert batch_ready == 0

	reloaded := t2c_file_names_record('next', 0, 20)
	assert C.tccbin_file_names_apply_batch_status(0, u64(reloaded.len), 65_536, &valid_bytes,
		&offset, &batch_ready, &finished) == 1
	assert C.tccbin_file_names_decode_record(reloaded.data, valid_bytes, &offset, &batch_ready,
		output.data, u64(output.len), &output_length) == 1
	assert t2c_u16_ascii(output, int(output_length)) == 'next'
	assert batch_ready == 0
	assert C.tccbin_file_names_apply_batch_status(i32(-2_147_483_642), 0, 65_536, &valid_bytes,
		&offset, &batch_ready, &finished) == 0
	assert valid_bytes == 0
	assert offset == 0
	assert batch_ready == 0
	assert finished == 1
}

fn test_file_names_raw_helpers_reject_status_and_information_failures() {
	mut valid_bytes := u64(0)
	mut offset := u64(0)
	mut batch_ready := 0
	mut finished := 0
	assert C.tccbin_file_names_apply_batch_status(0, 0, 65_536, &valid_bytes, &offset,
		&batch_ready, &finished) == -2
	assert C.tccbin_file_names_apply_batch_status(0, 65_537, 65_536, &valid_bytes, &offset,
		&batch_ready, &finished) == -2
	assert C.tccbin_file_names_apply_batch_status(i32(-2_147_483_642), 1, 65_536, &valid_bytes,
		&offset, &batch_ready, &finished) == -2
	for status in [i32(-2_147_483_643), i32(0x103), i32(-1)] {
		assert C.tccbin_file_names_apply_batch_status(status, 12, 65_536, &valid_bytes, &offset,
			&batch_ready, &finished) == -1
	}
}

fn test_file_names_raw_helpers_reject_malformed_records_and_capacity() {
	short := []u8{len: 11}
	assert t2c_file_names_decode_code(short, u64(short.len), 64) == -3
	zero_name := []u8{len: 12}
	assert t2c_file_names_decode_code(zero_name, u64(zero_name.len), 64) == -4
	mut odd_name := []u8{len: 13}
	t2c_put_u32_native(mut odd_name, 8, 1)
	assert t2c_file_names_decode_code(odd_name, u64(odd_name.len), 64) == -4
	mut truncated_name := []u8{len: 14}
	t2c_put_u32_native(mut truncated_name, 8, 4)
	assert t2c_file_names_decode_code(truncated_name, u64(truncated_name.len), 64) == -4
	too_small_next := t2c_file_names_record('a', 12, 28)
	assert t2c_file_names_decode_code(too_small_next, u64(too_small_next.len), 64) == -5
	unaligned_next := t2c_file_names_record('a', 15, 30)
	assert t2c_file_names_decode_code(unaligned_next, u64(unaligned_next.len), 64) == -5
	outside_next := t2c_file_names_record('a', 64, 32)
	assert t2c_file_names_decode_code(outside_next, u64(outside_next.len), 64) == -5
	no_next_header := t2c_file_names_record('a', 16, 20)
	assert t2c_file_names_decode_code(no_next_header, u64(no_next_header.len), 64) == -5
	alpha := t2c_file_names_record('alpha', 0, 22)
	assert t2c_file_names_decode_code(alpha, u64(alpha.len), 5) == -6
}

fn t2c_windows_directory_entries(handle voidptr, maximum int) ![]string {
	assert maximum >= 0 && maximum <= 1056
	$if windows {
		enumerator := C.tccbin_windows_open_directory_enumerator(handle)
		if enumerator == unsafe { nil } {
			return error('native primitive directory cannot be enumerated')
		}
		defer {
			C.tccbin_windows_close_directory_enumerator(enumerator)
		}
		mut buffer := []u16{len: 1024}
		mut entries := []string{cap: maximum}
		for {
			read := C.tccbin_windows_read_directory_entry(enumerator, buffer.data, u64(buffer.len))
			if read == 0 {
				break
			}
			if read < 0 {
				return error('native primitive directory enumeration failed')
			}
			if entries.len >= maximum {
				return error('native primitive walker crossed its strict bound')
			}
			entries << unsafe { string_from_wide2(buffer.data, read) }
		}
		return entries
	} $else {
		return error('native Windows directory enumeration is unavailable on this host')
	}
}

fn t2c_primitive_directory_entry_count(path string, maximum int) !int {
	assert maximum >= 0 && maximum <= 1056
	$if windows {
		wide_path := path.to_wide()
		defer {
			unsafe { free(voidptr(wide_path)) }
		}
		handle := C.tccbin_windows_open_directory_path_no_follow(wide_path)
		if handle == voidptr(-1) || handle == unsafe { nil } {
			return error('native primitive directory cannot be opened')
		}
		defer {
			C.CloseHandle(handle)
		}
		entries := t2c_windows_directory_entries(handle, maximum)!
		return entries.len
	} $else {
		fd := C.tccbin_open_directory_no_follow(&char(path.str))
		if fd < 0 {
			return error('native primitive directory cannot be opened')
		}
		defer {
			C.tccbin_close_document(fd)
		}
		enumerator := C.tccbin_open_directory_enumerator(fd)
		if enumerator == unsafe { nil } {
			return error('native primitive directory cannot be enumerated')
		}
		defer {
			C.tccbin_close_directory_enumerator(enumerator)
		}
		mut buffer := []u8{len: 4096}
		mut count := 0
		for {
			read := C.tccbin_read_directory_entry(enumerator, &char(buffer.data), u64(buffer.len))
			if read == 0 {
				break
			}
			if read < 0 {
				return error('native primitive directory enumeration failed')
			}
			if count >= maximum {
				return error('native primitive walker crossed its strict bound')
			}
			count++
		}
		return count
	}
}

fn t2c_read_posix_child(parent_fd int, name string) !string {
	fd := C.tccbin_openat_no_follow(parent_fd, &char(name.str), 0)
	if fd < 0 {
		return error('native primitive child cannot be opened')
	}
	defer {
		C.tccbin_close_document(fd)
	}
	mut buffer := []u8{len: 128}
	read := C.tccbin_read_document(fd, buffer.data, u64(buffer.len))
	if read < 0 || read > buffer.len {
		return error('native primitive child cannot be read')
	}
	return buffer[..int(read)].bytestr()
}

fn t2c_read_windows_child(parent voidptr, name string) !string {
	$if windows {
		wide_name := name.to_wide()
		defer {
			unsafe { free(voidptr(wide_name)) }
		}
		handle := C.tccbin_windows_open_child_no_follow(parent, wide_name, 0)
		if handle == voidptr(-1) || handle == unsafe { nil } {
			return error('native Windows relative document cannot be opened')
		}
		defer {
			C.CloseHandle(handle)
		}
		mut buffer := []u8{len: 128}
		mut bytes_read := u32(0)
		if !C.ReadFile(handle, buffer.data, u32(buffer.len), &bytes_read, unsafe { nil })
			|| bytes_read > u32(buffer.len) {
			return error('native Windows relative document cannot be read')
		}
		return buffer[..int(bytes_read)].bytestr()
	} $else {
		return error('native Windows relative document is unavailable on this host')
	}
}

fn load_contract_value(path string) bin.JsonValue {
	return bin.parse_strict_json(os.read_file(path) or { panic(err) }) or { panic(err) }
}

fn minimal_elf64_relocatable() []u8 {
	mut bytes := []u8{len: 64}
	bytes[0] = 0x7f
	bytes[1] = `E`
	bytes[2] = `L`
	bytes[3] = `F`
	bytes[4] = 2
	bytes[5] = 1
	bytes[6] = 1
	bytes[7] = 0
	bytes[16] = 1
	bytes[17] = 0
	bytes[18] = 0x3e
	bytes[19] = 0
	return bytes
}

fn prepare_provenance_tree(suffix string, bytes []u8, executable bool) (string, string) {
	base := os.join_path(os.temp_dir(), 'tccbin-provenance-${os.getpid()}-${suffix}')
	source_repo := os.join_path(base, 'source')
	staging_root := os.join_path(base, 'staging')
	os.rmdir_all(base) or {}
	os.mkdir_all(os.join_path(source_repo, 'lib')) or { panic(err) }
	os.mkdir_all(os.join_path(staging_root, 'lib')) or { panic(err) }
	os.write_file_array(os.join_path(source_repo, 'lib', 'openlibm.o'), bytes) or { panic(err) }
	os.write_file_array(os.join_path(staging_root, 'lib', 'openlibm.o'), bytes) or { panic(err) }
	if executable {
		os.chmod(os.join_path(source_repo, 'lib', 'openlibm.o'), 0o755) or { panic(err) }
	}
	commands := [
		['git', '-C', source_repo, 'init', '-q'],
		['git', '-C', source_repo, 'config', 'user.email', 'ci@example.invalid'],
		['git', '-C', source_repo, 'config', 'user.name', 'Contract Test'],
		['git', '-C', source_repo, 'config', 'core.autocrlf', 'false'],
		['git', '-C', source_repo, 'add', '--', 'lib/openlibm.o'],
		['git', '-C', source_repo, 'commit', '-qm', 'fixture'],
		['git', '-C', source_repo, 'checkout', '--detach', '-q', 'HEAD'],
	]
	for args in commands {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	return source_repo, staging_root
}

fn synthetic_opaque_contract(bytes []u8) (bin.JsonValue, bin.JsonValue) {
	base := os.join_path(os.temp_dir(), 'tccbin-opaque-authority-${os.getpid()}')
	os.rmdir_all(base) or {}
	authority := t2a_prepare_toolchain_authority(base, 'windows-amd64')
	defer {
		os.rmdir_all(base) or {}
	}
	expected_hash := sha256_bytes(bytes)
	registry_source := os.read_file(os.join_path(authority.root, 'targets.json')) or { panic(err) }
	mut manifest_source := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'manifest-windows-opaque.valid.json')) or { panic(err) }
	manifest_source = t2a_resolved_manifest_toolchain(manifest_source, authority)
	production_hash := '9a11e182e1f6b522030d1b8685666147de0ebb562b9d02cce189690fd07cb7db'
	registry := bin.parse_strict_json(registry_source.replace(production_hash, expected_hash)) or {
		panic(err)
	}
	manifest := bin.parse_strict_json(manifest_source.replace(production_hash, expected_hash)) or {
		panic(err)
	}
	return registry, manifest
}

fn sha256_bytes(bytes []u8) string {
	return sha256.sum256(bytes).hex()
}

fn staging_contract(source_repo string, staging_root string) bin.StagingContract {
	result := os.exec(['git', '-C', source_repo, 'rev-parse', 'HEAD'])
	assert result.exit_code == 0
	return bin.StagingContract{
		staging_root:    staging_root
		source_git_root: source_repo
		source_git_ref:  result.output.trim_space()
	}
}

struct CompleteCandidateFixture {
	base            string
	automation_root string
	target_id       string
	source_repo     string
	staging_root    string
	manifest_path   string
	manifest_source string
	inventory_bytes []u8
	output_bytes    []u8
	parent_ref      string
	authority       SyntheticToolchainAuthority
	contract        bin.StagingContract
}

struct LegacyCompositionFixture {
	base            string
	automation_root string
	base_repo_root  string
	raw_root        string
	manifest_path   string
	result_root     string
	base_sha        string
	manifest_source string
}

struct ManagedBaselineActivationFixture {
	base                      string
	automation_root           string
	contract_root             string
	target_id                 string
	base_repo_root            string
	raw_root                  string
	manifest_path             string
	result_root               string
	base_sha                  string
	parent_sha                string
	base_tree                 string
	base_manifest_source      string
	candidate_manifest_source string
	policy_path               string
	policy_sha256             string
	producer_source           string
	runtime                   bin.RuntimeContractBinding
	source_commit_evidence    bin.JsonValue
}

const managed_baseline_phase_a_contract_sha = '7545e515b434cd399333d43659238427d72e22e7'

fn activation_string(value string) bin.JsonValue {
	return bin.JsonValue{
		kind:         .string_value
		string_value: value
	}
}

fn activation_object_with_replacements(value bin.JsonValue,
	replacements map[string]bin.JsonValue) bin.JsonValue {
	assert value.kind == .object
	mut values := value.object_values.clone()
	for key, replacement in replacements {
		index := value.object_keys.index(key)
		assert index >= 0, key
		values[index] = replacement
	}
	return bin.JsonValue{
		kind:          .object
		object_keys:   value.object_keys.clone()
		object_values: values
	}
}

fn activation_array(values []bin.JsonValue) bin.JsonValue {
	return bin.JsonValue{
		kind:        .array
		array_value: values
	}
}

fn activation_unresolved_provenance(value bin.JsonValue) bin.JsonValue {
	return activation_object_with_replacements(value, {
		'status':      activation_string('incomplete')
		'repository':  bin.JsonValue{
			kind: .null_value
		}
		'sha':         bin.JsonValue{
			kind: .null_value
		}
		'source_path': bin.JsonValue{
			kind: .null_value
		}
		'license':     bin.JsonValue{
			kind: .null_value
		}
	})
}

fn activation_candidate_manifest(source string, runtime_sha string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	return bin.canonical_json(activation_object_with_replacements(root, {
		'contract_repository': activation_string('vlang/v')
		'contract_sha':        activation_string(runtime_sha)
		'contract_mode':       activation_string('production')
		'v_source_sha':        activation_string(runtime_sha)
	}))
}

fn activation_candidate_manifest_with_runtime_source(source string, runtime_sha string,
	runtime_tree string) string {
	root := bin.parse_strict_json(activation_candidate_manifest(source, runtime_sha)) or {
		panic(err)
	}
	sources_value := root.object_value('sources') or { panic('sources missing') }
	mut sources := sources_value.array_value.clone()
	mut found := false
	for index, candidate_source in sources {
		id := candidate_source.object_value('id') or { panic('source ID missing') }
		if id.string_value == 'v-libgc' {
			sources[index] = activation_object_with_replacements(candidate_source, {
				'sha':  activation_string(runtime_sha)
				'tree': activation_string(runtime_tree)
			})
			found = true
		}
	}
	assert found
	return bin.canonical_json(activation_object_with_replacements(root, {
		'sources': activation_array(sources)
	}))
}

fn activation_manifest_with_repository_provenance_sha(source string, repository string,
	sha string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	mut replacements := map[string]bin.JsonValue{}
	for collection in ['overlays', 'inventory', 'outputs'] {
		collection_value := root.object_value(collection) or { panic('${collection} missing') }
		mut entries := collection_value.array_value.clone()
		for index, entry in entries {
			provenance := entry.object_value('provenance') or { panic('provenance missing') }
			provenance_repository := provenance.object_value('repository') or {
				panic('provenance repository missing')
			}
			if provenance_repository.kind == .string_value
				&& provenance_repository.string_value == repository {
				entries[index] = activation_object_with_replacements(entry, {
					'provenance': activation_object_with_replacements(provenance, {
						'sha': activation_string(sha)
					})
				})
			}
		}
		replacements[collection] = activation_array(entries)
	}
	return bin.canonical_json(activation_object_with_replacements(root, replacements))
}

fn activation_base_manifest(candidate_source string) string {
	root := bin.parse_strict_json(candidate_source) or { panic(err) }
	mut sources := []bin.JsonValue{}
	source_values := root.object_value('sources') or { panic('sources missing') }
	for source in source_values.array_value {
		sources << activation_object_with_replacements(source, {
			'sha':  bin.JsonValue{
				kind: .null_value
			}
			'tree': bin.JsonValue{
				kind: .null_value
			}
		})
	}
	mut replacements := {
		'contract_sha':      activation_string(managed_baseline_phase_a_contract_sha)
		'v_source_sha':      activation_string(managed_baseline_phase_a_contract_sha)
		'provenance_status': activation_string('incomplete')
		'sources':           activation_array(sources)
		'toolchain':         bin.parse_strict_json('{"profile_id":null,"profile_sha256":null,"producer_observation":null}') or {
			panic(err)
		}
	}
	for collection in ['overlays', 'inventory', 'outputs'] {
		mut entries := []bin.JsonValue{}
		collection_value := root.object_value(collection) or { panic('${collection} missing') }
		for entry in collection_value.array_value {
			provenance := entry.object_value('provenance') or { panic('provenance missing') }
			entries << activation_object_with_replacements(entry, {
				'provenance': activation_unresolved_provenance(provenance)
			})
		}
		replacements[collection] = activation_array(entries)
	}
	return bin.canonical_json(activation_object_with_replacements(root, replacements))
}

fn activation_registry_with_binding(source string, target_id string, parent_sha string,
	base_sha string, base_tree string, base_manifest_sha256 string, policy_path string,
	policy_sha256 string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	managed := root.object_value('managed_ci_targets') or { panic('managed targets missing') }
	mut targets := managed.array_value.clone()
	mut found := false
	for index, target in targets {
		id := target.object_value('id') or { panic('target ID missing') }
		if id.string_value != target_id {
			continue
		}
		legacy := target.object_value('legacy_onboarding') or { panic('legacy binding missing') }
		activation := bin.parse_strict_json('{"base_sha":"${base_sha}","base_tree":"${base_tree}","parent_sha":"${parent_sha}","base_manifest_sha256":"${base_manifest_sha256}","base_contract_repository":"vlang/v","base_contract_sha":"${managed_baseline_phase_a_contract_sha}","policy_path":"${policy_path}","policy_sha256":"${policy_sha256}"}') or {
			panic(err)
		}
		targets[index] = activation_object_with_replacements(target, {
			'legacy_onboarding':           activation_object_with_replacements(legacy, {
				'base_sha': activation_string(parent_sha)
			})
			'managed_baseline_activation': activation
		})
		found = true
		break
	}
	assert found
	return bin.canonical_json(activation_object_with_replacements(root, {
		'managed_ci_targets': activation_array(targets)
	}))
}

fn prepare_managed_baseline_activation_fixture(suffix string) ManagedBaselineActivationFixture {
	fixture := prepare_complete_candidate('managed-baseline-${suffix}', false, '')
	runtime := bin.RuntimeContractBinding{
		repository: 'vlang/v'
		sha:        'a'.repeat(40)
	}
	unsealed_candidate_source := activation_candidate_manifest(fixture.manifest_source, runtime.sha)
	evidence_fixture := managed_baseline_evidence_fixture(unsealed_candidate_source)
	candidate_source := evidence_fixture.manifest_source
	base_source := activation_base_manifest(candidate_source)
	os.write_file(fixture.manifest_path, base_source) or { panic(err) }
	base_sha := commit_candidate_paths(fixture.source_repo, [
		'automation/bundle-manifest.json',
	], 'reviewed incomplete managed baseline')
	parent_result := os.exec(['git', '-C', fixture.source_repo, 'rev-parse', '${base_sha}^'])
	tree_result := os.exec(['git', '-C', fixture.source_repo, 'rev-parse', '${base_sha}^{tree}'])
	assert parent_result.exit_code == 0, parent_result.output
	assert tree_result.exit_code == 0, tree_result.output
	parent_sha := parent_result.output.trim_space()
	base_tree := tree_result.output.trim_space()
	manifest_path := os.join_path(fixture.base, 'managed-baseline-candidate.json')
	os.write_file(manifest_path, candidate_source) or { panic(err) }
	manifest := bin.parse_strict_json(candidate_source) or { panic(err) }
	policy := bin.managed_baseline_activation_policy_projection(manifest, evidence_fixture.evidence) or {
		panic(err)
	}
	policy_source := bin.canonical_json(policy)
	policy_sha256 := sha256_bytes(policy_source.bytes())
	policy_relative_path := 'baseline-activation/linux-amd64.policy.json'
	policy_path := os.join_path(fixture.automation_root, policy_relative_path)
	os.mkdir_all(os.dir(policy_path)) or { panic(err) }
	os.write_file(policy_path, policy_source) or { panic(err) }
	registry_path := os.join_path(fixture.automation_root, 'targets.json')
	registry_source := os.read_file(registry_path) or { panic(err) }
	os.write_file(registry_path, activation_registry_with_binding(registry_source, 'linux-amd64',
		parent_sha, base_sha, base_tree, sha256_bytes(base_source.bytes()), policy_relative_path,
		policy_sha256)) or { panic(err) }
	registry_issues := bin.validate_registry(fixture.automation_root) or { panic(err) }
	assert registry_issues.len == 0, '${registry_issues}'
	base_issues := bin.validate_manifest(fixture.automation_root, fixture.manifest_path) or {
		panic(err)
	}
	assert base_issues.len == 0, '${base_issues}'
	return ManagedBaselineActivationFixture{
		base:                      fixture.base
		automation_root:           fixture.automation_root
		contract_root:             fixture.authority.contract_root
		target_id:                 'linux-amd64'
		base_repo_root:            fixture.source_repo
		raw_root:                  fixture.staging_root
		manifest_path:             manifest_path
		result_root:               os.join_path(fixture.base, 'managed-baseline-result')
		base_sha:                  base_sha
		parent_sha:                parent_sha
		base_tree:                 base_tree
		base_manifest_source:      base_source
		candidate_manifest_source: candidate_source
		policy_path:               policy_path
		policy_sha256:             policy_sha256
		producer_source:           fixture.authority.producer_source
		runtime:                   runtime
		source_commit_evidence:    evidence_fixture.evidence
	}
}

fn initialize_managed_baseline_runtime_contract_checkout(contract_root string) (string, string) {
	for args in [
		['git', '-C', contract_root, 'init', '-q'],
		['git', '-C', contract_root, 'config', 'user.email', 'ci@example.invalid'],
		['git', '-C', contract_root, 'config', 'user.name', 'Contract Test'],
		['git', '-C', contract_root, 'config', 'core.autocrlf', 'false'],
		['git', '-C', contract_root, 'remote', 'add', 'origin', 'https://github.com/vlang/v.git'],
		['git', '-C', contract_root, 'add', '--all'],
		['git', '-C', contract_root, 'commit', '-qm', 'runtime contract authority'],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	sha_result := os.exec(['git', '-C', contract_root, 'rev-parse', 'HEAD'])
	tree_result := os.exec(['git', '-C', contract_root, 'rev-parse', 'HEAD^{tree}'])
	assert sha_result.exit_code == 0, sha_result.output
	assert tree_result.exit_code == 0, tree_result.output
	sha := sha_result.output.trim_space()
	tree := tree_result.output.trim_space()
	detach := os.exec(['git', '-C', contract_root, 'checkout', '--detach', '-q', sha])
	assert detach.exit_code == 0, detach.output
	return sha, tree
}

fn prepare_managed_baseline_runtime_activation_fixture(
	suffix string) ManagedBaselineActivationFixture {
	fixture := prepare_t2b_windows_matrix_candidate('managed-baseline-runtime-${suffix}')
	provisional_runtime_sha := 'a'.repeat(40)
	provisional_runtime_tree := activation_source_tree(fixture.manifest_source, 'v-libgc')
	provisional_candidate := activation_candidate_manifest_with_runtime_source(fixture.manifest_source,
		provisional_runtime_sha, provisional_runtime_tree)
	evidence_fixture := managed_baseline_evidence_fixture(provisional_candidate)
	base_source := activation_base_manifest(evidence_fixture.manifest_source)
	os.write_file(fixture.manifest_path, base_source) or { panic(err) }
	base_sha := commit_candidate_paths(fixture.source_repo, [
		'automation/bundle-manifest.json',
	], 'reviewed incomplete Windows managed baseline')
	parent_result := os.exec(['git', '-C', fixture.source_repo, 'rev-parse', '${base_sha}^'])
	base_tree_result :=
		os.exec(['git', '-C', fixture.source_repo, 'rev-parse', '${base_sha}^{tree}'])
	assert parent_result.exit_code == 0, parent_result.output
	assert base_tree_result.exit_code == 0, base_tree_result.output
	parent_sha := parent_result.output.trim_space()
	base_tree := base_tree_result.output.trim_space()
	policy_manifest := bin.parse_strict_json(evidence_fixture.manifest_source) or { panic(err) }
	policy := bin.managed_baseline_activation_policy_projection(policy_manifest,
		evidence_fixture.evidence) or { panic(err) }
	policy_source := bin.canonical_json(policy)
	policy_sha256 := sha256_bytes(policy_source.bytes())
	policy_relative_path := 'baseline-activation/windows-amd64.policy.json'
	policy_path := os.join_path(fixture.automation_root, policy_relative_path)
	os.mkdir_all(os.dir(policy_path)) or { panic(err) }
	os.write_file(policy_path, policy_source) or { panic(err) }
	registry_path := os.join_path(fixture.automation_root, 'targets.json')
	registry_source := os.read_file(registry_path) or { panic(err) }
	os.write_file(registry_path, activation_registry_with_binding(registry_source, 'windows-amd64',
		parent_sha, base_sha, base_tree, sha256_bytes(base_source.bytes()), policy_relative_path,
		policy_sha256)) or { panic(err) }
	runtime_sha, runtime_tree :=
		initialize_managed_baseline_runtime_contract_checkout(fixture.authority.contract_root)
	mut candidate_source := activation_candidate_manifest_with_runtime_source(evidence_fixture.manifest_source,
		runtime_sha, runtime_tree)
	candidate_source = activation_manifest_with_repository_provenance_sha(candidate_source,
		'vlang/tccbin', base_sha)
	manifest_path := os.join_path(fixture.base, 'managed-baseline-runtime-candidate.json')
	os.write_file(manifest_path, candidate_source) or { panic(err) }
	registry_issues := bin.validate_registry(fixture.automation_root) or { panic(err) }
	assert registry_issues.len == 0, '${registry_issues}'
	base_issues := bin.validate_manifest(fixture.automation_root, fixture.manifest_path) or {
		panic(err)
	}
	assert base_issues.len == 0, '${base_issues}'
	candidate_issues := bin.validate_manifest(fixture.automation_root, manifest_path) or {
		panic(err)
	}
	assert candidate_issues.len == 0, '${candidate_issues}'
	return ManagedBaselineActivationFixture{
		base:                      fixture.base
		automation_root:           fixture.automation_root
		contract_root:             fixture.authority.contract_root
		target_id:                 'windows-amd64'
		base_repo_root:            fixture.source_repo
		raw_root:                  fixture.staging_root
		manifest_path:             manifest_path
		result_root:               os.join_path(fixture.base, 'managed-baseline-runtime-result')
		base_sha:                  base_sha
		parent_sha:                parent_sha
		base_tree:                 base_tree
		base_manifest_source:      base_source
		candidate_manifest_source: candidate_source
		policy_path:               policy_path
		policy_sha256:             policy_sha256
		producer_source:           fixture.authority.producer_source
		runtime:                   bin.RuntimeContractBinding{
			repository: 'vlang/v'
			sha:        runtime_sha
		}
		source_commit_evidence:    evidence_fixture.evidence
	}
}

fn replace_managed_baseline_activation_policy(fixture ManagedBaselineActivationFixture,
	manifest_source string) {
	manifest := bin.parse_strict_json(manifest_source) or { panic(err) }
	policy := bin.managed_baseline_activation_policy_projection(manifest,
		fixture.source_commit_evidence) or { panic(err) }
	policy_source := bin.canonical_json(policy)
	new_hash := sha256_bytes(policy_source.bytes())
	registry_path := os.join_path(fixture.automation_root, 'targets.json')
	mut registry_source := os.read_file(registry_path) or { panic(err) }
	assert registry_source.count(fixture.policy_sha256) == 1
	registry_source = registry_source.replace_once(fixture.policy_sha256, new_hash)
	os.write_file(fixture.policy_path, policy_source) or { panic(err) }
	os.write_file(registry_path, registry_source) or { panic(err) }
}

fn activation_manifest_with_unobserved_producer(source string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	toolchain := root.object_value('toolchain') or { panic('toolchain missing') }
	return bin.canonical_json(activation_object_with_replacements(root, {
		'provenance_status': activation_string('incomplete')
		'toolchain':         activation_object_with_replacements(toolchain, {
			'producer_observation': bin.JsonValue{
				kind: .null_value
			}
		})
	}))
}

fn activation_manifest_with_first_inventory_sha(source string, sha string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	inventory := root.object_value('inventory') or { panic('inventory missing') }
	mut entries := inventory.array_value.clone()
	assert entries.len > 0
	entries[0] = activation_object_with_replacements(entries[0], {
		'sha256': activation_string(sha)
	})
	return bin.canonical_json(activation_object_with_replacements(root, {
		'inventory': activation_array(entries)
	}))
}

fn activation_manifest_with_first_inventory_provenance(source string, repository string,
	sha string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	inventory := root.object_value('inventory') or { panic('inventory missing') }
	mut entries := inventory.array_value.clone()
	assert entries.len > 0
	provenance := entries[0].object_value('provenance') or { panic('provenance missing') }
	entries[0] = activation_object_with_replacements(entries[0], {
		'provenance': activation_object_with_replacements(provenance, {
			'repository': activation_string(repository)
			'sha':        activation_string(sha)
		})
	})
	return bin.canonical_json(activation_object_with_replacements(root, {
		'inventory': activation_array(entries)
	}))
}

fn activation_manifest_with_recipe_version(source string, version i64) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	recipe := root.object_value('recipe') or { panic('recipe missing') }
	return bin.canonical_json(activation_object_with_replacements(root, {
		'recipe': activation_object_with_replacements(recipe, {
			'version': bin.JsonValue{
				kind:      .integer
				int_value: version
			}
		})
	}))
}

fn activation_source_sha(source string, source_id string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	sources := root.object_value('sources') or { panic('sources missing') }
	for candidate_source in sources.array_value {
		id := candidate_source.object_value('id') or { panic('source ID missing') }
		if id.string_value == source_id {
			return (candidate_source.object_value('sha') or { panic('source SHA missing') }).string_value
		}
	}
	panic('source ${source_id} missing')
}

fn activation_source_tree(source string, source_id string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	sources := root.object_value('sources') or { panic('sources missing') }
	for candidate_source in sources.array_value {
		id := candidate_source.object_value('id') or { panic('source ID missing') }
		if id.string_value == source_id {
			return (candidate_source.object_value('tree') or { panic('source tree missing') }).string_value
		}
	}
	panic('source ${source_id} missing')
}

fn activation_first_source_evidence_with_replacements(evidence bin.JsonValue,
	replacements map[string]bin.JsonValue) bin.JsonValue {
	assert evidence.kind == .array
	mut entries := evidence.array_value.clone()
	assert entries.len > 0
	entries[0] = activation_object_with_replacements(entries[0], replacements)
	return activation_array(entries)
}

fn managed_baseline_projection_error(manifest_source string, evidence bin.JsonValue) string {
	manifest := bin.parse_strict_json(manifest_source) or { panic(err) }
	mut message := ''
	bin.managed_baseline_activation_policy_projection(manifest, evidence) or { message = err.msg() }
	return message
}

fn activation_git_commit_oid(raw []u8) string {
	mut material := 'commit ${raw.len}\x00'.bytes()
	material << raw
	return sha1.sum(material).hex()
}

fn runtime_contract_binding(production bool) bin.RuntimeContractBinding {
	return bin.RuntimeContractBinding{
		repository: if production { 'vlang/v' } else { 'GGRei/v' }
		sha:        'a'.repeat(40)
	}
}

fn prepare_complete_candidate(suffix string, production bool,
	unresolved string) CompleteCandidateFixture {
	assert unresolved in ['', 'source', 'toolchain']
	base := os.join_path(os.temp_dir(), 'tccbin-complete-candidate-${os.getpid()}-${suffix}')
	source_repo := os.join_path(base, 'source')
	staging_root := os.join_path(base, 'payload')
	os.rmdir_all(base) or {}
	os.mkdir_all(source_repo) or { panic(err) }
	os.mkdir_all(staging_root) or { panic(err) }
	authority := t2a_prepare_toolchain_authority(base, 'linux-amd64')
	t2a_assert_contract_authority_is_sibling(authority, [source_repo, staging_root])
	for args in [
		['git', '-C', source_repo, 'init', '-q'],
		['git', '-C', source_repo, 'config', 'user.email', 'ci@example.invalid'],
		['git', '-C', source_repo, 'config', 'user.name', 'Contract Test'],
		['git', '-C', source_repo, 'config', 'core.autocrlf', 'false'],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	os.write_file(os.join_path(source_repo, 'README.md'), 'baseline\n') or { panic(err) }
	for args in [
		['git', '-C', source_repo, 'add', '--', 'README.md'],
		['git', '-C', source_repo, 'commit', '-qm', 'baseline'],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	recipe_bytes := 'set -eu\nprintf candidate\n'.bytes()
	inventory_bytes := 'int candidate_source;\n'.bytes()
	output_bytes := 'candidate executable bytes\n'.bytes()
	mut manifest_source := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'manifest-complete.valid.json')) or { panic(err) }
	manifest_source = if unresolved == 'toolchain' {
		t2a_profile_bound_unobserved_toolchain(manifest_source, authority)
	} else {
		t2a_resolved_manifest_toolchain(manifest_source, authority)
	}
	manifest_source = manifest_source.replace_once('1'.repeat(64), sha256_bytes(recipe_bytes))
	manifest_source = manifest_source.replace_once('3'.repeat(64), sha256_bytes(inventory_bytes))
	manifest_source = manifest_source.replace_once('4'.repeat(64), sha256_bytes(output_bytes))
	if production {
		manifest_source = manifest_source.replace_once('"contract_repository": "GGRei/v"',
			'"contract_repository": "vlang/v"').replace_once('"contract_mode": "fork-dry-run"',
			'"contract_mode": "production"')
	}
	if unresolved == 'source' {
		manifest_source = manifest_source.replace_once('"sha": "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",\n      "tree": "ffffffffffffffffffffffffffffffffffffffff"',
			'"sha": null,\n      "tree": null').replace_once('"provenance_status": "complete"',
			'"provenance_status": "incomplete"')
	}
	manifest_path := os.join_path(source_repo, 'automation', 'bundle-manifest.json')
	recipe_path := os.join_path(source_repo, 'build.sh')
	inventory_path := os.join_path(source_repo, 'src', 'tcc.c')
	output_path := os.join_path(source_repo, 'tcc.exe')
	for directory in [os.dir(manifest_path), os.dir(inventory_path),
		os.join_path(staging_root, 'src')] {
		os.mkdir_all(directory) or { panic(err) }
	}
	os.write_file(manifest_path, manifest_source) or { panic(err) }
	os.write_file_array(recipe_path, recipe_bytes) or { panic(err) }
	os.write_file_array(inventory_path, inventory_bytes) or { panic(err) }
	os.write_file_array(output_path, output_bytes) or { panic(err) }
	os.write_file_array(os.join_path(staging_root, 'src', 'tcc.c'), inventory_bytes) or {
		panic(err)
	}
	os.write_file_array(os.join_path(staging_root, 'tcc.exe'), output_bytes) or { panic(err) }
	os.chmod(output_path, 0o755) or { panic(err) }
	os.chmod(os.join_path(staging_root, 'tcc.exe'), 0o755) or { panic(err) }
	for args in [
		['git', '-C', source_repo, 'add', '--', '.'],
		['git', '-C', source_repo, 'commit', '-qm', 'candidate'],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	ref_result := os.exec(['git', '-C', source_repo, 'rev-parse', 'HEAD'])
	assert ref_result.exit_code == 0, ref_result.output
	source_ref := ref_result.output.trim_space()
	parent_result := os.exec(['git', '-C', source_repo, 'rev-parse', 'HEAD^'])
	assert parent_result.exit_code == 0, parent_result.output
	parent_ref := parent_result.output.trim_space()
	detach := os.exec(['git', '-C', source_repo, 'checkout', '--detach', '-q', source_ref])
	assert detach.exit_code == 0, detach.output
	return CompleteCandidateFixture{
		base:            base
		automation_root: authority.root
		target_id:       'linux-amd64'
		source_repo:     source_repo
		staging_root:    staging_root
		manifest_path:   manifest_path
		manifest_source: manifest_source
		inventory_bytes: inventory_bytes
		output_bytes:    output_bytes
		parent_ref:      parent_ref
		authority:       authority
		contract:        bin.StagingContract{
			staging_root:    staging_root
			source_git_root: source_repo
			source_git_ref:  source_ref
		}
	}
}

fn prepare_t2b_windows_matrix_candidate(suffix string) CompleteCandidateFixture {
	base := os.join_path(os.temp_dir(), 'tccbin-windows-matrix-${os.getpid()}-${suffix}')
	source_repo := os.join_path(base, 'source')
	staging_root := os.join_path(base, 'payload')
	os.rmdir_all(base) or {}
	os.mkdir_all(source_repo) or { panic(err) }
	os.mkdir_all(staging_root) or { panic(err) }
	authority := t2a_prepare_toolchain_authority(base, 'windows-amd64')
	canonical_openlibm_sha := '9a11e182e1f6b522030d1b8685666147de0ebb562b9d02cce189690fd07cb7db'
	synthetic_openlibm_bytes := minimal_elf64_relocatable()
	synthetic_openlibm_sha := sha256_bytes(synthetic_openlibm_bytes)
	assert synthetic_openlibm_sha == '02947710712bf0e2117aec44327a7bb21042b9733432ec8a4efe325847651beb'
	assert synthetic_openlibm_sha != canonical_openlibm_sha
	registry_path := os.join_path(authority.root, 'targets.json')
	registry_before := os.read_file(registry_path) or { panic(err) }
	assert registry_before.count(canonical_openlibm_sha) == 1
	assert registry_before.count(synthetic_openlibm_sha) == 0
	registry_before_value := bin.parse_strict_json(registry_before) or { panic(err) }
	registry_before_acceptances := registry_before_value.object_value('opaque_acceptances') or {
		panic('opaque acceptances missing')
	}
	assert registry_before_acceptances.array_value.len == 1
	registry_before_acceptance := registry_before_acceptances.array_value[0]
	assert (registry_before_acceptance.object_value('id') or {
		panic('opaque acceptance ID missing')
	}).string_value == 'windows-amd64-openlibm-v1'
	assert (registry_before_acceptance.object_value('path') or {
		panic('opaque acceptance path missing')
	}).string_value == 'lib/openlibm.o'
	assert (registry_before_acceptance.object_value('sha256') or {
		panic('opaque acceptance hash missing')
	}).string_value == canonical_openlibm_sha
	for args in [
		['git', '-C', source_repo, 'init', '-q'],
		['git', '-C', source_repo, 'config', 'user.email', 'ci@example.invalid'],
		['git', '-C', source_repo, 'config', 'user.name', 'Contract Test'],
		['git', '-C', source_repo, 'config', 'core.autocrlf', 'false'],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	mut manifest_source := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'manifest-windows-opaque.valid.json')) or { panic(err) }
	manifest_source = t2a_resolved_manifest_toolchain(manifest_source, authority)
	root_provenance_before := '"provenance_status": "opaque-accepted"'
	root_provenance_after := '"provenance_status": "complete"'
	openlibm_provenance_before := '"provenance": {"status": "incomplete", "repository": null, "sha": null, "source_path": null, "license": null}'
	openlibm_provenance_after := '"provenance": {"status": "complete", "repository": "vlang/tccbin", "sha": "${'f'.repeat(40)}", "source_path": "lib/openlibm.o", "license": "bundle-reviewed"}'
	openlibm_opaque_before := '"opaque": true'
	openlibm_opaque_after := '"opaque": false'
	openlibm_acceptance_before := '"opaque_acceptance_id": "windows-amd64-openlibm-v1"'
	openlibm_acceptance_after := '"opaque_acceptance_id": null'
	for marker in [root_provenance_before, openlibm_provenance_before, openlibm_opaque_before,
		openlibm_acceptance_before] {
		assert manifest_source.count(marker) == 1
	}
	manifest_source = manifest_source.replace_once(root_provenance_before, root_provenance_after).replace_once(openlibm_provenance_before,
		openlibm_provenance_after).replace_once(openlibm_opaque_before, openlibm_opaque_after).replace_once(openlibm_acceptance_before,
		openlibm_acceptance_after)
	manifest := bin.parse_strict_json(manifest_source) or { panic(err) }
	assert (manifest.object_value('provenance_status') or { panic('provenance status missing') }).string_value == 'complete'
	manifest_inventory := manifest.object_value('inventory') or { panic('inventory missing') }
	mut openlibm_entries := []bin.JsonValue{}
	for entry in manifest_inventory.array_value {
		path := entry.object_value('path') or { panic('inventory path missing') }
		if path.string_value == 'lib/openlibm.o' {
			openlibm_entries << entry
		}
	}
	assert openlibm_entries.len == 1
	openlibm_entry := openlibm_entries[0]
	openlibm_provenance := openlibm_entry.object_value('provenance') or {
		panic('openlibm provenance missing')
	}
	assert (openlibm_provenance.object_value('status') or {
		panic('openlibm provenance status missing')
	}).string_value == 'complete'
	assert (openlibm_provenance.object_value('repository') or {
		panic('openlibm provenance repository missing')
	}).string_value == 'vlang/tccbin'
	assert (openlibm_provenance.object_value('sha') or { panic('openlibm provenance SHA missing') }).string_value == 'f'.repeat(40)
	assert (openlibm_provenance.object_value('source_path') or {
		panic('openlibm provenance source path missing')
	}).string_value == 'lib/openlibm.o'
	assert (openlibm_provenance.object_value('license') or {
		panic('openlibm provenance license missing')
	}).string_value == 'bundle-reviewed'
	assert !(openlibm_entry.object_value('opaque') or { panic('openlibm opaque flag missing') }).bool_value
	assert (openlibm_entry.object_value('opaque_acceptance_id') or {
		panic('openlibm acceptance ID missing')
	}).kind == .null_value
	assert (openlibm_entry.object_value('role') or { panic('openlibm role missing') }).string_value == 'legacy-math-runtime'
	for member, expected in {
		'format':      'ELF64 little-endian'
		'object_type': 'ET_REL'
		'machine':     'EM_X86_64'
		'os_abi':      'System V'
	} {
		assert (openlibm_entry.object_value(member) or { panic('openlibm ${member} missing') }).string_value == expected
	}
	mut controls := [manifest.object_value('recipe') or { panic('recipe missing') }]
	controls << (manifest.object_value('patches') or { panic('patches missing') }).array_value
	controls << (manifest.object_value('transforms') or { panic('transforms missing') }).array_value
	for index, control in controls {
		path := (control.object_value('path') or { panic('control path missing') }).string_value
		old_hash := (control.object_value('sha256') or { panic('control hash missing') }).string_value
		bytes := 'reviewed matrix control ${index}: ${path}\n'.bytes()
		assert manifest_source.count(old_hash) == 1
		manifest_source = manifest_source.replace_once(old_hash, sha256_bytes(bytes))
		full_path := os.join_path(source_repo, path)
		os.mkdir_all(os.dir(full_path)) or { panic(err) }
		os.write_file_array(full_path, bytes) or { panic(err) }
	}
	mut entries :=
		(manifest.object_value('overlays') or { panic('overlays missing') }).array_value.clone()
	entries << (manifest.object_value('inventory') or { panic('inventory missing') }).array_value
	entries << (manifest.object_value('outputs') or { panic('outputs missing') }).array_value
	mut inventory_bytes := []u8{}
	mut output_bytes := []u8{}
	for index, entry in entries {
		path := (entry.object_value('path') or { panic('entry path missing') }).string_value
		old_hash := (entry.object_value('sha256') or { panic('entry hash missing') }).string_value
		bytes := if path == 'lib/openlibm.o' {
			synthetic_openlibm_bytes.clone()
		} else {
			'reviewed matrix payload ${index}: ${path}\n'.bytes()
		}
		new_hash := sha256_bytes(bytes)
		assert manifest_source.count(old_hash) == 1
		manifest_source = manifest_source.replace_once(old_hash, new_hash)
		if path == 'lib/openlibm.o' {
			assert old_hash == canonical_openlibm_sha
			assert new_hash == synthetic_openlibm_sha
		}
		for root in [source_repo, staging_root] {
			full_path := os.join_path(root, path)
			os.mkdir_all(os.dir(full_path)) or { panic(err) }
			os.write_file_array(full_path, bytes) or { panic(err) }
			if (entry.object_value('git_mode') or { panic('entry mode missing') }).string_value == '100755' {
				os.chmod(full_path, 0o755) or { panic(err) }
			}
		}
		if path == 'src/tcc.c' {
			inventory_bytes = bytes.clone()
		}
		if path == 'tcc.exe' {
			output_bytes = bytes.clone()
		}
	}
	assert manifest_source.count(canonical_openlibm_sha) == 0
	assert manifest_source.count(synthetic_openlibm_sha) == 1
	registry_after := os.read_file(registry_path) or { panic(err) }
	assert registry_after == registry_before
	assert registry_after.count(canonical_openlibm_sha) == 1
	assert registry_after.count(synthetic_openlibm_sha) == 0
	registry_after_value := bin.parse_strict_json(registry_after) or { panic(err) }
	registry_after_acceptances := registry_after_value.object_value('opaque_acceptances') or {
		panic('opaque acceptances missing after materialization')
	}
	assert registry_after_acceptances.array_value.len == 1
	registry_after_acceptance := registry_after_acceptances.array_value[0]
	assert (registry_after_acceptance.object_value('id') or {
		panic('opaque acceptance ID missing after materialization')
	}).string_value == 'windows-amd64-openlibm-v1'
	assert (registry_after_acceptance.object_value('path') or {
		panic('opaque acceptance path missing after materialization')
	}).string_value == 'lib/openlibm.o'
	assert (registry_after_acceptance.object_value('sha256') or {
		panic('opaque acceptance hash missing after materialization')
	}).string_value == canonical_openlibm_sha
	manifest_path := os.join_path(source_repo, 'automation', 'bundle-manifest.json')
	os.mkdir_all(os.dir(manifest_path)) or { panic(err) }
	os.write_file(manifest_path, manifest_source) or { panic(err) }
	for args in [
		['git', '-C', source_repo, 'add', '--', '.'],
		['git', '-C', source_repo, 'commit', '-qm', 'reviewed Windows matrix fixture'],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	ref_result := os.exec(['git', '-C', source_repo, 'rev-parse', 'HEAD'])
	assert ref_result.exit_code == 0, ref_result.output
	source_ref := ref_result.output.trim_space()
	parent_result := os.exec(['git', '-C', source_repo, 'rev-parse', 'HEAD^'])
	parent_ref := if parent_result.exit_code == 0 {
		parent_result.output.trim_space()
	} else {
		source_ref
	}
	detach := os.exec(['git', '-C', source_repo, 'checkout', '--detach', '-q', source_ref])
	assert detach.exit_code == 0, detach.output
	return CompleteCandidateFixture{
		base:            base
		automation_root: authority.root
		target_id:       'windows-amd64'
		source_repo:     source_repo
		staging_root:    staging_root
		manifest_path:   manifest_path
		manifest_source: manifest_source
		inventory_bytes: inventory_bytes
		output_bytes:    output_bytes
		parent_ref:      parent_ref
		authority:       authority
		contract:        bin.StagingContract{
			staging_root:    staging_root
			source_git_root: source_repo
			source_git_ref:  source_ref
		}
	}
}

fn prepare_legacy_composition_fixture(suffix string,
	unresolved string) LegacyCompositionFixture {
	fixture := prepare_complete_candidate('legacy-${suffix}', false, unresolved)
	workflow_path := os.join_path(fixture.source_repo, '.github', 'workflows', 'build-and-test.yml')
	os.mkdir_all(os.dir(workflow_path)) or { panic(err) }
	os.write_file(workflow_path, 'name: legacy fixture\non: [push]\n') or { panic(err) }
	os.rm(fixture.manifest_path) or { panic(err) }
	os.rm(os.join_path(fixture.source_repo, 'README.md')) or { panic(err) }
	for args in [
		['git', '-C', fixture.source_repo, 'add', '--all'],
		['git', '-C', fixture.source_repo, 'commit', '-qm', 'reviewed legacy base'],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	base_ref := os.exec(['git', '-C', fixture.source_repo, 'rev-parse', 'HEAD'])
	assert base_ref.exit_code == 0, base_ref.output
	base_sha := base_ref.output.trim_space()
	manifest_path := os.join_path(fixture.base, 'external-manifest.json')
	os.write_file(manifest_path, fixture.manifest_source) or { panic(err) }
	contract_automation_root := os.join_path(fixture.base, 'contract', 'thirdparty',
		'tccbin_automation')
	os.mkdir_all(contract_automation_root) or { panic(err) }
	os.cp_all(os.join_path(automation_root(), 'schemas'), os.join_path(contract_automation_root,
		'schemas'), true) or { panic(err) }
	os.cp_all(os.join_path(fixture.automation_root, 'toolchain-profiles'), os.join_path(contract_automation_root,
		'toolchain-profiles'), true) or { panic(err) }
	manifest := bin.parse_strict_json(fixture.manifest_source) or { panic(err) }
	policy := bin.legacy_onboarding_policy_projection(manifest) or { panic(err) }
	policy_source := bin.canonical_json(policy)
	policy_hash := sha256_bytes(policy_source.bytes())
	policy_relative_path := 'onboarding/linux-amd64.policy.json'
	policy_path := os.join_path(contract_automation_root, policy_relative_path)
	os.mkdir_all(os.dir(policy_path)) or { panic(err) }
	os.write_file(policy_path, policy_source) or { panic(err) }
	mut registry_source := os.read_file(os.join_path(fixture.automation_root, 'targets.json')) or {
		panic(err)
	}
	registry_marker := '"base_sha": "ece46f06fbe6eb701d52442f11dd59c48d166cae",\n        "policy_path": null,\n        "policy_sha256": null'
	assert registry_source.count(registry_marker) == 1
	registry_source = registry_source.replace_once(registry_marker,
		'"base_sha": "${base_sha}",\n        "policy_path": "${policy_relative_path}",\n        "policy_sha256": "${policy_hash}"')
	activation_parent_marker := '"parent_sha": "ece46f06fbe6eb701d52442f11dd59c48d166cae"'
	assert registry_source.count(activation_parent_marker) == 1
	registry_source = registry_source.replace_once(activation_parent_marker,
		'"parent_sha": "${base_sha}"')
	os.write_file(os.join_path(contract_automation_root, 'targets.json'), registry_source) or {
		panic(err)
	}
	return LegacyCompositionFixture{
		base:            fixture.base
		automation_root: contract_automation_root
		base_repo_root:  fixture.source_repo
		raw_root:        fixture.staging_root
		manifest_path:   manifest_path
		result_root:     os.join_path(fixture.base, 'legacy-result')
		base_sha:        base_sha
		manifest_source: fixture.manifest_source
	}
}

fn replace_legacy_composition_policy(fixture LegacyCompositionFixture,
	manifest_source string) {
	policy_path := os.join_path(fixture.automation_root, 'onboarding', 'linux-amd64.policy.json')
	old_policy_source := os.read_file(policy_path) or { panic(err) }
	old_policy_hash := sha256_bytes(old_policy_source.bytes())
	manifest := bin.parse_strict_json(manifest_source) or { panic(err) }
	policy := bin.legacy_onboarding_policy_projection(manifest) or { panic(err) }
	policy_source := bin.canonical_json(policy)
	policy_hash := sha256_bytes(policy_source.bytes())
	registry_path := os.join_path(fixture.automation_root, 'targets.json')
	mut registry_source := os.read_file(registry_path) or { panic(err) }
	assert registry_source.count(old_policy_hash) == 1
	registry_source = registry_source.replace_once(old_policy_hash, policy_hash)
	os.write_file(policy_path, policy_source) or { panic(err) }
	os.write_file(registry_path, registry_source) or { panic(err) }
	os.write_file(fixture.manifest_path, manifest_source) or { panic(err) }
}

fn prepare_transform_candidate(suffix string) (CompleteCandidateFixture, string, []u8) {
	base_fixture := prepare_complete_candidate(suffix, false, '')
	transform_path := 'inputs/reviewed-transform.patch'
	transform_bytes := 'reviewed transform bytes\n'.bytes()
	mut manifest_source := base_fixture.manifest_source.replace_once('"transforms": []',
		'"transforms": [{"id":"reviewed-transform","path":"${transform_path}","sha256":"${sha256_bytes(transform_bytes)}","owner":"bundle-overlay","order":1,"apply_stage":"bundle-payload-post-copy","effect_ids":["reviewed-effect"]}]')
	os.mkdir_all(os.join_path(base_fixture.source_repo, 'inputs')) or { panic(err) }
	os.write_file_array(os.join_path(base_fixture.source_repo, transform_path), transform_bytes) or {
		panic(err)
	}
	os.write_file(base_fixture.manifest_path, manifest_source) or { panic(err) }
	for args in [
		['git', '-C', base_fixture.source_repo, 'add', '--', 'automation/bundle-manifest.json',
			transform_path],
		['git', '-C', base_fixture.source_repo, 'commit', '-qm', 'transform candidate'],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	ref_result := os.exec(['git', '-C', base_fixture.source_repo, 'rev-parse', 'HEAD'])
	assert ref_result.exit_code == 0, ref_result.output
	source_ref := ref_result.output.trim_space()
	return CompleteCandidateFixture{
		...base_fixture
		manifest_source: manifest_source
		parent_ref:      base_fixture.contract.source_git_ref
		contract:        bin.StagingContract{
			staging_root:    base_fixture.staging_root
			source_git_root: base_fixture.source_repo
			source_git_ref:  source_ref
		}
	}, transform_path, transform_bytes
}

fn prepare_symlink_candidate(suffix string, symlink_path string,
	target string) CompleteCandidateFixture {
	base_fixture := prepare_complete_candidate(suffix, false, '')
	mac_authority := t2a_prepare_toolchain_authority(base_fixture.base, 'macos-amd64')
	mut manifest_source := base_fixture.manifest_source.replace_once('"target_id": "linux-amd64"',
		'"target_id": "macos-amd64"').replace_once('"branch": "thirdparty-linux-amd64"',
		'"branch": "thirdparty-macos-amd64"').replace_once('"affected_targets": ["linux-amd64"]',
		'"affected_targets": ["macos-amd64"]')
	manifest_source = t2a_rebind_manifest_toolchain(manifest_source, base_fixture.authority,
		mac_authority)
	source_marker := '      "tree": "ffffffffffffffffffffffffffffffffffffffff"\n    }\n  ],\n  "recipe":'
	source_replacement := '      "tree": "ffffffffffffffffffffffffffffffffffffffff"\n    },\n    {\n      "id": "libatomic_ops",\n      "repository": "https://github.com/bdwgc/libatomic_ops.git",\n      "ref": "master",\n      "sha": "1111111111111111111111111111111111111111",\n      "tree": "2222222222222222222222222222222222222222"\n    }\n  ],\n  "recipe":'
	assert manifest_source.count(source_marker) == 1
	manifest_source = manifest_source.replace_once(source_marker, source_replacement)
	current_inventory_sha := sha256_bytes(base_fixture.inventory_bytes)
	inventory_sha_marker := '"sha256": "${current_inventory_sha}"'
	assert manifest_source.count(inventory_sha_marker) == 1
	target_json := bin.canonical_json(bin.JsonValue{
		kind:         .string_value
		string_value: target
	})
	manifest_source = manifest_source.replace_once('"path": "src/tcc.c"',
		'"path": "${symlink_path}"').replace_once('"kind": "file"', '"kind": "symlink"').replace_once('"git_mode": "100644"',
		'"git_mode": "120000"').replace_once(inventory_sha_marker,
		'"sha256": "${sha256_bytes(target.bytes())}"').replace_once('"symlink_target": null',
		'"symlink_target": ${target_json}')
	for root in [base_fixture.source_repo, base_fixture.staging_root] {
		old_path := os.join_path(root, 'src', 'tcc.c')
		os.rm(old_path) or { panic(err) }
		os.rmdir(os.dir(old_path)) or { panic(err) }
		full_symlink_path := os.join_path(root, symlink_path)
		os.mkdir_all(os.dir(full_symlink_path)) or { panic(err) }
		os.symlink(target, full_symlink_path) or { panic(err) }
	}
	os.write_file(base_fixture.manifest_path, manifest_source) or { panic(err) }
	for args in [
		['git', '-C', base_fixture.source_repo, 'add', '--all'],
		['git', '-C', base_fixture.source_repo, 'commit', '-qm', 'symlink candidate'],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	ref_result := os.exec(['git', '-C', base_fixture.source_repo, 'rev-parse', 'HEAD'])
	assert ref_result.exit_code == 0, ref_result.output
	return CompleteCandidateFixture{
		...base_fixture
		automation_root: mac_authority.root
		target_id:       'macos-amd64'
		authority:       mac_authority
		manifest_source: manifest_source
		parent_ref:      base_fixture.contract.source_git_ref
		contract:        bin.StagingContract{
			staging_root:    base_fixture.staging_root
			source_git_root: base_fixture.source_repo
			source_git_ref:  ref_result.output.trim_space()
		}
	}
}

fn committed_contract_for(fixture CompleteCandidateFixture, message string) bin.StagingContract {
	for args in [
		['git', '-C', fixture.source_repo, 'add', '--all'],
		['git', '-C', fixture.source_repo, 'commit', '-qm', message],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	ref_result := os.exec(['git', '-C', fixture.source_repo, 'rev-parse', 'HEAD'])
	assert ref_result.exit_code == 0, ref_result.output
	return bin.StagingContract{
		staging_root:    fixture.staging_root
		source_git_root: fixture.source_repo
		source_git_ref:  ref_result.output.trim_space()
	}
}

struct ToolchainObservationFixture {
	base             string
	automation_root  string
	profile_path     string
	observation_path string
	registry_source  string
	profile_source   string
	profile_sha256   string
	producer_source  string
	validator_source string
}

fn provenance_toolchain_role_id(phase string, strategy string) string {
	base := if phase == 'producer' { 'bundle-builder' } else { 'contract-validator' }
	suffix := if strategy == 'cpa-guest' { 'guest' } else { 'host' }
	return '${base}-${suffix}'
}

fn provenance_toolchain_policy_role(phase string, strategy string) string {
	role_id := provenance_toolchain_role_id(phase, strategy)
	if strategy == 'cpa-guest' {
		return '{"role_id":"${role_id}","identity_strategy":"cpa-guest","identity_policy":[{"name":"arch","match":"exact","value":"amd64"},{"name":"compiler_binary_sha256","match":"sha256"},{"name":"compiler_command","match":"exact","value":"clang"},{"name":"compiler_family","match":"exact","value":"clang"},{"name":"compiler_target","match":"present"},{"name":"compiler_version","match":"present"},{"name":"guest_os","match":"exact","value":"freebsd"},{"name":"observed_release","match":"release-compatible","value":"15.1"},{"name":"requested_release","match":"exact","value":"15.1"}]}'
	}
	return '{"role_id":"${role_id}","identity_strategy":"cpa-host","identity_policy":[{"name":"action_sha","match":"exact","value":"${'a'.repeat(40)}"},{"name":"arch","match":"exact","value":"amd64"},{"name":"compiler_binary_sha256","match":"sha256"},{"name":"compiler_command","match":"exact","value":"clang"},{"name":"compiler_family","match":"exact","value":"clang"},{"name":"compiler_target","match":"present"},{"name":"compiler_version","match":"present"},{"name":"image_os","match":"present"},{"name":"image_version","match":"present"},{"name":"os","match":"exact","value":"linux"},{"name":"runner_label","match":"exact","value":"ubuntu-24.04"}]}'
}

fn provenance_toolchain_resolved_role(phase string, strategy string,
	resolution_placeholder string, evidence_sha256 string) string {
	role_id := provenance_toolchain_role_id(phase, strategy)
	if strategy == 'cpa-guest' {
		return '{"role_id":"${role_id}","identity_strategy":"cpa-guest","resolved_identity":[{"name":"arch","value":"amd64"},{"name":"compiler_binary_sha256","value":"${'3'.repeat(64)}"},{"name":"compiler_command","value":"clang"},{"name":"compiler_family","value":"clang"},{"name":"compiler_target","value":"x86_64-unknown-freebsd15.1"},{"name":"compiler_version","value":"clang 19.1.7"},{"name":"guest_os","value":"freebsd"},{"name":"observed_release","value":"15.1-RELEASE-p2"},{"name":"requested_release","value":"15.1"}],"resolution_digest":"${resolution_placeholder}","evidence_sha256":"${evidence_sha256}"}'
	}
	return '{"role_id":"${role_id}","identity_strategy":"cpa-host","resolved_identity":[{"name":"action_sha","value":"${'a'.repeat(40)}"},{"name":"arch","value":"amd64"},{"name":"compiler_binary_sha256","value":"${'4'.repeat(64)}"},{"name":"compiler_command","value":"clang"},{"name":"compiler_family","value":"clang"},{"name":"compiler_target","value":"x86_64-unknown-linux-gnu"},{"name":"compiler_version","value":"clang 19.1.7"},{"name":"image_os","value":"ubuntu24"},{"name":"image_version","value":"20260801.1"},{"name":"os","value":"linux"},{"name":"runner_label","value":"ubuntu-24.04"}],"resolution_digest":"${resolution_placeholder}","evidence_sha256":"${evidence_sha256}"}'
}

fn provenance_toolchain_profile_source() string {
	mut producer_roles := []string{}
	mut validator_roles := []string{}
	for strategy in ['cpa-guest', 'cpa-host'] {
		producer_roles << provenance_toolchain_policy_role('producer', strategy)
		validator_roles << provenance_toolchain_policy_role('validator', strategy)
	}
	profile := bin.parse_strict_json('{"schema_version":1,"profile_id":"freebsd-amd64-synthetic-v1","target_id":"freebsd-amd64","producer":[${producer_roles.join(',')}],"validator":[${validator_roles.join(',')}]}') or {
		panic(err)
	}
	return bin.canonical_json(profile)
}

fn provenance_toolchain_observation_source(profile_sha256 string, phase string) string {
	mut role_sources := []string{}
	for index, strategy in ['cpa-guest', 'cpa-host'] {
		resolution_placeholder := if index == 0 { '8'.repeat(64) } else { '9'.repeat(64) }
		evidence_sha256 := if index == 0 { 'e'.repeat(64) } else { 'f'.repeat(64) }
		role_sources << provenance_toolchain_resolved_role(phase, strategy, resolution_placeholder,
			evidence_sha256)
	}
	observation_placeholder := '0'.repeat(64)
	mut source := bin.canonical_json(bin.parse_strict_json('{"schema_version":1,"target_id":"freebsd-amd64","profile_id":"freebsd-amd64-synthetic-v1","profile_sha256":"${profile_sha256}","phase":"${phase}","roles":[${role_sources.join(',')}],"observation_digest":"${observation_placeholder}"}') or {
		panic(err)
	})
	observation := bin.parse_strict_json(source) or { panic(err) }
	roles := observation.object_value('roles') or { panic('toolchain roles missing') }
	for role in roles.array_value {
		resolution_placeholder := (role.object_value('resolution_digest') or {
			panic('resolution digest missing')
		}).string_value
		resolution_digest := bin.toolchain_role_resolution_digest(observation, role) or {
			panic(err)
		}
		source = source.replace_once('"resolution_digest":"${resolution_placeholder}"',
			'"resolution_digest":"${resolution_digest}"')
	}
	with_resolution := bin.parse_strict_json(source) or { panic(err) }
	observation_digest := bin.toolchain_observation_digest(with_resolution) or { panic(err) }
	return source.replace_once('"observation_digest":"${observation_placeholder}"',
		'"observation_digest":"${observation_digest}"')
}

fn prepare_toolchain_observation_fixture(suffix string) ToolchainObservationFixture {
	base := os.join_path(os.temp_dir(), 'tccbin-toolchain-${os.getpid()}-${suffix}')
	automation := os.join_path(base, 'automation')
	os.rmdir_all(base) or {}
	os.mkdir_all(automation) or { panic(err) }
	os.cp_all(os.join_path(automation_root(), 'schemas'), os.join_path(automation, 'schemas'), true) or {
		panic(err)
	}
	profile_source := provenance_toolchain_profile_source()
	profile := bin.parse_strict_json(profile_source) or { panic(err) }
	profile_sha256 := bin.json_sha256(profile)
	profile_relative_path := 'toolchain-profiles/freebsd-amd64.profile.json'
	profile_path := os.join_path(automation, profile_relative_path)
	os.mkdir_all(os.dir(profile_path)) or { panic(err) }
	os.write_file(profile_path, profile_source) or { panic(err) }
	mut registry_source := os.read_file(os.join_path(automation_root(), 'targets.json')) or {
		panic(err)
	}
	binding_marker := '"toolchain_profile": {\n        "profile_id": null,\n        "profile_path": null,\n        "profile_sha256": null\n      }'
	assert registry_source.count(binding_marker) == 6
	registry_source = registry_source.replace_once(binding_marker,
		'"toolchain_profile": {\n        "profile_id": "freebsd-amd64-synthetic-v1",\n        "profile_path": "${profile_relative_path}",\n        "profile_sha256": "${profile_sha256}"\n      }')
	os.write_file(os.join_path(automation, 'targets.json'), registry_source) or { panic(err) }
	producer_source := provenance_toolchain_observation_source(profile_sha256, 'producer')
	validator_source := provenance_toolchain_observation_source(profile_sha256, 'validator')
	observation_path := os.join_path(base, 'toolchain-observation.json')
	os.write_file(observation_path, producer_source) or { panic(err) }
	return ToolchainObservationFixture{
		base:             base
		automation_root:  automation
		profile_path:     profile_path
		observation_path: observation_path
		registry_source:  registry_source
		profile_source:   profile_source
		profile_sha256:   profile_sha256
		producer_source:  producer_source
		validator_source: validator_source
	}
}

fn authenticate_toolchain_error(root string, observation_path string) string {
	bin.authenticate_toolchain_observation_file(root, 'freebsd-amd64', observation_path) or {
		return err.msg()
	}
	return ''
}

fn registry_toolchain_error(root string) string {
	issues := bin.validate_registry(root) or { return err.msg() }
	if issues.len == 0 {
		return ''
	}
	return issues[0].message
}

fn mutate_toolchain_document_same_inode(path string) ! {
	before := os.lstat(path)!
	source := os.read_file(path)!
	mutated := source.replace_once('"a"', '"b"')
	if mutated == source || mutated.len != source.len {
		return error('same-inode mutation fixture is not length preserving')
	}
	os.write_file(path, mutated)!
	os.utime(path, before.atime, before.mtime)!
	after := os.lstat(path)!
	if before.size != after.size || before.mode != after.mode || before.mtime != after.mtime {
		return error('same-inode mutation fixture did not preserve size, mode, and mtime')
	}
	$if !windows {
		if before.dev != after.dev || before.inode != after.inode {
			return error('same-inode mutation fixture replaced the physical file')
		}
	}
}

fn test_toolchain_document_path_handle_association_rejects_same_metadata_replacement() {
	base := os.join_path(os.temp_dir(), 'tccbin-toolchain-association-${os.getpid()}')
	os.rmdir_all(base) or {}
	os.mkdir_all(base) or { panic(err) }
	defer {
		os.rmdir_all(base) or {}
	}
	document_path := os.join_path(base, 'document.json')
	replacement_path := os.join_path(base, 'replacement.json')
	opened_path := os.join_path(base, 'opened-document.json')
	document_source := '{"value":"a"}\n'
	replacement_source := '{"value":"b"}\n'
	assert document_source.len == replacement_source.len
	os.write_file(document_path, document_source) or { panic(err) }
	os.write_file(replacement_path, replacement_source) or { panic(err) }
	os.chmod(document_path, 0o640) or { panic(err) }
	os.chmod(replacement_path, 0o640) or { panic(err) }
	stable_time := i64(1_700_000_000)
	os.utime(document_path, stable_time, stable_time) or { panic(err) }
	os.utime(replacement_path, stable_time, stable_time) or { panic(err) }
	document_stat := os.lstat(document_path) or { panic(err) }
	replacement_stat := os.lstat(replacement_path) or { panic(err) }
	assert document_stat.size == replacement_stat.size
	assert document_stat.mode == replacement_stat.mode
	assert document_stat.atime == replacement_stat.atime
	assert document_stat.mtime == replacement_stat.mtime
	$if !windows {
		assert document_stat.inode != replacement_stat.inode
	}

	mut opened := os.open(document_path) or { panic(err) }
	defer {
		opened.close()
	}
	bin.attest_toolchain_document_path_handle(document_path, &opened) or { panic(err) }
	$if windows {
		mut mismatch := ''
		bin.attest_toolchain_document_path_handle(replacement_path, &opened) or {
			mismatch = err.msg()
		}
		assert mismatch == 'toolchain document path does not identify its open handle'
	} $else {
		os.mv(document_path, opened_path) or { panic(err) }
		os.mv(replacement_path, document_path) or { panic(err) }
		replaced_stat := os.lstat(document_path) or { panic(err) }
		assert replaced_stat.size == document_stat.size
		assert replaced_stat.mode == document_stat.mode
		assert replaced_stat.atime == document_stat.atime
		assert replaced_stat.mtime == document_stat.mtime
		mut mismatch := ''
		bin.attest_toolchain_document_path_handle(document_path, &opened) or {
			mismatch = err.msg()
		}
		assert mismatch == 'toolchain document path does not identify its open handle'
	}
}

fn test_toolchain_document_native_open_and_high_resolution_handle_snapshot_are_closed() {
	base := os.join_path(os.temp_dir(), 'tccbin-toolchain-native-open-${os.getpid()}')
	os.rmdir_all(base) or {}
	os.mkdir_all(base) or { panic(err) }
	defer {
		os.rmdir_all(base) or {}
	}
	document_path := os.join_path(base, 'document.json')
	os.write_file(document_path, '{"value":"a"}\n') or { panic(err) }
	os.utime(document_path, 1_700_000_000, 1_700_000_000) or { panic(err) }
	bin.attest_native_toolchain_document_open(document_path) or { panic(err) }
	mut mutation_error := ''
	bin.attest_stable_toolchain_document_boundary(document_path,
		mutate_toolchain_document_same_inode) or { mutation_error = err.msg() }
	assert mutation_error == 'toolchain document changed while being read'
	assert os.read_file(document_path) or { panic(err) } == '{"value":"b"}\n'

	$if !windows {
		fifo_path := os.join_path(base, 'no-writer.fifo')
		fifo_result := os.exec(['mkfifo', fifo_path])
		assert fifo_result.exit_code == 0, fifo_result.output
		mut fifo_error := ''
		bin.attest_native_toolchain_document_open(fifo_path) or { fifo_error = err.msg() }
		assert fifo_error == 'native toolchain document is not a physical regular file'
		mut device_error := ''
		bin.attest_native_toolchain_document_open('/dev/null') or { device_error = err.msg() }
		assert device_error == 'native toolchain document is not a physical regular file'
	}
}

fn test_native_timestamp_snapshots_keep_posix_components_and_windows_ticks_independent() {
	header_source := os.read_file(os.join_path(automation_root(), 'bin', 'provenance_native_nix.h')) or {
		panic(err)
	}
	posix_source := os.read_file(os.join_path(automation_root(), 'bin', 'provenance_nix.c.v')) or {
		panic(err)
	}
	windows_source := os.read_file(os.join_path(automation_root(), 'bin', 'provenance_windows.c.v')) or {
		panic(err)
	}
	common_source := os.read_file(os.join_path(automation_root(), 'bin', 'provenance.v')) or {
		panic(err)
	}
	assert !header_source.contains('tccbin_timespec_to_ns')
	assert !header_source.contains('1000000000')
	for member in ['mtime_sec', 'mtime_nsec', 'ctime_sec', 'ctime_nsec'] {
		assert header_source.count(member) == 3
		assert posix_source.count(member) == 3
		assert common_source.count(member) == 1
	}
	for member in ['mtime_windows_ticks', 'ctime_windows_ticks'] {
		assert windows_source.count(member) == 1
		assert common_source.count(member) == 1
	}
	assert !windows_source.contains('last_write_time *')
	assert !windows_source.contains('change_time *')
	for forbidden in ['#include <winternl.h>', 'GetFileInformationByHandleEx',
		'FileIdBothDirectoryRestartInfo', 'FileIdBothDirectoryInfo', 'FILE_ID_BOTH_DIR_INFO',
		'GetFinalPathNameByHandleW', 'FindFirstFileW', 'FindNextFileW', 'tccbin_windows_child_path'] {
		assert !header_source.contains(forbidden)
	}
	for required in ['GetModuleHandleW(L"ntdll.dll")', 'GetProcAddress(module, "NtOpenFile")',
		'GetProcAddress(module, "NtQueryDirectoryFile")', 'RootDirectory',
		'TCCBIN_FILE_NAMES_INFORMATION = 12', 'TCCBIN_FILE_NAMES_HEADER_SIZE = 12',
		'TCCBIN_FILE_NAMES_NEXT_OFFSET = 0', 'TCCBIN_FILE_NAMES_INDEX_OFFSET = 4',
		'TCCBIN_FILE_NAMES_LENGTH_OFFSET = 8', 'memcpy(&next', 'memcpy(&file_index',
		'memcpy(&filename_length_field', 'information != 0', 'TCCBIN_FILE_NAMES_STATUS_SUCCESS',
		'TCCBIN_FILE_NAMES_STATUS_NO_MORE_FILES', 'FILE_OPEN_REPARSE_POINT',
		'FILE_SYNCHRONOUS_IO_NONALERT'] {
		assert header_source.contains(required)
	}
	assert header_source.count('static int tccbin_file_names_apply_batch_status(') == 1
	assert header_source.count('batch_result = tccbin_file_names_apply_batch_status(') == 1
	assert header_source.count('static int tccbin_file_names_decode_record(') == 1
	assert header_source.count('decoded = tccbin_file_names_decode_record(') == 1
	for layout in ['tccbin_windows_unicode_string', 'tccbin_windows_object_attributes',
		'tccbin_windows_io_status_value', 'tccbin_windows_io_status_block'] {
		assert header_source.contains(layout)
	}
	assert header_source.count('TCCBIN_WINDOWS_ABI_ASSERT') == 35
	for forbidden in ['UNICODE_STRING', 'OBJECT_ATTRIBUTES', 'IO_STATUS_BLOCK',
		'FILE_NAMES_INFORMATION'] {
		assert !windows_source.contains(forbidden)
	}
	assert !windows_source.contains('open_child_snapshot_no_follow')
	assert windows_source.count('tccbin_windows_open_child_no_follow') == 4
}

fn test_toolchain_profile_is_dormant_then_authenticates_both_closed_phases() {
	dormant := authenticate_toolchain_error(automation_root(), os.join_path(os.temp_dir(),
		'tccbin-absent-toolchain-observation.json'))
	assert dormant == 'target has no reviewed toolchain profile'
	fixture := prepare_toolchain_observation_fixture('positive')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	issues := bin.validate_registry(fixture.automation_root) or { panic(err) }
	assert issues.len == 0, '${issues}'
	producer := bin.authenticate_toolchain_observation_file(fixture.automation_root,
		'freebsd-amd64', fixture.observation_path) or { panic(err) }
	assert producer.target_id == 'freebsd-amd64'
	assert producer.profile_id == 'freebsd-amd64-synthetic-v1'
	assert producer.profile_sha256 == fixture.profile_sha256
	assert producer.phase == 'producer'
	assert producer.observation_sha256 == bin.json_sha256(bin.parse_strict_json(fixture.producer_source) or {
		panic(err)
	})
	os.write_file(fixture.observation_path, fixture.validator_source) or { panic(err) }
	validator := bin.authenticate_toolchain_observation_file(fixture.automation_root,
		'freebsd-amd64', fixture.observation_path) or { panic(err) }
	assert validator.phase == 'validator'
	assert validator.observation_sha256 != producer.observation_sha256
}

fn test_toolchain_profile_loader_rejects_path_hash_target_id_jcs_and_weak_roles() {
	fixture := prepare_toolchain_observation_fixture('profile-negative')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	registry_path := os.join_path(fixture.automation_root, 'targets.json')
	mutated_path := fixture.registry_source.replace_once('toolchain-profiles/freebsd-amd64.profile.json',
		'toolchain-profiles/linux-amd64.profile.json')
	os.write_file(registry_path, mutated_path) or { panic(err) }
	assert registry_toolchain_error(fixture.automation_root) == 'managed target toolchain profile path is not exact'
	os.write_file(registry_path, fixture.registry_source.replace_once(fixture.profile_sha256,
		'a'.repeat(64))) or { panic(err) }
	assert registry_toolchain_error(fixture.automation_root) == 'toolchain profile hash differs from the registry'
	os.write_file(registry_path, fixture.registry_source) or { panic(err) }

	binding_mutations := [
		[
			fixture.profile_source.replace_once('"target_id":"freebsd-amd64"',
				'"target_id":"linux-amd64"'),
			'toolchain profile target differs from its registry target',
		],
		[
			fixture.profile_source.replace_once('"profile_id":"freebsd-amd64-synthetic-v1"',
				'"profile_id":"freebsd-amd64-synthetic-v2"'),
			'toolchain profile ID differs from the registry',
		],
		[
			'${fixture.profile_source}\n',
			'toolchain profile bytes must be exact canonical JSON',
		],
	]
	for mutation in binding_mutations {
		os.write_file(fixture.profile_path, mutation[0]) or { panic(err) }
		assert registry_toolchain_error(fixture.automation_root) == mutation[1]
	}

	profile := bin.parse_strict_json(fixture.profile_source) or { panic(err) }
	producer := profile.object_value('producer') or { panic('producer roles missing') }
	producer_guest := bin.canonical_json(producer.array_value[0])
	producer_host := bin.canonical_json(producer.array_value[1])
	producer_marker := '"producer":[${producer_guest},${producer_host}]'
	semantic_mutations := [
		[
			fixture.profile_source.replace_once(',{"match":"exact","name":"requested_release","value":"15.1"}', ''),
			'toolchain identity policy differs from the exact strategy requirements',
		],
		[
			fixture.profile_source.replace_once('"role_id":"contract-validator-guest"',
				'"role_id":"bundle-builder-guest"'),
			'toolchain profile roles must be globally unique',
		],
		[
			fixture.profile_source.replace_once(producer_marker,
				'"producer":[${producer_host},${producer_guest}]'),
			'toolchain producer roles must be in strict lexical order',
		],
		[
			fixture.profile_source.replace_once('"name":"requested_release"',
				'"name":"observed_release"'),
			'toolchain identity_policy fact names must be unique',
		],
	]
	for mutation in semantic_mutations {
		mutated_profile := bin.parse_strict_json(mutation[0]) or { panic(err) }
		mutated_hash := bin.json_sha256(mutated_profile)
		mutated_registry := fixture.registry_source.replace_once(fixture.profile_sha256,
			mutated_hash)
		assert mutated_registry != fixture.registry_source
		os.write_file(registry_path, mutated_registry) or { panic(err) }
		os.write_file(fixture.profile_path, mutation[0]) or { panic(err) }
		assert registry_toolchain_error(fixture.automation_root) == mutation[1]
	}

	os.write_file(registry_path, fixture.registry_source) or { panic(err) }
	os.write_file(fixture.profile_path, 'x'.repeat(512 * 1024 + 1)) or { panic(err) }
	assert registry_toolchain_error(fixture.automation_root) == 'toolchain profile exceeds its strict byte bound'
	$if !windows {
		sentinel_path := os.join_path(fixture.base, 'profile-sentinel.json')
		sentinel_source := '{"sentinel":true}\n'
		os.write_file(sentinel_path, sentinel_source) or { panic(err) }
		os.rm(fixture.profile_path) or { panic(err) }
		os.symlink(sentinel_path, fixture.profile_path) or { panic(err) }
		assert registry_toolchain_error(fixture.automation_root) == 'toolchain profile is not a physical regular file'
		assert os.read_file(sentinel_path) or { panic(err) } == sentinel_source
	}
}

fn test_toolchain_observation_rejects_binding_sets_matches_and_digest_tampering() {
	fixture := prepare_toolchain_observation_fixture('observation-negative')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	producer := bin.parse_strict_json(fixture.producer_source) or { panic(err) }
	producer_roles := producer.object_value('roles') or { panic('toolchain roles missing') }
	producer_role := producer_roles.array_value[0]
	resolution_digest := (producer_role.object_value('resolution_digest') or {
		panic('resolution digest missing')
	}).string_value
	observation_digest := (producer.object_value('observation_digest') or {
		panic('observation digest missing')
	}).string_value
	wrong_resolution_digest := if resolution_digest == 'a'.repeat(64) {
		'b'.repeat(64)
	} else {
		'a'.repeat(64)
	}
	wrong_observation_digest := if observation_digest == 'b'.repeat(64) {
		'c'.repeat(64)
	} else {
		'b'.repeat(64)
	}
	mutations := [
		[
			fixture.producer_source.replace_once('"target_id":"freebsd-amd64"',
				'"target_id":"linux-amd64"'),
			'toolchain observation target differs from the requested target',
		],
		[
			fixture.producer_source.replace_once('"profile_id":"freebsd-amd64-synthetic-v1"',
				'"profile_id":"freebsd-amd64-synthetic-v2"'),
			'toolchain observation profile binding differs from the registry',
		],
		[
			fixture.producer_source.replace_once('"profile_sha256":"${fixture.profile_sha256}"',
				'"profile_sha256":"${'a'.repeat(64)}"'),
			'toolchain observation profile binding differs from the registry',
		],
		[
			fixture.producer_source.replace_once('"phase":"producer"', '"phase":"validator"'),
			'toolchain observation roles differ from the exact profile phase',
		],
		[
			fixture.producer_source.replace_once('"role_id":"bundle-builder-guest"',
				'"role_id":"bundle-builder-ghost"'),
			'toolchain observation roles differ from the exact profile phase',
		],
		[
			fixture.producer_source.replace_once('"identity_strategy":"cpa-guest"',
				'"identity_strategy":"cpa-host"'),
			'toolchain observation strategy differs from the reviewed profile',
		],
		[
			fixture.producer_source.replace_once('"name":"arch","value":"amd64"',
				'"name":"arch","value":"arm64"'),
			'toolchain observed fact does not satisfy its reviewed match policy',
		],
		[
			fixture.producer_source.replace_once('"name":"compiler_version","value":"clang 19.1.7"',
				'"name":"compiler_version","value":""'),
			'toolchain observed fact does not satisfy its reviewed match policy',
		],
		[
			fixture.producer_source.replace_once('"name":"compiler_target","value":"x86_64-unknown-freebsd15.1"',
				'"name":"compiler_target","value":""'),
			'toolchain observed fact does not satisfy its reviewed match policy',
		],
		[
			fixture.producer_source.replace_once('"name":"compiler_binary_sha256","value":"${'3'.repeat(64)}"',
				'"name":"compiler_binary_sha256","value":"not-a-sha256"'),
			'toolchain observed fact does not satisfy its reviewed match policy',
		],
		[
			fixture.producer_source.replace_once('"name":"observed_release","value":"15.1-RELEASE-p2"',
				'"name":"observed_release","value":"14.3-RELEASE"'),
			'toolchain observed fact does not satisfy its reviewed match policy',
		],
		[
			fixture.producer_source.replace_once(',{"name":"requested_release","value":"15.1"}', ''),
			'toolchain observed facts differ from the exact strategy fact set',
		],
		[
			fixture.producer_source.replace_once('"resolution_digest":"${resolution_digest}"',
				'"resolution_digest":"${wrong_resolution_digest}"'),
			'toolchain observation resolution digest is not derived from its canonical identity',
		],
		[
			fixture.producer_source.replace_once('"evidence_sha256":"${'e'.repeat(64)}"',
				'"evidence_sha256":"${'f'.repeat(64)}"'),
			'toolchain observation digest is not derived from the complete canonical observation',
		],
		[
			fixture.producer_source.replace_once('"observation_digest":"${observation_digest}"',
				'"observation_digest":"${wrong_observation_digest}"'),
			'toolchain observation digest is not derived from the complete canonical observation',
		],
		[
			'${fixture.producer_source}\n',
			'toolchain observation bytes must be exact canonical JSON',
		],
	]
	for mutation in mutations {
		os.write_file(fixture.observation_path, mutation[0]) or { panic(err) }
		assert authenticate_toolchain_error(fixture.automation_root, fixture.observation_path) == mutation[1]
	}

	duplicate := fixture.producer_source.replace_once('"name":"requested_release"',
		'"name":"observed_release"')
	os.write_file(fixture.observation_path, duplicate) or { panic(err) }
	assert authenticate_toolchain_error(fixture.automation_root, fixture.observation_path) == 'toolchain resolved_identity fact names must be unique'
	first_fact := '{"name":"arch","value":"amd64"}'
	second_fact := '{"name":"compiler_binary_sha256","value":"${'3'.repeat(64)}"}'
	assert fixture.producer_source.count(first_fact) == 2
	assert fixture.producer_source.count(second_fact) == 1
	unordered := fixture.producer_source.replace_once(first_fact, '__toolchain_first_fact__').replace_once(second_fact,
		first_fact).replace_once('__toolchain_first_fact__', second_fact)
	os.write_file(fixture.observation_path, unordered) or { panic(err) }
	assert authenticate_toolchain_error(fixture.automation_root, fixture.observation_path) == 'toolchain resolved_identity facts must be in strict lexical order'

	os.write_file(fixture.observation_path, 'x'.repeat(512 * 1024 + 1)) or { panic(err) }
	assert authenticate_toolchain_error(fixture.automation_root, fixture.observation_path) == 'toolchain observation exceeds its strict byte bound'
	$if !windows {
		sentinel_path := os.join_path(fixture.base, 'observation-sentinel.json')
		sentinel_source := '{"sentinel":true}\n'
		os.write_file(sentinel_path, sentinel_source) or { panic(err) }
		os.rm(fixture.observation_path) or { panic(err) }
		os.symlink(sentinel_path, fixture.observation_path) or { panic(err) }
		assert authenticate_toolchain_error(fixture.automation_root, fixture.observation_path) == 'toolchain observation is not a physical regular file'
		assert os.read_file(sentinel_path) or { panic(err) } == sentinel_source
	}
}

fn test_complete_and_opaque_provenance_are_static_but_observed() {
	bytes := minimal_elf64_relocatable()
	source_repo, staging_root := prepare_provenance_tree('positive', bytes, false)
	defer {
		os.rmdir_all(os.dir(source_repo)) or {}
	}
	registry, windows := synthetic_opaque_contract(bytes)
	authority := t2a_prepare_toolchain_authority(os.join_path(os.dir(source_repo),
		'complete-authority'), 'linux-amd64')
	complete_source := t2a_resolved_manifest_toolchain(os.read_file(os.join_path(automation_root(),
		'tests', 'fixtures', 'manifest-complete.valid.json')) or { panic(err) }, authority)
	complete := bin.parse_strict_json(complete_source) or { panic(err) }
	assert bin.recalculate_provenance(complete, registry, []) or { panic(err) } == 'complete'
	observations := bin.scan_manifest_opaque_inputs(windows, registry, staging_contract(source_repo,
		staging_root)) or { panic(err) }
	assert observations.len == 1
	assert observations[0].git_mode == '100644'
	assert observations[0].format == 'ELF64 little-endian'
	assert observations[0].object_type == 'ET_REL'
	assert observations[0].machine == 'EM_X86_64'
	assert observations[0].os_abi == 'System V'
	assert bin.recalculate_provenance(windows, registry, observations) or { panic(err) } == 'opaque-accepted'
}

fn test_transform_hash_and_mode_are_bound_to_the_candidate_git_blob() {
	fixture, transform_path, transform_bytes := prepare_transform_candidate('transform-input')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	manifest := bin.parse_strict_json(fixture.manifest_source) or { panic(err) }
	positive := bin.validate_staged_manifest_material(manifest, fixture.manifest_source,
		fixture.contract) or { panic(err) }
	assert positive.len == 0, '${positive}'
	full_transform_path := os.join_path(fixture.source_repo, transform_path)
	os.write_file(full_transform_path, 'changed transform bytes\n') or { panic(err) }
	changed_contract := committed_contract_for(fixture, 'change transform bytes')
	changed := bin.validate_staged_manifest_material(manifest, fixture.manifest_source,
		changed_contract) or { panic(err) }
	assert changed.any(it.path == '$/transforms/0'
		&& it.message.contains('declared SHA-256 differs')), '${changed}'
	os.write_file_array(full_transform_path, transform_bytes) or { panic(err) }
	$if !windows {
		os.chmod(full_transform_path, 0o755) or { panic(err) }
		mode_contract := committed_contract_for(fixture, 'change transform mode')
		mode_issues := bin.validate_staged_manifest_material(manifest, fixture.manifest_source,
			mode_contract) or { panic(err) }
		assert mode_issues.any(it.path == '$/transforms/0'
			&& it.message.contains('regular Git blobs')), '${mode_issues}'
	}
}

fn test_real_macos_symlink_payloads_bind_git_mode_target_and_stage_type() {
	$if !windows {
		cases := [
			['lib/libgc.la', '../libgc.la'],
			['lib/libc.dylib', '/System/DriverKit/usr/lib/libSystem.dylib'],
		]
		for index, symlink_case in cases {
			fixture := prepare_symlink_candidate('real-symlink-${index}', symlink_case[0],
				symlink_case[1])
			defer {
				os.rmdir_all(fixture.base) or {}
			}
			decision := bin.evaluate_staged_manifest_for_execution(fixture.automation_root,
				fixture.manifest_path, fixture.contract, runtime_contract_binding(false), false) or {
				panic(err)
			}
			assert decision.eligible
			assert !decision.publish_allowed
			staged_path := os.join_path(fixture.staging_root, symlink_case[0])
			os.rm(staged_path) or { panic(err) }
			mutated_target := if index == 0 { '../libgccpp.la' } else { 'libSystem.dylib' }
			os.symlink(mutated_target, staged_path) or { panic(err) }
			assert_staged_ineligible(fixture, fixture.manifest_path, fixture.contract,
				runtime_contract_binding(false))
			os.rm(staged_path) or { panic(err) }
			os.write_file(staged_path, symlink_case[1]) or { panic(err) }
			assert_staged_ineligible(fixture, fixture.manifest_path, fixture.contract,
				runtime_contract_binding(false))
		}
	}
}

fn test_opaque_scanner_rejects_absent_truncated_byte_header_type_and_mode_changes() {
	base_bytes := minimal_elf64_relocatable()
	registry, manifest := synthetic_opaque_contract(base_bytes)
	mut cases := [][]u8{}
	cases << base_bytes[..8].clone()
	mut byte_changed := base_bytes.clone()
	byte_changed[63] = 1
	cases << byte_changed
	for offset_value in [HeaderMutation{4, 1}, HeaderMutation{5, 2},
		HeaderMutation{7, 3}, HeaderMutation{16, 2}, HeaderMutation{18, 3}] {
		mut changed := base_bytes.clone()
		changed[offset_value.offset] = offset_value.value
		cases << changed
	}
	for index, changed in cases {
		source_repo, staging_root := prepare_provenance_tree('mutation-${index}', base_bytes, false)
		os.write_file_array(os.join_path(staging_root, 'lib', 'openlibm.o'), changed) or {
			panic(err)
		}
		observed := bin.scan_manifest_opaque_inputs(manifest, registry, staging_contract(source_repo,
			staging_root)) or { panic(err) }
		assert bin.recalculate_provenance(manifest, registry, observed) or { panic(err) } == 'incomplete'
		os.rmdir_all(os.dir(source_repo)) or {}
	}
	source_repo, staging_root := prepare_provenance_tree('absent', base_bytes, false)
	os.rm(os.join_path(staging_root, 'lib', 'openlibm.o')) or { panic(err) }
	absent := bin.scan_manifest_opaque_inputs(manifest, registry, staging_contract(source_repo,
		staging_root)) or { panic(err) }
	assert !absent[0].present
	assert bin.recalculate_provenance(manifest, registry, absent) or { panic(err) } == 'incomplete'
	os.rmdir_all(os.dir(source_repo)) or {}
	executable_repo, executable_staging := prepare_provenance_tree('mode', base_bytes, true)
	mode_observation := bin.scan_manifest_opaque_inputs(manifest, registry, staging_contract(executable_repo,
		executable_staging)) or { panic(err) }
	assert mode_observation[0].git_mode == '100755'
	assert bin.recalculate_provenance(manifest, registry, mode_observation) or { panic(err) } == 'incomplete'
	os.rmdir_all(os.dir(executable_repo)) or {}
}

struct HeaderMutation {
	offset int
	value  u8
}

fn test_opaque_scanner_rejects_symlink_case_variant_and_duplicate_observation() {
	bytes := minimal_elf64_relocatable()
	registry, manifest := synthetic_opaque_contract(bytes)
	source_repo, staging_root := prepare_provenance_tree('path-type', bytes, false)
	defer {
		os.rmdir_all(os.dir(source_repo)) or {}
	}
	path := os.join_path(staging_root, 'lib', 'openlibm.o')
	os.rm(path) or { panic(err) }
	os.symlink('other.o', path) or { panic(err) }
	symlinked := bin.scan_manifest_opaque_inputs(manifest, registry, staging_contract(source_repo,
		staging_root)) or { panic(err) }
	assert symlinked[0].kind == 'symlink'
	assert bin.recalculate_provenance(manifest, registry, symlinked) or { panic(err) } == 'incomplete'
	os.rm(path) or { panic(err) }
	os.write_file_array(os.join_path(staging_root, 'lib', 'OpenLibm.o'), bytes) or { panic(err) }
	case_variant := bin.scan_manifest_opaque_inputs(manifest, registry, staging_contract(source_repo,
		staging_root)) or { panic(err) }
	assert !case_variant[0].present
	os.rm(os.join_path(staging_root, 'lib', 'OpenLibm.o')) or { panic(err) }
	os.write_file_array(path, bytes) or { panic(err) }
	valid_observation := bin.scan_opaque_input(staging_contract(source_repo, staging_root),
		'windows-amd64', 'lib/openlibm.o') or { bin.OpaqueObservation{} }
	assert bin.recalculate_provenance(manifest, registry, [valid_observation, valid_observation]) or {
		panic(err)
	} == 'incomplete'
}

fn test_input_and_complete_artifact_fingerprints_are_separate_and_stable() {
	base := os.join_path(os.temp_dir(), 'tccbin-fingerprint-toolchain-${os.getpid()}')
	os.rmdir_all(base) or {}
	authority := t2a_prepare_toolchain_authority(base, 'linux-amd64')
	defer {
		os.rmdir_all(base) or {}
	}
	registry := load_contract_value(os.join_path(authority.root, 'targets.json'))
	manifest := t2a_resolved_manifest_toolchain(os.read_file(os.join_path(automation_root(),
		'tests', 'fixtures', 'manifest-complete.valid.json')) or { panic(err) }, authority)
	first := bin.manifest_fingerprints(manifest, registry) or { panic(err) }
	second := bin.manifest_fingerprints(manifest, registry) or { panic(err) }
	assert first == second
	reformatted := manifest.replace_once('  "schema_version": 1,', '\t"schema_version": 1,')
	semantically_same := bin.manifest_fingerprints(reformatted, registry) or { panic(err) }
	assert first.manifest_hash != semantically_same.manifest_hash
	assert first.input_fingerprint == semantically_same.input_fingerprint
	assert first.artifact_fingerprint == semantically_same.artifact_fingerprint
	refreshed := t2a_authority_with_refreshed_producer(authority, '6'.repeat(64))
	input_changed := bin.manifest_fingerprints(t2a_rebind_manifest_toolchain(manifest, authority,
		refreshed), registry) or { panic(err) }
	assert first.input_fingerprint != input_changed.input_fingerprint
	assert first.artifact_fingerprint == input_changed.artifact_fingerprint
	for mutation in [
		manifest.replace_once('"path": "tcc.exe",', '"path": "tcc-new.exe",'),
		manifest.replace_once('"kind": "executable",', '"kind": "file",'),
		manifest.replace_once('"git_mode": "100755",', '"git_mode": "100644",'),
		manifest.replace_once('"sha256": "4444444444444444444444444444444444444444444444444444444444444444",\n      "symlink_target": null,',
			'"sha256": "4444444444444444444444444444444444444444444444444444444444444444",\n      "symlink_target": "tcc-target",'),
		manifest.replace_once('4444444444444444444444444444444444444444444444444444444444444444',
			'5444444444444444444444444444444444444444444444444444444444444444'),
	] {
		changed := bin.manifest_fingerprints(mutation, registry) or { panic(err) }
		assert changed.artifact_fingerprint != first.artifact_fingerprint
	}
}

fn t2b_native_subject(fixture CompleteCandidateFixture,
	authenticated bin.AuthenticatedManifestModel) bin.NativeGateSubjectModel {
	fingerprints := bin.authenticated_manifest_fingerprints(authenticated) or { panic(err) }
	tree_result := os.exec(['git', '-C', fixture.source_repo, 'rev-parse',
		'${fixture.contract.source_git_ref}^{tree}'])
	assert tree_result.exit_code == 0, tree_result.output
	mut digests := []bin.DigestModel{}
	for line in fingerprints.digest_lines {
		parts := line.split('\t')
		assert parts.len == 2, line
		digests << bin.DigestModel{
			path:   parts[0]
			sha256: parts[1]
		}
	}
	consumer_id := 'c'.repeat(64)
	return bin.NativeGateSubjectModel{
		consumer_id:            consumer_id
		consumer_kind:          'publish_candidate'
		intent_or_operation_id: consumer_id
		target_id:              fixture.target_id
		subject_generation:     1
		initial_run_mode:       'original_push'
		sha:                    fixture.contract.source_git_ref
		tree:                   tree_result.output.trim_space()
		original_ref:           'tccbin-candidate/${fixture.target_id}/${consumer_id}'
		input_fingerprint:      fingerprints.input_fingerprint
		artifact_fingerprint:   fingerprints.artifact_fingerprint
		manifest_hash:          fingerprints.manifest_hash
		digests:                digests
	}
}

fn t2b_replace_matrix_object_member(source string, object_key string, member string,
	replacement string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	object := root.object_value(object_key) or { panic('${object_key} missing') }
	object_source := bin.canonical_json(object)
	updated_object := replace_canonical_root_member(object_source, object, member, replacement)
	return replace_canonical_root_member(source, root, object_key, updated_object)
}

fn t2b_matrix_with_result_mutation(source string, mutation string) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	result_value := root.object_value('results') or { panic('matrix results missing') }
	mut results := result_value.array_value.clone()
	assert results.len > 2
	match mutation {
		'missing' {
			results.delete(results.len - 1)
		}
		'extra' {
			results << results[0]
		}
		'duplicate' {
			results[1] = results[0]
		}
		'order' {
			first := results[0]
			results[0] = results[1]
			results[1] = first
		}
		else {
			panic('unknown result mutation ${mutation}')
		}
	}
	updated := bin.JsonValue{
		kind:        .array
		array_value: results
	}
	return replace_canonical_root_member(source, root, 'results', bin.canonical_json(updated))
}

fn t2b_matrix_error(automation_root_path string, manifest bin.AuthenticatedManifestModel,
	subject bin.NativeGateSubjectModel, matrix_path string, source string) string {
	os.rm(matrix_path) or { panic(err) }
	os.write_file(matrix_path, source) or { panic(err) }
	bin.authenticate_native_lane_matrix_file(automation_root_path, manifest, subject, matrix_path) or {
		return err.msg()
	}
	return ''
}

fn t2c_capsule_error(automation_root_path string, manifest bin.AuthenticatedManifestModel,
	subject bin.NativeGateSubjectModel, capsule_root string) string {
	bin.authenticate_native_validation_capsule(automation_root_path, manifest, subject,
		capsule_root) or { return err.msg() }
	return ''
}

fn t2c_expected_capsule_digest(subject bin.NativeGateSubjectModel, matrix_source string,
	evidence_root string) string {
	mut names := os.ls(evidence_root) or { panic(err) }
	names.sort()
	mut evidence := []string{cap: names.len}
	for name in names {
		stat := os.lstat(os.join_path(evidence_root, name)) or { panic(err) }
		assert stat.get_filetype() == .regular
		evidence << '{"sha256":"${name}","size":${stat.size}}'
	}
	projection_source := '{"evidence":[${evidence.join(',')}],"manifest_hash":"${subject.manifest_hash}","matrix_digest":"${sha256_bytes(matrix_source.bytes())}","schema_version":1,"subject_hash":"${bin.native_gate_subject_hash(subject) or {
		panic(err)
	}}"}'
	projection := bin.parse_strict_json(projection_source) or { panic(err) }
	assert bin.canonical_json(projection) == projection_source
	return sha256_bytes(projection_source.bytes())
}

fn t2c_expanded_fixture(fixture CompleteCandidateFixture, target_id string, total int,
	suffix string) CompleteCandidateFixture {
	authority := t2a_prepare_toolchain_authority(fixture.base, target_id)
	manifest_source := t2c_manifest_for_authority(fixture.manifest_source, authority, total)
	manifest_path := os.join_path(fixture.base, 'native-validation-manifest-${suffix}.json')
	os.write_file(manifest_path, manifest_source) or { panic(err) }
	return CompleteCandidateFixture{
		...fixture
		automation_root: authority.root
		target_id:       target_id
		manifest_path:   manifest_path
		manifest_source: manifest_source
		authority:       authority
	}
}

fn test_native_validation_capsule_authenticates_exact_evidence_deduplicates_and_is_deterministic() {
	fixture := prepare_complete_candidate('native-validation-capsule-positive', false, '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	authenticated := bin.authenticate_manifest_file(fixture.automation_root, fixture.manifest_path) or {
		panic(err)
	}
	subject := t2b_native_subject(fixture, authenticated)
	matrix_source := t2b_native_matrix_source(fixture.manifest_source, fixture.authority, subject)
	matrix := bin.parse_strict_json(matrix_source) or { panic(err) }
	validator := matrix.object_value('validator_observation') or {
		panic('validator observation missing')
	}
	validator_roles := validator.object_value('roles') or { panic('validator roles missing') }
	validator_evidence := (validator_roles.array_value[0].object_value('evidence_sha256') or {
		panic('validator evidence missing')
	}).string_value
	deduplicated_source := t2b_replace_matrix_result_member(matrix_source, 0, 'evidence_sha256',
		'"${validator_evidence}"')
	deduplicated := bin.parse_strict_json(deduplicated_source) or { panic(err) }
	result_count := (deduplicated.object_value('results') or { panic('matrix results missing') }).array_value.len
	producer := bin.parse_strict_json(fixture.authority.producer_source) or { panic(err) }
	producer_count := (producer.object_value('roles') or { panic('producer roles missing') }).array_value.len
	validator_count := validator_roles.array_value.len
	capsule_base := os.real_path(fixture.base)

	mut capsule_digests := []string{}
	for index, reverse_order in [false, true] {
		capsule_root := os.join_path(capsule_base, 'native-validation-capsule-${index}')
		names := t2c_write_native_validation_capsule(capsule_root, deduplicated_source,
			fixture.authority, reverse_order)
		assert names.len == producer_count + validator_count + result_count - 1
		capsule := bin.authenticate_native_validation_capsule(fixture.automation_root,
			authenticated, subject, capsule_root) or { panic(err) }
		digest := bin.authenticated_native_validation_capsule_digest(capsule) or { panic(err) }
		assert digest == t2c_expected_capsule_digest(subject, deduplicated_source, os.join_path(capsule_root,
			'evidence'))
		capsule_digests << digest
	}
	assert capsule_digests.len == 2
	assert capsule_digests[0] == capsule_digests[1]
}

fn test_native_validation_capsule_authenticates_windows_matrix_evidence() {
	fixture := prepare_t2b_windows_matrix_candidate('capsule-positive')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	authenticated := bin.authenticate_staged_manifest_file(fixture.automation_root,
		fixture.manifest_path, fixture.contract) or { panic(err) }
	subject := t2b_native_subject(fixture, authenticated)
	matrix_source := t2b_native_matrix_source(fixture.manifest_source, fixture.authority, subject)
	capsule_root := os.join_path(os.real_path(fixture.base), 'native-validation-capsule')
	names := t2c_write_native_validation_capsule(capsule_root, matrix_source, fixture.authority,
		false)
	assert names.len == 47
	capsule := bin.authenticate_native_validation_capsule(fixture.automation_root, authenticated,
		subject, capsule_root) or { panic(err) }
	assert bin.authenticated_native_validation_capsule_digest(capsule) or { panic(err) } == t2c_expected_capsule_digest(subject,
		matrix_source, os.join_path(capsule_root, 'evidence'))
}

fn test_native_validation_capsule_closes_semantic_and_walker_cardinality_bounds() {
	base_fixture := prepare_complete_candidate('native-validation-cardinality', false, '')
	defer {
		os.rmdir_all(base_fixture.base) or {}
	}
	fixture := t2c_expanded_fixture(base_fixture, 'freebsd-amd64', 1024, '1028')
	authenticated := bin.authenticate_manifest_file(fixture.automation_root, fixture.manifest_path) or {
		panic(err)
	}
	subject := t2b_native_subject(fixture, authenticated)
	matrix_source := t2b_native_matrix_source(fixture.manifest_source, fixture.authority, subject)
	sources := t2c_native_validation_evidence_sources(matrix_source, fixture.authority)
	assert sources.len == 1028
	capsule_root := os.join_path(os.real_path(fixture.base), 'capsule-semantic-1028')
	assert t2c_write_native_validation_capsule_with_sources(capsule_root, matrix_source, sources,
		true).len == 1028
	capsule := bin.authenticate_native_validation_capsule(fixture.automation_root, authenticated,
		subject, capsule_root) or { panic(err) }
	assert bin.authenticated_native_validation_capsule_digest(capsule) or { panic(err) } == t2c_expected_capsule_digest(subject,
		matrix_source, os.join_path(capsule_root, 'evidence'))

	overflow := t2c_expanded_fixture(base_fixture, 'freebsd-amd64', 1025, '1029')
	mut overflow_error := ''
	bin.authenticate_manifest_file(overflow.automation_root, overflow.manifest_path) or {
		overflow_error = err.msg()
	}
	assert overflow_error == 'manifest schema or semantics failed with 1 issue(s)'

	walker_root := os.join_path(os.real_path(fixture.base), 'primitive-walker')
	os.mkdir(walker_root) or { panic(err) }
	for index in 0 .. 1056 {
		os.write_file(os.join_path(walker_root, 'entry-${index:04}-${'x'.repeat(64)}'), '') or {
			panic(err)
		}
	}
	assert t2c_primitive_directory_entry_count(walker_root, 1056) or { panic(err) } == 1056
	os.write_file(os.join_path(walker_root, 'entry-1056-${'x'.repeat(64)}'), '') or { panic(err) }
	mut walker_error := ''
	t2c_primitive_directory_entry_count(walker_root, 1056) or { walker_error = err.msg() }
	assert walker_error == 'native primitive walker crossed its strict bound'
}

fn test_native_validation_capsule_closes_per_file_and_total_byte_bounds() {
	base_fixture := prepare_complete_candidate('native-validation-byte-bounds', false, '')
	defer {
		os.rmdir_all(base_fixture.base) or {}
	}
	fixture := t2c_expanded_fixture(base_fixture, 'freebsd-amd64', 64, 'bytes')
	authenticated := bin.authenticate_manifest_file(fixture.automation_root, fixture.manifest_path) or {
		panic(err)
	}
	subject := t2b_native_subject(fixture, authenticated)
	base_matrix_source := t2b_native_matrix_source(fixture.manifest_source, fixture.authority,
		subject)

	mut exact_file_sizes := []int{len: 64, init: 64}
	exact_file_sizes[0] = 256 * 1024
	exact_file := t2c_matrix_with_evidence_sizes(base_matrix_source, fixture.authority,
		exact_file_sizes)
	exact_file_root := os.join_path(os.real_path(fixture.base), 'capsule-evidence-256k')
	t2c_write_native_validation_capsule_with_sources(exact_file_root, exact_file.matrix_source,
		exact_file.sources, false)
	bin.authenticate_native_validation_capsule(fixture.automation_root, authenticated, subject,
		exact_file_root) or { panic(err) }

	mut oversized_file_sizes := exact_file_sizes.clone()
	oversized_file_sizes[0]++
	oversized_file := t2c_matrix_with_evidence_sizes(base_matrix_source, fixture.authority,
		oversized_file_sizes)
	oversized_file_root := os.join_path(os.real_path(fixture.base),
		'capsule-evidence-256k-plus-one')
	t2c_write_native_validation_capsule_with_sources(oversized_file_root,
		oversized_file.matrix_source, oversized_file.sources, false)
	assert t2c_capsule_error(fixture.automation_root, authenticated, subject, oversized_file_root) == 'native validation evidence is empty or exceeds its strict byte bound'

	mut toolchain_bytes := 0
	for _, source in t2c_toolchain_evidence_sources(base_matrix_source, fixture.authority) {
		toolchain_bytes += source.len
	}
	mut remaining := 16 * 1024 * 1024 - base_matrix_source.len - toolchain_bytes
	mut exact_total_sizes := []int{cap: 64}
	for index in 0 .. 64 {
		left := 63 - index
		minimum_left := left * 64
		size := int_min(256 * 1024, remaining - minimum_left)
		assert size >= 64
		exact_total_sizes << size
		remaining -= size
	}
	assert remaining == 0
	exact_total := t2c_matrix_with_evidence_sizes(base_matrix_source, fixture.authority,
		exact_total_sizes)
	assert exact_total.matrix_source.len == base_matrix_source.len
	exact_total_root := os.join_path(os.real_path(fixture.base), 'capsule-total-16m')
	t2c_write_native_validation_capsule_with_sources(exact_total_root, exact_total.matrix_source,
		exact_total.sources, true)
	bin.authenticate_native_validation_capsule(fixture.automation_root, authenticated, subject,
		exact_total_root) or { panic(err) }

	mut oversized_total_sizes := exact_total_sizes.clone()
	assert oversized_total_sizes.last() < 256 * 1024
	oversized_total_sizes[oversized_total_sizes.len - 1]++
	oversized_total := t2c_matrix_with_evidence_sizes(base_matrix_source, fixture.authority,
		oversized_total_sizes)
	assert oversized_total.matrix_source.len == base_matrix_source.len
	oversized_total_root := os.join_path(os.real_path(fixture.base), 'capsule-total-16m-plus-one')
	t2c_write_native_validation_capsule_with_sources(oversized_total_root,
		oversized_total.matrix_source, oversized_total.sources, false)
	assert t2c_capsule_error(fixture.automation_root, authenticated, subject, oversized_total_root) == 'native validation capsule exceeds its strict total byte bound'
}

fn test_native_validation_native_handles_anchor_root_and_evidence_swaps() {
	base := os.join_path(os.temp_dir(), 'tccbin-native-validation-swaps-${os.getpid()}')
	os.rmdir_all(base) or {}
	defer {
		os.rmdir_all(base) or {}
	}
	for name, source in {
		'root-a': 'root-a\n'
		'root-b': 'root-b\n'
	} {
		os.mkdir_all(os.join_path(base, name, 'capsule')) or { panic(err) }
		os.write_file(os.join_path(base, name, 'capsule', 'sentinel'), source) or { panic(err) }
	}
	os.write_file(os.join_path(base, 'root-a', 'capsule', 'only-a'), 'only-a\n') or { panic(err) }
	os.write_file(os.join_path(base, 'root-b', 'capsule', 'only-b'), 'only-b\n') or { panic(err) }
	ancestor_a := os.join_path(base, 'root-a')
	ancestor_b := os.join_path(base, 'root-b')
	ancestor_retained := os.join_path(base, 'root-retained')
	root_a := os.join_path(ancestor_a, 'capsule')
	root_b := os.join_path(ancestor_b, 'capsule')
	$if windows {
		wide_base := base.to_wide()
		wide_root := root_a.to_wide()
		wide_ancestor := 'root-a'.to_wide()
		wide_sentinel := 'sentinel'.to_wide()
		defer {
			unsafe {
				free(voidptr(wide_base))
				free(voidptr(wide_root))
				free(voidptr(wide_ancestor))
				free(voidptr(wide_sentinel))
			}
		}
		base_handle := C.tccbin_windows_open_directory_path_no_follow(wide_base)
		assert base_handle != voidptr(-1) && base_handle != unsafe { nil }
		defer {
			C.CloseHandle(base_handle)
		}
		directory_snapshot := C.tccbin_windows_open_child_no_follow(base_handle, wide_ancestor, 2)
		assert directory_snapshot != voidptr(-1) && directory_snapshot != unsafe { nil }
		C.CloseHandle(directory_snapshot)
		handle := C.tccbin_windows_open_directory_path_no_follow(wide_root)
		assert handle != voidptr(-1) && handle != unsafe { nil }
		defer {
			C.CloseHandle(handle)
		}
		file_snapshot := C.tccbin_windows_open_child_no_follow(handle, wide_sentinel, 2)
		assert file_snapshot != voidptr(-1) && file_snapshot != unsafe { nil }
		C.CloseHandle(file_snapshot)
		assert t2c_read_windows_child(handle, 'sentinel') or { panic(err) } == 'root-a\n'
		mut held_names := t2c_windows_directory_entries(handle, 2) or { panic(err) }
		held_names.sort()
		assert held_names == ['only-a', 'sentinel']
		mut renamed := true
		os.rename(ancestor_a, ancestor_retained) or { renamed = false }
		if renamed {
			os.rename(ancestor_b, ancestor_a) or { panic(err) }
			assert t2c_read_windows_child(handle, 'sentinel') or { panic(err) } == 'root-a\n'
			assert os.read_file(os.join_path(root_a, 'sentinel')) or { panic(err) } == 'root-b\n'
			assert os.read_file(os.join_path(ancestor_retained, 'capsule', 'sentinel')) or {
				panic(err)
			} == 'root-a\n'
			mut replacement_names := os.ls(root_a) or { panic(err) }
			replacement_names.sort()
			assert replacement_names == ['only-b', 'sentinel']
			mut retained_names := os.ls(os.join_path(ancestor_retained, 'capsule')) or {
				panic(err)
			}
			retained_names.sort()
			assert retained_names == ['only-a', 'sentinel']
		} else {
			assert os.read_file(os.join_path(root_a, 'sentinel')) or { panic(err) } == 'root-a\n'
			assert os.read_file(os.join_path(root_b, 'sentinel')) or { panic(err) } == 'root-b\n'
			mut a_names := os.ls(root_a) or { panic(err) }
			a_names.sort()
			assert a_names == ['only-a', 'sentinel']
			mut b_names := os.ls(root_b) or { panic(err) }
			b_names.sort()
			assert b_names == ['only-b', 'sentinel']
		}
		held_names = t2c_windows_directory_entries(handle, 2) or { panic(err) }
		held_names.sort()
		assert held_names == ['only-a', 'sentinel']
	} $else {
		root_fd := C.tccbin_open_directory_no_follow(&char(root_a.str))
		assert root_fd >= 0
		defer {
			C.tccbin_close_document(root_fd)
		}
		os.rename(ancestor_a, ancestor_retained) or { panic(err) }
		os.rename(ancestor_b, ancestor_a) or { panic(err) }
		assert t2c_read_posix_child(root_fd, 'sentinel') or { panic(err) } == 'root-a\n'
		assert os.read_file(os.join_path(root_a, 'sentinel')) or { panic(err) } == 'root-b\n'
		assert os.read_file(os.join_path(ancestor_retained, 'capsule', 'sentinel')) or {
			panic(err)
		} == 'root-a\n'
	}

	root := os.join_path(base, 'evidence-root')
	evidence_a := os.join_path(root, 'evidence')
	evidence_b := os.join_path(root, 'evidence-b')
	evidence_retained := os.join_path(root, 'evidence-retained')
	for path, source in {
		evidence_a: 'evidence-a\n'
		evidence_b: 'evidence-b\n'
	} {
		os.mkdir_all(path) or { panic(err) }
		os.write_file(os.join_path(path, 'sentinel'), source) or { panic(err) }
	}
	os.write_file(os.join_path(evidence_a, 'only-a'), 'only-a\n') or { panic(err) }
	os.write_file(os.join_path(evidence_b, 'only-b'), 'only-b\n') or { panic(err) }
	$if windows {
		wide_root := root.to_wide()
		wide_evidence := 'evidence'.to_wide()
		defer {
			unsafe {
				free(voidptr(wide_root))
				free(voidptr(wide_evidence))
			}
		}
		root_handle := C.tccbin_windows_open_directory_path_no_follow(wide_root)
		assert root_handle != voidptr(-1) && root_handle != unsafe { nil }
		defer {
			C.CloseHandle(root_handle)
		}
		evidence_handle := C.tccbin_windows_open_child_no_follow(root_handle, wide_evidence, 1)
		assert evidence_handle != voidptr(-1) && evidence_handle != unsafe { nil }
		defer {
			C.CloseHandle(evidence_handle)
		}
		assert t2c_read_windows_child(evidence_handle, 'sentinel') or { panic(err) } == 'evidence-a\n'
		mut held_names := t2c_windows_directory_entries(evidence_handle, 2) or { panic(err) }
		held_names.sort()
		assert held_names == ['only-a', 'sentinel']
		mut renamed := true
		os.rename(evidence_a, evidence_retained) or { renamed = false }
		if renamed {
			os.rename(evidence_b, evidence_a) or { panic(err) }
			assert t2c_read_windows_child(evidence_handle, 'sentinel') or { panic(err) } == 'evidence-a\n'
			assert os.read_file(os.join_path(evidence_a, 'sentinel')) or { panic(err) } == 'evidence-b\n'
			assert os.read_file(os.join_path(evidence_retained, 'sentinel')) or { panic(err) } == 'evidence-a\n'
			mut replacement_names := os.ls(evidence_a) or { panic(err) }
			replacement_names.sort()
			assert replacement_names == ['only-b', 'sentinel']
			mut retained_names := os.ls(evidence_retained) or { panic(err) }
			retained_names.sort()
			assert retained_names == ['only-a', 'sentinel']
		} else {
			assert os.read_file(os.join_path(evidence_a, 'sentinel')) or { panic(err) } == 'evidence-a\n'
			assert os.read_file(os.join_path(evidence_b, 'sentinel')) or { panic(err) } == 'evidence-b\n'
			mut a_names := os.ls(evidence_a) or { panic(err) }
			a_names.sort()
			assert a_names == ['only-a', 'sentinel']
			mut b_names := os.ls(evidence_b) or { panic(err) }
			b_names.sort()
			assert b_names == ['only-b', 'sentinel']
		}
		held_names = t2c_windows_directory_entries(evidence_handle, 2) or { panic(err) }
		held_names.sort()
		assert held_names == ['only-a', 'sentinel']
	} $else {
		root_fd := C.tccbin_open_directory_no_follow(&char(root.str))
		assert root_fd >= 0
		defer {
			C.tccbin_close_document(root_fd)
		}
		evidence_fd := C.tccbin_openat_no_follow(root_fd, &char(c'evidence'), 1)
		assert evidence_fd >= 0
		defer {
			C.tccbin_close_document(evidence_fd)
		}
		os.rename(evidence_a, evidence_retained) or { panic(err) }
		os.rename(evidence_b, evidence_a) or { panic(err) }
		assert t2c_read_posix_child(evidence_fd, 'sentinel') or { panic(err) } == 'evidence-a\n'
		assert os.read_file(os.join_path(evidence_a, 'sentinel')) or { panic(err) } == 'evidence-b\n'
		assert os.read_file(os.join_path(evidence_retained, 'sentinel')) or { panic(err) } == 'evidence-a\n'
	}
}

fn test_native_validation_capsule_rejects_closed_set_bytes_types_and_bounds() {
	fixture := prepare_complete_candidate('native-validation-capsule-negative', false, '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	authenticated := bin.authenticate_manifest_file(fixture.automation_root, fixture.manifest_path) or {
		panic(err)
	}
	subject := t2b_native_subject(fixture, authenticated)
	matrix_source := t2b_native_matrix_source(fixture.manifest_source, fixture.authority, subject)
	capsule_base := os.real_path(fixture.base)
	assert t2c_capsule_error(fixture.automation_root, authenticated, subject, 'capsule-relative') == 'native validation capsule root must be an exact absolute physical directory'

	missing_root := os.join_path(capsule_base, 'capsule-missing')
	missing_names := t2c_write_native_validation_capsule(missing_root, matrix_source,
		fixture.authority, false)
	os.rm(os.join_path(missing_root, 'evidence', missing_names[0])) or { panic(err) }
	assert t2c_capsule_error(fixture.automation_root, authenticated, subject, missing_root) == 'native validation capsule evidence directory differs from the exact declared set'

	extra_root := os.join_path(capsule_base, 'capsule-extra')
	extra_names := t2c_write_native_validation_capsule(extra_root, matrix_source,
		fixture.authority, false)
	mut extra_digest := '0'.repeat(64)
	if extra_digest in extra_names {
		extra_digest = '1'.repeat(64)
	}
	os.write_file(os.join_path(extra_root, 'evidence', extra_digest), 'extra\n') or { panic(err) }
	assert t2c_capsule_error(fixture.automation_root, authenticated, subject, extra_root) == 'native validation capsule evidence directory differs from the exact declared set'

	root_extra := os.join_path(capsule_base, 'capsule-root-extra')
	t2c_write_native_validation_capsule(root_extra, matrix_source, fixture.authority, false)
	os.write_file(os.join_path(root_extra, 'caller-index.json'), '{}') or { panic(err) }
	assert t2c_capsule_error(fixture.automation_root, authenticated, subject, root_extra) == 'native validation capsule root differs from its exact closed entries'

	matrix_directory_root := os.join_path(capsule_base, 'capsule-matrix-directory')
	t2c_write_native_validation_capsule(matrix_directory_root, matrix_source, fixture.authority,
		false)
	matrix_directory_path := os.join_path(matrix_directory_root, 'native-lane-matrix.json')
	os.rm(matrix_directory_path) or { panic(err) }
	os.mkdir(matrix_directory_path) or { panic(err) }
	assert t2c_capsule_error(fixture.automation_root, authenticated, subject, matrix_directory_root) == 'native validation capsule matrix is not a physical regular file'

	wrong_root := os.join_path(capsule_base, 'capsule-wrong-bytes')
	wrong_names := t2c_write_native_validation_capsule(wrong_root, matrix_source,
		fixture.authority, false)
	os.write_file(os.join_path(wrong_root, 'evidence', wrong_names[0]), 'tampered\n') or {
		panic(err)
	}
	assert t2c_capsule_error(fixture.automation_root, authenticated, subject, wrong_root) == 'native validation evidence bytes differ from their filename digest'

	empty_root := os.join_path(capsule_base, 'capsule-empty')
	empty_names := t2c_write_native_validation_capsule(empty_root, matrix_source,
		fixture.authority, false)
	os.write_file(os.join_path(empty_root, 'evidence', empty_names[0]), '') or { panic(err) }
	assert t2c_capsule_error(fixture.automation_root, authenticated, subject, empty_root) == 'native validation evidence is empty or exceeds its strict byte bound'

	oversized_root := os.join_path(capsule_base, 'capsule-oversized')
	oversized_names := t2c_write_native_validation_capsule(oversized_root, matrix_source,
		fixture.authority, false)
	os.write_file(os.join_path(oversized_root, 'evidence', oversized_names[0]), 'x'.repeat(
		256 * 1024 + 1)) or { panic(err) }
	assert t2c_capsule_error(fixture.automation_root, authenticated, subject, oversized_root) == 'native validation evidence is empty or exceeds its strict byte bound'

	case_root := os.join_path(capsule_base, 'capsule-case')
	case_names := t2c_write_native_validation_capsule(case_root, matrix_source, fixture.authority,
		false)
	os.rename(os.join_path(case_root, 'evidence', case_names[0]), os.join_path(case_root,
		'evidence', case_names[0].to_upper())) or { panic(err) }
	assert t2c_capsule_error(fixture.automation_root, authenticated, subject, case_root) == 'native validation capsule evidence directory differs from the exact declared set'

	nested_root := os.join_path(capsule_base, 'capsule-nested')
	t2c_write_native_validation_capsule(nested_root, matrix_source, fixture.authority, false)
	os.mkdir(os.join_path(nested_root, 'evidence', 'nested')) or { panic(err) }
	assert t2c_capsule_error(fixture.automation_root, authenticated, subject, nested_root) == 'native validation capsule evidence directory differs from the exact declared set'

	$if !windows {
		hardlink_root := os.join_path(capsule_base, 'capsule-hardlink')
		hardlink_names := t2c_write_native_validation_capsule(hardlink_root, matrix_source,
			fixture.authority, false)
		hardlink_path := os.join_path(hardlink_root, 'evidence', hardlink_names[0])
		external_hardlink := os.join_path(fixture.base, 'external-evidence-hardlink')
		os.link(hardlink_path, external_hardlink) or { panic(err) }
		assert t2c_capsule_error(fixture.automation_root, authenticated, subject, hardlink_root) == 'native validation evidence must have exactly one physical link'
		external_hardlink_source := os.read_file(external_hardlink) or { panic(err) }
		hardlink_source := os.read_file(hardlink_path) or { panic(err) }
		assert external_hardlink_source == hardlink_source
		os.rm(external_hardlink) or { panic(err) }

		symlink_root := os.join_path(capsule_base, 'capsule-symlink')
		symlink_names := t2c_write_native_validation_capsule(symlink_root, matrix_source,
			fixture.authority, false)
		symlink_path := os.join_path(symlink_root, 'evidence', symlink_names[0])
		sentinel_path := os.join_path(fixture.base, 'external-evidence-sentinel')
		sentinel_source := os.read_file(symlink_path) or { panic(err) }
		os.write_file(sentinel_path, sentinel_source) or { panic(err) }
		os.rm(symlink_path) or { panic(err) }
		os.symlink(sentinel_path, symlink_path) or { panic(err) }
		assert t2c_capsule_error(fixture.automation_root, authenticated, subject, symlink_root) == 'native validation evidence cannot be opened as a physical regular file'
		assert os.read_file(sentinel_path) or { panic(err) } == sentinel_source

		matrix_symlink_root := os.join_path(capsule_base, 'capsule-matrix-symlink')
		t2c_write_native_validation_capsule(matrix_symlink_root, matrix_source, fixture.authority,
			false)
		matrix_symlink_path := os.join_path(matrix_symlink_root, 'native-lane-matrix.json')
		matrix_sentinel_path := os.join_path(fixture.base, 'external-matrix-sentinel')
		os.write_file(matrix_sentinel_path, matrix_source) or { panic(err) }
		os.rm(matrix_symlink_path) or { panic(err) }
		os.symlink(matrix_sentinel_path, matrix_symlink_path) or { panic(err) }
		assert t2c_capsule_error(fixture.automation_root, authenticated, subject,
			matrix_symlink_root) == 'native validation capsule matrix is not a physical regular file'
		assert os.read_file(matrix_sentinel_path) or { panic(err) } == matrix_source

		fifo_root := os.join_path(capsule_base, 'capsule-fifo')
		fifo_names := t2c_write_native_validation_capsule(fifo_root, matrix_source,
			fixture.authority, false)
		fifo_path := os.join_path(fifo_root, 'evidence', fifo_names[0])
		os.rm(fifo_path) or { panic(err) }
		fifo_result := os.exec(['mkfifo', fifo_path])
		assert fifo_result.exit_code == 0, fifo_result.output
		assert t2c_capsule_error(fixture.automation_root, authenticated, subject, fifo_root) == 'native validation evidence cannot be opened as a physical regular file'

		root_target := os.join_path(capsule_base, 'capsule-root-symlink-target')
		t2c_write_native_validation_capsule(root_target, matrix_source, fixture.authority, false)
		root_link := os.join_path(capsule_base, 'capsule-root-symlink')
		os.symlink(root_target, root_link) or { panic(err) }
		assert t2c_capsule_error(fixture.automation_root, authenticated, subject, root_link) == 'native validation capsule root must be an exact absolute physical directory'

		directory_root := os.join_path(capsule_base, 'capsule-directory-symlink')
		t2c_write_native_validation_capsule(directory_root, matrix_source, fixture.authority, false)
		evidence_root := os.join_path(directory_root, 'evidence')
		external_evidence_root := os.join_path(capsule_base, 'external-evidence-directory')
		os.rename(evidence_root, external_evidence_root) or { panic(err) }
		os.symlink(external_evidence_root, evidence_root) or { panic(err) }
		assert t2c_capsule_error(fixture.automation_root, authenticated, subject, directory_root) == 'native validation evidence directory must be an exact physical directory'
	}
}

fn test_native_lane_matrix_file_authenticates_linux_sentinel_and_closed_bindings() {
	fixture := prepare_complete_candidate('native-lane-matrix-linux', false, '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	authenticated := bin.authenticate_manifest_file(fixture.automation_root, fixture.manifest_path) or {
		panic(err)
	}
	subject := t2b_native_subject(fixture, authenticated)
	matrix_source := t2b_native_matrix_source(fixture.manifest_source, fixture.authority, subject)
	matrix_path := os.join_path(fixture.base, 'native-lane-matrix.json')
	os.write_file(matrix_path, matrix_source) or { panic(err) }
	matrix := bin.authenticate_native_lane_matrix_file(fixture.automation_root, authenticated,
		subject, matrix_path) or { panic(err) }
	assert bin.authenticated_native_lane_matrix_digest(matrix) or { panic(err) } == sha256_bytes(matrix_source.bytes())
	parsed := bin.parse_strict_json(matrix_source) or { panic(err) }
	results := parsed.object_value('results') or { panic('matrix results missing') }
	sentinels :=
		results.array_value.filter((it.object_value('lane_id') or { bin.JsonValue{} }).string_value == 'expected=0')
	assert sentinels.len == 1
	assert (sentinels[0].object_value('probe_id') or { panic('probe missing') }).string_value == 'patch-probes'
	assert (sentinels[0].object_value('expected_count') or { panic('count missing') }).int_value == 0
	assert (sentinels[0].object_value('status') or { panic('status missing') }).string_value == 'passed'

	// The matrix must use the exact reviewed profile retained by the manifest. Removing both disk
	// authorities after manifest authentication cannot change the validator authority.
	os.rm(os.join_path(fixture.automation_root, 'toolchain-profiles', 'linux-amd64.profile.json')) or {
		panic(err)
	}
	os.write_file(os.join_path(fixture.automation_root, 'targets.json'), '{}') or { panic(err) }
	sealed := bin.authenticate_native_lane_matrix_file(fixture.automation_root, authenticated,
		subject, matrix_path) or { panic(err) }
	assert bin.authenticated_native_lane_matrix_digest(sealed) or { panic(err) } == sha256_bytes(matrix_source.bytes())

	root := bin.parse_strict_json(matrix_source) or { panic(err) }
	mutations := [
		[
			t2b_replace_matrix_object_member(matrix_source, 'validator_observation', 'target_id',
				'"windows-amd64"'),
			'toolchain observation target differs from the requested target',
		],
		[
			t2b_replace_matrix_object_member(matrix_source, 'validator_observation', 'profile_id',
				'"linux-amd64-synthetic-v2"'),
			'toolchain observation profile binding differs from the registry',
		],
		[
			replace_canonical_root_member(matrix_source, root, 'validator_observation',
				fixture.authority.producer_source),
			'native lane matrix validator differs from the sealed target profile',
		],
		[
			replace_canonical_root_member(matrix_source, root, 'subject_hash',
				'"${'9'.repeat(64)}"'),
			'native lane matrix subject hash is not derived from its complete subject',
		],
		[
			t2b_replace_matrix_object_member(matrix_source, 'subject', 'tree',
				'"${'8'.repeat(40)}"'),
			'native lane matrix subject differs from the complete expected subject',
		],
		[
			t2b_replace_matrix_object_member(matrix_source, 'producer_toolchain',
				'observation_digest', '"${'7'.repeat(64)}"'),
			'native lane matrix producer differs from the authenticated manifest',
		],
		[
			t2b_matrix_with_result_mutation(matrix_source, 'order'),
			'native lane matrix results differ from the strict manifest order',
		],
		[
			t2b_matrix_with_result_mutation(matrix_source, 'missing'),
			'native lane matrix result count differs from the manifest order',
		],
		[
			t2b_matrix_with_result_mutation(matrix_source, 'extra'),
			'native lane matrix result count differs from the manifest order',
		],
		[
			t2b_matrix_with_result_mutation(matrix_source, 'duplicate'),
			'native lane matrix contains a duplicate probe and lane result',
		],
		[
			t2b_replace_matrix_result_member(matrix_source, 0, 'status', '"skipped"'),
			'native lane matrix schema failed with 1 issue(s)',
		],
		[
			t2b_replace_matrix_object_member(matrix_source, 'selected_run', 'run_attempt', '3'),
			'native lane matrix schema failed with 1 issue(s)',
		],
	]
	for index, mutation in mutations {
		assert t2b_matrix_error(fixture.automation_root, authenticated, subject, matrix_path,
			mutation[0]) == mutation[1], '${index}: ${mutation[1]}'
	}
	mut sentinel_index := -1
	for index, result in results.array_value {
		probe_id := result.object_value('probe_id') or { panic('probe ID missing') }
		lane_id := result.object_value('lane_id') or { panic('lane ID missing') }
		if probe_id.string_value == 'patch-probes' && lane_id.string_value == 'expected=0' {
			sentinel_index = index
			break
		}
	}
	assert sentinel_index >= 0
	bad_sentinel := t2b_replace_matrix_result_member(matrix_source, sentinel_index,
		'fallback_used', 'true')
	assert t2b_matrix_error(fixture.automation_root, authenticated, subject, matrix_path,
		bad_sentinel) == 'empty patch-probes requires the exact passed expected=0 result'

	changed_evidence := t2b_replace_matrix_result_member(matrix_source, 0, 'evidence_sha256',
		'"${'6'.repeat(64)}"')
	os.write_file(matrix_path, changed_evidence) or { panic(err) }
	changed := bin.authenticate_native_lane_matrix_file(fixture.automation_root, authenticated,
		subject, matrix_path) or { panic(err) }
	assert bin.authenticated_native_lane_matrix_digest(changed) or { panic(err) } == sha256_bytes(changed_evidence.bytes())
	assert sha256_bytes(changed_evidence.bytes()) != sha256_bytes(matrix_source.bytes())

	assert t2b_matrix_error(fixture.automation_root, authenticated, subject, matrix_path,
		'${matrix_source}\n') == 'native lane matrix bytes must be exact canonical JSON'
	oversized := '${matrix_source}${' '.repeat(512 * 1024)}'
	assert t2b_matrix_error(fixture.automation_root, authenticated, subject, matrix_path, oversized) == 'native lane matrix exceeds its strict byte bound'
	$if !windows {
		sentinel_path := os.join_path(fixture.base, 'matrix-sentinel.json')
		os.write_file(sentinel_path, matrix_source) or { panic(err) }
		assert os.is_file(matrix_path)
		assert !os.is_link(matrix_path)
		os.rm(matrix_path) or { panic(err) }
		os.symlink(sentinel_path, matrix_path) or { panic(err) }
		assert os.is_link(matrix_path)
		assert os.readlink(matrix_path) or { panic(err) } == sentinel_path
		mut no_follow_error := ''
		bin.authenticate_native_lane_matrix_file(fixture.automation_root, authenticated, subject,
			matrix_path) or { no_follow_error = err.msg() }
		assert no_follow_error == 'native lane matrix is not a physical regular file'
		assert os.read_file(sentinel_path) or { panic(err) } == matrix_source
	}
}

fn test_native_lane_matrix_file_authenticates_windows_consumers_and_expected_counts() {
	fixture := prepare_t2b_windows_matrix_candidate('positive')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	authenticated := bin.authenticate_staged_manifest_file(fixture.automation_root,
		fixture.manifest_path, fixture.contract) or { panic(err) }
	subject := t2b_native_subject(fixture, authenticated)
	matrix_source := t2b_native_matrix_source(fixture.manifest_source, fixture.authority, subject)
	matrix_path := os.join_path(fixture.base, 'windows-native-lane-matrix.json')
	os.write_file(matrix_path, matrix_source) or { panic(err) }
	matrix := bin.authenticate_native_lane_matrix_file(fixture.automation_root, authenticated,
		subject, matrix_path) or { panic(err) }
	matrix_digest := bin.authenticated_native_lane_matrix_digest(matrix) or { panic(err) }
	assert matrix_digest == sha256_bytes(matrix_source.bytes())
	root := bin.parse_strict_json(matrix_source) or { panic(err) }
	results := (root.object_value('results') or { panic('results missing') }).array_value
	assert results.len == 45
	mut openlibm_indexes := []int{}
	mut groups := []string{}
	for index, result in results {
		if (result.object_value('probe_id') or { panic('probe missing') }).string_value == 'opaque-openlibm' {
			openlibm_indexes << index
			assert (result.object_value('expected_count') or { panic('count missing') }).int_value == 5
			assert (result.object_value('object_linked') or { panic('object flag missing') }).bool_value
			groups << (result.object_value('consumer_group') or { panic('consumer group missing') }).string_value
		}
	}
	assert openlibm_indexes.len == 5
	groups.sort()
	assert groups == ['fontstash', 'json', 'math', 'stbi', 'vorbis']
	red_fallback := t2b_replace_matrix_result_member(t2b_replace_matrix_result_member(matrix_source,
		openlibm_indexes[0], 'fallback_used', 'true'), openlibm_indexes[0], 'object_linked',
		'false')
	os.write_file(matrix_path, red_fallback) or { panic(err) }
	assert os.is_file(matrix_path)
	assert !os.is_link(matrix_path)
	red_matrix := bin.authenticate_native_lane_matrix_file(fixture.automation_root, authenticated,
		subject, matrix_path) or { panic(err) }
	red_digest := bin.authenticated_native_lane_matrix_digest(red_matrix) or { panic(err) }
	assert red_digest == sha256_bytes(red_fallback.bytes())
	assert red_digest != matrix_digest
	assert os.read_file(matrix_path) or { panic(err) } == red_fallback
	bad_object := t2b_replace_matrix_result_member(matrix_source, openlibm_indexes[0],
		'object_linked', 'false')
	assert t2b_matrix_error(fixture.automation_root, authenticated, subject, matrix_path,
		bad_object) == 'openlibm matrix result is not bound to its exact linked consumer'
	bad_group := t2b_replace_matrix_result_member(matrix_source, openlibm_indexes[1],
		'consumer_group', '"none"')
	assert t2b_matrix_error(fixture.automation_root, authenticated, subject, matrix_path, bad_group) == 'openlibm matrix result is not bound to its exact linked consumer'
	bad_count := t2b_replace_matrix_result_member(matrix_source, openlibm_indexes[2],
		'expected_count', '4')
	assert t2b_matrix_error(fixture.automation_root, authenticated, subject, matrix_path, bad_count) == 'native lane matrix result is not one explicit closed result'
}

fn assert_staged_ineligible(fixture CompleteCandidateFixture, manifest_path string,
	contract bin.StagingContract,
	runtime bin.RuntimeContractBinding) {
	decision := bin.evaluate_staged_manifest_for_execution(fixture.automation_root, manifest_path,
		contract, runtime, false) or { panic(err) }
	assert !decision.eligible
	assert !decision.publish_allowed
	assert decision.reason == 'staged_provenance_ineligible'
	mut publish_rejected := false
	bin.evaluate_staged_manifest_for_execution(fixture.automation_root, manifest_path, contract,
		runtime, true) or { publish_rejected = true }
	assert publish_rejected
}

fn staged_execution_error(contract_automation_root string, manifest_path string,
	contract bin.StagingContract,
	runtime bin.RuntimeContractBinding, publish_requested bool) string {
	bin.evaluate_staged_manifest_for_execution(contract_automation_root, manifest_path, contract,
		runtime, publish_requested) or { return err.msg() }
	panic('staged execution unexpectedly succeeded')
}

fn candidate_execution_error(fixture CompleteCandidateFixture, base_sha string,
	candidate_sha string, work_root string, runtime bin.RuntimeContractBinding,
	publish_requested bool) string {
	bin.evaluate_candidate_manifest_for_execution(fixture.automation_root, fixture.target_id,
		.monthly, fixture.source_repo, base_sha, candidate_sha, work_root, runtime,
		publish_requested) or { return err.msg() }
	panic('candidate execution unexpectedly succeeded')
}

fn candidate_composition_error(request bin.CandidateCompositionRequest,
	runtime bin.RuntimeContractBinding) string {
	return candidate_composition_error_at(automation_root(), request, runtime)
}

fn candidate_composition_error_at(contract_automation_root string,
	request bin.CandidateCompositionRequest, runtime bin.RuntimeContractBinding) string {
	bin.compose_candidate_for_execution(contract_automation_root, request, runtime) or {
		return err.msg()
	}
	panic('candidate composition unexpectedly succeeded')
}

fn candidate_case_variant_environment_error(fixture CompleteCandidateFixture, work_root string) string {
	os.setenv('git_dir', '/tmp/forbidden-candidate-git-dir', true)
	defer {
		os.unsetenv('git_dir')
	}
	return candidate_execution_error(fixture, fixture.parent_ref, fixture.contract.source_git_ref,
		work_root, runtime_contract_binding(false), false)
}

fn commit_candidate_fixture_mutation(fixture CompleteCandidateFixture, extra_path string,
	extra_source string, message string) string {
	manifest_source := os.read_file(fixture.manifest_path) or { panic(err) }
	os.write_file(fixture.manifest_path, '${manifest_source} \n') or { panic(err) }
	mut add_paths := ['automation/bundle-manifest.json']
	if extra_path != '' {
		full_path := os.join_path(fixture.source_repo, extra_path)
		os.mkdir_all(os.dir(full_path)) or { panic(err) }
		os.write_file(full_path, extra_source) or { panic(err) }
		add_paths << extra_path
	}
	mut add_command := ['git', '-C', fixture.source_repo, 'add', '--']
	add_command << add_paths
	for args in [add_command, ['git', '-C', fixture.source_repo, 'commit', '-qm', message]] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	result := os.exec(['git', '-C', fixture.source_repo, 'rev-parse', 'HEAD'])
	assert result.exit_code == 0, result.output
	return result.output.trim_space()
}

fn advance_candidate_fixture(fixture CompleteCandidateFixture, message string) CompleteCandidateFixture {
	base_sha := fixture.contract.source_git_ref
	manifest_source := '${os.read_file(fixture.manifest_path) or { panic(err) }} \n'
	os.write_file(fixture.manifest_path, manifest_source) or { panic(err) }
	for args in [
		['git', '-C', fixture.source_repo, 'add', '--', 'automation/bundle-manifest.json'],
		['git', '-C', fixture.source_repo, 'commit', '-qm', message],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	ref_result := os.exec(['git', '-C', fixture.source_repo, 'rev-parse', 'HEAD'])
	assert ref_result.exit_code == 0, ref_result.output
	return CompleteCandidateFixture{
		...fixture
		manifest_source: manifest_source
		parent_ref:      base_sha
		contract:        bin.StagingContract{
			staging_root:    fixture.staging_root
			source_git_root: fixture.source_repo
			source_git_ref:  ref_result.output.trim_space()
		}
	}
}

fn extra_candidate_inventory_record(path string, bytes []u8) string {
	return ',\n' +
		['    {', '      "path": "${path}",', '      "kind": "file",', '      "git_mode": "100644",', '      "sha256": "${sha256_bytes(bytes)}",', '      "symlink_target": null,', '      "provenance": {', '        "status": "complete",', '        "repository": "TinyCC/tinycc",', '        "sha": "cccccccccccccccccccccccccccccccccccccccc",', '        "source_path": "${path}",', '        "license": "LGPL-2.1-or-later"', '      },', '      "role": "compiler-source",', '      "opaque": false,', '      "opaque_acceptance_id": null,', '      "format": null,', '      "object_type": null,', '      "machine": null,', '      "os_abi": null', '    }'].join('\n')
}

fn manifest_with_extra_candidate_inventory(source string, path string, bytes []u8) string {
	marker := '\n  ],\n  "outputs":'
	assert source.count(marker) == 1
	return source.replace_once(marker, '${extra_candidate_inventory_record(path, bytes)}${marker}')
}

fn manifest_with_payload_collections(source string, overlays []bin.JsonValue,
	inventory []bin.JsonValue, outputs []bin.JsonValue) string {
	root := bin.parse_strict_json(source) or { panic(err) }
	mut values := root.object_values.clone()
	replacements := {
		'overlays':  overlays
		'inventory': inventory
		'outputs':   outputs
	}
	for key, entries in replacements {
		index := root.object_keys.index(key)
		assert index >= 0
		values[index] = bin.JsonValue{
			kind:        .array
			array_value: entries
		}
	}
	return bin.canonical_json(bin.JsonValue{
		kind:          .object
		object_keys:   root.object_keys.clone()
		object_values: values
	})
}

fn commit_candidate_paths(source_repo string, paths []string, message string) string {
	mut add := ['git', '-C', source_repo, 'add', '--']
	add << paths
	for args in [add, ['git', '-C', source_repo, 'commit', '-qm', message]] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	ref_result := os.exec(['git', '-C', source_repo, 'rev-parse', 'HEAD'])
	assert ref_result.exit_code == 0, ref_result.output
	return ref_result.output.trim_space()
}

fn candidate_git_entry_for_test(repository string, reference string, path string) (string, string) {
	result := os.exec(['git', '--no-replace-objects', '-C', repository, 'ls-tree', '-z',
		'--full-tree', reference, '--', path])
	assert result.exit_code == 0, result.output
	records := result.output.split('\x00')
	assert records.len == 2 && records[1] == ''
	parts := records[0].split_nth('\t', 2)
	assert parts.len == 2 && parts[1] == path
	metadata := parts[0].fields()
	assert metadata.len == 3 && metadata[1] == 'blob'
	return metadata[0], metadata[2]
}

fn candidate_raw_oid_for_test(repository string, path string) string {
	result := os.exec(['git', '--no-replace-objects', '-C', repository, 'hash-object', '--no-filters',
		'--', path])
	assert result.exit_code == 0, result.output
	return result.output.trim_space()
}

fn commit_manifest_symlink(source_repo string, target string, message string) string {
	manifest_path := os.join_path(source_repo, 'automation', 'bundle-manifest.json')
	if os.exists(manifest_path) || os.is_link(manifest_path) {
		os.rm(manifest_path) or { panic(err) }
	}
	os.symlink(target, manifest_path) or { panic(err) }
	return commit_candidate_paths(source_repo, ['automation/bundle-manifest.json'], message)
}

fn update_legacy_fixture_base_sha(fixture LegacyCompositionFixture, old_sha string,
	new_sha string) {
	registry_path := os.join_path(fixture.automation_root, 'targets.json')
	mut registry_source := os.read_file(registry_path) or { panic(err) }
	assert registry_source.count(old_sha) == 2
	registry_source = registry_source.replace(old_sha, new_sha)
	os.write_file(registry_path, registry_source) or { panic(err) }
}

fn assert_no_candidate_composition_scratch(parent string) {
	mut names := os.ls(parent) or { panic(err) }
	names = names.filter(it.starts_with('.tccbin-compose-'))
	assert names.len == 0, '${names}'
}

fn prepare_windows_immutable_transform_candidate(suffix string) (CompleteCandidateFixture, string, string) {
	base := os.join_path(os.temp_dir(), 'tccbin-windows-controls-${os.getpid()}-${suffix}')
	source_repo := os.join_path(base, 'source')
	os.rmdir_all(base) or {}
	os.mkdir_all(source_repo) or { panic(err) }
	authority := t2a_prepare_toolchain_authority(base, 'windows-amd64')
	for args in [
		['git', '-C', source_repo, 'init', '-q'],
		['git', '-C', source_repo, 'config', 'user.email', 'ci@example.invalid'],
		['git', '-C', source_repo, 'config', 'user.name', 'Contract Test'],
		['git', '-C', source_repo, 'config', 'core.autocrlf', 'false'],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	mut manifest_source := os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'manifest-windows-opaque.valid.json')) or { panic(err) }
	manifest_source = t2a_resolved_manifest_toolchain(manifest_source, authority)
	manifest := bin.parse_strict_json(manifest_source) or { panic(err) }
	recipe := manifest.object_value('recipe') or { panic('recipe missing') }
	patches := manifest.object_value('patches') or { panic('patches missing') }
	transforms := manifest.object_value('transforms') or { panic('transforms missing') }
	overlays := manifest.object_value('overlays') or { panic('overlays missing') }
	assert overlays.array_value.len == 1
	mut controls := [recipe]
	controls << patches.array_value
	controls << transforms.array_value
	mut transform_path := ''
	for index, control in controls {
		path_value := control.object_value('path') or { panic('control path missing') }
		hash_value := control.object_value('sha256') or { panic('control hash missing') }
		assert path_value.kind == .string_value
		assert hash_value.kind == .string_value
		path := path_value.string_value
		bytes := 'reviewed control ${index}: ${path}\n'.bytes()
		assert manifest_source.count(hash_value.string_value) == 1
		manifest_source = manifest_source.replace_once(hash_value.string_value, sha256_bytes(bytes))
		full_path := os.join_path(source_repo, path)
		os.mkdir_all(os.dir(full_path)) or { panic(err) }
		os.write_file_array(full_path, bytes) or { panic(err) }
		if index == controls.len - 1 {
			transform_path = path
		}
	}
	assert transform_path != ''
	overlay := overlays.array_value[0]
	overlay_path_value := overlay.object_value('path') or { panic('overlay path missing') }
	overlay_hash_value := overlay.object_value('sha256') or { panic('overlay hash missing') }
	assert overlay_path_value.kind == .string_value
	assert overlay_hash_value.kind == .string_value
	overlay_bytes := 'reviewed immutable Windows overlay\n'.bytes()
	assert manifest_source.count(overlay_hash_value.string_value) == 1
	manifest_source = manifest_source.replace_once(overlay_hash_value.string_value,
		sha256_bytes(overlay_bytes))
	overlay_path := os.join_path(source_repo, overlay_path_value.string_value)
	os.mkdir_all(os.dir(overlay_path)) or { panic(err) }
	os.write_file_array(overlay_path, overlay_bytes) or { panic(err) }
	manifest_path := os.join_path(source_repo, 'automation', 'bundle-manifest.json')
	os.mkdir_all(os.dir(manifest_path)) or { panic(err) }
	os.write_file(manifest_path, manifest_source) or { panic(err) }
	base_sha := commit_candidate_paths(source_repo, ['.'], 'reviewed Windows control base')
	os.write_file(os.join_path(source_repo, transform_path), 'changed transform bytes\n') or {
		panic(err)
	}
	os.write_file(manifest_path, '${manifest_source} \n') or { panic(err) }
	candidate_sha := commit_candidate_paths(source_repo, [
		'automation/bundle-manifest.json',
		transform_path,
	], 'mutate immutable transform blob')
	return CompleteCandidateFixture{
		base:            base
		automation_root: authority.root
		target_id:       'windows-amd64'
		source_repo:     source_repo
		manifest_path:   manifest_path
		manifest_source: manifest_source
		authority:       authority
	}, base_sha, candidate_sha
}

fn test_managed_baseline_activation_parser_and_both_entrypoints_are_dormant_by_default() {
	assert bin.parse_candidate_transition_kind('baseline-activate') or { panic(err) } == .baseline_activate
	mut parse_message := ''
	bin.parse_candidate_transition_kind('baseline_activate') or { parse_message = err.msg() }
	assert parse_message == 'candidate transition kind must be monthly, legacy-onboard, or baseline-activate'

	result_root := os.join_path(os.temp_dir(), 'tccbin-dormant-activation-result-${os.getpid()}')
	work_root := os.join_path(os.temp_dir(), 'tccbin-dormant-activation-work-${os.getpid()}')
	os.rmdir_all(result_root) or {}
	os.rmdir_all(work_root) or {}
	runtime := bin.RuntimeContractBinding{
		repository: 'vlang/v'
		sha:        'a'.repeat(40)
	}
	compose_message := candidate_composition_error(bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: '/absent/base-repository'
		base_sha:       '0'.repeat(40)
		raw_root:       '/absent/raw-root'
		manifest_path:  '/absent/manifest.json'
		result_root:    result_root
	}, runtime)
	assert compose_message == 'target has no reviewed managed baseline activation policy'
	mut preflight_message := ''
	bin.evaluate_candidate_manifest_for_execution(automation_root(), 'linux-amd64',
		.baseline_activate, '/absent/candidate-repository', '0'.repeat(40), '1'.repeat(40),
		work_root, runtime, false) or { preflight_message = err.msg() }
	assert preflight_message == 'target has no reviewed managed baseline activation policy'
	assert !os.exists(result_root)
	assert !os.exists(work_root)
}

fn test_managed_baseline_activation_composes_only_a_complete_manifest_direct_child() {
	fixture := prepare_managed_baseline_activation_fixture('complete')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	os.write_file(os.join_path(fixture.raw_root, 'raw-extra-must-not-enter.txt'), 'ignored\n') or {
		panic(err)
	}
	result := bin.compose_candidate_for_execution(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime) or { panic(err) }
	assert result.kind == .baseline_activate
	assert result.base_sha == fixture.base_sha
	assert result.decision.eligible
	assert !result.decision.publish_allowed
	mut exposed := os.ls(fixture.result_root) or { panic(err) }
	exposed.sort()
	assert exposed == ['candidate-repository']
	repository := os.join_path(fixture.result_root, 'candidate-repository')
	assert (os.read_file(os.join_path(repository, 'automation', 'bundle-manifest.json')) or {
		panic(err)
	}) == fixture.candidate_manifest_source
	assert !os.exists(os.join_path(repository, 'raw-extra-must-not-enter.txt'))
	diff := os.exec(['git', '--no-replace-objects', '-C', repository, 'diff-tree', '--no-commit-id',
		'--name-status', '-r', '--no-renames', fixture.base_sha, result.candidate_sha, '--'])
	parent := os.exec(['git', '--no-replace-objects', '-C', repository, 'rev-parse', 'HEAD^'])
	status := os.exec(['git', '--no-replace-objects', '-C', repository, 'status', '--porcelain=v1',
		'--untracked-files=all', '--ignored=matching'])
	assert diff.exit_code == 0 && diff.output == 'M\tautomation/bundle-manifest.json\n'
	assert parent.exit_code == 0 && parent.output.trim_space() == fixture.base_sha
	assert status.exit_code == 0 && status.output == ''
	for path in ['src/tcc.c', 'tcc.exe'] {
		base_mode, base_oid := candidate_git_entry_for_test(repository, fixture.base_sha, path)
		candidate_mode, candidate_oid := candidate_git_entry_for_test(repository,
			result.candidate_sha, path)
		assert candidate_mode == base_mode
		assert candidate_oid == base_oid
	}
}

fn test_managed_baseline_activation_binds_v_libgc_to_a_hardened_runtime_checkout() {
	fixture := prepare_managed_baseline_runtime_activation_fixture('runtime-positive')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	result := bin.compose_candidate_for_execution(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      fixture.target_id
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime) or { panic(err) }
	assert result.decision.eligible
	assert !result.decision.publish_allowed
	runtime_tree := os.exec(['git', '-C', fixture.contract_root, 'rev-parse',
		'${fixture.runtime.sha}^{tree}'])
	assert runtime_tree.exit_code == 0, runtime_tree.output
	assert activation_source_tree(fixture.candidate_manifest_source, 'v-libgc') == runtime_tree.output.trim_space()
}

fn test_managed_baseline_activation_rejects_a_non_authoritative_runtime_origin() {
	fixture := prepare_managed_baseline_runtime_activation_fixture('runtime-origin-negative')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	changed_origin := os.exec(['git', '-C', fixture.contract_root, 'remote', 'set-url', 'origin',
		'https://example.invalid/untrusted/v.git'])
	assert changed_origin.exit_code == 0, changed_origin.output
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      fixture.target_id
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime)
	assert message == 'managed baseline runtime contract origin is not the vlang/v HTTPS repository'
	assert !os.exists(fixture.result_root)
	assert_no_candidate_composition_scratch(fixture.base)
}

fn test_managed_baseline_activation_rejects_runtime_tree_and_head_drift() {
	fixture := prepare_managed_baseline_runtime_activation_fixture('runtime-tree-head-negative')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	wrong_tree_source := activation_candidate_manifest_with_runtime_source(fixture.candidate_manifest_source,
		fixture.runtime.sha, '9'.repeat(40))
	os.write_file(fixture.manifest_path, wrong_tree_source) or { panic(err) }
	tree_message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      fixture.target_id
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime)
	assert tree_message == 'managed baseline activation source differs from reviewed commit evidence'
	assert !os.exists(fixture.result_root)
	assert_no_candidate_composition_scratch(fixture.base)
	os.write_file(fixture.manifest_path, fixture.candidate_manifest_source) or { panic(err) }
	os.write_file(os.join_path(fixture.contract_root, 'runtime-head-drift.txt'), 'drift\n') or {
		panic(err)
	}
	for args in [
		['git', '-C', fixture.contract_root, 'add', '--all'],
		['git', '-C', fixture.contract_root, '-c', 'commit.gpgsign=false', 'commit', '-qm',
			'runtime head drift'],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	head_message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      fixture.target_id
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime)
	assert head_message == 'managed baseline runtime contract checkout is not at the runtime SHA'
	assert !os.exists(fixture.result_root)
	assert_no_candidate_composition_scratch(fixture.base)
}

fn test_managed_baseline_activation_rejects_altered_raw_without_exposing_a_partial_result() {
	fixture := prepare_managed_baseline_activation_fixture('raw-altered')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	os.write_file(os.join_path(fixture.raw_root, 'src', 'tcc.c'), 'untrusted replacement\n') or {
		panic(err)
	}
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime)
	assert message == 'candidate RAW entry differs from its manifest declaration'
	assert !os.exists(fixture.result_root)
	assert_no_candidate_composition_scratch(fixture.base)
}

fn test_managed_baseline_activation_rejects_absent_raw_without_exposing_a_partial_result() {
	fixture := prepare_managed_baseline_activation_fixture('raw-absent')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	os.rm(os.join_path(fixture.raw_root, 'src', 'tcc.c')) or { panic(err) }
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime)
	assert message == 'declared payload path is absent from staging'
	assert !os.exists(fixture.result_root)
	assert_no_candidate_composition_scratch(fixture.base)
}

fn test_managed_baseline_activation_candidate_preflight_exports_an_independent_payload() {
	fixture := prepare_managed_baseline_activation_fixture('preflight-complete')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	os.write_file(os.join_path(fixture.base_repo_root, 'automation', 'bundle-manifest.json'),
		fixture.candidate_manifest_source) or { panic(err) }
	candidate_sha := commit_candidate_paths(fixture.base_repo_root, [
		'automation/bundle-manifest.json',
	], 'activate reviewed managed baseline manifest')
	work_root := os.join_path(fixture.base, 'managed-baseline-preflight-success')
	decision := bin.evaluate_candidate_manifest_for_execution(fixture.automation_root,
		'linux-amd64', .baseline_activate, fixture.base_repo_root, fixture.base_sha, candidate_sha,
		work_root, fixture.runtime, false) or { panic(err) }
	assert decision.eligible
	assert !decision.publish_allowed
	assert decision.reason == 'authenticated_staging'
	payload_root := os.join_path(work_root, 'payload')
	source_root := os.join_path(work_root, 'candidate-source')
	assert os.is_dir(payload_root)
	assert !os.exists(os.join_path(payload_root, '.git'))
	assert !os.exists(os.join_path(payload_root, 'automation'))
	parent := os.exec(['git', '--no-replace-objects', '-C', source_root, 'rev-parse', 'HEAD^'])
	diff := os.exec(['git', '--no-replace-objects', '-C', source_root, 'diff-tree', '--no-commit-id',
		'--name-status', '-r', '--no-renames', fixture.base_sha, candidate_sha, '--'])
	assert parent.exit_code == 0 && parent.output.trim_space() == fixture.base_sha
	assert diff.exit_code == 0 && diff.output == 'M\tautomation/bundle-manifest.json\n'
	for path in ['src/tcc.c', 'tcc.exe'] {
		assert os.read_bytes(os.join_path(payload_root, path)) or { panic(err) } == os.read_bytes(os.join_path(source_root,
			path)) or { panic(err) }
	}
	$if !windows {
		payload_stat := os.lstat(os.join_path(payload_root, 'tcc.exe')) or { panic(err) }
		source_stat := os.lstat(os.join_path(source_root, 'tcc.exe')) or { panic(err) }
		assert payload_stat.dev != source_stat.dev || payload_stat.inode != source_stat.inode
		assert payload_stat.nlink == 1
	}
}

fn test_managed_baseline_activation_attests_the_exact_reviewed_base_before_exposure() {
	fixture := prepare_managed_baseline_activation_fixture('base-tree-pin')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	registry_path := os.join_path(fixture.automation_root, 'targets.json')
	mut registry_source := os.read_file(registry_path) or { panic(err) }
	assert registry_source.count(fixture.base_tree) == 1
	registry_source = registry_source.replace_once(fixture.base_tree, '8'.repeat(40))
	os.write_file(registry_path, registry_source) or { panic(err) }
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime)
	assert message == 'managed baseline activation base tree differs from the reviewed pin'
	assert !os.exists(fixture.result_root)
	assert_no_candidate_composition_scratch(fixture.base)
}

fn test_managed_baseline_activation_rejects_wrong_manifest_and_parent_pins() {
	manifest_fixture := prepare_managed_baseline_activation_fixture('base-manifest-pin')
	defer {
		os.rmdir_all(manifest_fixture.base) or {}
	}
	manifest_registry_path := os.join_path(manifest_fixture.automation_root, 'targets.json')
	mut manifest_registry := os.read_file(manifest_registry_path) or { panic(err) }
	base_manifest_sha256 := sha256_bytes(manifest_fixture.base_manifest_source.bytes())
	assert manifest_registry.count(base_manifest_sha256) == 1
	manifest_registry = manifest_registry.replace_once(base_manifest_sha256, '6'.repeat(64))
	os.write_file(manifest_registry_path, manifest_registry) or { panic(err) }
	manifest_message := candidate_composition_error_at(manifest_fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: manifest_fixture.base_repo_root
		base_sha:       manifest_fixture.base_sha
		raw_root:       manifest_fixture.raw_root
		manifest_path:  manifest_fixture.manifest_path
		result_root:    manifest_fixture.result_root
	}, manifest_fixture.runtime)
	assert manifest_message == 'managed baseline activation base manifest differs from the reviewed pin'
	assert !os.exists(manifest_fixture.result_root)
	assert_no_candidate_composition_scratch(manifest_fixture.base)

	parent_fixture := prepare_managed_baseline_activation_fixture('base-parent-pin')
	defer {
		os.rmdir_all(parent_fixture.base) or {}
	}
	parent_registry_path := os.join_path(parent_fixture.automation_root, 'targets.json')
	mut parent_registry := os.read_file(parent_registry_path) or { panic(err) }
	parent_marker := '"parent_sha":"${parent_fixture.parent_sha}"'
	assert parent_registry.count(parent_marker) == 1
	parent_registry = parent_registry.replace_once(parent_marker,
		'"parent_sha":"${'8'.repeat(40)}"')
	os.write_file(parent_registry_path, parent_registry) or { panic(err) }
	parent_message := candidate_composition_error_at(parent_fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: parent_fixture.base_repo_root
		base_sha:       parent_fixture.base_sha
		raw_root:       parent_fixture.raw_root
		manifest_path:  parent_fixture.manifest_path
		result_root:    parent_fixture.result_root
	}, parent_fixture.runtime)
	assert parent_message == 'managed baseline activation base parent differs from the reviewed pin'
	assert !os.exists(parent_fixture.result_root)
	assert_no_candidate_composition_scratch(parent_fixture.base)
}

fn test_managed_baseline_activation_rejects_old_runtime_and_contract_authority_migration() {
	old_runtime_fixture := prepare_managed_baseline_activation_fixture('old-runtime')
	defer {
		os.rmdir_all(old_runtime_fixture.base) or {}
	}
	old_runtime_root := bin.parse_strict_json(old_runtime_fixture.candidate_manifest_source) or {
		panic(err)
	}
	old_runtime_source := bin.canonical_json(activation_object_with_replacements(old_runtime_root, {
		'contract_sha': activation_string(managed_baseline_phase_a_contract_sha)
		'v_source_sha': activation_string(managed_baseline_phase_a_contract_sha)
	}))
	os.write_file(old_runtime_fixture.manifest_path, old_runtime_source) or { panic(err) }
	old_runtime := bin.RuntimeContractBinding{
		repository: 'vlang/v'
		sha:        managed_baseline_phase_a_contract_sha
	}
	old_runtime_message := candidate_composition_error_at(old_runtime_fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: old_runtime_fixture.base_repo_root
		base_sha:       old_runtime_fixture.base_sha
		raw_root:       old_runtime_fixture.raw_root
		manifest_path:  old_runtime_fixture.manifest_path
		result_root:    old_runtime_fixture.result_root
	}, old_runtime)
	assert old_runtime_message == 'managed baseline activation runtime contract must differ from the reviewed base'
	assert !os.exists(old_runtime_fixture.result_root)
	assert_no_candidate_composition_scratch(old_runtime_fixture.base)

	authority_fixture := prepare_managed_baseline_activation_fixture('contract-authority')
	defer {
		os.rmdir_all(authority_fixture.base) or {}
	}
	authority_root := bin.parse_strict_json(authority_fixture.candidate_manifest_source) or {
		panic(err)
	}
	authority_source := bin.canonical_json(activation_object_with_replacements(authority_root, {
		'contract_repository': activation_string('GGRei/v')
		'contract_mode':       activation_string('fork-dry-run')
	}))
	os.write_file(authority_fixture.manifest_path, authority_source) or { panic(err) }
	fork_runtime := bin.RuntimeContractBinding{
		repository: 'GGRei/v'
		sha:        authority_fixture.runtime.sha
	}
	authority_message := candidate_composition_error_at(authority_fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: authority_fixture.base_repo_root
		base_sha:       authority_fixture.base_sha
		raw_root:       authority_fixture.raw_root
		manifest_path:  authority_fixture.manifest_path
		result_root:    authority_fixture.result_root
	}, fork_runtime)
	assert authority_message == 'managed baseline activation candidate must retain the production contract authority'
	assert !os.exists(authority_fixture.result_root)
	assert_no_candidate_composition_scratch(authority_fixture.base)
}

fn test_managed_baseline_activation_rejects_incomplete_candidates_without_exposure() {
	fixture := prepare_managed_baseline_activation_fixture('incomplete')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	incomplete_source :=
		activation_manifest_with_unobserved_producer(fixture.candidate_manifest_source)
	os.write_file(fixture.manifest_path, incomplete_source) or { panic(err) }
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime)
	assert message == 'managed baseline activation candidate must have complete resolved provenance'
	assert !os.exists(fixture.result_root)
	assert_no_candidate_composition_scratch(fixture.base)
}

fn test_managed_baseline_activation_closes_reviewed_policy_and_payload_byte_drift() {
	payload_fixture := prepare_managed_baseline_activation_fixture('payload-sha-drift')
	defer {
		os.rmdir_all(payload_fixture.base) or {}
	}
	payload_drift := activation_manifest_with_first_inventory_sha(payload_fixture.candidate_manifest_source,
		'7'.repeat(64))
	os.write_file(payload_fixture.manifest_path, payload_drift) or { panic(err) }
	payload_message := candidate_composition_error_at(payload_fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: payload_fixture.base_repo_root
		base_sha:       payload_fixture.base_sha
		raw_root:       payload_fixture.raw_root
		manifest_path:  payload_fixture.manifest_path
		result_root:    payload_fixture.result_root
	}, payload_fixture.runtime)
	assert payload_message == 'managed baseline activation changed immutable payload bytes or policy'
	assert !os.exists(payload_fixture.result_root)
	assert_no_candidate_composition_scratch(payload_fixture.base)

	policy_fixture := prepare_managed_baseline_activation_fixture('reviewed-policy-drift')
	defer {
		os.rmdir_all(policy_fixture.base) or {}
	}
	policy_drift :=
		activation_manifest_with_recipe_version(policy_fixture.candidate_manifest_source, 2)
	replace_managed_baseline_activation_policy(policy_fixture, policy_drift)
	os.write_file(policy_fixture.manifest_path, policy_drift) or { panic(err) }
	policy_message := candidate_composition_error_at(policy_fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: policy_fixture.base_repo_root
		base_sha:       policy_fixture.base_sha
		raw_root:       policy_fixture.raw_root
		manifest_path:  policy_fixture.manifest_path
		result_root:    policy_fixture.result_root
	}, policy_fixture.runtime)
	assert policy_message == 'managed baseline activation changed immutable manifest policy'
	assert !os.exists(policy_fixture.result_root)
	assert_no_candidate_composition_scratch(policy_fixture.base)
}

fn test_managed_baseline_activation_rejects_arbitrary_payload_provenance_sha_without_exposure() {
	fixture := prepare_managed_baseline_activation_fixture('arbitrary-provenance-sha')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	malicious_source := activation_manifest_with_first_inventory_provenance(fixture.candidate_manifest_source,
		'TinyCC/tinycc', '9'.repeat(40))
	os.write_file(fixture.manifest_path, malicious_source) or { panic(err) }
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime)
	assert message == 'managed baseline activation payload provenance SHA differs from its authenticated authority'
	assert !os.exists(fixture.result_root)
	assert_no_candidate_composition_scratch(fixture.base)
}

fn test_managed_baseline_activation_rejects_coordinated_source_and_provenance_sha_drift() {
	fixture := prepare_managed_baseline_activation_fixture('coordinated-source-provenance-sha')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	reviewed_sha := activation_source_sha(fixture.candidate_manifest_source, 'tinycc')
	malicious_source := fixture.candidate_manifest_source.replace(reviewed_sha, '9'.repeat(40))
	os.write_file(fixture.manifest_path, malicious_source) or { panic(err) }
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime)
	assert message == 'managed baseline activation source differs from reviewed commit evidence'
	assert !os.exists(fixture.result_root)
	assert_no_candidate_composition_scratch(fixture.base)
}

fn test_managed_baseline_activation_rejects_source_tree_drift() {
	fixture := prepare_managed_baseline_activation_fixture('source-tree-drift')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	reviewed_tree := activation_source_tree(fixture.candidate_manifest_source, 'tinycc')
	malicious_source := fixture.candidate_manifest_source.replace(reviewed_tree, '9'.repeat(40))
	os.write_file(fixture.manifest_path, malicious_source) or { panic(err) }
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime)
	assert message == 'managed baseline activation source differs from reviewed commit evidence'
	assert !os.exists(fixture.result_root)
	assert_no_candidate_composition_scratch(fixture.base)
}

fn test_managed_baseline_activation_commit_evidence_is_closed_bounded_and_self_authenticating() {
	fixture := prepare_managed_baseline_activation_fixture('commit-evidence-negative-matrix')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	first := fixture.source_commit_evidence.array_value[0]
	raw_base64 := (first.object_value('raw_commit_base64') or {
		panic('raw commit evidence missing')
	}).string_value
	raw := base64.decode(raw_base64)
	mut tampered_raw := raw.clone()
	tampered_raw[tampered_raw.len - 1] = if tampered_raw[tampered_raw.len - 1] == `x` {
		`y`
	} else {
		`x`
	}
	tampered := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'raw_commit_base64': activation_string(base64.encode(tampered_raw))
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, tampered) == 'managed baseline raw commit evidence SHA differs from its Git object ID'

	wrong_sha := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'sha': activation_string('9'.repeat(40))
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, wrong_sha) == 'managed baseline raw commit evidence SHA differs from its Git object ID'

	wrong_tree := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'tree': activation_string('9'.repeat(40))
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, wrong_tree) == 'managed baseline raw commit evidence tree differs from its declared tree'

	malformed_raw := 'author Missing Tree <source@example.invalid> 0 +0000\n\nmalformed\n'.bytes()
	malformed := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'sha':               activation_string(activation_git_commit_oid(malformed_raw))
		'raw_commit_base64': activation_string(base64.encode(malformed_raw))
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, malformed) == 'managed baseline raw commit evidence must start with one exact tree header'
	reviewed_tree := (first.object_value('tree') or { panic('evidence tree missing') }).string_value
	nonfirst_tree_raw :=
		'author Wrong Order <source@example.invalid> 0 +0000\ntree ${reviewed_tree}\n\nmalformed\n'.bytes()
	nonfirst_tree := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'sha':               activation_string(activation_git_commit_oid(nonfirst_tree_raw))
		'raw_commit_base64': activation_string(base64.encode(nonfirst_tree_raw))
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, nonfirst_tree) == 'managed baseline raw commit evidence must start with one exact tree header'
	duplicate_tree_raw := 'tree ${reviewed_tree}\ntree ${reviewed_tree}\n\nmalformed\n'.bytes()
	duplicate_tree_header := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'sha':               activation_string(activation_git_commit_oid(duplicate_tree_raw))
		'raw_commit_base64': activation_string(base64.encode(duplicate_tree_raw))
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source,
		duplicate_tree_header) == 'managed baseline raw commit evidence must start with one exact tree header'
	carriage_return_raw := 'tree ${reviewed_tree}\r\n\nmalformed\n'.bytes()
	carriage_return := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'sha':               activation_string(activation_git_commit_oid(carriage_return_raw))
		'raw_commit_base64': activation_string(base64.encode(carriage_return_raw))
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, carriage_return) == 'managed baseline raw commit evidence has a malformed Git commit header'
	nul_header_raw := 'tree ${reviewed_tree}\x00\n\nmalformed\n'.bytes()
	nul_header := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'sha':               activation_string(activation_git_commit_oid(nul_header_raw))
		'raw_commit_base64': activation_string(base64.encode(nul_header_raw))
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, nul_header) == 'managed baseline raw commit evidence has a malformed Git commit header'
	no_separator_raw :=
		'tree ${reviewed_tree}\nauthor Missing Separator <source@example.invalid> 0 +0000\n'.bytes()
	no_separator := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'sha':               activation_string(activation_git_commit_oid(no_separator_raw))
		'raw_commit_base64': activation_string(base64.encode(no_separator_raw))
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, no_separator) == 'managed baseline raw commit evidence has no complete Git commit header'

	noncanonical := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'raw_commit_base64': activation_string('${raw_base64}=')
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, noncanonical) == 'managed baseline raw commit evidence is not canonical bounded base64'

	duplicate := activation_array([first, first])
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, duplicate) == 'managed baseline source commit evidence contains a duplicate source ID'
	manifest := bin.parse_strict_json(fixture.candidate_manifest_source) or { panic(err) }
	sources_value := manifest.object_value('sources') or { panic('sources missing') }
	mut duplicate_sources := sources_value.array_value.clone()
	duplicate_sources[1] = activation_object_with_replacements(duplicate_sources[1], {
		'id': activation_string('tinycc')
	})
	duplicate_source_manifest := bin.canonical_json(activation_object_with_replacements(manifest, {
		'sources': activation_array(duplicate_sources)
	}))
	assert managed_baseline_projection_error(duplicate_source_manifest,
		fixture.source_commit_evidence) == 'managed baseline source matrix contains a duplicate source ID'
	mut open_entry_keys := first.object_keys.clone()
	mut open_entry_values := first.object_values.clone()
	open_entry_keys << 'unexpected'
	open_entry_values << bin.JsonValue{
		kind:       .boolean
		bool_value: true
	}
	open_entry := bin.JsonValue{
		kind:          .object
		object_keys:   open_entry_keys
		object_values: open_entry_values
	}
	open_evidence := activation_array([open_entry, fixture.source_commit_evidence.array_value[1]])
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, open_evidence) == 'closed contract object has missing, duplicate, or unknown members'
	permuted := activation_array([fixture.source_commit_evidence.array_value[1], first])
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, permuted) == 'managed baseline source commit evidence order differs from the source matrix'
	unsupported_id := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'id': activation_string('unreviewed')
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, unsupported_id) == 'managed baseline external commit authority has an unsupported source ID'
	unreferenced_id := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'id': activation_string('libatomic_ops')
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, unreferenced_id) == 'managed baseline source commit evidence contains an unreferenced source ID'
	mismatched_repository := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'repository': activation_string('https://example.invalid/tinycc.git')
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source,
		mismatched_repository) == 'managed baseline source commit evidence changed source repository or ref'
	mismatched_ref := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'ref': activation_string('unreviewed')
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, mismatched_ref) == 'managed baseline source commit evidence changed source repository or ref'
	unsupported_authority := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'authority': activation_string('unreviewed')
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source,
		unsupported_authority) == 'managed baseline source commit evidence authority is unsupported'
	missing := activation_array([first])
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, missing) == 'managed baseline source commit evidence must be bijective with the source matrix'
	oversize := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'raw_commit_base64': activation_string('A'.repeat(87_385))
	})
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, oversize) == 'managed baseline raw commit evidence exceeds its encoded byte bound'
	decoded_oversize_raw := []u8{len: 65_537, init: `x`}
	decoded_oversize := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'raw_commit_base64': activation_string(base64.encode(decoded_oversize_raw))
	})
	assert base64.encode(decoded_oversize_raw).len == 87_384
	assert managed_baseline_projection_error(fixture.candidate_manifest_source, decoded_oversize) == 'managed baseline raw commit evidence is not canonical bounded base64'
}

fn test_managed_baseline_activation_rejects_publication_before_candidate_input() {
	fixture := prepare_managed_baseline_activation_fixture('publication-disabled')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	work_root := os.join_path(fixture.base, 'publication-work-must-remain-absent')
	mut message := ''
	bin.evaluate_candidate_manifest_for_execution(fixture.automation_root, 'linux-amd64',
		.baseline_activate, '/absent/candidate-repository', fixture.base_sha, '9'.repeat(40),
		work_root, fixture.runtime, true) or { message = err.msg() }
	assert message == 'managed baseline activation publication is disabled pending native publication proof'
	assert !os.exists(work_root)
}

fn test_managed_baseline_activation_rejects_valid_evidence_outside_the_registry_hash() {
	fixture := prepare_managed_baseline_activation_fixture('evidence-registry-hash')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	first := fixture.source_commit_evidence.array_value[0]
	tree := (first.object_value('tree') or { panic('evidence tree missing') }).string_value
	raw :=
		'tree ${tree}\nauthor Alternate Source <source@example.invalid> 1 +0000\ncommitter Alternate Source <source@example.invalid> 1 +0000\n\nalternate reviewed source\n'.bytes()
	mutated_evidence := activation_first_source_evidence_with_replacements(fixture.source_commit_evidence, {
		'sha':               activation_string(activation_git_commit_oid(raw))
		'raw_commit_base64': activation_string(base64.encode(raw))
	})
	manifest := bin.parse_strict_json(fixture.candidate_manifest_source) or { panic(err) }
	mutated_policy := bin.managed_baseline_activation_policy_projection(manifest, mutated_evidence) or {
		panic(err)
	}
	os.write_file(fixture.policy_path, bin.canonical_json(mutated_policy)) or { panic(err) }
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      fixture.target_id
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime)
	assert message == 'managed baseline activation policy hash differs from the registry'
	assert !os.exists(fixture.result_root)
	assert_no_candidate_composition_scratch(fixture.base)
}

fn test_managed_baseline_activation_preflight_rejects_cross_source_provenance_sha_without_exposure() {
	fixture := prepare_managed_baseline_activation_fixture('cross-source-provenance-sha')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	malicious_source := activation_manifest_with_first_inventory_provenance(fixture.candidate_manifest_source,
		'TinyCC/tinycc', activation_source_sha(fixture.candidate_manifest_source, 'bdwgc'))
	os.write_file(os.join_path(fixture.base_repo_root, 'automation', 'bundle-manifest.json'),
		malicious_source) or { panic(err) }
	candidate_sha := commit_candidate_paths(fixture.base_repo_root, [
		'automation/bundle-manifest.json',
	], 'attempt cross-source managed baseline provenance')
	work_root := os.join_path(fixture.base, 'cross-source-provenance-preflight')
	mut message := ''
	bin.evaluate_candidate_manifest_for_execution(fixture.automation_root, 'linux-amd64',
		.baseline_activate, fixture.base_repo_root, fixture.base_sha, candidate_sha, work_root,
		fixture.runtime, false) or { message = err.msg() }
	assert message == 'managed baseline activation payload provenance SHA differs from its authenticated authority'
	assert !os.exists(work_root)
}

fn test_managed_baseline_activation_rejects_unreviewed_payload_provenance_repository() {
	fixture := prepare_managed_baseline_activation_fixture('unreviewed-provenance-repository')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	malicious_source := activation_manifest_with_first_inventory_provenance(fixture.candidate_manifest_source,
		'example/unreviewed', '9'.repeat(40))
	replace_managed_baseline_activation_policy(fixture, malicious_source)
	os.write_file(fixture.manifest_path, malicious_source) or { panic(err) }
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime)
	assert message == 'managed baseline activation payload provenance repository is not an authenticated authority'
	assert !os.exists(fixture.result_root)
	assert_no_candidate_composition_scratch(fixture.base)
}

fn assert_managed_baseline_activation_rejects_absent_payload_provenance_source(suffix string,
	repository string, sha string) {
	fixture := prepare_managed_baseline_activation_fixture(suffix)
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	malicious_source := activation_manifest_with_first_inventory_provenance(fixture.candidate_manifest_source,
		repository, sha)
	replace_managed_baseline_activation_policy(fixture, malicious_source)
	os.write_file(fixture.manifest_path, malicious_source) or { panic(err) }
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime)
	assert message == 'managed baseline activation payload provenance authority is absent or ambiguous'
	assert !os.exists(fixture.result_root)
	assert_no_candidate_composition_scratch(fixture.base)
}

fn test_managed_baseline_activation_rejects_provenance_authority_absent_from_target_sources() {
	assert_managed_baseline_activation_rejects_absent_payload_provenance_source('absent-libatomic-provenance',
		'bdwgc/libatomic_ops', '1'.repeat(40))
	assert_managed_baseline_activation_rejects_absent_payload_provenance_source('absent-v-libgc-provenance',
		'vlang/v', 'a'.repeat(40))
}

fn assert_managed_baseline_activation_accepts_payload_provenance_authority(suffix string,
	repository string) {
	fixture := prepare_managed_baseline_activation_fixture(suffix)
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	sha := match repository {
		'ivmai/bdwgc' { activation_source_sha(fixture.candidate_manifest_source, 'bdwgc') }
		'vlang/tccbin' { fixture.base_sha }
		else { panic('unsupported positive provenance authority') }
	}
	candidate_source := activation_manifest_with_first_inventory_provenance(fixture.candidate_manifest_source,
		repository, sha)
	replace_managed_baseline_activation_policy(fixture, candidate_source)
	os.write_file(fixture.manifest_path, candidate_source) or { panic(err) }
	result := bin.compose_candidate_for_execution(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .baseline_activate
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, fixture.runtime) or { panic(err) }
	assert result.decision.eligible
	assert !result.decision.publish_allowed
}

fn test_managed_baseline_activation_accepts_exact_source_and_baseline_provenance_shas() {
	assert_managed_baseline_activation_accepts_payload_provenance_authority('bdwgc-provenance',
		'ivmai/bdwgc')
	assert_managed_baseline_activation_accepts_payload_provenance_authority('baseline-provenance',
		'vlang/tccbin')
}

fn test_managed_baseline_activation_preflight_rejects_any_non_manifest_tree_delta() {
	fixture := prepare_managed_baseline_activation_fixture('tree-closure')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	os.write_file(os.join_path(fixture.base_repo_root, 'automation', 'bundle-manifest.json'),
		fixture.candidate_manifest_source) or { panic(err) }
	os.write_file(os.join_path(fixture.base_repo_root, 'README.md'), 'unreviewed tree delta\n') or {
		panic(err)
	}
	candidate_sha := commit_candidate_paths(fixture.base_repo_root, [
		'automation/bundle-manifest.json',
		'README.md',
	], 'attempt non-manifest activation delta')
	work_root := os.join_path(fixture.base, 'managed-baseline-preflight-work')
	mut message := ''
	bin.evaluate_candidate_manifest_for_execution(fixture.automation_root, 'linux-amd64',
		.baseline_activate, fixture.base_repo_root, fixture.base_sha, candidate_sha, work_root,
		fixture.runtime, false) or { message = err.msg() }
	assert message == 'managed baseline activation transition must modify only the authoritative manifest'
	assert !os.exists(work_root)
}

fn test_candidate_preflight_exports_an_independent_payload_and_binds_publication() {
	fixture := advance_candidate_fixture(prepare_complete_candidate('candidate-preflight-production',
		true, ''), 'candidate manifest refresh')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	work_root := os.join_path(fixture.base, 'candidate-preflight-work')
	decision := bin.evaluate_candidate_manifest_for_execution(fixture.automation_root,
		fixture.target_id, .monthly, fixture.source_repo, fixture.parent_ref,
		fixture.contract.source_git_ref, work_root, runtime_contract_binding(true), true) or {
		panic(err)
	}
	assert decision.eligible
	assert decision.publish_allowed
	assert decision.reason == 'authenticated_staging'
	payload_root := os.join_path(work_root, 'payload')
	source_root := os.join_path(work_root, 'candidate-source')
	assert os.read_bytes(os.join_path(payload_root, 'src', 'tcc.c')) or { panic(err) } == fixture.inventory_bytes
	assert os.read_bytes(os.join_path(payload_root, 'tcc.exe')) or { panic(err) } == fixture.output_bytes
	assert !os.exists(os.join_path(payload_root, '.git'))
	assert !os.exists(os.join_path(payload_root, 'automation'))
	head := os.exec(['git', '--no-replace-objects', '-C', source_root, 'rev-parse', 'HEAD'])
	assert head.exit_code == 0
	assert head.output.trim_space() == fixture.contract.source_git_ref
	symbolic := os.exec(['git', '--no-replace-objects', '-C', source_root, 'symbolic-ref', '-q',
		'HEAD'])
	assert symbolic.exit_code == 1
	status := os.exec(['git', '--no-replace-objects', '-C', source_root, 'status', '--porcelain=v1',
		'--untracked-files=all', '--ignored=matching'])
	assert status.exit_code == 0
	assert status.output == ''
	$if !windows {
		staged_stat := os.lstat(os.join_path(payload_root, 'tcc.exe')) or { panic(err) }
		source_stat := os.lstat(os.join_path(source_root, 'tcc.exe')) or { panic(err) }
		assert staged_stat.dev != source_stat.dev || staged_stat.inode != source_stat.inode
		assert staged_stat.nlink == 1
	}
}

fn test_candidate_composition_is_atomic_direct_child_and_ignores_raw_extras() {
	fixture := prepare_complete_candidate('candidate-compose-monthly', false, '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	final_manifest_path := os.join_path(fixture.base, 'final-manifest.json')
	refreshed_authority := t2a_authority_with_refreshed_producer(fixture.authority, '6'.repeat(64))
	assert refreshed_authority.profile_id == fixture.authority.profile_id
	assert refreshed_authority.profile_sha256 == fixture.authority.profile_sha256
	assert refreshed_authority.producer_sha256 != fixture.authority.producer_sha256
	final_manifest_source := t2a_rebind_manifest_toolchain(fixture.manifest_source,
		fixture.authority, refreshed_authority).replace_once('"v_source_sha": "${'b'.repeat(40)}"',
		'"v_source_sha": "${'f'.repeat(40)}"')
	assert final_manifest_source != fixture.manifest_source
	os.write_file(final_manifest_path, final_manifest_source) or { panic(err) }
	os.write_file(os.join_path(fixture.staging_root, 'raw-extra-is-ignored.bin'), 'ignored\n') or {
		panic(err)
	}
	result_root := os.join_path(fixture.base, 'composition-result')
	result := bin.compose_candidate_for_execution(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .monthly
		base_repo_root: fixture.source_repo
		base_sha:       fixture.contract.source_git_ref
		raw_root:       fixture.staging_root
		manifest_path:  final_manifest_path
		result_root:    result_root
	}, runtime_contract_binding(false)) or { panic(err) }
	assert result.target_id == 'linux-amd64'
	assert result.kind == .monthly
	assert result.base_sha == fixture.contract.source_git_ref
	assert result.candidate_sha.len == 40
	assert result.tree.len == 40
	assert result.decision.eligible
	assert !result.decision.publish_allowed
	mut result_entries := os.ls(result_root) or { panic(err) }
	result_entries.sort()
	assert result_entries == ['candidate-repository']
	repository := os.join_path(result_root, 'candidate-repository')
	head := os.exec(['git', '--no-replace-objects', '-C', repository, 'rev-parse', 'HEAD'])
	parent := os.exec(['git', '--no-replace-objects', '-C', repository, 'rev-parse', 'HEAD^'])
	tree := os.exec(['git', '--no-replace-objects', '-C', repository, 'rev-parse', 'HEAD^{tree}'])
	assert head.exit_code == 0 && head.output.trim_space() == result.candidate_sha
	assert parent.exit_code == 0
	assert parent.output.trim_space() == fixture.contract.source_git_ref
	assert tree.exit_code == 0 && tree.output.trim_space() == result.tree
	symbolic := os.exec(['git', '--no-replace-objects', '-C', repository, 'symbolic-ref', '-q',
		'HEAD'])
	assert symbolic.exit_code == 1
	status := os.exec(['git', '--no-replace-objects', '-C', repository, 'status', '--porcelain=v1',
		'--untracked-files=all', '--ignored=matching'])
	assert status.exit_code == 0
	assert status.output == ''
	assert (os.read_file(os.join_path(repository, 'automation', 'bundle-manifest.json')) or {
		panic(err)
	}) == final_manifest_source
	assert !os.exists(os.join_path(repository, 'raw-extra-is-ignored.bin'))
	second_result_root := os.join_path(fixture.base, 'composition-result-second')
	second := bin.compose_candidate_for_execution(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .monthly
		base_repo_root: fixture.source_repo
		base_sha:       fixture.contract.source_git_ref
		raw_root:       fixture.staging_root
		manifest_path:  final_manifest_path
		result_root:    second_result_root
	}, runtime_contract_binding(false)) or { panic(err) }
	assert second.candidate_sha == result.candidate_sha
	assert second.tree == result.tree
	base_epoch := os.exec(['git', '--no-replace-objects', '-C', fixture.source_repo, 'show', '-s',
		'--format=%ct', fixture.contract.source_git_ref])
	assert base_epoch.exit_code == 0
	commit_epoch := base_epoch.output.trim_space().i64() + 1
	raw_commit := os.exec(['git', '--no-replace-objects', '-C', repository, 'cat-file', 'commit',
		result.candidate_sha])
	assert raw_commit.exit_code == 0
	manifest_hash := sha256_bytes(final_manifest_source.bytes())
	expected_commit := 'tree ${result.tree}\nparent ${fixture.contract.source_git_ref}\nauthor vlang-bot <alexander+bot@vlang.io> ${commit_epoch} +0000\ncommitter vlang-bot <alexander+bot@vlang.io> ${commit_epoch} +0000\n\ntccbin: compose linux-amd64 ${manifest_hash}\n'
	assert raw_commit.output == expected_commit
}

fn test_monthly_composition_takes_inventory_and_outputs_from_raw_but_overlays_from_base() {
	fixture := prepare_complete_candidate('candidate-compose-raw-versus-base', false, '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	overlay_path := 'reviewed/base-overlay.h'
	base_overlay_bytes := 'reviewed immutable overlay\n'.bytes()
	raw_overlay_bytes := 'untrusted RAW overlay must be ignored\n'.bytes()
	overlay := bin.parse_strict_json('{"path":"${overlay_path}","kind":"file","git_mode":"100644","sha256":"${sha256_bytes(base_overlay_bytes)}","symlink_target":null,"provenance":{"status":"complete","repository":"TinyCC/tinycc","sha":"${'c'.repeat(40)}","source_path":"reviewed/base-overlay.h","license":"LGPL-2.1-or-later"},"role":"reviewed-overlay","opaque":false,"opaque_acceptance_id":null,"format":null,"object_type":null,"machine":null,"os_abi":null}') or {
		panic(err)
	}
	initial_manifest := bin.parse_strict_json(fixture.manifest_source) or { panic(err) }
	inventory := (initial_manifest.object_value('inventory') or { panic('inventory missing') }).array_value
	outputs := (initial_manifest.object_value('outputs') or { panic('outputs missing') }).array_value
	base_manifest_source := manifest_with_payload_collections(fixture.manifest_source, [
		overlay,
	], inventory, outputs)
	base_overlay_path := os.join_path(fixture.source_repo, overlay_path)
	os.mkdir_all(os.dir(base_overlay_path)) or { panic(err) }
	os.write_file_array(base_overlay_path, base_overlay_bytes) or { panic(err) }
	os.write_file(fixture.manifest_path, base_manifest_source) or { panic(err) }
	base_sha := commit_candidate_paths(fixture.source_repo, [
		'automation/bundle-manifest.json',
		overlay_path,
	], 'bind immutable base overlay for RAW discrimination')

	new_inventory_bytes := 'int candidate_source_from_raw;\n'.bytes()
	new_output_bytes := 'candidate executable bytes from RAW\n'.bytes()
	old_inventory_hash := sha256_bytes(fixture.inventory_bytes)
	old_output_hash := sha256_bytes(fixture.output_bytes)
	assert base_manifest_source.count(old_inventory_hash) == 1
	assert base_manifest_source.count(old_output_hash) == 1
	final_manifest_source := base_manifest_source.replace_once(old_inventory_hash,
		sha256_bytes(new_inventory_bytes)).replace_once(old_output_hash,
		sha256_bytes(new_output_bytes))
	final_manifest_path := os.join_path(fixture.base, 'raw-versus-base-manifest.json')
	os.write_file(final_manifest_path, final_manifest_source) or { panic(err) }
	os.write_file_array(os.join_path(fixture.staging_root, 'src', 'tcc.c'), new_inventory_bytes) or {
		panic(err)
	}
	os.write_file_array(os.join_path(fixture.staging_root, 'tcc.exe'), new_output_bytes) or {
		panic(err)
	}
	os.chmod(os.join_path(fixture.staging_root, 'tcc.exe'), 0o755) or { panic(err) }
	raw_overlay_path := os.join_path(fixture.staging_root, overlay_path)
	os.mkdir_all(os.dir(raw_overlay_path)) or { panic(err) }
	os.write_file_array(raw_overlay_path, raw_overlay_bytes) or { panic(err) }

	result_root := os.join_path(fixture.base, 'raw-versus-base-result')
	result := bin.compose_candidate_for_execution(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .monthly
		base_repo_root: fixture.source_repo
		base_sha:       base_sha
		raw_root:       fixture.staging_root
		manifest_path:  final_manifest_path
		result_root:    result_root
	}, runtime_contract_binding(false)) or { panic(err) }
	assert result.decision.eligible
	assert !result.decision.publish_allowed
	repository := os.join_path(result_root, 'candidate-repository')
	assert os.read_bytes(os.join_path(repository, 'src', 'tcc.c')) or { panic(err) } == new_inventory_bytes
	assert os.read_bytes(os.join_path(repository, 'tcc.exe')) or { panic(err) } == new_output_bytes
	assert os.read_bytes(os.join_path(repository, overlay_path)) or { panic(err) } == base_overlay_bytes

	base_inventory_mode, base_inventory_oid := candidate_git_entry_for_test(repository, base_sha,
		'src/tcc.c')
	candidate_inventory_mode, candidate_inventory_oid := candidate_git_entry_for_test(repository,
		result.candidate_sha, 'src/tcc.c')
	base_output_mode, base_output_oid := candidate_git_entry_for_test(repository, base_sha,
		'tcc.exe')
	candidate_output_mode, candidate_output_oid := candidate_git_entry_for_test(repository,
		result.candidate_sha, 'tcc.exe')
	base_overlay_mode, base_overlay_oid := candidate_git_entry_for_test(repository, base_sha,
		overlay_path)
	candidate_overlay_mode, candidate_overlay_oid := candidate_git_entry_for_test(repository,
		result.candidate_sha, overlay_path)
	assert base_inventory_mode == '100644' && candidate_inventory_mode == '100644'
	assert base_output_mode == '100755' && candidate_output_mode == '100755'
	assert base_inventory_oid != candidate_inventory_oid
	assert base_output_oid != candidate_output_oid
	assert candidate_inventory_oid == candidate_raw_oid_for_test(repository, os.join_path(fixture.staging_root,
		'src', 'tcc.c'))
	assert candidate_output_oid == candidate_raw_oid_for_test(repository, os.join_path(fixture.staging_root,
		'tcc.exe'))
	assert base_overlay_mode == '100644' && candidate_overlay_mode == '100644'
	assert candidate_overlay_oid == base_overlay_oid
	assert candidate_overlay_oid != candidate_raw_oid_for_test(repository, raw_overlay_path)
	status := os.exec(['git', '--no-replace-objects', '-C', repository, 'status', '--porcelain=v1',
		'--untracked-files=all', '--ignored=matching'])
	tree := os.exec(['git', '--no-replace-objects', '-C', repository, 'rev-parse', 'HEAD^{tree}'])
	assert status.exit_code == 0 && status.output == ''
	assert tree.exit_code == 0 && tree.output.trim_space() == result.tree
}

fn test_candidate_blob_materialization_uses_exact_crlf_bytes_despite_git_attributes() {
	$if !windows {
		base := os.join_path(os.temp_dir(), 'tccbin-no-filters-' + os.getpid().str())
		os.rmdir_all(base) or {}
		os.mkdir_all(base) or { panic(err) }
		defer {
			os.rmdir_all(base) or {}
		}
		for args in [
			['git', '-C', base, 'init', '-q'],
			['git', '-C', base, 'config', 'core.autocrlf', 'false'],
		] {
			result := os.exec(args)
			assert result.exit_code == 0, result.output
		}
		raw_source := 'first\r\nsecond\r\n'
		payload_path := os.join_path(base, 'payload.txt')
		os.write_file(os.join_path(base, '.gitattributes'), 'payload.txt text eol=lf\n') or {
			panic(err)
		}
		os.write_file(payload_path, raw_source) or { panic(err) }
		filtered := os.exec(['git', '--no-replace-objects', '-C', base, 'hash-object',
			'--path=payload.txt', '--', payload_path])
		raw := os.exec(['git', '--no-replace-objects', '-C', base, 'hash-object', '-w',
			'--no-filters', '--', payload_path])
		assert filtered.exit_code == 0 && raw.exit_code == 0
		assert filtered.output.trim_space() != raw.output.trim_space()
		materialized := os.exec(['git', '--no-replace-objects', '-C', base, 'cat-file', 'blob',
			raw.output.trim_space()])
		assert materialized.exit_code == 0
		assert materialized.output == raw_source
	}
}

fn test_candidate_composition_materializes_the_declared_symlink_blob_without_following_it() {
	$if !windows {
		fixture := prepare_symlink_candidate('candidate-compose-symlink', 'lib/libgc.la',
			'../libgc.la')
		defer {
			os.rmdir_all(fixture.base) or {}
		}
		final_manifest_path := os.join_path(fixture.base, 'final-symlink-manifest.json')
		final_manifest_source := fixture.manifest_source.replace_once('"v_source_sha": "${'b'.repeat(40)}"',
			'"v_source_sha": "${'f'.repeat(40)}"')
		os.write_file(final_manifest_path, final_manifest_source) or { panic(err) }
		result_root := os.join_path(fixture.base, 'symlink-composition-result')
		result := bin.compose_candidate_for_execution(fixture.automation_root, bin.CandidateCompositionRequest{
			target_id:      'macos-amd64'
			kind:           .monthly
			base_repo_root: fixture.source_repo
			base_sha:       fixture.contract.source_git_ref
			raw_root:       fixture.staging_root
			manifest_path:  final_manifest_path
			result_root:    result_root
		}, runtime_contract_binding(false)) or { panic(err) }
		assert result.decision.eligible
		link := os.join_path(result_root, 'candidate-repository', 'lib', 'libgc.la')
		assert os.is_link(link)
		assert os.readlink(link) or { panic(err) } == '../libgc.la'
		assert !os.exists(os.join_path(result_root, 'candidate-repository', 'libgc.la'))
	}
}

fn test_candidate_manifest_attestation_rejects_symlinks_without_touching_external_sentinels() {
	$if !windows {
		monthly := prepare_complete_candidate('candidate-manifest-monthly-symlink', false, '')
		defer {
			os.rmdir_all(monthly.base) or {}
		}
		monthly_sentinel := os.join_path(monthly.base, 'monthly-sentinel.json')
		monthly_sentinel_source := 'monthly sentinel must remain byte-identical\n'
		os.write_file(monthly_sentinel, monthly_sentinel_source) or { panic(err) }
		monthly_base_sha := commit_manifest_symlink(monthly.source_repo, monthly_sentinel,
			'monthly manifest symlink sentinel')
		monthly_external := os.join_path(monthly.base, 'monthly-final-manifest.json')
		os.write_file(monthly_external, monthly.manifest_source) or { panic(err) }
		monthly_result := os.join_path(monthly.base, 'monthly-symlink-result')
		monthly_message := candidate_composition_error_at(monthly.automation_root, bin.CandidateCompositionRequest{
			target_id:      'linux-amd64'
			kind:           .monthly
			base_repo_root: monthly.source_repo
			base_sha:       monthly_base_sha
			raw_root:       monthly.staging_root
			manifest_path:  monthly_external
			result_root:    monthly_result
		}, runtime_contract_binding(false))
		assert monthly_message == 'candidate manifest must be an exact physical 100644 Git blob before parsing'
		assert (os.read_file(monthly_sentinel) or { panic(err) }) == monthly_sentinel_source
		assert !os.exists(monthly_result)
		assert_no_candidate_composition_scratch(monthly.base)

		legacy := prepare_legacy_composition_fixture('manifest-legacy-symlink', '')
		defer {
			os.rmdir_all(legacy.base) or {}
		}
		legacy_sentinel := os.join_path(legacy.base, 'legacy-sentinel.json')
		legacy_sentinel_source := 'legacy sentinel must remain byte-identical\n'
		os.write_file(legacy_sentinel, legacy_sentinel_source) or { panic(err) }
		legacy_base_sha := commit_manifest_symlink(legacy.base_repo_root,
			'../../legacy-sentinel.json', 'legacy manifest traversal symlink sentinel')
		update_legacy_fixture_base_sha(legacy, legacy.base_sha, legacy_base_sha)
		legacy_message := candidate_composition_error_at(legacy.automation_root, bin.CandidateCompositionRequest{
			target_id:      'linux-amd64'
			kind:           .legacy_onboard
			base_repo_root: legacy.base_repo_root
			base_sha:       legacy_base_sha
			raw_root:       legacy.raw_root
			manifest_path:  legacy.manifest_path
			result_root:    legacy.result_root
		}, runtime_contract_binding(false))
		assert legacy_message == 'legacy onboarding base manifest must be absent from Git and checkout'
		assert (os.read_file(legacy_sentinel) or { panic(err) }) == legacy_sentinel_source
		assert !os.exists(legacy.result_root)
		assert_no_candidate_composition_scratch(legacy.base)

		legacy_candidate :=
			prepare_legacy_composition_fixture('manifest-legacy-candidate-symlink', '')
		defer {
			os.rmdir_all(legacy_candidate.base) or {}
		}
		legacy_candidate_sentinel := os.join_path(legacy_candidate.base,
			'legacy-candidate-sentinel.json')
		legacy_candidate_sentinel_source := 'legacy candidate sentinel must remain byte-identical\n'
		os.write_file(legacy_candidate_sentinel, legacy_candidate_sentinel_source) or { panic(err) }
		legacy_candidate_sha := commit_manifest_symlink(legacy_candidate.base_repo_root,
			legacy_candidate_sentinel, 'add legacy candidate manifest symlink sentinel')
		legacy_candidate_work := os.join_path(legacy_candidate.base, 'legacy-candidate-work')
		mut legacy_candidate_message := ''
		bin.evaluate_candidate_manifest_for_execution(legacy_candidate.automation_root,
			'linux-amd64', .legacy_onboard, legacy_candidate.base_repo_root,
			legacy_candidate.base_sha, legacy_candidate_sha, legacy_candidate_work,
			runtime_contract_binding(false), false) or { legacy_candidate_message = err.msg() }
		assert legacy_candidate_message == 'candidate manifest must be an exact physical 100644 Git blob before parsing'
		assert (os.read_file(legacy_candidate_sentinel) or { panic(err) }) == legacy_candidate_sentinel_source
		assert !os.exists(legacy_candidate_work)

		preflight := prepare_complete_candidate('candidate-manifest-preflight-symlink', false, '')
		defer {
			os.rmdir_all(preflight.base) or {}
		}
		preflight_sentinel := os.join_path(preflight.base, 'preflight-sentinel.json')
		preflight_sentinel_source := 'preflight sentinel must remain byte-identical\n'
		os.write_file(preflight_sentinel, preflight_sentinel_source) or { panic(err) }
		candidate_sha := commit_manifest_symlink(preflight.source_repo, preflight_sentinel,
			'candidate manifest symlink sentinel')
		preflight_work := os.join_path(preflight.base, 'preflight-symlink-work')
		preflight_message := candidate_execution_error(preflight,
			preflight.contract.source_git_ref, candidate_sha, preflight_work,
			runtime_contract_binding(false), false)
		assert preflight_message == 'candidate manifest must be an exact physical 100644 Git blob before parsing'
		assert (os.read_file(preflight_sentinel) or { panic(err) }) == preflight_sentinel_source
		assert !os.exists(preflight_work)
	}
}

fn test_monthly_composition_rejects_a_payload_path_replacement_before_materialization() {
	fixture := prepare_complete_candidate('candidate-compose-replacement', false, '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	final_manifest_source := fixture.manifest_source.replace_once('"path": "src/tcc.c"',
		'"path": "src/tcc.c/source.c"')
	final_manifest_path := os.join_path(fixture.base, 'replacement-manifest.json')
	os.write_file(final_manifest_path, final_manifest_source) or { panic(err) }
	old_raw_path := os.join_path(fixture.staging_root, 'src', 'tcc.c')
	os.rm(old_raw_path) or { panic(err) }
	new_raw_path := os.join_path(fixture.staging_root, 'src', 'tcc.c', 'source.c')
	os.mkdir_all(os.dir(new_raw_path)) or { panic(err) }
	os.write_file_array(new_raw_path, fixture.inventory_bytes) or { panic(err) }
	result_root := os.join_path(fixture.base, 'replacement-composition-result')
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .monthly
		base_repo_root: fixture.source_repo
		base_sha:       fixture.contract.source_git_ref
		raw_root:       fixture.staging_root
		manifest_path:  final_manifest_path
		result_root:    result_root
	}, runtime_contract_binding(false))
	assert message == 'candidate immutable payload policy differs from its base'
	assert !os.exists(result_root)
}

fn test_monthly_transition_rejects_output_inventory_collection_swaps() {
	fixture := prepare_complete_candidate('candidate-collection-swap', false, '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	root := bin.parse_strict_json(fixture.manifest_source) or { panic(err) }
	overlays := (root.object_value('overlays') or { panic('overlays missing') }).array_value
	inventory := (root.object_value('inventory') or { panic('inventory missing') }).array_value
	outputs := (root.object_value('outputs') or { panic('outputs missing') }).array_value
	assert inventory.len == 1 && outputs.len == 1
	swapped_source := manifest_with_payload_collections(fixture.manifest_source, overlays, outputs,
		inventory)
	os.write_file(fixture.manifest_path, swapped_source) or { panic(err) }
	candidate_sha := commit_candidate_paths(fixture.source_repo, [
		'automation/bundle-manifest.json',
	], 'swap inventory and output collections')
	work_root := os.join_path(fixture.base, 'collection-swap-work')
	assert candidate_execution_error(fixture, fixture.contract.source_git_ref, candidate_sha,
		work_root, runtime_contract_binding(false), false) == 'candidate immutable payload policy differs from its base'
	assert !os.exists(work_root)
}

fn test_monthly_composition_rejects_overlay_to_inventory_even_with_different_raw_bytes() {
	fixture := prepare_complete_candidate('candidate-overlay-collection', false, '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	secondary_path := 'src/secondary.c'
	secondary_bytes := 'int secondary_source;\n'.bytes()
	with_secondary := manifest_with_extra_candidate_inventory(fixture.manifest_source,
		secondary_path, secondary_bytes)
	with_secondary_root := bin.parse_strict_json(with_secondary) or { panic(err) }
	inventory := (with_secondary_root.object_value('inventory') or { panic('inventory missing') }).array_value
	outputs := (with_secondary_root.object_value('outputs') or { panic('outputs missing') }).array_value
	assert inventory.len == 2 && outputs.len == 1
	base_source := manifest_with_payload_collections(with_secondary, [inventory[0]], [
		inventory[1],
	], outputs)
	for root in [fixture.source_repo, fixture.staging_root] {
		secondary_full_path := os.join_path(root, secondary_path)
		os.mkdir_all(os.dir(secondary_full_path)) or { panic(err) }
		os.write_file_array(secondary_full_path, secondary_bytes) or { panic(err) }
	}
	os.write_file(fixture.manifest_path, base_source) or { panic(err) }
	base_sha := commit_candidate_paths(fixture.source_repo, ['.'],
		'bind source as an immutable base overlay')
	different_raw_bytes := 'int reviewed_overlay_replacement;\n'.bytes()
	old_inventory_hash := sha256_bytes(fixture.inventory_bytes)
	moved_source := bin.canonical_json(inventory[0]).replace_once(old_inventory_hash,
		sha256_bytes(different_raw_bytes))
	moved_inventory := bin.parse_strict_json(moved_source) or { panic(err) }
	final_source := manifest_with_payload_collections(base_source, [], [
		inventory[1],
		moved_inventory,
	], outputs)
	final_manifest_path := os.join_path(fixture.base, 'overlay-to-inventory-manifest.json')
	os.write_file(final_manifest_path, final_source) or { panic(err) }
	os.write_file_array(os.join_path(fixture.staging_root, 'src', 'tcc.c'), different_raw_bytes) or {
		panic(err)
	}
	result_root := os.join_path(fixture.base, 'overlay-to-inventory-result')
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .monthly
		base_repo_root: fixture.source_repo
		base_sha:       base_sha
		raw_root:       fixture.staging_root
		manifest_path:  final_manifest_path
		result_root:    result_root
	}, runtime_contract_binding(false))
	assert message == 'candidate immutable payload policy differs from its base'
	assert !os.exists(result_root)
	assert_no_candidate_composition_scratch(fixture.base)
	exact_layout_source := manifest_with_payload_collections(base_source, [
		moved_inventory,
	], [inventory[1]], outputs)
	os.write_file_array(os.join_path(fixture.source_repo, 'src', 'tcc.c'), different_raw_bytes) or {
		panic(err)
	}
	os.write_file(fixture.manifest_path, exact_layout_source) or { panic(err) }
	candidate_sha := commit_candidate_paths(fixture.source_repo, [
		'automation/bundle-manifest.json',
		'src/tcc.c',
	], 'mutate immutable overlay bytes')
	work_root := os.join_path(fixture.base, 'overlay-bytes-work')
	assert candidate_execution_error(fixture, base_sha, candidate_sha, work_root,
		runtime_contract_binding(false), false) == 'candidate overlay bytes must remain exact from its base'
	assert !os.exists(work_root)
}

fn test_legacy_composition_without_a_reviewed_policy_fails_before_any_input_is_observed() {
	result_root := os.join_path(os.temp_dir(), 'tccbin-unreviewed-result-${os.getpid()}')
	os.rmdir_all(result_root) or {}
	message := candidate_composition_error(bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .legacy_onboard
		base_repo_root: '/absent/base-repository'
		base_sha:       '0'.repeat(40)
		raw_root:       '/absent/raw-root'
		manifest_path:  '/absent/manifest.json'
		result_root:    result_root
	}, runtime_contract_binding(false))
	assert message == 'target has no reviewed legacy onboarding policy'
	assert !os.exists(result_root)
}

fn test_reviewed_legacy_composition_exposes_only_a_preflighted_direct_child_capsule() {
	for unresolved in ['', 'source'] {
		fixture := prepare_legacy_composition_fixture(if unresolved == '' {
			'complete'
		} else {
			'incomplete'
		}, unresolved)
		defer {
			os.rmdir_all(fixture.base) or {}
		}
		extra_path := os.join_path(fixture.raw_root, 'RAW-extra-is-not-selected.txt')
		os.write_file(extra_path, 'ignored RAW extra\n') or { panic(err) }
		result := bin.compose_candidate_for_execution(fixture.automation_root, bin.CandidateCompositionRequest{
			target_id:      'linux-amd64'
			kind:           .legacy_onboard
			base_repo_root: fixture.base_repo_root
			base_sha:       fixture.base_sha
			raw_root:       fixture.raw_root
			manifest_path:  fixture.manifest_path
			result_root:    fixture.result_root
		}, runtime_contract_binding(false)) or { panic(err) }
		assert result.kind == .legacy_onboard
		assert result.base_sha == fixture.base_sha
		assert !result.decision.publish_allowed
		if unresolved == '' {
			assert result.decision.eligible
			assert result.decision.reason == 'authenticated_staging'
		} else {
			assert !result.decision.eligible
			assert result.decision.reason == 'staged_provenance_incomplete'
		}
		mut exposed := os.ls(fixture.result_root) or { panic(err) }
		exposed.sort()
		assert exposed == ['candidate-repository']
		repository := os.join_path(fixture.result_root, 'candidate-repository')
		assert !os.exists(os.join_path(repository, 'RAW-extra-is-not-selected.txt'))
		assert (os.read_file(os.join_path(repository, 'automation', 'bundle-manifest.json')) or {
			panic(err)
		}) == fixture.manifest_source
		head := os.exec(['git', '--no-replace-objects', '-C', repository, 'rev-parse', 'HEAD'])
		parent := os.exec(['git', '--no-replace-objects', '-C', repository, 'rev-parse', 'HEAD^'])
		symbolic := os.exec(['git', '--no-replace-objects', '-C', repository, 'symbolic-ref', '-q',
			'HEAD'])
		status := os.exec(['git', '--no-replace-objects', '-C', repository, 'status',
			'--porcelain=v1', '--untracked-files=all', '--ignored=matching'])
		diff := os.exec(['git', '--no-replace-objects', '-C', repository, 'diff-tree',
			'--no-commit-id', '--name-status', '-r', '--no-renames', fixture.base_sha, result.candidate_sha,
			'--'])
		assert head.exit_code == 0 && head.output.trim_space() == result.candidate_sha
		assert parent.exit_code == 0 && parent.output.trim_space() == fixture.base_sha
		assert symbolic.exit_code == 1
		assert status.exit_code == 0 && status.output == ''
		assert diff.exit_code == 0
		assert diff.output == 'A\tautomation/bundle-manifest.json\n'
	}
}

fn test_reviewed_legacy_payload_may_use_nested_recipe_and_patch_like_names() {
	fixture := prepare_legacy_composition_fixture('payload-names', '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	manifest_source := fixture.manifest_source.replace_once('"path": "src/tcc.c"',
		'"path": "nested/build.sh"').replace_once('"path": "tcc.exe"',
		'"path": "payload/fix.patch"')
	assert manifest_source != fixture.manifest_source
	for root in [fixture.base_repo_root, fixture.raw_root] {
		for old_path, new_path in {
			'src/tcc.c': 'nested/build.sh'
			'tcc.exe':   'payload/fix.patch'
		} {
			new_full_path := os.join_path(root, new_path)
			os.mkdir_all(os.dir(new_full_path)) or { panic(err) }
			os.mv(os.join_path(root, old_path), new_full_path) or { panic(err) }
		}
	}
	new_base_sha := commit_candidate_paths(fixture.base_repo_root, ['.'],
		'review payload paths with control-like basenames')
	replace_legacy_composition_policy(fixture, manifest_source)
	update_legacy_fixture_base_sha(fixture, fixture.base_sha, new_base_sha)
	result := bin.compose_candidate_for_execution(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .legacy_onboard
		base_repo_root: fixture.base_repo_root
		base_sha:       new_base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, runtime_contract_binding(false)) or { panic(err) }
	assert result.decision.eligible
	repository := os.join_path(fixture.result_root, 'candidate-repository')
	assert os.is_file(os.join_path(repository, 'nested', 'build.sh'))
	assert os.is_file(os.join_path(repository, 'payload', 'fix.patch'))
}

fn test_legacy_composition_rejects_policy_drift_without_exposing_a_partial_result() {
	fixture := prepare_legacy_composition_fixture('policy-drift', '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	drifted := fixture.manifest_source.replace_once('"version": 1', '"version": 2')
	assert drifted != fixture.manifest_source
	os.write_file(fixture.manifest_path, drifted) or { panic(err) }
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .legacy_onboard
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, runtime_contract_binding(false))
	assert message == 'candidate manifest differs from the reviewed legacy onboarding policy'
	assert !os.exists(fixture.result_root)
}

fn test_incomplete_legacy_composition_still_requires_the_exact_runtime_contract_binding() {
	fixture := prepare_legacy_composition_fixture('incomplete-binding', 'source')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .legacy_onboard
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, runtime_contract_binding(true))
	assert message == 'runtime contract binding differs from the authenticated manifest'
	assert !os.exists(fixture.result_root)
}

fn test_legacy_composition_cannot_drop_a_base_payload_by_omitting_its_declaration() {
	fixture := prepare_legacy_composition_fixture('no-delete', '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	mutated := fixture.manifest_source.replace_once('"path": "src/tcc.c"',
		'"path": "src/replacement.c"')
	assert mutated != fixture.manifest_source
	os.mkdir_all(os.join_path(fixture.raw_root, 'src')) or { panic(err) }
	os.write_file_array(os.join_path(fixture.raw_root, 'src', 'replacement.c'),
		'int candidate_source;\n'.bytes()) or { panic(err) }
	replace_legacy_composition_policy(fixture, mutated)
	message := candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      'linux-amd64'
		kind:           .legacy_onboard
		base_repo_root: fixture.base_repo_root
		base_sha:       fixture.base_sha
		raw_root:       fixture.raw_root
		manifest_path:  fixture.manifest_path
		result_root:    fixture.result_root
	}, runtime_contract_binding(false))
	assert message == 'legacy onboarding base path is outside the reviewed control and payload closure'
	assert !os.exists(fixture.result_root)
}

fn test_candidate_preflight_exports_a_real_macos_symlink_without_following_it() {
	$if !windows {
		fixture := advance_candidate_fixture(prepare_symlink_candidate('candidate-symlink-export',
			'lib/libgc.la', '../libgc.la'), 'symlink candidate refresh')
		defer {
			os.rmdir_all(fixture.base) or {}
		}
		work_root := os.join_path(fixture.base, 'symlink-work')
		decision := bin.evaluate_candidate_manifest_for_execution(fixture.automation_root,
			fixture.target_id, .monthly, fixture.source_repo, fixture.parent_ref,
			fixture.contract.source_git_ref, work_root, runtime_contract_binding(false), false) or {
			panic(err)
		}
		assert decision.eligible
		assert !decision.publish_allowed
		exported := os.join_path(work_root, 'payload', 'lib', 'libgc.la')
		assert os.is_link(exported)
		assert os.readlink(exported) or { panic(err) } == '../libgc.la'
		assert !os.exists(os.join_path(work_root, 'payload', 'libgc.la'))
	}
}

fn test_candidate_preflight_rejects_runtime_mismatch_ancestry_and_open_transitions() {
	binding_fixture := advance_candidate_fixture(prepare_complete_candidate('candidate-preflight-binding',
		false, ''), 'binding candidate refresh')
	defer {
		os.rmdir_all(binding_fixture.base) or {}
	}
	binding_work := os.join_path(binding_fixture.base, 'binding-work')
	assert candidate_execution_error(binding_fixture, binding_fixture.parent_ref,
		binding_fixture.contract.source_git_ref, binding_work, runtime_contract_binding(true),
		false) == 'runtime contract binding differs from the authenticated manifest'
	assert !os.exists(binding_work)

	workflow_fixture := advance_candidate_fixture(prepare_complete_candidate('candidate-preflight-workflow',
		false, ''), 'workflow candidate refresh')
	defer {
		os.rmdir_all(workflow_fixture.base) or {}
	}
	workflow_sha := commit_candidate_fixture_mutation(workflow_fixture,
		'.github/workflows/unreviewed.yml', 'name: unreviewed\n', 'unreviewed workflow')
	workflow_work := os.join_path(workflow_fixture.base, 'workflow-work')
	assert candidate_execution_error(workflow_fixture, workflow_fixture.contract.source_git_ref,
		workflow_sha, workflow_work, runtime_contract_binding(false), false) == 'candidate transition must not modify workflow controls'
	assert !os.exists(workflow_work)
	ancestry_work := os.join_path(workflow_fixture.base, 'ancestry-work')
	assert candidate_execution_error(workflow_fixture, workflow_fixture.parent_ref, workflow_sha,
		ancestry_work, runtime_contract_binding(false), false) == 'candidate commit must have the exact base as its sole parent'
	assert !os.exists(ancestry_work)

	automation_fixture := advance_candidate_fixture(prepare_complete_candidate('candidate-preflight-automation',
		false, ''), 'automation candidate refresh')
	defer {
		os.rmdir_all(automation_fixture.base) or {}
	}
	automation_sha := commit_candidate_fixture_mutation(automation_fixture,
		'automation/unreviewed.json', '{}\n', 'unreviewed automation')
	automation_work := os.join_path(automation_fixture.base, 'automation-work')
	assert candidate_execution_error(automation_fixture,
		automation_fixture.contract.source_git_ref, automation_sha, automation_work,
		runtime_contract_binding(false), false) == 'candidate transition may modify only the authoritative automation manifest'
	assert !os.exists(automation_work)
}

fn test_candidate_preflight_rechecks_target_branch_and_declared_controls() {
	transform_fixture := advance_candidate_fixture(prepare_complete_candidate('candidate-preflight-transform',
		false, ''), 'transform candidate refresh')
	defer {
		os.rmdir_all(transform_fixture.base) or {}
	}
	transform_sha := commit_candidate_fixture_mutation(transform_fixture,
		'unreviewed-transform.patch', 'unreviewed transform\n', 'unreviewed transform')
	transform_work := os.join_path(transform_fixture.base, 'transform-work')
	assert candidate_execution_error(transform_fixture, transform_fixture.contract.source_git_ref,
		transform_sha, transform_work, runtime_contract_binding(false), false) == 'candidate transition contains a path outside its manifest closure'
	assert !os.exists(transform_work)

	branch_fixture := advance_candidate_fixture(prepare_complete_candidate('candidate-preflight-branch',
		false, ''), 'branch candidate refresh')
	defer {
		os.rmdir_all(branch_fixture.base) or {}
	}
	branch_source := os.read_file(branch_fixture.manifest_path) or { panic(err) }
	os.write_file(branch_fixture.manifest_path, branch_source.replace_once('"branch": "thirdparty-linux-amd64"',
		'"branch": "thirdparty-openbsd-amd64"')) or { panic(err) }
	for args in [
		['git', '-C', branch_fixture.source_repo, 'add', '--', 'automation/bundle-manifest.json'],
		['git', '-C', branch_fixture.source_repo, 'commit', '-qm', 'branch drift'],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	ref_result := os.exec(['git', '-C', branch_fixture.source_repo, 'rev-parse', 'HEAD'])
	assert ref_result.exit_code == 0
	branch_sha := ref_result.output.trim_space()
	branch_work := os.join_path(branch_fixture.base, 'branch-work')
	assert candidate_execution_error(branch_fixture, branch_fixture.contract.source_git_ref,
		branch_sha, branch_work, runtime_contract_binding(false), false) == 'candidate manifest schema or semantics failed with 2 issue(s)'
	assert !os.exists(branch_work)
}

fn test_candidate_transition_binds_the_real_windows_transform_blob_to_its_base() {
	fixture, base_sha, candidate_sha :=
		prepare_windows_immutable_transform_candidate('immutable-transform')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	work_root := os.join_path(fixture.base, 'immutable-transform-work')
	assert candidate_execution_error(fixture, base_sha, candidate_sha, work_root,
		runtime_contract_binding(false), false) == 'candidate patch or transform blob differs from its base'
	assert !os.exists(work_root)
}

fn test_candidate_preflight_rejects_stale_base_controls_and_git_storage_redirects() {
	stale_raw := prepare_complete_candidate('candidate-stale-base', false, '')
	current_recipe_sha := sha256_bytes(os.read_bytes(os.join_path(stale_raw.source_repo, 'build.sh')) or {
		panic(err)
	})
	stale_source := os.read_file(stale_raw.manifest_path) or { panic(err) }
	assert stale_source.count(current_recipe_sha) == 1
	os.write_file(stale_raw.manifest_path, stale_source.replace_once(current_recipe_sha,
		'5'.repeat(64))) or { panic(err) }
	stale_contract := committed_contract_for(stale_raw, 'stale base recipe declaration')
	stale_base := CompleteCandidateFixture{
		...stale_raw
		manifest_source: os.read_file(stale_raw.manifest_path) or { panic(err) }
		parent_ref:      stale_raw.contract.source_git_ref
		contract:        stale_contract
	}
	stale_fixture := advance_candidate_fixture(stale_base, 'stale base final candidate')
	defer {
		os.rmdir_all(stale_fixture.base) or {}
	}
	stale_work := os.join_path(stale_fixture.base, 'stale-work')
	assert candidate_execution_error(stale_fixture, stale_fixture.parent_ref,
		stale_fixture.contract.source_git_ref, stale_work, runtime_contract_binding(false), false) == 'base manifest controls differ from their immutable Git blobs'
	assert !os.exists(stale_work)

	redirect_fixture := advance_candidate_fixture(prepare_complete_candidate('candidate-alternate',
		false, ''), 'redirect candidate base')
	defer {
		os.rmdir_all(redirect_fixture.base) or {}
	}
	alternate_objects := os.join_path(redirect_fixture.base, 'alternate-objects')
	os.mkdir_all(os.join_path(alternate_objects, 'info')) or { panic(err) }
	os.mkdir_all(os.join_path(alternate_objects, 'pack')) or { panic(err) }
	alternates_path := os.join_path(redirect_fixture.source_repo, '.git', 'objects', 'info',
		'alternates')
	os.write_file(alternates_path, '${alternate_objects}\n') or { panic(err) }
	redirect_work := os.join_path(redirect_fixture.base, 'redirect-work')
	assert candidate_execution_error(redirect_fixture, redirect_fixture.parent_ref,
		redirect_fixture.contract.source_git_ref, redirect_work, runtime_contract_binding(false),
		false) == 'candidate repository contains a graft or object alternate'
	assert !os.exists(redirect_work)
}

fn test_candidate_transition_rejects_payload_layout_drift() {
	fixture := advance_candidate_fixture(prepare_complete_candidate('candidate-preflight-replace',
		false, ''), 'replacement candidate base')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	mut manifest_source := os.read_file(fixture.manifest_path) or { panic(err) }
	manifest_source = manifest_source.replace_once('"path": "src/tcc.c"',
		'"path": "src/tcc-renamed.c"')
	assert manifest_source.contains('"path": "src/tcc-renamed.c"')
	os.mv(os.join_path(fixture.source_repo, 'src', 'tcc.c'), os.join_path(fixture.source_repo,
		'src', 'tcc-renamed.c')) or { panic(err) }
	os.write_file(fixture.manifest_path, manifest_source) or { panic(err) }
	for args in [
		['git', '-C', fixture.source_repo, 'add', '--all'],
		['git', '-C', fixture.source_repo, 'commit', '-qm', 'replace old payload and recipe'],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	ref_result := os.exec(['git', '-C', fixture.source_repo, 'rev-parse', 'HEAD'])
	assert ref_result.exit_code == 0, ref_result.output
	work_root := os.join_path(fixture.base, 'replacement-work')
	assert candidate_execution_error(fixture, fixture.contract.source_git_ref,
		ref_result.output.trim_space(), work_root, runtime_contract_binding(false), false) == 'candidate immutable payload policy differs from its base'
	assert !os.exists(work_root)
}

fn test_candidate_transition_rejects_recipe_refresh_in_the_monthly_composer() {
	fixture := prepare_complete_candidate('candidate-recipe-refresh', false, '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	old_recipe := os.read_bytes(os.join_path(fixture.source_repo, 'build.sh')) or { panic(err) }
	new_recipe := 'set -eu\nprintf reviewed-recipe-refresh\n'.bytes()
	manifest_source := os.read_file(fixture.manifest_path) or { panic(err) }
	refreshed_source := manifest_source.replace_once('"version": 1,\n    "sha256": "' +
		sha256_bytes(old_recipe) + '"', '"version": 2,\n    "sha256": "' +
		sha256_bytes(new_recipe) + '"')
	assert refreshed_source != manifest_source
	os.write_file_array(os.join_path(fixture.source_repo, 'build.sh'), new_recipe) or { panic(err) }
	os.write_file(fixture.manifest_path, refreshed_source) or { panic(err) }
	candidate_sha := commit_candidate_paths(fixture.source_repo, [
		'automation/bundle-manifest.json',
		'build.sh',
	], 'refresh reviewed recipe with fixed payload policy')
	work_root := os.join_path(fixture.base, 'recipe-refresh-work')
	assert candidate_execution_error(fixture, fixture.contract.source_git_ref, candidate_sha,
		work_root, runtime_contract_binding(false), false) == 'candidate immutable policy projection differs from its base'
	assert !os.exists(work_root)

	blob_fixture := prepare_complete_candidate('candidate-recipe-blob-drift', false, '')
	defer {
		os.rmdir_all(blob_fixture.base) or {}
	}
	os.write_file(os.join_path(blob_fixture.source_repo, 'build.sh'),
		'set -eu\nprintf unreviewed-recipe-blob\n') or { panic(err) }
	$if !windows {
		os.chmod(os.join_path(blob_fixture.source_repo, 'build.sh'), 0o755) or { panic(err) }
	}
	base_manifest := os.read_file(blob_fixture.manifest_path) or { panic(err) }
	os.write_file(blob_fixture.manifest_path, '${base_manifest} \n') or { panic(err) }
	blob_candidate_sha := commit_candidate_paths(blob_fixture.source_repo, [
		'automation/bundle-manifest.json',
		'build.sh',
	], 'mutate immutable recipe blob and mode')
	blob_work_root := os.join_path(blob_fixture.base, 'recipe-blob-work')
	assert candidate_execution_error(blob_fixture, blob_fixture.contract.source_git_ref,
		blob_candidate_sha, blob_work_root, runtime_contract_binding(false), false) == 'candidate immutable recipe blob differs from its base'
	assert !os.exists(blob_work_root)
}

fn test_monthly_transition_never_promotes_an_unobserved_profile_to_a_producer_observation() {
	fixture := prepare_complete_candidate('candidate-toolchain-null-to-full', false, '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	producer_marker := '"producer_observation": ${fixture.authority.producer_source}'
	assert fixture.manifest_source.count(producer_marker) == 1
	bound_source := fixture.manifest_source.replace_once(producer_marker,
		'"producer_observation": null').replace_once('"provenance_status": "complete"',
		'"provenance_status": "incomplete"')
	os.write_file(fixture.manifest_path, bound_source) or { panic(err) }
	bound_sha := commit_candidate_paths(fixture.source_repo, [
		'automation/bundle-manifest.json',
	], 'bind reviewed profile without producer observation')
	os.write_file(fixture.manifest_path, fixture.manifest_source) or { panic(err) }
	candidate_sha := commit_candidate_paths(fixture.source_repo, [
		'automation/bundle-manifest.json',
	], 'attempt monthly producer observation activation')
	work_root := os.join_path(fixture.base, 'toolchain-null-to-full-work')
	assert candidate_execution_error(fixture, bound_sha, candidate_sha, work_root,
		runtime_contract_binding(false), false) == 'monthly candidate requires authenticated producer observations in both manifests'
	assert !os.exists(work_root)
}

fn test_monthly_transition_rejects_a_producer_profile_id_and_hash_migration_atomically() {
	fixture := prepare_complete_candidate('candidate-toolchain-profile-migration', false, '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	migrated := t2a_authority_with_migrated_profile(fixture.authority)
	assert migrated.profile_id != fixture.authority.profile_id
	assert migrated.profile_sha256 != fixture.authority.profile_sha256
	migrated_source := t2a_rebind_manifest_toolchain(fixture.manifest_source, fixture.authority,
		migrated)
	os.write_file(fixture.manifest_path, migrated_source) or { panic(err) }
	candidate_sha := commit_candidate_paths(fixture.source_repo, [
		'automation/bundle-manifest.json',
	], 'attempt monthly producer profile migration')
	work_root := os.join_path(fixture.base, 'profile-migration-work')
	assert candidate_execution_error(fixture, fixture.contract.source_git_ref, candidate_sha,
		work_root, runtime_contract_binding(false), false) == 'candidate manifest schema or semantics failed with 1 issue(s)'
	assert !os.exists(work_root)
	checkout_base := os.exec(['git', '-C', fixture.source_repo, 'checkout', '--detach', '-q',
		fixture.contract.source_git_ref])
	assert checkout_base.exit_code == 0, checkout_base.output

	external_manifest := os.join_path(fixture.base, 'profile-migration-manifest.json')
	os.write_file(external_manifest, migrated_source) or { panic(err) }
	result_root := os.join_path(fixture.base, 'profile-migration-result')
	assert candidate_composition_error_at(fixture.automation_root, bin.CandidateCompositionRequest{
		target_id:      fixture.target_id
		kind:           .monthly
		base_repo_root: fixture.source_repo
		base_sha:       fixture.contract.source_git_ref
		raw_root:       fixture.staging_root
		manifest_path:  external_manifest
		result_root:    result_root
	}, runtime_contract_binding(false)) == 'candidate composition manifest failed with 1 issue(s)'
	assert !os.exists(result_root)
	assert !(os.ls(fixture.base) or { panic(err) }).any(it.starts_with('.tccbin-compose-'))
}

fn test_candidate_transition_freezes_payload_membership_before_git_status_deltas() {
	stable_fixture := prepare_complete_candidate('candidate-stable-absorption', false, '')
	defer {
		os.rmdir_all(stable_fixture.base) or {}
	}
	stable_path := 'src/stable-extra.c'
	stable_bytes := 'int stable_extra;\n'.bytes()
	os.write_file_array(os.join_path(stable_fixture.source_repo, stable_path), stable_bytes) or {
		panic(err)
	}
	stable_base_sha := commit_candidate_paths(stable_fixture.source_repo, [
		stable_path,
	], 'add stable undeclared source')
	stable_manifest := manifest_with_extra_candidate_inventory(os.read_file(stable_fixture.manifest_path) or {
		panic(err)
	}, stable_path, stable_bytes)
	os.write_file(stable_fixture.manifest_path, stable_manifest) or { panic(err) }
	stable_candidate_sha := commit_candidate_paths(stable_fixture.source_repo, [
		'automation/bundle-manifest.json',
	], 'declare stable source without adding it')
	stable_work := os.join_path(stable_fixture.base, 'stable-absorption-work')
	assert candidate_execution_error(stable_fixture, stable_base_sha, stable_candidate_sha,
		stable_work, runtime_contract_binding(false), false) == 'candidate immutable payload policy differs from its base'
	assert !os.exists(stable_work)

	removal_fixture := prepare_complete_candidate('candidate-declaration-removal', false, '')
	defer {
		os.rmdir_all(removal_fixture.base) or {}
	}
	removal_path := 'src/removal-extra.c'
	removal_bytes := 'int removal_extra;\n'.bytes()
	removal_record := extra_candidate_inventory_record(removal_path, removal_bytes)
	removal_manifest := manifest_with_extra_candidate_inventory(os.read_file(removal_fixture.manifest_path) or {
		panic(err)
	}, removal_path, removal_bytes)
	os.write_file_array(os.join_path(removal_fixture.source_repo, removal_path), removal_bytes) or {
		panic(err)
	}
	os.write_file(removal_fixture.manifest_path, removal_manifest) or { panic(err) }
	removal_base_sha := commit_candidate_paths(removal_fixture.source_repo, [
		'automation/bundle-manifest.json',
		removal_path,
	], 'declare removable source')
	assert removal_manifest.count(removal_record) == 1
	os.write_file(removal_fixture.manifest_path, removal_manifest.replace_once(removal_record, '')) or {
		panic(err)
	}
	removal_candidate_sha := commit_candidate_paths(removal_fixture.source_repo, [
		'automation/bundle-manifest.json',
	], 'remove declaration without deleting source')
	removal_work := os.join_path(removal_fixture.base, 'declaration-removal-work')
	assert candidate_execution_error(removal_fixture, removal_base_sha, removal_candidate_sha,
		removal_work, runtime_contract_binding(false), false) == 'candidate immutable payload policy differs from its base'
	assert !os.exists(removal_work)

	shared_fixture := prepare_complete_candidate('candidate-shared-add', false, '')
	defer {
		os.rmdir_all(shared_fixture.base) or {}
	}
	shared_path := 'src/shared-extra.c'
	shared_bytes := 'int shared_extra;\n'.bytes()
	shared_manifest := manifest_with_extra_candidate_inventory(os.read_file(shared_fixture.manifest_path) or {
		panic(err)
	}, shared_path, shared_bytes)
	os.write_file(shared_fixture.manifest_path, shared_manifest) or { panic(err) }
	shared_base_sha := commit_candidate_paths(shared_fixture.source_repo, [
		'automation/bundle-manifest.json',
	], 'declare source absent from base')
	os.write_file_array(os.join_path(shared_fixture.source_repo, shared_path), shared_bytes) or {
		panic(err)
	}
	os.write_file(shared_fixture.manifest_path, '${shared_manifest} \n') or { panic(err) }
	shared_candidate_sha := commit_candidate_paths(shared_fixture.source_repo, [
		'automation/bundle-manifest.json',
		shared_path,
	], 'add source shared by both manifests')
	shared_work := os.join_path(shared_fixture.base, 'shared-add-work')
	assert candidate_execution_error(shared_fixture, shared_base_sha, shared_candidate_sha,
		shared_work, runtime_contract_binding(false), false) == 'candidate transition cannot add or delete a shared payload path'
	assert !os.exists(shared_work)
}

fn test_candidate_transition_keeps_the_monthly_contract_sha_immutable() {
	fixture := prepare_complete_candidate('candidate-contract-sha-drift', false, '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	manifest_source := os.read_file(fixture.manifest_path) or { panic(err) }
	assert manifest_source.count('"contract_sha": "${'a'.repeat(40)}"') == 1
	os.write_file(fixture.manifest_path, manifest_source.replace_once('"contract_sha": "${'a'.repeat(40)}"',
		'"contract_sha": "${'9'.repeat(40)}"')) or { panic(err) }
	candidate_sha := commit_candidate_paths(fixture.source_repo, [
		'automation/bundle-manifest.json',
	], 'drift monthly contract authority')
	work_root := os.join_path(fixture.base, 'contract-sha-work')
	candidate_runtime := bin.RuntimeContractBinding{
		repository: 'GGRei/v'
		sha:        '9'.repeat(40)
	}
	assert candidate_execution_error(fixture, fixture.contract.source_git_ref, candidate_sha,
		work_root, candidate_runtime, false) == 'candidate immutable policy projection differs from its base'
	assert !os.exists(work_root)
}

fn test_candidate_transition_rejects_policy_drift_case_only_rename_and_case_variant_git_env() {
	policy_fixture := advance_candidate_fixture(prepare_complete_candidate('candidate-policy-drift',
		false, ''), 'policy candidate base')
	defer {
		os.rmdir_all(policy_fixture.base) or {}
	}
	policy_source := os.read_file(policy_fixture.manifest_path) or { panic(err) }
	os.write_file(policy_fixture.manifest_path, policy_source.replace_once('strict schema and semantic validation',
		'strict schema and semantic validation v2')) or { panic(err) }
	for args in [
		['git', '-C', policy_fixture.source_repo, 'add', '--', 'automation/bundle-manifest.json'],
		['git', '-C', policy_fixture.source_repo, 'commit', '-qm', 'drift immutable policy'],
	] {
		result := os.exec(args)
		assert result.exit_code == 0, result.output
	}
	policy_ref := os.exec(['git', '-C', policy_fixture.source_repo, 'rev-parse', 'HEAD'])
	assert policy_ref.exit_code == 0, policy_ref.output
	policy_work := os.join_path(policy_fixture.base, 'policy-work')
	assert candidate_execution_error(policy_fixture, policy_fixture.contract.source_git_ref,
		policy_ref.output.trim_space(), policy_work, runtime_contract_binding(false), false) == 'candidate immutable policy projection differs from its base'
	assert !os.exists(policy_work)

	environment_fixture := advance_candidate_fixture(prepare_complete_candidate('candidate-env-case',
		false, ''), 'environment candidate base')
	defer {
		os.rmdir_all(environment_fixture.base) or {}
	}
	environment_work := os.join_path(environment_fixture.base, 'environment-work')
	assert candidate_case_variant_environment_error(environment_fixture, environment_work) == 'candidate Git environment contains a repository, object, or configuration redirection'
	assert !os.exists(environment_work)

	$if !windows {
		mac_base := prepare_symlink_candidate('candidate-case-only', 'lib/libgc.la', '../libgc.la')
		mac_fixture := advance_candidate_fixture(mac_base, 'macOS candidate base')
		defer {
			os.rmdir_all(mac_fixture.base) or {}
		}
		case_source := os.read_file(mac_fixture.manifest_path) or { panic(err) }
		assert case_source.count('"path": "tcc.exe"') == 1
		os.write_file(mac_fixture.manifest_path, case_source.replace_once('"path": "tcc.exe"',
			'"path": "TCC.exe"')) or { panic(err) }
		for args in [
			['git', '-C', mac_fixture.source_repo, 'mv', '--', 'tcc.exe', 'tcc-case.tmp'],
			['git', '-C', mac_fixture.source_repo, 'mv', '--', 'tcc-case.tmp', 'TCC.exe'],
		] {
			result := os.exec(args)
			assert result.exit_code == 0, result.output
		}
		for args in [
			['git', '-C', mac_fixture.source_repo, 'add', '--all'],
			['git', '-C', mac_fixture.source_repo, 'commit', '-qm', 'case-only payload rename'],
		] {
			result := os.exec(args)
			assert result.exit_code == 0, result.output
		}
		case_ref := os.exec(['git', '-C', mac_fixture.source_repo, 'rev-parse', 'HEAD'])
		assert case_ref.exit_code == 0, case_ref.output
		case_work := os.join_path(mac_fixture.base, 'case-work')
		assert candidate_execution_error(mac_fixture, mac_fixture.contract.source_git_ref,
			case_ref.output.trim_space(), case_work, runtime_contract_binding(false), false) == 'candidate transition cannot rename a payload by case only'
		assert !os.exists(case_work)
	}
}

fn test_runtime_contract_binding_is_validated_before_staged_material() {
	missing_path := os.join_path(os.temp_dir(), 'tccbin-runtime-binding-material-absent')
	valid_sha := 'a'.repeat(40)
	for repository in ['', 'evil/v'] {
		message := staged_execution_error(automation_root(), missing_path, bin.StagingContract{}, bin.RuntimeContractBinding{
			repository: repository
			sha:        valid_sha
		}, false)
		assert message == 'runtime contract repository is not allowlisted'
	}
	for sha in ['', 'abcdef0', 'A'.repeat(40)] {
		message := staged_execution_error(automation_root(), missing_path, bin.StagingContract{}, bin.RuntimeContractBinding{
			repository: 'GGRei/v'
			sha:        sha
		}, false)
		assert message == 'runtime contract SHA must be a full lowercase commit SHA'
	}
}

fn test_runtime_contract_binding_matches_manifest_and_controls_publication() {
	fork_fixture := prepare_complete_candidate('runtime-binding-fork', false, '')
	production_fixture := prepare_complete_candidate('runtime-binding-production', true, '')
	defer {
		os.rmdir_all(fork_fixture.base) or {}
		os.rmdir_all(production_fixture.base) or {}
	}
	fork_runtime := runtime_contract_binding(false)
	production_runtime := runtime_contract_binding(true)
	fork_decision := bin.evaluate_staged_manifest_for_execution(fork_fixture.automation_root,
		fork_fixture.manifest_path, fork_fixture.contract, fork_runtime, false) or { panic(err) }
	assert fork_decision.eligible
	assert !fork_decision.publish_allowed
	assert fork_decision.reason == 'authenticated_staging'
	assert staged_execution_error(fork_fixture.automation_root, fork_fixture.manifest_path,
		fork_fixture.contract, production_runtime, false) == 'runtime contract binding differs from the authenticated manifest'
	assert staged_execution_error(production_fixture.automation_root,
		production_fixture.manifest_path, production_fixture.contract, fork_runtime, false) == 'runtime contract binding differs from the authenticated manifest'
	assert staged_execution_error(fork_fixture.automation_root, fork_fixture.manifest_path,
		fork_fixture.contract, bin.RuntimeContractBinding{
		repository: fork_runtime.repository
		sha:        'b'.repeat(40)
	}, false) == 'runtime contract binding differs from the authenticated manifest'
	assert staged_execution_error(fork_fixture.automation_root, fork_fixture.manifest_path,
		fork_fixture.contract, fork_runtime, true) == 'publication requires an authenticated production vlang/v contract'
	production_dry_run := bin.evaluate_staged_manifest_for_execution(production_fixture.automation_root,
		production_fixture.manifest_path, production_fixture.contract, production_runtime, false) or {
		panic(err)
	}
	assert production_dry_run.eligible
	assert !production_dry_run.publish_allowed
	production_publish := bin.evaluate_staged_manifest_for_execution(production_fixture.automation_root,
		production_fixture.manifest_path, production_fixture.contract, production_runtime, true) or {
		panic(err)
	}
	assert production_publish.eligible
	assert production_publish.publish_allowed
}

fn restore_candidate_output(fixture CompleteCandidateFixture) {
	path := os.join_path(fixture.staging_root, 'tcc.exe')
	os.rmdir_all(path) or {}
	os.write_file_array(path, fixture.output_bytes) or { panic(err) }
	$if !windows {
		os.chmod(path, 0o755) or { panic(err) }
	}
}

fn test_production_eligibility_binds_the_exact_candidate_and_payload_export() {
	fixture := prepare_complete_candidate('production-eligibility', true, '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	decision := bin.evaluate_staged_manifest_for_execution(fixture.automation_root,
		fixture.manifest_path, fixture.contract, runtime_contract_binding(true), false) or {
		panic(err)
	}
	assert decision.eligible
	assert !decision.publish_allowed
	assert decision.reason == 'authenticated_staging'
	assert decision.manifest_hash.len == 64
	missing_path := os.join_path(fixture.base, 'missing-manifest.json')
	missing := bin.evaluate_staged_manifest_for_execution(fixture.automation_root, missing_path,
		fixture.contract, runtime_contract_binding(true), false) or { panic(err) }
	assert !missing.eligible
	assert !missing.publish_allowed
	assert missing.reason == 'phase_a_material_absent'
	mut missing_publish_rejected := false
	bin.evaluate_staged_manifest_for_execution(fixture.automation_root, missing_path,
		fixture.contract, runtime_contract_binding(true), true) or {
		missing_publish_rejected = true
	}
	assert missing_publish_rejected

	mutated_manifest_path := os.join_path(fixture.base, 'mutated-manifest.json')
	producer_evidence :=
		sha256_bytes(t2c_toolchain_evidence_source('producer', 'bundle-builder').bytes())
	evidence_marker := '"evidence_sha256":"${producer_evidence}"'
	assert fixture.manifest_source.count(evidence_marker) == 1
	os.write_file(mutated_manifest_path, fixture.manifest_source.replace_once(evidence_marker,
		'"evidence_sha256":"${'d'.repeat(64)}"')) or { panic(err) }
	assert_staged_ineligible(fixture, mutated_manifest_path, fixture.contract,
		runtime_contract_binding(true))
	parent_contract := bin.StagingContract{
		staging_root:    fixture.staging_root
		source_git_root: fixture.source_repo
		source_git_ref:  fixture.parent_ref
	}
	assert_staged_ineligible(fixture, fixture.manifest_path, parent_contract,
		runtime_contract_binding(true))
	overlap_contract := bin.StagingContract{
		staging_root:    fixture.source_repo
		source_git_root: fixture.source_repo
		source_git_ref:  fixture.contract.source_git_ref
	}
	assert_staged_ineligible(fixture, fixture.manifest_path, overlap_contract,
		runtime_contract_binding(true))
	os.write_file(os.join_path(fixture.source_repo, '.git', 'info', 'exclude'),
		'ignored-candidate.tmp\n') or { panic(err) }
	os.write_file(os.join_path(fixture.source_repo, 'ignored-candidate.tmp'), 'ignored\n') or {
		panic(err)
	}
	assert_staged_ineligible(fixture, fixture.manifest_path, fixture.contract,
		runtime_contract_binding(true))
}

fn test_payload_export_rejects_mutation_extra_type_mode_and_hardlink_aliases() {
	fixture := prepare_complete_candidate('payload-mutations', true, '')
	defer {
		os.rmdir_all(fixture.base) or {}
	}
	output_path := os.join_path(fixture.staging_root, 'tcc.exe')
	os.write_file(output_path, 'mutated payload\n') or { panic(err) }
	assert_staged_ineligible(fixture, fixture.manifest_path, fixture.contract,
		runtime_contract_binding(true))
	restore_candidate_output(fixture)

	extra_path := os.join_path(fixture.staging_root, 'undeclared.bin')
	os.write_file(extra_path, 'extra\n') or { panic(err) }
	assert_staged_ineligible(fixture, fixture.manifest_path, fixture.contract,
		runtime_contract_binding(true))
	os.rm(extra_path) or { panic(err) }

	$if !windows {
		os.chmod(output_path, 0o644) or { panic(err) }
		assert_staged_ineligible(fixture, fixture.manifest_path, fixture.contract,
			runtime_contract_binding(true))
		os.chmod(output_path, 0o755) or { panic(err) }
		external_alias := os.join_path(fixture.base, 'external-output-hardlink')
		os.link(output_path, external_alias) or { panic(err) }
		assert_staged_ineligible(fixture, fixture.manifest_path, fixture.contract,
			runtime_contract_binding(true))
		os.rm(external_alias) or { panic(err) }
	}

	os.rm(output_path) or { panic(err) }
	assert_staged_ineligible(fixture, fixture.manifest_path, fixture.contract,
		runtime_contract_binding(true))
	os.mkdir(output_path) or { panic(err) }
	assert_staged_ineligible(fixture, fixture.manifest_path, fixture.contract,
		runtime_contract_binding(true))
	restore_candidate_output(fixture)

	$if !windows {
		os.rm(output_path) or { panic(err) }
		os.link(os.join_path(fixture.source_repo, 'tcc.exe'), output_path) or { panic(err) }
		assert_staged_ineligible(fixture, fixture.manifest_path, fixture.contract,
			runtime_contract_binding(true))
		restore_candidate_output(fixture)
	}
}

fn test_unresolved_source_or_toolchain_is_valid_but_never_authenticates_or_publishes() {
	for unresolved in ['source', 'toolchain'] {
		fixture := prepare_complete_candidate('incomplete-${unresolved}', true, unresolved)
		defer {
			os.rmdir_all(fixture.base) or {}
		}
		issues := bin.validate_manifest(fixture.automation_root, fixture.manifest_path) or {
			panic(err)
		}
		assert issues.len == 0, '${unresolved}: ${issues}'
		mut static_auth_rejected := false
		bin.authenticate_manifest_file(fixture.automation_root, fixture.manifest_path) or {
			static_auth_rejected = err.msg().contains('incomplete provenance')
		}
		assert static_auth_rejected
		decision := bin.evaluate_staged_manifest_for_execution(fixture.automation_root,
			fixture.manifest_path, fixture.contract, runtime_contract_binding(true), false) or {
			panic(err)
		}
		assert !decision.eligible
		assert !decision.publish_allowed
		assert decision.reason == 'staged_provenance_incomplete'
		mut publish_rejected := false
		bin.evaluate_staged_manifest_for_execution(fixture.automation_root, fixture.manifest_path,
			fixture.contract, runtime_contract_binding(true), true) or {
			publish_rejected = err.msg() == 'publication refused because staged provenance is incomplete'
		}
		assert publish_rejected
	}
}
