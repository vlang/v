module bin

import crypto.sha256
import crypto.sha1
import os

const staged_payload_max_file_size = u64(64 * 1024 * 1024)

const staged_payload_hash_buffer_size = 64 * 1024

struct GitBlobObservation {
	git_mode string
	oid      string
}

struct StagedPathObservation {
	kind              string
	git_mode          string
	sha256            string
	git_blob_oid      string
	symlink_target    string
	prefix            []u8
	identity_volume   u64
	identity_index    u64
	identity_reliable bool
	nlink             u64
}

struct NativeFileIdentity {
	volume   u64
	index    u64
	nlink    u64
	reliable bool
}

struct NativeFileSnapshot {
	identity            NativeFileIdentity
	mode                u32
	size                u64
	mtime_sec           i64
	mtime_nsec          i64
	ctime_sec           i64
	ctime_nsec          i64
	mtime_windows_ticks i64
	ctime_windows_ticks i64
	regular             bool
}

struct NativeToolchainDocument {
mut:
	handle voidptr
	fd     int
	opened bool
}

fn native_path_file_snapshot(path string) !NativeFileSnapshot {
	$if windows {
		return windows_path_file_snapshot(path)!
	} $else {
		return posix_path_file_snapshot(path)!
	}
}

fn native_open_file_snapshot(file &os.File) !NativeFileSnapshot {
	$if windows {
		return windows_file_snapshot(file)!
	} $else {
		return posix_file_snapshot(file)!
	}
}

struct RegularFileHashes {
	sha256       string
	git_blob_oid string
	prefix       []u8
	identity     NativeFileIdentity
	stat         os.Stat
}

struct StagedPayloadInventory {
	paths       []string
	directories []string
}

// scan_manifest_opaque_inputs inspects bytes and file types in staging and Git modes in one
// immutable authoritative source tree. No manifest field is copied into an observation.
pub fn scan_manifest_opaque_inputs(manifest JsonValue, registry JsonValue,
	staging StagingContract) ![]OpaqueObservation {
	validate_staging_contract_roots(staging)!
	target_id := require_string_member(manifest, 'target_id')!
	mut entries := require_array_member(manifest, 'inventory')!
	entries << require_array_member(manifest, 'overlays')!
	mut observations := []OpaqueObservation{}
	for entry in entries {
		if !require_bool_member(entry, 'opaque')! {
			continue
		}
		acceptance_id := require_nullable_string_member(entry, 'opaque_acceptance_id')!
		acceptance := opaque_acceptance_by_id(registry, acceptance_id)!
		if require_string_member(acceptance, 'target_id')! != target_id {
			return error('opaque acceptance target does not match the manifest target')
		}
		observations << scan_opaque_input(staging, target_id, require_string_member(acceptance,
			'path')!)!
	}
	return observations
}

// scan_opaque_input returns only facts observed from staging bytes and an authoritative Git tree.
pub fn scan_opaque_input(staging StagingContract, target_id string,
	relative_path string) !OpaqueObservation {
	if target_id !in managed_target_ids || !contract_relative_path_is_safe(relative_path) {
		return error('opaque scanner received an invalid target or relative path')
	}
	validate_staging_contract_roots(staging)!
	git_entry := authoritative_git_entry(staging.source_git_root, staging.source_git_ref,
		relative_path) or { GitBlobObservation{} }
	observed := observe_staged_path(staging.staging_root, relative_path) or {
		return OpaqueObservation{
			present:   false
			target_id: target_id
			path:      relative_path
			git_mode:  git_entry.git_mode
		}
	}
	if observed.kind != 'file' {
		return OpaqueObservation{
			present:   true
			target_id: target_id
			path:      relative_path
			kind:      observed.kind
			git_mode:  git_entry.git_mode
		}
	}
	elf := parse_elf64_relocatable_header(observed.prefix) or { ElfObservation{} }
	return OpaqueObservation{
		present:     true
		target_id:   target_id
		path:        relative_path
		kind:        observed.kind
		git_mode:    git_entry.git_mode
		sha256:      observed.sha256
		format:      elf.format
		object_type: elf.object_type
		machine:     elf.machine
		os_abi:      elf.os_abi
	}
}

// validate_staged_manifest_material binds the canonical manifest and every declared payload byte
// to one immutable candidate commit and to a physically separate, payload-only staging tree.
pub fn validate_staged_manifest_material(manifest JsonValue, manifest_source string,
	staging StagingContract) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	validate_staging_contract_roots(staging) or {
		issues << SchemaIssue{'$', err.msg()}
		return issues
	}
	manifest_entry := authoritative_git_entry(staging.source_git_root, staging.source_git_ref,
		'automation/bundle-manifest.json') or {
		issues << SchemaIssue{'$', err.msg()}
		return issues
	}
	candidate_manifest := observe_candidate_path(staging.source_git_root,
		'automation/bundle-manifest.json') or {
		issues << SchemaIssue{'$', err.msg()}
		return issues
	}
	manifest_source_bytes := manifest_source.bytes()
	if manifest_entry.git_mode != '100644' || candidate_manifest.kind != 'file'
		|| candidate_manifest.git_blob_oid != manifest_entry.oid
		|| candidate_manifest.sha256 != sha256.sum256(manifest_source_bytes).hex()
		|| u64(manifest_source_bytes.len) > staged_payload_max_file_size {
		issues << SchemaIssue{
			path:    '$'
			message: 'manifest bytes must match automation/bundle-manifest.json in the immutable candidate commit'
		}
		return issues
	}
	target_id := require_string_member(manifest, 'target_id')!
	recipe := require_object_member(manifest, 'recipe')!
	recipe_path := require_string_member(recipe, 'path')!
	mut control_paths := [
		manifest_path_key(target_id, 'automation/bundle-manifest.json'),
		manifest_path_key(target_id, recipe_path),
	]
	if manifest_path_is_reserved(target_id, recipe_path) {
		issues << SchemaIssue{'$/recipe/path', 'control input paths must be globally unique and outside reserved control-plane trees'}
	}
	issues << validate_git_input_hash(staging, recipe_path,
		require_string_member(recipe, 'sha256')!, '$/recipe', true)!
	for index, patch in require_array_member(manifest, 'patches')! {
		path := require_string_member(patch, 'path')!
		path_key := manifest_path_key(target_id, path)
		if path_key in control_paths || manifest_path_is_reserved(target_id, path) {
			issues << SchemaIssue{'$/patches/${index}/path', 'control input paths must be globally unique'}
			continue
		}
		control_paths << path_key
		issues << validate_git_input_hash(staging, path, require_string_member(patch, 'sha256')!,
			'$/patches/${index}', false)!
	}
	for index, transform in require_array_member(manifest, 'transforms')! {
		path := require_string_member(transform, 'path')!
		path_key := manifest_path_key(target_id, path)
		if path_key in control_paths || manifest_path_is_reserved(target_id, path) {
			issues << SchemaIssue{'$/transforms/${index}/path', 'control input paths must be globally unique'}
			continue
		}
		control_paths << path_key
		issues << validate_git_input_hash(staging, path,
			require_string_member(transform, 'sha256')!, '$/transforms/${index}', false)!
	}
	if issues.len > 0 {
		return issues
	}
	mut entries := []JsonValue{}
	mut issue_paths := []string{}
	for collection_name in ['inventory', 'overlays', 'outputs'] {
		for index, entry in require_array_member(manifest, collection_name)! {
			entries << entry
			issue_paths << '$/${collection_name}/${index}'
		}
	}
	mut expected_paths := []string{cap: entries.len}
	mut expected_path_keys := []string{cap: entries.len}
	for index, entry in entries {
		path := require_string_member(entry, 'path')!
		path_key := manifest_path_key(target_id, path)
		if manifest_path_is_reserved(target_id, path) || path_key in control_paths {
			issues << SchemaIssue{issue_paths[index], 'payload paths cannot overlap control-plane paths'}
			continue
		}
		if path_key in expected_path_keys {
			issues << SchemaIssue{issue_paths[index], 'payload paths must be globally unique'}
			continue
		}
		expected_paths << path
		expected_path_keys << path_key
		if require_string_member(entry, 'kind')! == 'symlink' {
			target := require_nullable_string_member(entry, 'symlink_target')!
			if !symlink_target_is_allowed(target_id, path, target) {
				issues << SchemaIssue{issue_paths[index], 'symlink target is not allowed for this target and path'}
				continue
			}
		}
		issues << validate_staged_payload_entry(staging, entry, issue_paths[index])!
	}
	if issues.len > 0 {
		return issues
	}
	expected_paths.sort()
	expected_directories := payload_parent_directories(expected_paths)
	observed_inventory := scan_staged_payload_inventory(staging.staging_root) or {
		issues << SchemaIssue{'$/inventory', err.msg()}
		return issues
	}
	if observed_inventory.paths != expected_paths
		|| observed_inventory.directories != expected_directories {
		issues << SchemaIssue{
			path:    '$/inventory'
			message: 'payload-only staging must exactly equal inventory, overlays, and outputs'
		}
	}
	return issues
}

fn validate_staging_contract_roots(staging StagingContract) ! {
	if !os.is_dir(staging.staging_root) || !os.is_dir(staging.source_git_root)
		|| os.is_link(staging.staging_root) || os.is_link(staging.source_git_root)
		|| !git_reference_is_safe(staging.source_git_ref) {
		return error('staging roots or immutable Git ref are invalid')
	}
	staging_root := canonical_contract_root(staging.staging_root)!
	source_root := canonical_contract_root(staging.source_git_root)!
	if roots_overlap(staging_root, source_root) {
		return error('staging and immutable source Git roots must be physically separate')
	}
	commit_expression := '${staging.source_git_ref}^{commit}'
	result := os.exec(['git', '--no-replace-objects', '-C', staging.source_git_root, '-c',
		'core.autocrlf=false', 'rev-parse', '--verify', commit_expression])
	if result.exit_code != 0 || result.output.trim_space() != staging.source_git_ref {
		return error('immutable source Git ref does not resolve to the exact candidate commit')
	}
	autocrlf := os.exec(['git', '--no-replace-objects', '-C', staging.source_git_root, 'config',
		'--get', 'core.autocrlf'])
	if autocrlf.exit_code != 0 || autocrlf.output.trim_space() != 'false' {
		return error('source Git checkout must disable core.autocrlf')
	}
	head := os.exec(['git', '--no-replace-objects', '-C', staging.source_git_root, '-c',
		'core.autocrlf=false', 'rev-parse', 'HEAD'])
	if head.exit_code != 0 || head.output.trim_space() != staging.source_git_ref {
		return error('source Git checkout is not at the exact candidate commit')
	}
	symbolic := os.exec(['git', '--no-replace-objects', '-C', staging.source_git_root, '-c',
		'core.autocrlf=false', 'symbolic-ref', '-q', 'HEAD'])
	if symbolic.exit_code != 1 || symbolic.output != '' {
		return error('source Git checkout must be detached at the candidate commit')
	}
	status := os.exec(['git', '--no-replace-objects', '-C', staging.source_git_root, '-c',
		'core.autocrlf=false', 'status', '--porcelain=v1', '--untracked-files=all',
		'--ignored=matching'])
	if status.exit_code != 0 || status.output != '' {
		return error('source Git checkout must be clean at the candidate commit')
	}
}

fn canonical_contract_root(path string) !string {
	root := os.real_path(path).replace('\\', '/').trim_right('/')
	if root == '' {
		return error('contract root cannot resolve to an empty path')
	}
	return root
}

fn roots_overlap(left string, right string) bool {
	mut left_key := left
	mut right_key := right
	$if windows || macos {
		left_key = left_key.to_lower()
		right_key = right_key.to_lower()
	}
	return left_key == right_key || left_key.starts_with('${right_key}/')
		|| right_key.starts_with('${left_key}/')
}

fn validate_git_input_hash(staging StagingContract, path string, expected_sha256 string,
	issue_path string, executable_allowed bool) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	entry := authoritative_git_entry(staging.source_git_root, staging.source_git_ref, path) or {
		issues << SchemaIssue{issue_path, err.msg()}
		return issues
	}
	if entry.git_mode != '100644' && (!executable_allowed || entry.git_mode != '100755') {
		issues << SchemaIssue{issue_path, 'recipe and patch inputs must be regular Git blobs'}
		return issues
	}
	observed := observe_candidate_path(staging.source_git_root, path) or {
		issues << SchemaIssue{issue_path, err.msg()}
		return issues
	}
	if observed.kind !in ['file', 'executable'] || observed.git_blob_oid != entry.oid {
		issues << SchemaIssue{issue_path, 'candidate input bytes differ from the immutable Git blob'}
		return issues
	}
	if observed.sha256 != expected_sha256 {
		issues << SchemaIssue{issue_path, 'declared SHA-256 differs from the immutable candidate blob'}
	}
	return issues
}

fn validate_staged_payload_entry(staging StagingContract, entry JsonValue,
	issue_path string) ![]SchemaIssue {
	mut issues := []SchemaIssue{}
	path := require_string_member(entry, 'path')!
	git_entry := authoritative_git_entry(staging.source_git_root, staging.source_git_ref, path) or {
		issues << SchemaIssue{issue_path, err.msg()}
		return issues
	}
	observed := observe_staged_path(staging.staging_root, path) or {
		issues << SchemaIssue{issue_path, err.msg()}
		return issues
	}
	candidate := observe_candidate_path(staging.source_git_root, path) or {
		issues << SchemaIssue{issue_path, err.msg()}
		return issues
	}
	declared_kind := require_string_member(entry, 'kind')!
	declared_mode := require_string_member(entry, 'git_mode')!
	declared_sha256 := require_string_member(entry, 'sha256')!
	declared_target := require_nullable_string_member(entry, 'symlink_target')!
	mut kind_or_mode_differs := git_entry.git_mode != declared_mode
	$if windows {
		kind_or_mode_differs = kind_or_mode_differs || observed.kind != 'file'
			|| declared_kind !in ['file', 'executable']
	} $else {
		kind_or_mode_differs = kind_or_mode_differs || observed.kind != declared_kind
			|| observed.git_mode != declared_mode
	}
	if kind_or_mode_differs {
		issues << SchemaIssue{issue_path, 'staged kind or mode differs from the manifest or candidate Git tree'}
	}
	if observed.sha256 != declared_sha256 {
		issues << SchemaIssue{issue_path, 'staged SHA-256 differs from the manifest'}
	}
	if observed.git_blob_oid != git_entry.oid {
		issues << SchemaIssue{issue_path, 'staged bytes differ from the immutable candidate Git blob'}
	}
	if candidate.git_blob_oid != git_entry.oid
		|| candidate.symlink_target != observed.symlink_target {
		issues << SchemaIssue{issue_path, 'source checkout bytes differ from the immutable candidate Git blob or staging'}
	}
	if staged_and_source_are_hardlinked(observed, candidate) {
		issues << SchemaIssue{issue_path, 'payload staging must not hardlink candidate source files'}
	}
	if observed.symlink_target != declared_target {
		issues << SchemaIssue{issue_path, 'staged symlink target differs from the manifest'}
	}
	return issues
}

fn observe_staged_path(staging_root string, relative_path string) !StagedPathObservation {
	if !contract_relative_path_is_safe(relative_path) {
		return error('staged scanner received an unsafe payload path')
	}
	validate_staged_parent_chain(staging_root, relative_path)!
	full_path := os.join_path(staging_root, relative_path)
	if os.is_link(full_path) {
		$if windows {
			return error('Windows payload reparse points are not supported')
		}
		$if !windows {
			return observe_symlink_path(full_path)!
		}
	}
	stat := os.lstat(full_path) or { return error('declared payload path is absent from staging') }
	if stat.get_filetype() != .regular {
		return error('declared payload path is not a regular file or symlink')
	}
	hashes := hash_stable_regular_file(full_path)!
	$if windows {
		return StagedPathObservation{
			kind:              'file'
			sha256:            hashes.sha256
			git_blob_oid:      hashes.git_blob_oid
			prefix:            hashes.prefix
			identity_volume:   hashes.identity.volume
			identity_index:    hashes.identity.index
			identity_reliable: hashes.identity.reliable
			nlink:             hashes.identity.nlink
		}
	} $else {
		mode := stat.get_mode()
		executable := mode.owner.execute || mode.group.execute || mode.others.execute
		return StagedPathObservation{
			kind:              if executable { 'executable' } else { 'file' }
			git_mode:          if executable { '100755' } else { '100644' }
			sha256:            hashes.sha256
			git_blob_oid:      hashes.git_blob_oid
			prefix:            hashes.prefix
			identity_volume:   hashes.identity.volume
			identity_index:    hashes.identity.index
			identity_reliable: hashes.identity.reliable
			nlink:             hashes.identity.nlink
		}
	}
}

fn observe_candidate_path(source_root string, relative_path string) !StagedPathObservation {
	if !contract_relative_path_is_safe(relative_path) {
		return error('candidate scanner received an unsafe path')
	}
	validate_staged_parent_chain(source_root, relative_path)!
	full_path := os.join_path(source_root, relative_path)
	if os.is_link(full_path) {
		$if windows {
			return error('candidate Git checkout contains an unsupported reparse point')
		}
		$if !windows {
			return observe_symlink_path(full_path)!
		}
	}
	stat := os.lstat(full_path) or { return error('candidate Git checkout path is absent') }
	if stat.get_filetype() != .regular {
		return error('candidate Git checkout path is not a regular file or symlink')
	}
	hashes := hash_stable_regular_file(full_path)!
	return StagedPathObservation{
		kind:              'file'
		sha256:            hashes.sha256
		git_blob_oid:      hashes.git_blob_oid
		prefix:            hashes.prefix
		identity_volume:   hashes.identity.volume
		identity_index:    hashes.identity.index
		identity_reliable: hashes.identity.reliable
		nlink:             hashes.identity.nlink
	}
}

fn observe_symlink_path(path string) !StagedPathObservation {
	$if windows {
		return error('Windows payload reparse points are not supported')
	} $else {
		before := os.lstat(path)!
		if before.get_filetype() != .symbolic_link || before.size > 4096 {
			return error('symlink target is absent, unstable, or exceeds its byte bound')
		}
		target := os.readlink(path)!
		after := os.lstat(path)!
		if !same_file_snapshot(before, after) || u64(target.len) != before.size {
			return error('symlink changed while its target was observed')
		}
		bytes := target.bytes()
		return StagedPathObservation{
			kind:              'symlink'
			git_mode:          '120000'
			sha256:            sha256.sum256(bytes).hex()
			git_blob_oid:      git_blob_oid(bytes)
			symlink_target:    target
			identity_volume:   before.dev
			identity_index:    before.inode
			identity_reliable: before.inode != 0
			nlink:             before.nlink
		}
	}
}

fn hash_stable_regular_file(path string) !RegularFileHashes {
	before := os.lstat(path)!
	if before.get_filetype() != .regular || before.size > staged_payload_max_file_size {
		return error('regular payload file is absent or exceeds its byte bound')
	}
	mut file := os.open(path)!
	defer {
		file.close()
	}
	mut sha256_digest := sha256.new()
	mut git_digest := sha1.new()
	mut identity_before := NativeFileIdentity{}
	$if windows {
		identity_before = windows_file_identity(&file)!
	} $else {
		identity_before = NativeFileIdentity{
			volume:   before.dev
			index:    before.inode
			nlink:    before.nlink
			reliable: before.inode != 0
		}
	}
	if !identity_before.reliable {
		return error('regular payload file identity is unavailable')
	}
	git_digest.write('blob ${before.size}\x00'.bytes())!
	mut buffer := []u8{len: staged_payload_hash_buffer_size}
	mut prefix := []u8{cap: 64}
	mut total := u64(0)
	for {
		read := file.read(mut buffer) or {
			if err is os.Eof {
				break
			}
			return error('regular payload file cannot be read inside its byte bound')
		}
		if read <= 0 {
			break
		}
		total += u64(read)
		if total > staged_payload_max_file_size {
			return error('regular payload file grew beyond its byte bound while hashing')
		}
		if prefix.len < 64 {
			remaining := 64 - prefix.len
			take := if read < remaining { read } else { remaining }
			prefix << buffer[..take]
		}
		sha256_digest.write(buffer[..read])!
		git_digest.write(buffer[..read])!
	}
	after := os.lstat(path)!
	mut identity_after := NativeFileIdentity{}
	$if windows {
		identity_after = windows_file_identity(&file)!
		path_identity_after := windows_path_file_identity(path)!
		if identity_after != path_identity_after {
			return error('regular payload path changed while hashing')
		}
	} $else {
		identity_after = NativeFileIdentity{
			volume:   after.dev
			index:    after.inode
			nlink:    after.nlink
			reliable: after.inode != 0
		}
	}
	if total != before.size || !same_file_snapshot(before, after)
		|| identity_before != identity_after {
		return error('regular payload file changed while hashing')
	}
	return RegularFileHashes{
		sha256:       sha256_digest.sum([]).hex()
		git_blob_oid: git_digest.sum([]).hex()
		prefix:       prefix
		identity:     identity_before
		stat:         before
	}
}

fn same_file_snapshot(first os.Stat, second os.Stat) bool {
	return first.dev == second.dev && first.inode == second.inode && first.mode == second.mode
		&& first.nlink == second.nlink && first.uid == second.uid && first.gid == second.gid
		&& first.size == second.size && first.mtime == second.mtime && first.ctime == second.ctime
}

fn staged_and_source_are_hardlinked(staged StagedPathObservation,
	source StagedPathObservation) bool {
	if !staged.identity_reliable || !source.identity_reliable {
		return true
	}
	if staged.nlink != 1 {
		return true
	}
	shared_identity := staged.identity_volume == source.identity_volume
		&& staged.identity_index == source.identity_index
	return shared_identity
}

fn validate_staged_parent_chain(staging_root string, relative_path string) ! {
	parts := relative_path.split('/')
	mut current := staging_root
	for part in parts[..parts.len - 1] {
		current = os.join_path(current, part)
		if os.is_link(current) {
			return error('payload path has a symlinked parent directory')
		}
		stat := os.lstat(current) or { return error('payload parent directory is absent') }
		if stat.get_filetype() != .directory {
			return error('payload parent is not a directory')
		}
	}
}

fn authoritative_git_entry(repository_root string, source_ref string,
	relative_path string) !GitBlobObservation {
	if !git_reference_is_safe(source_ref) || !contract_relative_path_is_safe(relative_path) {
		return error('immutable Git lookup received an unsafe ref or path')
	}
	result := os.exec(['git', '--no-replace-objects', '-C', repository_root, '-c',
		'core.autocrlf=false', 'ls-tree', '-z', '--full-tree', source_ref, '--', relative_path])
	if result.exit_code != 0 {
		return error('authoritative Git tree lookup failed')
	}
	records := result.output.split('\x00')
	if records.len != 2 || records[1] != '' {
		return error('authoritative Git tree must contain exactly one exact payload path')
	}
	parts := records[0].split_nth('\t', 2)
	if parts.len != 2 || parts[1] != relative_path {
		return error('authoritative Git tree returned a different payload path')
	}
	metadata := parts[0].fields()
	if metadata.len != 3 || metadata[1] != 'blob' || metadata[0] !in ['100644', '100755', '120000']
		|| !is_lower_hex_40(metadata[2]) {
		return error('authoritative Git tree returned an unsupported mode or object')
	}
	return GitBlobObservation{
		git_mode: metadata[0]
		oid:      metadata[2]
	}
}

fn git_blob_oid(bytes []u8) string {
	mut material := 'blob ${bytes.len}\x00'.bytes()
	material << bytes
	return sha1.sum(material).hex()
}

fn payload_parent_directories(paths []string) []string {
	mut directories := []string{}
	for path in paths {
		parts := path.split('/')
		for index in 1 .. parts.len {
			directory := parts[..index].join('/')
			if directory !in directories {
				directories << directory
			}
		}
	}
	directories.sort()
	return directories
}

fn scan_staged_payload_inventory(staging_root string) !StagedPayloadInventory {
	mut pending := ['']
	mut paths := []string{}
	mut directories := []string{}
	for pending.len > 0 {
		relative_directory := pending.pop()
		full_directory := if relative_directory == '' {
			staging_root
		} else {
			os.join_path(staging_root, relative_directory)
		}
		mut names := os.ls(full_directory)!
		names.sort()
		for name in names {
			if name == '.git' {
				return error('payload-only staging cannot contain .git')
			}
			relative_path := if relative_directory == '' {
				name
			} else {
				'${relative_directory}/${name}'
			}
			if !contract_relative_path_is_safe(relative_path) {
				return error('payload-only staging contains an unsafe path')
			}
			full_path := os.join_path(staging_root, relative_path)
			if os.is_link(full_path) {
				paths << relative_path
				continue
			}
			stat := os.lstat(full_path)!
			match stat.get_filetype() {
				.regular {
					paths << relative_path
				}
				.directory {
					directories << relative_path
					pending << relative_path
				}
				else {
					return error('payload-only staging contains a special filesystem object')
				}
			}
		}
	}
	paths.sort()
	directories.sort()
	return StagedPayloadInventory{
		paths:       paths
		directories: directories
	}
}

fn symlink_target_is_allowed(target_id string, path string, target string) bool {
	if target == '/System/DriverKit/usr/lib/libSystem.dylib' {
		return target_id in ['macos-amd64', 'macos-arm64'] && path == 'lib/libc.dylib'
	}
	if target == '' || target.starts_with('/') || target.ends_with('/') || target.contains('\\')
		|| target.contains('\x00') || target.contains('\n') || target.contains('\r')
		|| target.contains('\t') {
		return false
	}
	mut resolved := os.dir(path).split('/').filter(it != '.' && it != '')
	for segment in target.split('/') {
		if segment == '' || segment == '.' || segment == '...' {
			return false
		}
		if segment == '..' {
			if resolved.len == 0 {
				return false
			}
			resolved.delete_last()
			continue
		}
		for byte in segment.bytes() {
			if !(byte.is_alnum() || byte in [`_`, `.`, `+`, `-`]) {
				return false
			}
		}
		resolved << segment
	}
	return resolved.len > 0
}

struct ElfObservation {
	format      string
	object_type string
	machine     string
	os_abi      string
}

fn parse_elf64_relocatable_header(bytes []u8) !ElfObservation {
	if bytes.len < 64 || bytes[0] != 0x7f || bytes[1] != `E` || bytes[2] != `L` || bytes[3] != `F` {
		return error('opaque object is missing a complete ELF header')
	}
	if bytes[4] != 2 || bytes[5] != 1 || bytes[6] != 1 {
		return error('opaque object is not ELF64 little-endian version 1')
	}
	if bytes[7] != 0 {
		return error('opaque object does not use the System V OS/ABI')
	}
	object_type := u16(bytes[16]) | (u16(bytes[17]) << 8)
	machine := u16(bytes[18]) | (u16(bytes[19]) << 8)
	if object_type != 1 || machine != 0x3e {
		return error('opaque object is not ET_REL for EM_X86_64')
	}
	return ElfObservation{
		format:      'ELF64 little-endian'
		object_type: 'ET_REL'
		machine:     'EM_X86_64'
		os_abi:      'System V'
	}
}

fn opaque_acceptance_by_id(registry JsonValue, acceptance_id string) !JsonValue {
	mut matches := []JsonValue{}
	for acceptance in require_array_member(registry, 'opaque_acceptances')! {
		if require_string_member(acceptance, 'id')! == acceptance_id {
			matches << acceptance
		}
	}
	if matches.len != 1 {
		return error('opaque acceptance must resolve to exactly one registry entry')
	}
	return matches[0]
}

// contract_relative_path_is_safe rejects traversal, dot-only segments, separators, and controls.
pub fn contract_relative_path_is_safe(path string) bool {
	if path == '' || path.starts_with('/') || path.ends_with('/') || path.contains('\\')
		|| path.contains('\x00') || path.contains('\n') || path.contains('\r')
		|| path.contains('\t') {
		return false
	}
	for segment in path.split('/') {
		if segment == '' || segment == '.' || segment == '..' || segment == '...' {
			return false
		}
		for byte in segment.bytes() {
			if !(byte.is_alnum() || byte in [`_`, `.`, `+`, `-`]) {
				return false
			}
		}
	}
	return true
}

fn git_reference_is_safe(reference string) bool {
	return is_lower_hex_40(reference)
}
