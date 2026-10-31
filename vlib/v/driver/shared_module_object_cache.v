module driver

import os
import crypto.sha256
import v.modulecache
import v.tempname
import v.cmdexec

fn v3_shared_file_metadata_identity(metadata string) string {
	// Source-signature memoization prefixes metadata with the V build hash.
	// Object sharing identifies the C compiler file independently of that build.
	return metadata.all_after(':')
}

fn v3_shared_file_identity(path string) string {
	metadata := modulecache.file_metadata_signature(path)
	if metadata != '' { return 'metadata=' + v3_shared_file_metadata_identity(metadata) }
	content := os.read_file(path) or { return 'missing' }
	return 'content=' + sha256.hexhash(content)
}

fn v3_shared_c_compiler_identity(compiler string) string {
	path := os.real_path(os.find_abs_path_of_executable(compiler) or { compiler })
	version := cmdexec.run(path, ['--version'])
	return '${path}\t${v3_shared_file_identity(path)}\t${version.exit_code}\t${version.output.replace('\n', ' ')}'
}

// Compiler-specific interfaces are regenerated before looking here. Sharing an
// object requires identical C, C compiler, flags, and native dependency contents.
fn v3_shared_module_object(entry modulecache.Entry, compiler V3CachedObjectCompiler, source string, compile_signature string, dependencies map[string]string) modulecache.Entry {
	return v3_shared_module_object_for_hash(entry, compiler, sha256.hexhash(source), compile_signature, dependencies)
}

// v3_shared_module_object_for_hash is v3_shared_module_object for a caller that has
// hashed the C already, and keeps the hash for v3_publish_shared_module_object.
fn v3_shared_module_object_for_hash(entry modulecache.Entry, compiler V3CachedObjectCompiler, source_hash string, compile_signature string, dependencies map[string]string) modulecache.Entry {
	identity := if compiler.identity != '' {
		compiler.identity
	} else {
		v3_shared_c_compiler_identity(compiler.path)
	}
	mut inputs := ['shared-module-object-v1', os.file_name(entry.c_source), source_hash, identity,
		compile_signature, os.getwd()]
	mut paths := dependencies.keys()
	paths.sort()
	for path in paths {
		// V interfaces already contributed their declarations to the generated C.
		// Their configuration directory changes when the V compiler is rebuilt.
		if path.ends_with('.vh') && os.dir(path) == os.dir(entry.object) { continue }
		inputs << [path, dependencies[path]]
	}
	key := sha256.hexhash(inputs.join('\x00'))
	base := os.join_path(os.dir(os.dir(entry.object)), '.shared_objects', key)
	return modulecache.Entry{
		object:       base + '.o'
		c_source:     base + '.c'
		object_stamp: base + '.sha256'
	}
}

fn v3_restore_shared_module_object(pooled modulecache.Entry, entry modulecache.Entry) bool {
	stamp := os.read_file(pooled.object_stamp) or { return false }
	object := os.read_file(pooled.object) or { return false }
	c_source := os.read_file(pooled.c_source) or { return false }
	if stamp != 'object=${sha256.hexhash(object)}\nsource=${sha256.hexhash(c_source)}\n' {
		return false
	}
	v3_link_cache_artifact(pooled.c_source, entry.c_source) or { return false }
	v3_link_cache_artifact(pooled.object, entry.object) or { return false }
	return true
}

fn v3_publish_shared_module_object(pooled modulecache.Entry, entry modulecache.Entry) {
	v3_publish_shared_module_object_for_hash(pooled, entry, '')
}

// v3_publish_shared_module_object_for_hash publishes the pooled copy of a module object.
// `source_hash` is the hash of the C that entry.c_source holds, when the caller knows it:
// the pooled source is a link to that file, so the hash is the one the stamp records, and
// the C (hundreds of KB) is not read and hashed a second time.
fn v3_publish_shared_module_object_for_hash(pooled modulecache.Entry, entry modulecache.Entry, source_hash string) {
	// This secondary index is optional: a failed publication must not invalidate
	// the successfully compiled object in the current compiler's configuration.
	os.mkdir_all(os.dir(pooled.object)) or { return }
	v3_link_cache_artifact(entry.c_source, pooled.c_source) or { return }
	v3_link_cache_artifact(entry.object, pooled.object) or { return }
	object := os.read_file(pooled.object) or { return }
	mut recorded_source_hash := source_hash
	if recorded_source_hash == '' {
		c_source := os.read_file(pooled.c_source) or { return }
		recorded_source_hash = sha256.hexhash(c_source)
	}
	tmp := '${pooled.object_stamp}.tmp.${tempname.unique_token()}'
	os.write_file(tmp, 'object=${sha256.hexhash(object)}\nsource=${recorded_source_hash}\n') or {
		os.rm(tmp) or {}
		return
	}
	os.mv(tmp, pooled.object_stamp) or { os.rm(tmp) or {} }
}

fn v3_link_cache_artifact(source string, destination string) ! {
	tmp := '${destination}.tmp.${tempname.unique_token()}'
	defer { os.rm(tmp) or {} }
	os.link(source, tmp) or { os.cp(source, tmp)! }
	os.mv(tmp, destination)!
}
