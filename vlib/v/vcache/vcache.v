module vcache

import os
import hash

// Using a 2 level cache, ensures a more even distribution of cache entries,
// so there will not be cramped folders that contain many thousands of them.
// Most filesystems can not handle performantly such folders, and slow down.
// The first level will contain a max of 256 folders, named from 00/ to ff/.
// Each of them will contain many entries, but hopefully < 1000.
// Note: using a hash here, makes the cache storage immune to special
// characters in the keys, like quotes, spaces and so on.
// Cleanup of the cache is simple: just delete the $VCACHE folder.
// The cache tree will look like this:
// │ $VCACHE
// │ ├── README.md <-- a short description of the folder's purpose.
// │ ├── 0f
// │ │   ├── 0f004f983ab9c487b0d7c1a0a73840a5.txt
// │ │   ├── 0f599edf5e16c2756fbcdd4c865087ac.description.txt <-- build details
// │ │   └── 0f599edf5e16c2756fbcdd4c865087ac.vh
// │ ├── 29
// │ │   ├── 294717dd02a1cca5f2a0393fca2c5c22.o
// │ │   └── 294717dd02a1cca5f2a0393fca2c5c22.description.txt <-- build details
// │ ├── 62
// │ │   └── 620d60d6b81fdcb3cab030a37fd86996.h
// │ └── 76
// │     └── 7674f983ab9c487b0d7c1a0ad73840a5.c
pub struct CacheManager {
pub:
	basepath       string
	original_vopts string
pub mut:
	vopts   string
	k2cpath map[string]string // key -> filesystem cache path for the object
}

pub fn new_cache_manager(opts []string) CacheManager {
	mut vcache_basepath := os.getenv('VCACHE')
	if vcache_basepath == '' {
		vcache_basepath = os.join_path(os.vmodules_dir(), 'cache')
	}
	nlog(@FN, 'vcache_basepath: $vcache_basepath\n         opts: $opts\n      os.args: ${os.args.join(' ')}')
	dlog(@FN, 'vcache_basepath: $vcache_basepath | opts:\n     $opts')
	if !os.is_dir(vcache_basepath) {
		os.mkdir_all(vcache_basepath, mode: 0o700) or { panic(err) } // keep directory private
		dlog(@FN, 'created folder:\n    $vcache_basepath')
	}
	readme_file := os.join_path(vcache_basepath, 'README.md')
	if !os.is_file(readme_file) {
		readme_content := 'This folder contains cached build artifacts from the V build system.
		|You can safely delete it, if it is getting too large.
		|It will be recreated the next time you compile something with V.
		|You can change its location with the VCACHE environment variable.
		'.strip_margin()
		os.write_file(readme_file, readme_content) or { panic(err) }
		dlog(@FN, 'created readme_file:\n    $readme_file')
	}
	mut deduped_opts := map[string]bool{}
	for o in opts {
		deduped_opts[o] = true
	}
	deduped_opts_keys := deduped_opts.keys().filter(it != '' && !it.starts_with("['gcboehm', "))
	// TODO: do not filter the gcboehm options here, instead just start `v build-module vlib/builtin` without the -d gcboehm etc.
	// Note: the current approach of filtering the gcboehm keys may interfere with (potential) other gc modes.
	original_vopts := deduped_opts_keys.join('|')
	return CacheManager{
		basepath: vcache_basepath
		vopts: original_vopts
		original_vopts: original_vopts
	}
}

// set_temporary_options can be used to add temporary options to the hash salt
// Note: these can be changed easily with another .set_temporary_options call
// without affecting the .original_vopts
pub fn (mut cm CacheManager) set_temporary_options(new_opts []string) {
	cm.vopts = cm.original_vopts + '#' + new_opts.join('|')
	dlog(@FN, 'cm.vopts:\n     $cm.vopts')
}

pub fn (mut cm CacheManager) key2cpath(key string) string {
	mut cpath := cm.k2cpath[key] or { '' }
	if cpath == '' {
		hk := cm.vopts + key
		a := hash.sum64_string(hk, 5).hex_full()
		b := hash.sum64_string(hk, 7).hex_full()
		khash := a + b
		prefix := khash[0..2]
		cprefix_folder := os.join_path(cm.basepath, prefix)
		cpath = os.join_path(cprefix_folder, khash)
		if !os.is_dir(cprefix_folder) {
			os.mkdir_all(cprefix_folder) or { panic(err) }
		}
		dlog(@FN, 'new hk')
		dlog(@FN, '       key: $key')
		dlog(@FN, '     cpath: $cpath')
		dlog(@FN, '  cm.vopts:\n     $cm.vopts')
		cm.k2cpath[key] = cpath
	}
	dlog(@FN, 'key: ${key:-30} => cpath: $cpath')
	return cpath
}

pub fn (mut cm CacheManager) postfix_with_key2cpath(postfix string, key string) string {
	prefix := cm.key2cpath(key)
	res := prefix + postfix
	return res
}

pub fn (mut cm CacheManager) exists(postfix string, key string) ?string {
	fpath := cm.postfix_with_key2cpath(postfix, key)
	dlog(@FN, 'postfix: $postfix | key: $key | fpath: $fpath')
	if !os.exists(fpath) {
		return error('does not exist yet')
	}
	return fpath
}

pub fn (mut cm CacheManager) save(postfix string, key string, content string) ?string {
	fpath := cm.postfix_with_key2cpath(postfix, key)
	os.write_file(fpath, content)?
	dlog(@FN, 'postfix: $postfix | key: $key | fpath: $fpath')
	return fpath
}

pub fn (mut cm CacheManager) load(postfix string, key string) ?string {
	fpath := cm.exists(postfix, key)?
	content := os.read_file(fpath)?
	dlog(@FN, 'postfix: $postfix | key: $key | fpath: $fpath')
	return content
}

[if trace_usecache ?]
pub fn dlog(fname string, s string) {
	xlog(fname, s)
}

[if trace_usecache_n ?]
fn nlog(fname string, s string) {
	xlog(fname, s)
}

fn xlog(fname string, s string) {
	pid := unsafe { mypid() }
	if fname[0] != `|` {
		eprintln('> VCache | pid: $pid | CacheManager.$fname $s')
	} else {
		eprintln('> VCache | pid: $pid $fname $s')
	}
}

[unsafe]
fn mypid() int {
	mut static pid := 0
	if pid == 0 {
		pid = os.getpid()
	}
	return pid
}
