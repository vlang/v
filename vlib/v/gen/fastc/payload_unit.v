module fastc

import os
import v.cmdexec

// fastc_payload_unit_min_bytes is the embedded-file data from which a split build
// compiles it as a separate, cached object.
const fastc_payload_unit_min_bytes = 1 << 20

// FastcPayloadUnit is the embedded-file data of a split build, compiled apart from the
// generated code. It names no header, so its object depends only on its own text.
pub struct FastcPayloadUnit {
pub:
	// units declares the payloads `extern` in the first unit too.
	units FastcUnitLayout
	// pieces define every payload; there are none when the build keeps them in place.
	// They stay apart: the payloads of a large program are tens of megabytes.
	pieces []string
}

// source returns the text of the payload unit.
pub fn (unit &FastcPayloadUnit) source() string {
	return unit.pieces.join('')
}

// fastc_split_payload_unit moves the embedded files out of the first unit of a split
// build. Their escaped bytes made that unit the slowest TinyCC job of a large program.
pub fn fastc_split_payload_unit(pieces []string, units FastcUnitLayout, jobs int) FastcPayloadUnit {
	mut payload_indexes := []int{}
	mut size := 0
	for e, text in units.define_texts {
		if e < units.extern_texts.len && text.starts_with('const u8 __vf_embed_') {
			payload_indexes << e
			size += text.len
		}
	}
	if size < fastc_payload_unit_min_bytes
		|| fastc_c_unit_plan('', pieces, units, jobs).paths.len < 2 {
		return FastcPayloadUnit{
			units: units
		}
	}
	mut define_texts := units.define_texts.clone()
	mut source := []string{cap: payload_indexes.len + 1}
	source << 'typedef unsigned char u8;\n'
	for e in payload_indexes {
		source << units.define_texts[e]
		define_texts[e] = units.extern_texts[e]
	}
	return FastcPayloadUnit{
		units:  FastcUnitLayout{
			...units
			define_texts: define_texts
		}
		pieces: source
	}
}

// FastcPayloadObject is the compiled payload unit: an object path, or the TinyCC error.
pub struct FastcPayloadObject {
pub:
	path  string
	error string
}

// fastc_payload_object returns the object of a payload unit, from the unit cache when
// `use_cache` is set and the same TinyCC and arguments already compiled that text.
// Without a payload there is no object. The text is joined only to compile it.
// The cache keeps one payload object per `output`: tens of megabytes of embedded files
// change with the program's assets, and a new object replaces the previous one.
pub fn fastc_payload_object(tcc string, base_args []string, pieces []string, build_dir string, use_cache bool, output string) FastcPayloadObject {
	if pieces.len == 0 {
		return FastcPayloadObject{}
	}
	configuration := fastc_unit_cache_configuration(tcc, base_args)
	mut key := fastc_unit_cache_hash('fastc-payload-v2\x00${configuration}', u64(0x9b05688c2b3e6c1f))
	for piece in pieces {
		key = fastc_unit_cache_hash(piece, key)
	}
	cache_dir := os.join_path_single(os.vtmp_dir(), 'v3_fastc_unit_cache')
	owner_prefix := fastc_payload_cache_prefix(output)
	cached := os.join_path_single(cache_dir, '${owner_prefix}${key.hex()}.o')
	if use_cache && os.is_file(cached) && os.file_size(cached) > 0 {
		return FastcPayloadObject{
			path: cached
		}
	}
	path := os.join_path_single(build_dir, 'payload.c')
	object := os.join_path_single(build_dir, 'payload.o')
	os.write_file(path, pieces.join('')) or {
		return FastcPayloadObject{
			error: err.msg()
		}
	}
	mut args := base_args.clone()
	args << ['-c', path, '-o', object]
	result := cmdexec.run(tcc, args)
	if result.exit_code != 0 || !os.is_file(object) {
		return FastcPayloadObject{
			error: result.output
		}
	}
	if use_cache {
		os.mkdir_all(cache_dir) or {
			return FastcPayloadObject{
				path: object
			}
		}
		fastc_publish_unit_cache(FastcUnitCacheEntry{
			object:       object
			cache_object: cached
		})
		fastc_remove_stale_payload_objects(cache_dir, owner_prefix, cached)
	}
	return FastcPayloadObject{
		path: object
	}
}

// fastc_payload_cache_prefix names the cached payload objects of one output.
fn fastc_payload_cache_prefix(output string) string {
	owner := fastc_unit_cache_hash(os.abs_path(output), u64(0x3c6ef372fe94f82b))
	return 'payload_${owner.hex()}_'
}

// fastc_remove_stale_payload_objects removes the payload objects of the same output other
// than `keep`. Concurrent builds of one output race on the output itself as well.
fn fastc_remove_stale_payload_objects(cache_dir string, prefix string, keep string) {
	if !os.is_file(keep) {
		return
	}
	keep_name := os.file_name(keep)
	for name in os.ls(cache_dir) or { return } {
		if name != keep_name && name.starts_with(prefix) && name.ends_with('.o') {
			os.rm(os.join_path_single(cache_dir, name)) or {}
		}
	}
}
