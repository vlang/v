module c

import os
import rand
import strings
import v.ast
import v.pref

fn (g &Gen) should_really_embed_file() bool {
	if 'embed_only_metadata' in g.pref.compile_defines {
		return false
	}
	return true
}

fn (g &Gen) should_use_incbin_embed() bool {
	if g.pref.out_name.ends_with('.c') || g.pref.generate_c_project != '' || g.pref.should_output_to_stdout() {
		return false
	}
	if g.pref.is_o {
		return false
	}
	if !g.should_really_embed_file() {
		return false
	}
	if g.pref.ccompiler_type == .msvc {
		return false
	}
	if g.pref.os == .windows && pref.get_host_os() != .windows {
		return false
	}
	if g.pref.ccompiler_type in [.gcc, .clang, .mingw, .emcc] {
		return true
	}
	if g.pref.ccompiler_type == .tinyc {
		// TCC can't assemble .S files itself, so we need a system assembler.
		// If none is found, fall back to C hex arrays (slower compile, but correct).
		if _ := os.find_abs_path_of_executable('clang') {
			return true
		} else if _ := os.find_abs_path_of_executable('gcc') {
			return true
		} else if _ := os.find_abs_path_of_executable('cc') {
			return true
		} else if _ := os.find_abs_path_of_executable('as') {
			return true
		}
		return false
	}
	return false
}

fn (mut g Gen) handle_embedded_files_finish() {
	if g.embedded_files.len > 0 {
		if g.should_really_embed_file() {
			g.gen_embedded_data()
		}
		g.gen_embedded_metadata()
	}
}

// gen_embed_file_struct generates C code for `$embed_file('...')` calls.
fn (mut g Gen) gen_embed_file_init(mut node ast.ComptimeCall) {
	$if trace_embed_file ? {
		eprintln('> gen_embed_file_init ${node.embed_file.apath}')
	}
	if g.should_really_embed_file() {
		file_bytes := os.read_bytes(node.embed_file.apath) or {
			panic('unable to read file: "${node.embed_file.rpath}')
		}

		if node.embed_file.compression_type == 'none' {
			node.embed_file.bytes = file_bytes
		} else {
			cache_dir := os.join_path(os.vmodules_dir(), '.cache', 'embed_file')
			cache_key := rand.ulid()
			// cache_key := md5.hexhash(node.embed_file.apath)
			if !os.exists(cache_dir) {
				os.mkdir_all(cache_dir) or { panic(err) }
			}
			cache_path := os.join_path(cache_dir, cache_key)

			vexe := pref.vexe_path()
			compress_cmd := '${os.quoted_path(vexe)} compress ${node.embed_file.compression_type} ${os.quoted_path(node.embed_file.apath)} ${os.quoted_path(cache_path)}'
			$if trace_embed_file ? {
				eprintln('> gen_embed_file_init, compress_cmd: ${compress_cmd}')
			}
			result := os.execute(compress_cmd)
			if result.exit_code != 0 {
				eprintln('unable to compress file "${node.embed_file.rpath}": ${result.output}')
				node.embed_file.bytes = file_bytes
			} else {
				compressed_bytes := os.read_bytes(cache_path) or {
					eprintln('unable to read compressed file')
					{
					}
					[]u8{}
				}
				node.embed_file.is_compressed = compressed_bytes.len > 0
					&& compressed_bytes.len < file_bytes.len
				if g.should_use_incbin_embed() && node.embed_file.is_compressed {
					node.embed_file.compressed_temp_path = cache_path
				} else {
					os.rm(cache_path) or {}
				}
				node.embed_file.bytes = if node.embed_file.is_compressed {
					compressed_bytes
				} else {
					file_bytes
				}
			}
		}
		if node.embed_file.bytes.len > 5242880 {
			if !g.should_use_incbin_embed() {
				eprintln('embedding of files >= ~5MB is currently not well supported')
			}
		}
		node.embed_file.len = file_bytes.len
	}
	ef_idx := node.embed_file.hash()
	g.write('_v_embed_file_metadata( ${ef_idx}U )')
	g.file.embedded_files << node.embed_file
	$if trace_embed_file ? {
		eprintln('> gen_embed_file_init => _v_embed_file_metadata(${ef_idx:-25}) | ${node.embed_file.apath:-50} | compression: ${node.embed_file.compression_type} | len: ${node.embed_file.len}')
	}
}

// gen_embedded_metadata embeds all of the deduplicated metadata in g.embedded_files, into the V target executable,
// into a single generated function _v_embed_file_metadata, that accepts a hash of the absolute path of the embedded
// files.
fn (mut g Gen) gen_embedded_metadata() {
	if g.pref.parallel_cc {
		g.extern_out.writeln('extern v__embed_file__EmbedFileData _v_embed_file_metadata(u64 ef_hash);')
	}
	g.embedded_data.writeln('v__embed_file__EmbedFileData _v_embed_file_metadata(u64 ef_hash) {')
	g.embedded_data.writeln('\tv__embed_file__EmbedFileData res;')
	g.embedded_data.writeln('\tmemset(&res, 0, sizeof(res));')
	g.embedded_data.writeln('\tswitch(ef_hash) {')
	for emfile in g.embedded_files {
		ef_idx := emfile.hash()
		g.embedded_data.writeln('\t\tcase ${ef_idx}U: {')
		g.embedded_data.writeln('\t\t\tres.path = ${ctoslit(emfile.rpath)};')
		if g.should_really_embed_file() {
			// apath is not needed in production and may leak information
			g.embedded_data.writeln('\t\t\tres.apath = ${ctoslit('')};')
		} else {
			g.embedded_data.writeln('\t\t\tres.apath = ${ctoslit(emfile.apath)};')
		}
		if g.should_really_embed_file() {
			// use function generated in Gen.gen_embedded_data()
			if emfile.is_compressed {
				g.embedded_data.writeln('\t\t\tres.compression_type = ${ctoslit(emfile.compression_type)};')
				g.embedded_data.writeln('\t\t\tres.compressed = v__embed_file__find_index_entry_by_path((voidptr)_v_embed_file_index, ${ctoslit(emfile.rpath)}, ${ctoslit(emfile.compression_type)})->data;')
				g.embedded_data.writeln('\t\t\tres.compressed_len = ${emfile.bytes.len};')
				g.embedded_data.writeln('\t\t\tres.uncompressed = NULL;')
			} else {
				g.embedded_data.writeln('\t\t\tres.uncompressed = v__embed_file__find_index_entry_by_path((voidptr)_v_embed_file_index, ${ctoslit(emfile.rpath)}, ${ctoslit(emfile.compression_type)})->data;')
			}
		} else {
			g.embedded_data.writeln('\t\t\tres.uncompressed = NULL;')
		}
		g.embedded_data.writeln('\t\t\tres.free_compressed = 0;')
		g.embedded_data.writeln('\t\t\tres.free_uncompressed = 0;')
		if g.should_really_embed_file() {
			g.embedded_data.writeln('\t\t\tres.len = ${emfile.len};')
		} else {
			file_size := os.file_size(emfile.apath)
			if file_size > 5242880 {
				if !g.should_use_incbin_embed() {
					eprintln('Warning: embedding of files >= ~5MB is currently not supported')
				}
			}
			g.embedded_data.writeln('\t\t\tres.len = ${file_size};')
		}
		g.embedded_data.writeln('\t\t\tbreak;')
		g.embedded_data.writeln('\t\t} // case ${ef_idx}')
	}
	g.embedded_data.writeln('\t\tdefault: builtin___v_panic(_S("unknown embed file"));')
	g.embedded_data.writeln('\t} // switch')
	g.embedded_data.writeln('\treturn res;')
	g.embedded_data.writeln('}')
}

// gen_embedded_data embeds data into the V target executable.
fn (mut g Gen) gen_embedded_data() {
	/*
	TODO implement support for large files - right now the setup has problems
	// with even just 10 - 50 MB files - the problem is both in V and C compilers.
	// maybe we need to write to separate files or have an external tool for large files
	// like the `rcc` tool in Qt?
	*/
	// Declare the index before any generated helper references it, to keep MSVC happy.
	index_len := g.embedded_files.len + 1
	g.embedded_data.writeln('static const v__embed_file__EmbedFileIndexEntry _v_embed_file_index[${index_len}];')
	if g.should_use_incbin_embed() {
		// .incbin path: extern declarations + .S file content
		for emfile in g.embedded_files {
			ef_hash := emfile.hash()
			g.embedded_data.writeln('extern const unsigned char _v_embed_blob_${ef_hash}[];')
			g.gen_embedded_asm_file(emfile)
		}
	} else {
		// C hex array path: current behavior
		for i, emfile in g.embedded_files {
			g.embedded_data.write_string('static const unsigned char _v_embed_blob_${i}[${emfile.bytes.len}] = {\n    ')
			for j := 0; j < emfile.bytes.len; j++ {
				b := emfile.bytes[j].hex()
				if j < emfile.bytes.len - 1 {
					g.embedded_data.write_string('0x${b},')
				} else {
					g.embedded_data.write_string('0x${b}')
				}
				if 0 == ((j + 1) % 16) {
					g.embedded_data.write_string('\n    ')
				}
			}
			g.embedded_data.writeln('\n};')
		}
	}
	g.embedded_data.writeln('')
	g.embedded_data.writeln('static const v__embed_file__EmbedFileIndexEntry _v_embed_file_index[${index_len}] = {')
	for i, emfile in g.embedded_files {
		blob_name := if g.should_use_incbin_embed() { emfile.hash().str() } else { i.str() }
		g.embedded_data.writeln('\t{${i}, { .str=(byteptr)("${cestring(emfile.rpath)}"), .len=${emfile.rpath.len}, .is_lit=1 }, { .str=(byteptr)("${cestring(emfile.compression_type)}"), .len=${emfile.compression_type.len}, .is_lit=1 }, (byteptr)_v_embed_blob_${blob_name}},')
	}
	g.embedded_data.writeln('\t{-1, { .str=(byteptr)(""), .len=0, .is_lit=1 }, { .str=(byteptr)(""), .len=0, .is_lit=1 }, NULL}')
	g.embedded_data.writeln('};')
	// see vlib/v/embed_file/embed_file.v, find_index_entry_by_id/2 and find_index_entry_by_path/2
}

fn (mut g Gen) gen_embedded_asm_file(emfile ast.EmbeddedFile) {
	mut incbin_path := emfile.apath
	if emfile.is_compressed {
		incbin_path = emfile.compressed_temp_path
	}

	ef_hash := emfile.hash()
	asm_filename := '_v_embed_blob_${ef_hash}.S'
	mut sb := strings.new_builder(512)
	sb.writeln('// V embedded file: ${emfile.rpath} (hash ${ef_hash}, uncompressed size ${emfile.len})')
	sb.writeln('#if defined(__APPLE__)')
	sb.writeln('    .section __TEXT,__const')
	sb.writeln('    .globl __v_embed_blob_${ef_hash}')
	sb.writeln('__v_embed_blob_${ef_hash}:')
	sb.writeln('#else')
	sb.writeln('    .section .rodata')
	sb.writeln('    .globl _v_embed_blob_${ef_hash}')
	sb.writeln('    .type _v_embed_blob_${ef_hash}, @object')
	sb.writeln('_v_embed_blob_${ef_hash}:')
	sb.writeln('#endif')
	sb.writeln('    .incbin "${cestring(incbin_path)}"')
	sb.writeln('#if !defined(__APPLE__)')
	sb.writeln('    .size _v_embed_blob_${ef_hash}, ${emfile.bytes.len}')
	sb.writeln('#endif')

	g.embedded_asm[asm_filename] = sb.str()

	if emfile.is_compressed {
		g.embedded_temp_files << emfile.compressed_temp_path
	}
}
