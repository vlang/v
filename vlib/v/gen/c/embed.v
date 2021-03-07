module c

import os
import v.ast

fn (mut g Gen) embed_file_is_prod_mode() bool {
	if g.pref.is_prod || 'debug_embed_file_in_prod' in g.pref.compile_defines {
		return true
	}
	return false
}

// gen_embed_file_struct generates C code for `$embed_file('...')` calls.
fn (mut g Gen) gen_embed_file_init(node ast.ComptimeCall) {
	g.writeln('(v__embed_file__EmbedFileData){')
	g.writeln('\t\t.path = ${ctoslit(node.embed_file.rpath)},')
	g.writeln('\t\t.apath = ${ctoslit(node.embed_file.apath)},')
	file_size := os.file_size(node.embed_file.apath)
	if file_size > 5242880 {
		eprintln('Warning: embedding of files >= ~5MB is currently not supported')
	}
	if g.embed_file_is_prod_mode() {
		// Use function generated in Gen.gen_embedded_data()
		g.writeln('\t\t.compressed = v__embed_file__find_index_entry_by_path((voidptr)_v_embed_file_index, ${ctoslit(node.embed_file.rpath)})->data,')
	}
	g.writeln('\t\t.uncompressed = NULL,')
	g.writeln('\t\t.free_compressed = 0,')
	g.writeln('\t\t.free_uncompressed = 0,')
	g.writeln('\t\t.len = $file_size')
	g.writeln('} // \$embed_file("$node.embed_file.apath")')
}

// gen_embedded_data embeds data into the V target executable.
fn (mut g Gen) gen_embedded_data() {
	/*
	TODO implement compression.
	See also the vlib/embed module where decompression should occur.
	*/
	/*
	TODO implement support for large files - right now the setup has problems
	// with even just 10 - 50 MB files - the problem is both in V and C compilers.
	// maybe we need to write to separate files or have an external tool for large files
	// like the `rcc` tool in Qt?
	*/
	for i, emfile in g.embedded_files {
		fbytes := os.read_bytes(emfile.apath) or { panic('Error while embedding file: $err') }
		g.embedded_data.write_string('static const unsigned char _v_embed_blob_$i[$fbytes.len] = {\n    ')
		for j := 0; j < fbytes.len; j++ {
			b := fbytes[j].hex()
			if j < fbytes.len - 1 {
				g.embedded_data.write_string('0x$b,')
			} else {
				g.embedded_data.write_string('0x$b')
			}
			if 0 == ((j + 1) % 16) {
				g.embedded_data.write_string('\n    ')
			}
		}
		g.embedded_data.writeln('\n};')
	}
	g.embedded_data.writeln('')
	g.embedded_data.writeln('const v__embed_file__EmbedFileIndexEntry _v_embed_file_index[] = {')
	for i, emfile in g.embedded_files {
		g.embedded_data.writeln('\t{$i, ${ctoslit(emfile.rpath)}, _v_embed_blob_$i},')
	}
	g.embedded_data.writeln('\t{-1, _SLIT(""), NULL}')
	g.embedded_data.writeln('};')
	// see vlib/v/embed_file/embed_file.v, find_index_entry_by_id/2 and find_index_entry_by_path/2
}
