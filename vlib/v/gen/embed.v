module gen

import os
import v.ast

// gen_embed_file_struct generates C code for `$embed_file('...')` calls.
fn (mut g Gen) gen_embed_file_init(node ast.ComptimeCall) {
	g.writeln('(embed__EmbeddedData){')
	g.writeln('\t.path = ${ctoslit(node.embed_file.rpath)},')
	g.writeln('\t.apath = ${ctoslit(node.embed_file.apath)},')
	file_size := os.file_size(node.embed_file.apath)
	if file_size > 5242880 {
		eprintln('Warning: embedding of files >= ~5MB is currently not supported')
	}
	if g.pref.is_prod {
		// Use function generated in Gen.gen_embedded_data()
		g.writeln('\t.compressed = _v_embed_locate_data(${ctoslit(node.embed_file.apath)}),')
	}
	g.writeln('\t.len = $file_size')
	g.writeln('} // $' + 'embed_file("$node.embed_file.apath")')
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
		g.embedded_data.write('static const unsigned char _v_embed_blob_$i[$fbytes.len] = {\n    ')
		for j := 0; j < fbytes.len; j++ {
			b := fbytes[j].hex()
			if j < fbytes.len - 1 {
				g.embedded_data.write('0x$b,')
			} else {
				g.embedded_data.write('0x$b')
			}
			if 0 == ((j + 1) % 16) {
				g.embedded_data.write('\n    ')
			}
		}
		g.embedded_data.writeln('\n};')
	}
	g.embedded_data.writeln('')
	g.embedded_data.writeln('const struct _v_embed {')
	g.embedded_data.writeln('\tstring id;')
	g.embedded_data.writeln('\tbyteptr data;')
	g.embedded_data.writeln('}')
	g.embedded_data.writeln('_v_embedded_data[] = {')
	for i, emfile in g.embedded_files {
		g.embedded_data.writeln('\t{${ctoslit(emfile.rpath)}, _v_embed_blob_$i},')
	}
	g.embedded_data.writeln('\t{_SLIT(""), NULL}')
	g.embedded_data.writeln('};')
	// See `vlib/v/gen/comptime.v` -> Gen.comptime_call_embed_file(), where this is called at runtime.
	// Generate function to locate the data.
	g.embedded_data.writeln('
// function to locate embedded data by a vstring
byteptr _v_embed_locate_data(string id) {
	const struct _v_embed *ve;
	for (ve = _v_embedded_data; !string_eq(ve->id, _SLIT("")) && ve->data != NULL; ve++) {
		if (string_eq(ve->id, id)) {
			return (byteptr) ve->data;
		}
	}
	return NULL;
}')
}
