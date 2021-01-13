module gen

import os

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
	for i, path in g.embedded_files {
		fbytes := os.read_bytes(path) or { panic('Error while embedding file: $err') }
		g.embedded_data.write('static const unsigned int _v_embed_blob_$i[] = {')
		for j := 1; j < fbytes.len; j++ {
			b := int(fbytes[j]).str()
			if j == fbytes.len - 1 {
				g.embedded_data.write('$b')
			} else {
				g.embedded_data.write('$b, ')
			}
		}
		g.embedded_data.writeln(' };')
	}
	g.embedded_data.writeln('')
	g.embedded_data.writeln('const struct _v_embed {')
	g.embedded_data.writeln('\tstring id;')
	g.embedded_data.writeln('\tbyteptr data;')
	g.embedded_data.writeln('} _v_embedded_data[] = {')
	for i, path in g.embedded_files {
		g.embedded_data.writeln('\t{_SLIT("$path"), _v_embed_blob_$i},')
	}
	g.embedded_data.writeln('\t{_SLIT("EOE"), NULL}')
	g.embedded_data.writeln('};')
	// See `vlib/v/gen/comptime.v` -> Gen.comptime_call_embed_file(), where this is called at runtime.
	// Generate function to locate the data.
	g.embedded_data.writeln('
// function to locate embedded data by a vstring
byteptr _v_embed_locate_data(string id) {
	const struct _v_embed *ve;
	for (ve = _v_embedded_data; !string_eq(ve->id,_SLIT("EOE")); ve++) {
		if (string_eq(ve->id, id)) {
			return (byteptr) ve->data;
		}
	}
	return NULL;
}')
}
