module sourcemap

import io
import os
import x.json2

const (
	source_map_version = 3
)

type SourceMapJson = map[string]json2.Any

pub struct SourceMap {
pub mut:
	version                int               [json: version]
	file                   string            [json: file]
	source_root            string            [json: source_root]
	sources                Sets              [json: sources]
	sources_content        map[string]string
	names                  Sets
	mappings               Mappings
	sources_content_inline bool
}

struct StringWriter {
pub mut:
	bytes []u8
}

pub fn new_sourcemap(file string, source_root string, sources_content_inline bool) SourceMap {
	return SourceMap{
		version: sourcemap.source_map_version
		file: file
		source_root: source_root
		mappings: new_mappings()
		sources_content_inline: sources_content_inline
	}
}

// Add a single mapping from original source line and column to the generated source's line and column for this source map being created.
pub fn (mut sm SourceMap) add_mapping(source_name string, source_position SourcePositionType, gen_line u32, gen_column u32, name string) {
	if source_name.len == 0 {
		panic('add_mapping, source_name should not be ""')
	}

	sources_ind := sm.sources.add(source_name)

	names_ind := if name.len != 0 {
		NameIndexType(IndexNumber(sm.names.add(name)))
	} else {
		NameIndexType(Empty{})
	}
	sm.mappings.add_mapping(gen_line, gen_column, sources_ind, source_position, names_ind)
}

// Add multiple mappings from the same source
pub fn (mut sm SourceMap) add_mapping_list(source_name string, mapping_list []MappingInput) ? {
	if source_name.len == 0 {
		panic('add_mapping_list, source_name should not be ""')
	}

	sources_ind := sm.sources.add(source_name)

	for mapping in mapping_list {
		names_ind := if mapping.name.len != 0 {
			NameIndexType(IndexNumber(sm.names.add(mapping.name)))
		} else {
			NameIndexType(Empty{})
		}
		sm.mappings.add_mapping(mapping.gen_line, mapping.gen_column, sources_ind, mapping.source_position,
			names_ind)
	}
}

// Set the source content for a source file.
pub fn (mut sm SourceMap) set_source_content(source_name string, source_content string) {
	sm.sources_content[source_name] = source_content
}

fn (mut sm SourceMap) export_mappings(mut writer io.Writer) {
	sm.mappings.export_mappings(mut writer) or { panic('export failed') }
}

fn (mut sm SourceMap) export_mappings_string() string {
	mut output := StringWriter{}

	sm.mappings.export_mappings(mut output) or { panic('export failed') }
	return output.bytes.bytestr()
}

// create a JSON representing the sourcemap
// Sourcemap Specs http://sourcemaps.info/spec.html
pub fn (mut sm SourceMap) to_json() SourceMapJson {
	mut source_map_json := map[string]json2.Any{}
	source_map_json['version'] = sm.version
	if sm.file != '' {
		source_map_json['file'] = json2.Any(sm.file)
	}
	if sm.source_root != '' {
		source_map_json['sourceRoot'] = json2.Any(sm.source_root)
	}
	mut sources_json := []json2.Any{}
	mut sources_content_json := []json2.Any{}
	for source_file, _ in sm.sources.value {
		sources_json << source_file
		if source_file in sm.sources_content {
			sources_content_json << sm.sources_content[source_file]
		} else {
			if sm.sources_content_inline {
				if source_file_content := os.read_file(source_file) {
					sources_content_json << source_file_content
				} else {
					sources_content_json << json2.null
				}
			} else {
				sources_content_json << json2.null
			}
		}
	}
	source_map_json['sources'] = json2.Any(sources_json)
	source_map_json['sourcesContent'] = json2.Any(sources_content_json)

	mut names_json := []json2.Any{}
	for name, _ in sm.names.value {
		names_json << name
	}
	source_map_json['names'] = json2.Any(names_json)
	source_map_json['mappings'] = sm.export_mappings_string()
	return source_map_json
}

fn (mut w StringWriter) write(buf []u8) ?int {
	w.bytes << buf
	return buf.len
}
