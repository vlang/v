module sourcemap

struct V3 {
	SourceMap
pub:
	sections []Section @[json: sections]
}

struct Offset {
pub mut:
	line   int @[json: line]
	column int @[json: column]
}

struct Section {
pub mut:
	offset     Offset    @[json: offset]
	source_map SourceMap @[json: map]
}

struct Generator {
mut:
	file string
	// source_root string
	sections []Section
}

pub fn generate_empty_map() &Generator {
	return &Generator{}
}

pub fn (mut g Generator) add_map(file string, source_root string, sources_content_inline bool, line_offset int,
	column_offset int) &SourceMap {
	source_map := new_sourcemap(file, source_root, sources_content_inline)

	offset := Offset{
		line:   line_offset
		column: column_offset
	}

	g.sections << Section{
		offset:     offset
		source_map: source_map
	}

	return &source_map
}
