module sourcemap

import v.gen.js.sourcemap.vlq
import io

struct Empty {}

pub struct SourcePosition {
	source_line   u32
	source_column u32
}

type IndexNumber = u32
type SourcePositionType = Empty | SourcePosition
type NameIndexType = Empty | IndexNumber

struct GenPosition {
	gen_line   u32
	gen_column u32
}

struct MappingInput {
	GenPosition
	name            string
	source_position SourcePositionType
}

struct Mapping {
	GenPosition
	sources_ind     u32
	names_ind       NameIndexType
	source_position SourcePositionType
}

struct Mappings {
mut:
	sorted bool
	last   Mapping
	values []Mapping
}

fn new_mappings() Mappings {
	return Mappings{
		last: Mapping{
			GenPosition: GenPosition{
				gen_column: 0
				gen_line: 0
			}
		}
		sorted: true
	}
}

// Add the given source mapping
fn (mut m Mappings) add_mapping(gen_line u32, gen_column u32, sources_ind u32, source_position SourcePositionType, names_ind NameIndexType) {
	if !(gen_line > m.last.gen_line
		|| (gen_line == m.last.gen_line && gen_column >= m.last.gen_column)) {
		m.sorted = false
	}
	m.values << Mapping{
		GenPosition: GenPosition{
			gen_line: gen_line
			gen_column: gen_column
		}
		sources_ind: sources_ind
		names_ind: names_ind
		source_position: source_position
	}
}

// Returns the flat, sorted array of mappings. The mappings are sorted by generated position.

fn (mut m Mappings) get_sorted_array() []Mapping {
	if !m.sorted {
		panic('not implemented')
	}
	return m.values
}

fn (mut m Mappings) export_mappings(mut output io.Writer) ? {
	mut previous_generated_line := u32(1)
	mut previous_generated_column := u32(0)
	mut previous_source_index := i64(0)
	mut previous_source_line := i64(0)
	mut previous_source_column := i64(0)
	mut previous_name_index := i64(0)

	line_mappings := m.get_sorted_array()
	len := line_mappings.len
	for i := 0; i < len; i++ {
		mapping := line_mappings[i]

		cloned_generated_line := mapping.gen_line
		if cloned_generated_line > 0 {
			// Write a ';' for each line between this and last line, way more efficient than storing empty lines or looping...
			output.write(';'.repeat(int(cloned_generated_line - previous_generated_line)).bytes()) or {
				panic('Writing vql failed!')
			}
		}
		if cloned_generated_line != previous_generated_line {
			previous_generated_column = 0
			previous_generated_line = cloned_generated_line
		} else {
			if i > 0 {
				if !compare_by_generated_positions_inflated(mapping, line_mappings[i - 1]) {
					continue
				}
				output.write(','.bytes()) or { panic('Writing vql failed!') }
			}
		}

		vlq.encode(i64(mapping.gen_column - previous_generated_column), mut &output)?
		previous_generated_column = mapping.gen_column
		match mapping.source_position {
			Empty {}
			SourcePosition {
				vlq.encode(i64(mapping.sources_ind - previous_source_index), mut &output)?
				previous_source_index = mapping.sources_ind
				// lines are stored 0-based in SourceMap spec version 3
				vlq.encode(i64(mapping.source_position.source_line - 1 - previous_source_line), mut
					output)?
				previous_source_line = mapping.source_position.source_line - 1
				vlq.encode(i64(mapping.source_position.source_column - previous_source_column), mut
					output)?
				previous_source_column = mapping.source_position.source_column

				match mapping.names_ind {
					Empty {}
					IndexNumber {
						vlq.encode(i64(mapping.names_ind - previous_name_index), mut &output)?
						previous_name_index = mapping.names_ind
					}
				}
			}
		}
	}
}

fn compare_by_generated_positions_inflated(mapping_a Mapping, mapping_b Mapping) bool {
	if mapping_a.gen_line != mapping_b.gen_line {
		return true
	}
	if mapping_a.gen_column != mapping_b.gen_column {
		return true
	}

	if mapping_a.sources_ind != mapping_b.sources_ind {
		return true
	}

	if mapping_a.source_position.type_name() == mapping_b.source_position.type_name()
		&& mapping_a.source_position is SourcePosition
		&& mapping_b.source_position is SourcePosition {
		if mapping_a.source_position.source_line != mapping_b.source_position.source_line
			|| mapping_a.source_position.source_column != mapping_b.source_position.source_column {
			return true
		}
	} else {
		if mapping_a.source_position.type_name() != mapping_b.source_position.type_name() {
			return true
		}
	}

	if mapping_a.names_ind.type_name() == mapping_b.names_ind.type_name()
		&& mapping_a.names_ind is IndexNumber && mapping_b.names_ind is IndexNumber {
		return mapping_a.names_ind != mapping_b.names_ind
	} else {
		return mapping_a.names_ind.type_name() != mapping_b.names_ind.type_name()
	}
}
