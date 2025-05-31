struct Empty {}

struct SourcePosition {
	source_line   u32
	source_column u32
}

type SourcePositionType = Empty | SourcePosition
type NameIndexType = Empty | u32

struct GenPosition {
	gen_line   u32
	gen_column u32
}

struct Mapping {
	GenPosition
	sources_ind     u32
	names_ind       NameIndexType
	source_position SourcePositionType
}

fn ok(mapping_a Mapping, mapping_b Mapping) bool {
	if mapping_a.source_position is SourcePosition && mapping_b.source_position is SourcePosition {
		return mapping_a.source_position.source_line != mapping_b.source_position.source_line
			|| mapping_a.source_position.source_column != mapping_b.source_position.source_column
	}
	return false
}

fn test_if_smartcast_multi_conds() {
	a := Mapping{
		source_position: SourcePosition{
			source_line:   11
			source_column: 22
		}
	}
	b := Mapping{
		source_position: SourcePosition{
			source_line:   22
			source_column: 11
		}
	}
	ret := ok(a, b)
	println(ret)
	assert ret
}
