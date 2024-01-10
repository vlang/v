module sourcemap

fn test_cmp_eq() {
	a := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: IndexNumber(3)
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	b := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: IndexNumber(3)
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	assert !compare_by_generated_positions_inflated(a, b)
}

fn test_cmp_name() {
	a := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: IndexNumber(3)
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	b := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: IndexNumber(4)
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	assert compare_by_generated_positions_inflated(a, b)
}

fn test_cmp_name_empty() {
	a := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: IndexNumber(3)
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	b := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	assert compare_by_generated_positions_inflated(a, b)
}

fn test_cmp_name_empty_empty() {
	a := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	b := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	assert !compare_by_generated_positions_inflated(a, b)
}

fn test_cmp_source_position_empty_eq() {
	a := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: Empty{}
	}

	b := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: Empty{}
	}

	assert !compare_by_generated_positions_inflated(a, b)
}

fn test_cmp_source_position_empty_diff() {
	a := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	b := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: Empty{}
	}

	assert compare_by_generated_positions_inflated(a, b)
}

fn test_cmp_source_position_column_diff() {
	a := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	b := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: SourcePosition{
			source_line: 4
			source_column: 99
		}
	}

	assert compare_by_generated_positions_inflated(a, b)
}

fn test_cmp_source_position_line_diff() {
	a := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	b := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: SourcePosition{
			source_line: 88
			source_column: 99
		}
	}

	assert compare_by_generated_positions_inflated(a, b)
}

fn test_cmp_sources() {
	a := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	b := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 99
		names_ind: Empty{}
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	assert compare_by_generated_positions_inflated(a, b)
}

fn test_cmp_gen_column() {
	a := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	b := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 99
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	assert compare_by_generated_positions_inflated(a, b)
}

fn test_cmp_gen_line() {
	a := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 0
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	b := Mapping{
		GenPosition: GenPosition{
			gen_line: 1
			gen_column: 99
		}
		sources_ind: 2
		names_ind: Empty{}
		source_position: SourcePosition{
			source_line: 4
			source_column: 5
		}
	}

	assert compare_by_generated_positions_inflated(a, b)
}
