module sourcemap

fn test_simple() {
	mut sg := generate_empty_map()
	mut sm := sg.add_map('hello.js', '/', true, 0, 0)
	sm.set_source_content('hello.v', "fn main(){ nprintln('Hello World! Helo \$a')\n}")

	mlist := [
		MappingInput{
			GenPosition: GenPosition{
				gen_line: 1
				gen_column: 0
			}
			name: ''
			source_position: SourcePosition{
				source_line: 1
				source_column: 0
			}
		},
		MappingInput{
			GenPosition: GenPosition{
				gen_line: 2
				gen_column: 0
			}
			name: ''
			source_position: SourcePosition{
				source_line: 1
				source_column: 0
			}
		},
		MappingInput{
			GenPosition: GenPosition{
				gen_line: 2
				gen_column: 2
			}
			name: ''
			source_position: SourcePosition{
				source_line: 1
				source_column: 0
			}
		},
		MappingInput{
			GenPosition: GenPosition{
				gen_line: 2
				gen_column: 9
			}
			name: ''
			source_position: SourcePosition{
				source_line: 1
				source_column: 7
			}
		},
		MappingInput{
			GenPosition: GenPosition{
				gen_line: 2
				gen_column: 10
			}
			name: 'hello_name'
			source_position: SourcePosition{
				source_line: 1
				source_column: 8
			}
		},
		MappingInput{
			GenPosition: GenPosition{
				gen_line: 2
				gen_column: 13
			}
			name: ''
			source_position: SourcePosition{
				source_line: 1
				source_column: 0
			}
		},
		MappingInput{
			GenPosition: GenPosition{
				gen_line: 2
				gen_column: 14
			}
			name: ''
			source_position: SourcePosition{
				source_line: 1
				source_column: 12
			}
		},
		MappingInput{
			GenPosition: GenPosition{
				gen_line: 2
				gen_column: 27
			}
			name: ''
			source_position: SourcePosition{
				source_line: 1
				source_column: 0
			}
		},
		MappingInput{
			GenPosition: GenPosition{
				gen_line: 2
				gen_column: 28
			}
			name: ''
			source_position: SourcePosition{
				source_line: 1
				source_column: 0
			}
		},
		MappingInput{
			GenPosition: GenPosition{
				gen_line: 2
				gen_column: 29
			}
			name: ''
			source_position: SourcePosition{
				source_line: 1
				source_column: 0
			}
		},
		MappingInput{
			GenPosition: GenPosition{
				gen_line: 3
				gen_column: 0
			}
			name: ''
			source_position: SourcePosition{
				source_line: 1
				source_column: 0
			}
		},
	]
	sm.add_mapping_list('hello.v', mlist) or { panic('x') }

	json_data := sm.to_json()

	expected := '{"version":3,"file":"hello.js","sourceRoot":"\\/","sources":["hello.v"],"sourcesContent":["fn main(){ nprintln(\'Hello World! Helo \$a\')\\n}"],"names":["hello_name"],"mappings":"AAAA;AAAA,EAAA,OAAO,CAACA,GAAR,CAAY,aAAZ,CAAA,CAAA;AAAA"}'
	assert json_data.str() == expected
}

fn test_source_null() {
	mut sg := generate_empty_map()
	mut sm := sg.add_map('hello.js', '/', true, 0, 0)
	sm.add_mapping('hello.v', SourcePosition{
		source_line: 0
		source_column: 0
	}, 1, 1, '')
	sm.add_mapping('hello_lib1.v', SourcePosition{
		source_line: 0
		source_column: 0
	}, 2, 1, '')
	sm.add_mapping('hello_lib2.v', SourcePosition{
		source_line: 0
		source_column: 0
	}, 3, 1, '')
	json_data := sm.to_json()

	expected := '{"version":3,"file":"hello.js","sourceRoot":"\\/","sources":["hello.v","hello_lib1.v","hello_lib2.v"],"sourcesContent":[null,null,null],"names":[],"mappings":"CA+\\/\\/\\/\\/\\/HA;CCAA;CCAA"}'
	assert json_data.str() == expected
}
