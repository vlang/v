module fastc

import os
import v.cmdexec
import v.pref

const nested_collection_source = r'module main

type Value = Empty | Function

struct Empty {}

struct Function {
	params []Value
	flags  []bool
}

fn transform(value Value) Value {
	return match value {
		Function {
			Value(Function{
				params: value.params.map(transform(it))
				flags:  value.flags
			})
		}
		else { value }
	}
}

fn main() {
	input := Value(Function{
		params: [Value(Empty{}), Value(Function{params: [Value(Empty{})], flags: [true]})]
		flags:  [false, true]
	})
	output := transform(input)
	match output {
		Function {
			assert output.params.len == 2
			assert output.flags == [false, true]
			inner := output.params[1]
			match inner {
				Function {
					assert inner.params.len == 1
					assert inner.flags == [true]
				}
				else { assert false }
			}
		}
		else { assert false }
	}
	println("nested collections")
}
'

fn test_fastc_map_preserves_enclosing_cast_and_struct_prefix() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate(nested_collection_source, 'nested_collection_expression.v', prefs) or {
		panic(err)
	}
	assert c_source.contains('transform(it)'), c_source
	assert c_source.contains('builtin__array_push('), c_source
}

fn test_fastc_nested_collection_casts_execute() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_nested_collections_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), nested_collection_source) or { panic(err) }
	program := os.join_path(root, 'program')
	compiled := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-no-prealloc', '-nocache', '-o',
		program, root])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(program, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output == 'nested collections\n'
}
