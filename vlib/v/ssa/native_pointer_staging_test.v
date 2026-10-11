module ssa

import os
import v.flat
import v.parser
import v.pref
import v.transform
import v.types

fn test_native_nil_staging_preserves_map_or_and_if_pointer_types() {
	source := 'module main
struct SourceFile { name string }
fn find_continue(sources map[string]&SourceFile, names []string) string {
    for name in names {
        file := sources[name] or { continue }
        return file.name
    }
    return "missing"
}
fn find_return(sources map[string]&SourceFile, name string) string {
    file := sources[name] or { return "missing" }
    return file.name
}
fn choose(first &SourceFile, second &SourceFile, condition bool) &SourceFile {
    return if condition { first } else { second }
}
fn main() {}
'
	path := os.join_path(os.vtmp_dir(), 'ssa_pointer_staging_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, source) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, tc)
	m := build_with_used(a, map[string]bool{}, tc)
	mut checked := 0
	for function in m.funcs {
		if function.name !in ['find_continue', 'find_return', 'choose'] {
			continue
		}
		checked++
		for block in function.blocks {
			for id in m.blocks[block].instrs {
				instruction := m.instrs[m.values[id].index]
				if instruction.op == .ret && instruction.operands.len > 0 {
					value := m.values[instruction.operands[0]]
					assert value.typ == function.typ, '${function.name}: return type ${value.typ}, expected ${function.typ}'
				}
			}
		}
	}
	assert checked == 3
}

fn test_native_declaration_without_initializer_remains_supported() {
	path := os.join_path(os.vtmp_dir(), 'ssa_missing_initializer_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'fn main() { missing := 0 }') or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	for node in a.nodes {
		if node.kind == .decl_assign {
			a.children[node.children_start + 1] = flat.empty_node
		}
	}
	m := build(a)
	assert m.funcs.filter(it.name == 'main').len == 1
}
