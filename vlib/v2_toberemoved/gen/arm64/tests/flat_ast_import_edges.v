import v2.ast

fn main() {
	file := ast.File{
		name:    @FILE
		mod:     'main'
		imports: [
			ast.ImportStmt{
				name:  'v2.pref'
				alias: 'pref'
			},
		]
		stmts:   [
			ast.Stmt(ast.ImportStmt{
				name:  'v2.pref'
				alias: 'pref'
			}),
		]
	}
	flat := ast.flatten_files([file])
	file_node := flat.nodes[flat.files[0].file_id]
	imports := flat.file_cursor(0).imports().import_stmts()
	println(file_node.edge_count)
	println(imports.len)
	if imports.len > 0 {
		println(imports[0].name)
		println(imports[0].alias)
	}
}
