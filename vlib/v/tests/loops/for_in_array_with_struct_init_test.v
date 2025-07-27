import v.ast

fn test_main() {
	for idx, s in [ast.Struct{
		scoped_name: '1'
	}, ast.Struct{
		embeds: [ast.Type(2), 3]
	}, ast.Struct{
		is_typedef: true
		is_union:   true
		is_heap:    true
	}, ast.Struct{
		is_minify: ast.Struct{
			is_anon: true
		}.is_generic
		is_shared: true
	}, ast.Struct{
		has_option: true
	}] {
		println('${idx} ${s}')
		if idx == 0 {
			assert s.scoped_name == '1'
		} else if idx == 1 {
			assert s.embeds == [ast.Type(2), 3]
		} else if idx == 2 {
			assert s.is_typedef == true
			assert s.is_union == true
			assert s.is_heap == true
		} else if idx == 3 {
			assert s.is_minify == false
			assert s.is_shared == true
		} else if idx == 4 {
			assert s.has_option == true
		}
	}
}
