type LoopStructType = int

struct LoopStruct {
	scoped_name string
	embeds      []LoopStructType
	is_typedef  bool
	is_union    bool
	is_heap     bool
	is_minify   bool
	is_anon     bool
	is_generic  bool
	is_shared   bool
	has_option  bool
}

fn test_main() {
	for idx, s in [LoopStruct{
		scoped_name: '1'
	}, LoopStruct{
		embeds: [LoopStructType(2), 3]
	}, LoopStruct{
		is_typedef: true
		is_union:   true
		is_heap:    true
	}, LoopStruct{
		is_minify: LoopStruct{
			is_anon: true
		}.is_generic
		is_shared: true
	}, LoopStruct{
		has_option: true
	}] {
		println('${idx} ${s}')
		if idx == 0 {
			assert s.scoped_name == '1'
		} else if idx == 1 {
			assert s.embeds == [LoopStructType(2), 3]
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
