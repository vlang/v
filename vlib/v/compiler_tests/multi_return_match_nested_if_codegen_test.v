const block_image = 'image'

struct BlockTarget {
	kind  string
	index int
}

fn block_target(object_kind string, object_index int, table_index int) ?BlockTarget {
	kind, index := match object_kind {
		'image' {
			block_image, object_index
		}
		else {
			if table_index >= 0 {
				'table', table_index
			} else {
				return none
			}
		}
	}
	return BlockTarget{
		kind:  kind
		index: index
	}
}

fn test_multi_return_match_nested_if_codegen() {
	assert block_target('image', 2, -1)? == BlockTarget{'image', 2}
	assert block_target('', 0, 3)? == BlockTarget{'table', 3}
	assert block_target('', 0, -1) == none
}
