pub struct AA {
	id string
}

pub struct BB {
pub mut:
	arr []&AA
}

fn test_for_in_mut_reference_selector_val() {
	mut bb := BB{
		arr: [&AA{
			id: 'Test1'
		}, &AA{
			id: 'Test2'
		}]
	}

	mut ret := []string{}
	for mut aa in bb.arr {
		println(aa.id)
		ret << aa.id
	}
	println(ret)
	assert ret == ['Test1', 'Test2']
}
