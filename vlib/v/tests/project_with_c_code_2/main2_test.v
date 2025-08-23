// vtest retry: 3
import v.tests.project_with_c_code_2.modc

fn test_using_c_code_in_the_same_module_works() {
	x := modc.new_vtype(123)
	modc.destroy_vtype(x)
	assert true
}

fn test_enum_with_dups_on_the_cside() {
	for e in [modc.MyEnum.unknown, .name1, .name2, .name3, .name4, .name5, .name6, .common_name1,
		.common_name2, .common_name3] {
		println('>>> e: ${e} | int(e): ${int(e)}')
	}
	assert modc.MyEnum.name1 == modc.MyEnum.common_name1
	assert modc.MyEnum.name2 == modc.MyEnum.common_name2
	assert modc.MyEnum.name3 == modc.MyEnum.common_name3
	assert modc.MyEnum.name4 != modc.MyEnum.common_name1
	assert modc.MyEnum.name5 != modc.MyEnum.common_name2
	assert modc.MyEnum.name6 != modc.MyEnum.common_name3
}
