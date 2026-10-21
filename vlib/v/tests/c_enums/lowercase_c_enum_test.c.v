#include "@VMODROOT/vlib/v/tests/c_enums/lowercase_c_enum.h"

enum LowercaseCEnum {
	first  = C.enum_first_opt
	second = C.enum_second_opt
	third  = C.enum_third_opt + 1
}

fn test_lowercase_c_enum_values() {
	assert int(LowercaseCEnum.first) == 3
	assert int(LowercaseCEnum.second) == 4
	assert int(LowercaseCEnum.third) == 6
	assert LowercaseCEnum.second.str() == 'second'
}
