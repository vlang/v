import time

type BranchEnum = u32

const branch_enum_first = BranchEnum(0x2800)
const branch_enum_second = BranchEnum(0x2801)

fn classify_branch_enum(name BranchEnum) int {
	match name {
		branch_enum_first, branch_enum_second {}
		BranchEnum(0x84fe) {
			return 1
		}
		else {
			match name {
				BranchEnum(0x813c), 0x884d {
					return 2
				}
				BranchEnum(0x912f) {
					return 3
				}
				else {
					return 4
				}
			}
		}
	}
	return 0
}

fn classify_duration(d time.Duration) int {
	return match d {
		time.Duration(5) { 1 }
		time.Duration(6), 7 { 2 }
		else { 3 }
	}
}

fn test_cast_values_are_match_branch_values() {
	assert classify_branch_enum(branch_enum_second) == 0
	assert classify_branch_enum(BranchEnum(0x84fe)) == 1
	assert classify_branch_enum(BranchEnum(0x884d)) == 2
	assert classify_branch_enum(BranchEnum(0x912f)) == 3
	assert classify_branch_enum(BranchEnum(1)) == 4
}

fn test_module_qualified_cast_values_are_match_branch_values() {
	assert classify_duration(time.Duration(5)) == 1
	assert classify_duration(time.Duration(7)) == 2
	assert classify_duration(time.second) == 3
}
