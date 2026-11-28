type AssignedValue = []int | string

fn assign_value[T](mut value T, replacement T) {
	value = replacement
}

fn test_generic_mut_sum_assignment_updates_the_caller() {
	mut value := AssignedValue([]int{})
	assign_value(mut value, AssignedValue('updated'))
	match value {
		string {
			assert value == 'updated'
		}
		else {
			assert false, 'the caller must hold the new string variant'
		}
	}
	assign_value(mut value, AssignedValue([2, 3]))
	match value {
		[]int {
			assert value == [2, 3]
		}
		else {
			assert false, 'the caller must hold the new array variant'
		}
	}
}
