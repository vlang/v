struct Entity {
	id u64
mut:
	components []IComponent
}

interface IComponent {
	hollow bool
}

struct IsControlledByPlayerTag {
	hollow bool
}

fn get_component[T](entity Entity) !&T {
	for component in entity.components {
		if component is T {
			return component
		}
	}

	return error('Entity does not have component')
}

fn test_array_of_interfaces_index() {
	entity := Entity{1, [IsControlledByPlayerTag{}]}

	id := entity.components.index(*get_component[IsControlledByPlayerTag](entity)!)
	println('id = ${id}')
	assert id == 0

	ret := entity.components.contains(*get_component[IsControlledByPlayerTag](entity)!)
	println(ret)
	assert ret
	assert entity.components.last_index(*get_component[IsControlledByPlayerTag](entity)!) == 0
	assert *get_component[IsControlledByPlayerTag](entity)! in entity.components
}

struct OtherComponent {
	hollow bool
}

fn component_needle[T](mut calls []string, value T) T {
	calls << 'needle'
	return value
}

fn component_values(mut calls []string, values []IComponent) []IComponent {
	calls << 'values'
	return values
}

fn test_interface_array_membership_preserves_concrete_needles_and_order() {
	values := [IComponent(IsControlledByPlayerTag{}), IsControlledByPlayerTag{true},
		IsControlledByPlayerTag{}]
	mut calls := []string{}
	assert component_values(mut calls, values).contains(component_needle(mut calls, IsControlledByPlayerTag{}))
	assert calls == ['values', 'needle']
	calls.clear()
	assert component_values(mut calls, values).index(component_needle(mut calls, IsControlledByPlayerTag{})) == 0
	assert calls == ['values', 'needle']
	calls.clear()
	assert component_values(mut calls, values).last_index(component_needle(mut calls, IsControlledByPlayerTag{})) == 2
	assert calls == ['values', 'needle']
	calls.clear()
	assert component_needle(mut calls, IsControlledByPlayerTag{}) in component_values(mut calls, values)
	assert calls == ['needle', 'values']
	calls.clear()
	assert component_needle(mut calls, OtherComponent{}) !in component_values(mut calls, values)
	assert calls == ['needle', 'values']
	assert values.index(IsControlledByPlayerTag{true}) == 1
	assert values.index(OtherComponent{}) == -1
	assert values.last_index(OtherComponent{}) == -1
	assert !values.contains(OtherComponent{})
}

fn component_values_after_change(mut needle IsControlledByPlayerTag, values []IComponent) ![]IComponent {
	needle = IsControlledByPlayerTag{true}
	return values
}

fn component_values_after_direct_change(mut needle IsControlledByPlayerTag, values []IComponent) []IComponent {
	needle = IsControlledByPlayerTag{true}
	return values
}

fn test_interface_membership_captures_needle_before_container_branch() {
	mut needle := IsControlledByPlayerTag{}
	values := [IComponent(IsControlledByPlayerTag{})]
	assert needle in (match 1 {
		1 { component_values_after_change(mut needle, values)! }
		else { values }
	})
	assert needle.hollow
	assert needle !in values
	needle = IsControlledByPlayerTag{}
	assert needle in component_values_after_direct_change(mut needle, values)
	assert needle.hollow
}
