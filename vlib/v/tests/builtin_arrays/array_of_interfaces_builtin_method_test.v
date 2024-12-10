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
}
