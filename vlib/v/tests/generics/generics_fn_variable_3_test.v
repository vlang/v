import ecs

struct Entity {
	components []Component
}

interface Component {}

fn two_components_filter_query[A, B](entity Entity) bool {
	return check_if_entity_has_component[A](entity) && check_if_entity_has_component[B](entity)
}

pub fn check_if_entity_has_component[T](entity Entity) bool {
	get_entity_component[T](entity) or { return false }

	return true
}

pub fn get_entity_component[T](entity Entity) !&T {
	for component in entity.components {
		if component is T {
			return component
		}
	}

	return error('Entity with does not have a component of type ${T.name}')
}

fn component_interface_hack() []Component {
	return [ecs.Position{}, ecs.Velocity{}]
}

fn test_generic_fn_variable() {
	query := two_components_filter_query[ecs.Position, ecs.Velocity]
	assert true
}
