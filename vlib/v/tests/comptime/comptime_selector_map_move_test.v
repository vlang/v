struct MapHolder {
	values map[string]string
}

fn make_map_holder() MapHolder {
	return MapHolder{
		values: {
			'a': 'b'
		}
	}
}

fn first_map_from_temporary[T]() map[string]string {
	$for field in T.fields {
		$if field.unaliased_typ is map[string]string {
			values := make_map_holder().$(field.name)
			return values
		}
	}
	return {}
}

fn test_comptime_selector_map_move_from_temporary() {
	assert first_map_from_temporary[MapHolder]() == {
		'a': 'b'
	}
}
