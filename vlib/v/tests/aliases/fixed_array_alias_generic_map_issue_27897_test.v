type GenericKeyMaps = [4]map[T]string
type GenericValueMaps = [4]map[string]T

fn test_fixed_array_alias_of_generic_maps() {
	key_maps := GenericKeyMaps{}
	value_maps := GenericValueMaps{}
	assert key_maps.len == 4
	assert value_maps.len == 4
}
