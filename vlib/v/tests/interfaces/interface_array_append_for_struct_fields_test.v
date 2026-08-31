interface EmptyValue {}

interface Value {
	EmptyValue
}

struct Empty {}

struct GeoLocation {
	name      string
	longitude f64
	latitude  f64
}

fn geoadd(key string, geo_locations ...GeoLocation) []Value {
	mut args := []Value{len: 0, cap: geo_locations.len * 3 + 2, init: Empty{}}
	args << 'GEOADD'
	args << key
	for _, location in geo_locations {
		args << location.longitude
		args << location.latitude
		args << location.name
	}
	return args
}

fn test_append_for_struct_fields_to_embedded_empty_interface_array() {
	args := geoadd('places', GeoLocation{
		name:      'home'
		longitude: 1.25
		latitude:  2.5
	})
	assert args.len == 5
}
