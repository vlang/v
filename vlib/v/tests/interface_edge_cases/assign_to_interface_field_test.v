interface Planet {
	name() string
}

struct Moon {}

fn (moon Moon) name() string {
	return 'moon'
}

struct Mars {}

fn (moon Mars) name() string {
	return 'mars'
}

struct AnyPlanet {
mut:
	planet Planet
}

fn test_a_struct_implementing_an_interface_can_be_assigned_without_explicit_casts() {
	mut anyplanet := AnyPlanet{}
	anyplanet.planet = Moon{}
	assert anyplanet.planet.name() == 'moon'
	anyplanet.planet = Mars{}
	assert anyplanet.planet.name() == 'mars'
}
