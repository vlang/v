interface IModel {
	model string
}

interface ICar {
	name string
}

pub struct Car {
	name  string
	model string
}

fn test_main() {
	mut cars := []ICar{}
	cars << Car{
		model: 'Tesla'
	}
	cars << Car{
		model: 'Toyota'
	}

	mut cc := []IModel{}
	for mut c in cars {
		if mut c is IModel {
			cc << c
		}
	}
	assert cc.len == 2

	a := get_all[IModel](mut cars)
	assert a.len == 2
}

pub fn get_all[T](mut cars []ICar) []T {
	mut cc := []T{}
	for mut c in cars {
		if mut c is T {
			cc << c
		}
	}
	return cc
}
