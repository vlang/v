import toml

const toml_text = '# This TOML can reflect to a struct
name = "Tom"
age = 45
height = 1.97

birthday = 1980-04-23

strings = [
  "v matures",
  "like rings",
  "spread in the",
  "water"
]

bools = [true, false, true, true]

floats = [0.0, 1.0, 2.0, 3.0]

int_map = {"a" = 0, "b" = 1, "c" = 2, "d" = 3}

[bio]
text = "Tom has done many great things"
years_of_service = 5

[config]
data = [ 1, 2, 3 ]
levels = { "info" = 1, "warn" = 2, "critical" = 3 }
'

struct Bio {
	text             string
	years_of_service int
}

struct User {
	name     string
	age      int
	height   f64
	birthday toml.Date
	strings  []string
	bools    []bool
	floats   []f32
	int_map  map[string]int

	config toml.Any
mut:
	bio Bio
}

fn test_reflect() {
	toml_doc := toml.parse_text(toml_text) or { panic(err) }

	mut user := toml_doc.reflect<User>()
	user.bio = toml_doc.value('bio').reflect<Bio>()

	assert user.name == 'Tom'
	assert user.age == 45
	assert user.height == 1.97
	assert user.birthday.str() == '1980-04-23'
	assert user.strings == ['v matures', 'like rings', 'spread in the', 'water']
	assert user.bools == [true, false, true, true]
	assert user.floats == [f32(0.0), 1.0, 2.0, 3.0]
	assert user.int_map == {
		'a': 0
		'b': 1
		'c': 2
		'd': 3
	}
	assert user.bio.text == 'Tom has done many great things'
	assert user.bio.years_of_service == 5

	assert user.config.value('data[0]').int() == 1
	assert user.config.value('levels.warn').int() == 2
}
