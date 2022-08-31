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

[field_remap]
txt = "I am remapped"
uint64 = 100

[config]
data = [ 1, 2, 3 ]
levels = { "info" = 1, "warn" = 2, "critical" = 3 }
'

struct FieldRemap {
	text string [toml: 'txt']
	num  u64    [toml: 'uint64']
}

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
	bio   Bio
	remap FieldRemap
}

fn test_reflect() {
	toml_doc := toml.parse_text(toml_text) or { panic(err) }

	mut user := toml_doc.reflect<User>()
	user.bio = toml_doc.value('bio').reflect<Bio>()
	user.remap = toml_doc.value('field_remap').reflect<FieldRemap>()

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

	assert user.remap.text == 'I am remapped'
	assert user.remap.num == 100

	assert user.config.value('data[0]').int() == 1
	assert user.config.value('levels.warn').int() == 2
}
