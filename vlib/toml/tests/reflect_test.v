import toml

const toml_text = '# This TOML can reflect to a struct
name = "Tom"
age = 45
height = 1.97

birthday = 1980-04-23

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

	config toml.Any
mut:
	bio Bio
}

fn test_reflect() {
	toml_doc := toml.parse(toml_text) or { panic(err) }

	mut user := toml_doc.reflect<User>()
	user.bio = toml_doc.value('bio').reflect<Bio>()

	assert user.name == 'Tom'
	assert user.age == 45
	assert user.height == 1.97
	assert user.birthday.str() == '1980-04-23'
	assert user.bio.text == 'Tom has done many great things'
	assert user.bio.years_of_service == 5

	assert user.config.value('data[0]').int() == 1
	assert user.config.value('levels.warn').int() == 2
}
