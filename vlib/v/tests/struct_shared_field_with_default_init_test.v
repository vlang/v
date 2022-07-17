struct App {
	id string
mut:
	app_data shared AppData
}

struct AppData {
	id           string
	sub_app_data SubAppData
}

struct SubAppData {
	id      string
	message string = 'Default message'
}

fn test_struct_field_with_default_init() {
	mut app := App{}
	println(app)
	rlock app.app_data {
		assert app.app_data.sub_app_data.message == 'Default message'
	}
}
