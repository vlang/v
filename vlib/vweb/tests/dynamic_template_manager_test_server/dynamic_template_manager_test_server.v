import vweb
import time
import x.templating.dtm
import os

struct App {
	vweb.Context
pub mut:
	dtmi               &dtm.DynamicTemplateManager = unsafe { nil } @[vweb_global]
	shared_data_string []string                    @[vweb_global]
	shared_data_int    []int                       @[vweb_global]
	shared_switch      int = 1                         @[vweb_global]
}

fn main() {
	mut app := &App{}

	app.shared_data_string << 'vweb dtm'
	app.shared_data_string << 'VWEB DTM'
	app.shared_data_string << '<span>this <br> is <br> a <br> test <br> with <br> included <br> HTML!</span>'
	app.shared_data_string << '<span>THIS <br> NOT <br> A <br> TEST!</span>'
	app.shared_data_string << '<p>escaped html tags test</p>'
	app.shared_data_int << 123456
	app.shared_data_int << 7891011

	cache_folder_path := os.join_path(os.dir(os.executable()), 'vcache_dtm')
	app.dtmi = dtm.initialize(def_cache_path: cache_folder_path)
	defer {
		app.dtmi.stop_cache_handler()
	}
	/*
	dtm.initialize(
	compress_html: false
	active_cache_server: false
	max_size_data_in_mem: 100)
	*/
	go app.update_data()

	app.mount_static_folder_at(os.resource_abs_path('static'), '/')

	vweb.run(app, 18081)
}

@['/']
pub fn (mut app App) index() vweb.Result {
	mut tmpl_var := map[string]dtm.DtmMultiTypeMap{}
	tmpl_var['title'] = app.shared_data_string[app.shared_switch]
	tmpl_var['non_string_type'] = app.shared_data_int[app.shared_switch]
	tmpl_var['string_type'] = app.shared_data_string[4]
	tmpl_var['html_#includehtml'] = app.shared_data_string[app.shared_switch + 2]

	// You can also modify the HTML template file directly without having to recompile the application.
	html_content := app.dtmi.expand('index.html',
		placeholders:           &tmpl_var
		cache_delay_expiration: dtm.cache_delay_expiration_at_min
	)
	return app.html(html_content)
}

fn (mut app App) update_data() {
	for {
		if app.shared_switch == 1 {
			app.shared_switch = 0
		} else {
			app.shared_switch = 1
		}
		time.sleep(10 * time.second)
	}
}
