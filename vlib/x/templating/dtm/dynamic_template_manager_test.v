module dtm

import os

const temp_dtm_dir = 'dynamic_template_manager_test'
const temp_templates_dir = 'templates'
const temp_html_fp = 'temp.html'
const temp_html_n = 'temp'
const vtmp_dir = os.vtmp_dir()

fn testsuite_begin() {
	temp_folder := os.join_path(vtmp_dir, temp_dtm_dir)
	os.mkdir_all(temp_folder)!

	templates_path := os.join_path(temp_folder, temp_templates_dir)

	os.mkdir_all(templates_path)!

	temp_html_file := os.join_path(templates_path, temp_html_fp)

	html_content := '
    <!DOCTYPE html>
    <html>
      <head>
        <title>TEST</title>
      </head>
      <body>
        <div>
          <H1>TEST</H1>
        </div>
      </body>
    </html>'

	os.write_file(temp_html_file, html_content)!
}

fn test_initialize_dtm() {
	dtmi := init_dtm(false, 0)
	assert dtmi.dtm_init_is_ok == true
}

fn test_create_template_cache_and_display() {
	mut dtmi := init_dtm(false, 0)
	temp_html_file := os.join_path(dtmi.template_folder, temp_html_fp)
	html_last_mod := os.file_last_mod_unix(temp_html_file)
	c_time := get_current_unix_micro_timestamp()
	cache_delay_exp := i64(500) * i64(1000000)
	placeholder := map[string]DtmMultiTypeMap{}
	content_checksum := ''
	html := dtmi.create_template_cache_and_display(.new, html_last_mod, c_time, temp_html_file,
		temp_html_n, cache_delay_exp, &placeholder, content_checksum, TemplateType.html)

	assert html.len > 10
}

fn test_return_cache_info_isexistent() {
	mut dtmi := init_dtm(false, 0)
	path_template := os.join_path(dtmi.template_folder, temp_html_fp)
	lock dtmi.template_caches {
		dtmi.template_caches << TemplateCache{
			id:   1
			path: path_template
		}
	}
	lock dtmi.nbr_of_remaining_template_request {
		dtmi.nbr_of_remaining_template_request << RemainingTemplateRequest{
			id: 1
		}
	}
	cache_exists, _, _, _, _, _, _ := dtmi.return_cache_info_isexistent(path_template)
	assert cache_exists == true
	lock dtmi.template_caches {
		dtmi.template_caches[0].id_redirection = 2
		dtmi.template_caches << TemplateCache{
			id:             2
			path:           path_template
			id_redirection: 3
		}
		dtmi.template_caches << TemplateCache{
			id:             3
			path:           path_template
			id_redirection: 4
		}
		dtmi.template_caches << TemplateCache{
			id:             4
			path:           path_template
			id_redirection: 5
		}
		dtmi.template_caches << TemplateCache{
			id:   5
			path: path_template
		}
	}
	lock dtmi.nbr_of_remaining_template_request {
		dtmi.nbr_of_remaining_template_request << RemainingTemplateRequest{
			id: 2
		}
		dtmi.nbr_of_remaining_template_request << RemainingTemplateRequest{
			id: 3
		}
		dtmi.nbr_of_remaining_template_request << RemainingTemplateRequest{
			id: 4
		}
		dtmi.nbr_of_remaining_template_request << RemainingTemplateRequest{
			id: 5
		}
	}
	_, id, _, _, _, _, _ := dtmi.return_cache_info_isexistent(path_template)
	assert id == 5
}

fn test_remaining_template_request() {
	mut dtmi := init_dtm(false, 0)

	lock dtmi.nbr_of_remaining_template_request {
		dtmi.nbr_of_remaining_template_request << RemainingTemplateRequest{
			id: 1
		}
	}
	dtmi.remaining_template_request(true, 1)
	rlock dtmi.nbr_of_remaining_template_request {
		assert dtmi.nbr_of_remaining_template_request[0].nbr_of_remaining_request == 1
	}
	dtmi.remaining_template_request(true, 1)
	rlock dtmi.nbr_of_remaining_template_request {
		assert dtmi.nbr_of_remaining_template_request[0].nbr_of_remaining_request == 2
	}
	dtmi.remaining_template_request(false, 1)
	rlock dtmi.nbr_of_remaining_template_request {
		assert dtmi.nbr_of_remaining_template_request[0].nbr_of_remaining_request == 1
	}
	dtmi.remaining_template_request(false, 1)
	rlock dtmi.nbr_of_remaining_template_request {
		assert dtmi.nbr_of_remaining_template_request[0].nbr_of_remaining_request == 0
	}
}

fn test_check_tmpl_and_placeholders_size() {
	mut dtmi := init_dtm(false, 0)
	temp_html_file := os.join_path(dtmi.template_folder, temp_html_fp)
	placeholders := map[string]DtmMultiTypeMap{}

	path, filename, content_checksum, tmpl_type := dtmi.check_tmpl_and_placeholders_size(temp_html_file,
		&placeholders)!

	is_html := if tmpl_type == TemplateType.html {
		true
	} else {
		false
	}
	assert path.len > 10
	assert filename.len > 3
	assert is_html == true
	//	assert content_checksum.len > 3
}

fn test_chandler_prevent_cache_duplicate_request() {
	dtmi := init_dtm(false, 0)
	temp_html_file := os.join_path(dtmi.template_folder, temp_html_fp)

	lock dtmi.template_caches {
		dtmi.template_caches << TemplateCache{
			id:            1
			path:          temp_html_file
			cache_request: .new
		}
		dtmi.template_caches << TemplateCache{
			id:                2
			path:              temp_html_file
			cache_request:     .update
			last_template_mod: i64(1)
		}
		dtmi.template_caches << TemplateCache{
			id:                3
			path:              temp_html_file
			cache_request:     .exp_update
			last_template_mod: i64(1)
			generate_at:       i64(100)
		}
		dtmi.template_caches << TemplateCache{
			id:            4
			cache_request: .delete
		}
	}
	new_cache := TemplateCache{
		id:            5
		path:          temp_html_file
		cache_request: .new
	}
	update_cache := TemplateCache{
		id:                6
		path:              temp_html_file
		cache_request:     .update
		last_template_mod: i64(1)
	}
	exp_update_cache := TemplateCache{
		id:                     7
		path:                   temp_html_file
		cache_request:          .exp_update
		last_template_mod:      i64(1)
		generate_at:            i64(10)
		cache_delay_expiration: i64(10)
	}
	delete_cache := TemplateCache{
		id:            4
		cache_request: .delete
	}
	mut is_duplicate := dtmi.chandler_prevent_cache_duplicate_request(&new_cache)
	assert is_duplicate == true
	is_duplicate = dtmi.chandler_prevent_cache_duplicate_request(&update_cache)
	assert is_duplicate == true
	is_duplicate = dtmi.chandler_prevent_cache_duplicate_request(&exp_update_cache)
	assert is_duplicate == true
	is_duplicate = dtmi.chandler_prevent_cache_duplicate_request(&delete_cache)
	assert is_duplicate == false

	lock dtmi.template_caches {
		dtmi.template_caches.delete(3)
	}

	is_duplicate = dtmi.chandler_prevent_cache_duplicate_request(&delete_cache)
	assert is_duplicate == true
}

fn test_chandler_remaining_cache_template_used() {
	mut dtmi := init_dtm(false, 0)
	lock dtmi.nbr_of_remaining_template_request {
		dtmi.nbr_of_remaining_template_request << RemainingTemplateRequest{
			id:                       1
			nbr_of_remaining_request: 0
		}
		dtmi.nbr_of_remaining_template_request << RemainingTemplateRequest{
			id:                       2
			nbr_of_remaining_request: 1
			need_to_delete:           true
		}
		dtmi.nbr_of_remaining_template_request << RemainingTemplateRequest{
			id:                       3
			nbr_of_remaining_request: 0
			need_to_delete:           true
		}
	}
	mut can_delete := dtmi.chandler_remaining_cache_template_used(CacheRequest.update,
		3, 3)
	assert can_delete == true
	can_delete = dtmi.chandler_remaining_cache_template_used(CacheRequest.update, 2, 2)
	assert can_delete == false
	can_delete = dtmi.chandler_remaining_cache_template_used(CacheRequest.delete, 1, 0)
	assert can_delete == true
	can_delete = dtmi.chandler_remaining_cache_template_used(CacheRequest.new, 4, 0)
	assert can_delete == true
}

fn test_parse_tmpl_file() {
	mut dtmi := init_dtm(false, 0)
	temp_folder := os.join_path(vtmp_dir, temp_dtm_dir)
	templates_path := os.join_path(temp_folder, temp_templates_dir)
	temp_html_file := os.join_path(templates_path, temp_html_fp)

	mut placeholders := map[string]DtmMultiTypeMap{}

	is_compressed := true
	html := dtmi.parse_tmpl_file(temp_html_file, temp_html_n, &placeholders, is_compressed,
		TemplateType.html)

	assert html.len > 0
}

fn test_check_if_cache_delay_iscorrect() {
	check_if_cache_delay_iscorrect(i64(300 * 1000000), temp_html_n) or { assert false }

	check_if_cache_delay_iscorrect(i64(-100), temp_html_n) or { assert true }
}

fn test_cache_request_route() {
	mut dtmi := init_dtm(false, 0)
	mut is_cache_exist := true
	mut cache_delay_expiration := i64(400)
	mut last_template_mod := get_current_unix_micro_timestamp()
	mut test_current_template_mod := last_template_mod
	mut cache_del_exp := 300
	mut gen_at := last_template_mod
	mut content_checksum := 'checksumtest1'
	mut current_content_checksum := 'checksumtest2'

	mut request_type, _ := dtmi.cache_request_route(is_cache_exist, cache_delay_expiration,
		last_template_mod, test_current_template_mod, cache_del_exp, gen_at, get_current_unix_micro_timestamp(),
		content_checksum, current_content_checksum)

	assert request_type == CacheRequest.update

	current_content_checksum = 'checksumtest1'

	request_type, _ = dtmi.cache_request_route(is_cache_exist, cache_delay_expiration,
		last_template_mod, test_current_template_mod, cache_del_exp, gen_at, get_current_unix_micro_timestamp(),
		content_checksum, current_content_checksum)

	assert request_type == CacheRequest.cached

	gen_at = (last_template_mod - 500)

	request_type, _ = dtmi.cache_request_route(is_cache_exist, cache_delay_expiration,
		last_template_mod, test_current_template_mod, cache_del_exp, gen_at, get_current_unix_micro_timestamp(),
		content_checksum, current_content_checksum)

	assert request_type == CacheRequest.exp_update

	is_cache_exist = false

	request_type, _ = dtmi.cache_request_route(is_cache_exist, cache_delay_expiration,
		last_template_mod, test_current_template_mod, cache_del_exp, gen_at, get_current_unix_micro_timestamp(),
		content_checksum, current_content_checksum)

	assert request_type == CacheRequest.new
}

fn testsuite_end() {
	temp_folder := os.join_path(vtmp_dir, temp_dtm_dir)
	os.rmdir_all(temp_folder) or {}
}

// Utilities function :

fn init_dtm(b bool, m int) &DynamicTemplateManager {
	temp_folder := os.join_path(vtmp_dir, temp_dtm_dir)
	templates_path := os.join_path(temp_folder, temp_templates_dir)

	init_params := DynamicTemplateManagerInitialisationParams{
		active_cache_server:  b
		max_size_data_in_mem: m
		test_template_dir:    templates_path
	}

	dtm := initialize(init_params)

	return dtm
}
