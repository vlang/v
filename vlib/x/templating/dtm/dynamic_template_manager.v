module dtm

import os
import v.parser
import crypto.md5
import hash.fnv1a
import time
import regex

// These are all the types of dynamic values that the DTM allows to be returned in the context of a map
type DtmMultiTypeMap = f32 | f64 | i16 | i64 | i8 | int | string | u16 | u32 | u64 | u8

// type MiddlewareFn = fn (mut Context, string) bool

// cache_delay_expiration_at_min is the minimum setting for cache expiration delay, fixed at 5 minutes (measured in seconds).
pub const cache_delay_expiration_at_min = 300
// cache_delay_expiration_at_max maximal is the maximal setting for cache expiration delay, fixed at 1 year (measured in seconds).
pub const cache_delay_expiration_at_max = 31536000
// cache_delay_expiration_by_default is the default setting for cache expiration delay, fixed at 1 day (measured in seconds).
pub const cache_delay_expiration_by_default = 86400
// Setting channel capacity of the cache handler.
const cache_handler_channel_cap = 200
// Setting the maximum data size (500 KB) to be stored at memory mode. If this limit is exceeded, cache hander switch to disk mode.
const max_size_data_in_memory = 500
// Defines the maximum character length for placeholder keys.
const max_placeholders_key_size = 50
// Sets the maximum character length for placeholder values.
const max_placeholders_value_size = 3000
// Internal DTM operations utilize microseconds for time management, primarily aims to minimize the risk of collisions during the creation of temporary cache files
// especially in cases of simultaneous requests that do not yet have a cache.
const convert_seconds = i64(1000000)
const converted_cache_delay_expiration_at_min = i64(cache_delay_expiration_at_min) * convert_seconds
const converted_cache_delay_expiration_at_max = i64(cache_delay_expiration_at_max) * convert_seconds

const internat_server_error = 'Internal Server Error'

const message_signature = '[Dynamic Template Manager]'
const message_signature_info = '[Dynamic Template Manager] Info :'
const message_signature_error = '[Dynamic Template Manager] Error :'
const message_signature_warn = '[Dynamic Template Manager] Warning :'

// CacheStorageMode
pub enum CacheStorageMode {
	memory
	disk
}

enum CacheRequest {
	new
	update
	exp_update
	cached
	delete
}

// DynamicTemplateManager
@[heap]
pub struct DynamicTemplateManager {
mut:
	// Determines if the DTM initialization was performed successfully and if DTM is usable
	dtm_init_is_ok bool
	// Store the path to the cache directory.
	template_cache_folder string
	// Store the path to the HTML templates directory.
	template_folder string
	// cache database
	template_caches shared []TemplateCache = []TemplateCache{}
	// counter for each individual TemplateCache created/updated
	id_counter       int = 1
	ch_cache_handler chan TemplateCache = chan TemplateCache{cap: dtm.cache_handler_channel_cap}
	// 'id_to_handlered' field is used exclusively by the cache handler to update or delete specific 'TemplateCache' in the cache database.
	id_to_handlered     int
	close_cache_handler bool
	// Initialisation params options for these two (Compress_html and active_cache_server)
	compress_html       bool = true
	active_cache_server bool = true
	// Initialisation of max data size in memory storage
	max_size_data_in_memory int = dtm.max_size_data_in_memory
	// This array is designed to store a control process that checks whether cached data is currently in use while simultaneously handling expiration.
	// This allows for the harmonious management of both aspects and facilitates the necessary actions.
	nbr_of_remaining_template_request shared []RemainingTemplateRequest = []RemainingTemplateRequest{}
	//	cache_block_middleware ?MiddlewareFn
}

// Represent individual template cache in database memory.
@[noinit]
struct TemplateCache {
mut:
	id   int
	name string
	// 'path' field contains the full path, name and file extension of targeted HTML template.
	path string
	// Checksum of the cached template, which is constructed as follows: the complete HTML content plus its full file path and the timestamp of the cache generation.
	// This approach is utilized to create a unique identifier."
	checksum string
	// The checksum for the dynamic content of an HTML Template is constructed as follows: all dynamic values are concatenated into a single string,
	// and the checksum is then calculated based on this string.
	// Given that the value of this checksum is specific to its parent template, there is no issue of collision, and it does not need to be unique.
	content_checksum string
	// Name of the temporary template created to hold the content until the cache manager processes and handles the request accordingly.
	// This file is then deleted once it has been utilized.
	tmp_name_file string
	// The timestamp of the last modification of the file (HTML template)
	last_template_mod i64
	// Timestamp of cache generation.
	generate_at i64
	// Timestamp of cache expiration define by user.
	cache_delay_expiration i64
	html_data              []u8
	cache_request          CacheRequest
	cache_storage_mode     CacheStorageMode
	// This field is special as it determines if a cache is obsolete but still in use, allowing the system to redirect to the updated cache.
	// This enables the eventual deletion of the obsolete cache without causing issues for requests utilizing the cache.
	id_redirection int
}

// Represents controls stored in the 'nbr_of_remaining_template_request' of the DTM.
// It is used to monitor whether a HTML cache template is in use in 'template_caches' and to manage cache deletion procedures
// as soon as they become feasible and if required of course.
@[noinit]
struct RemainingTemplateRequest {
	// The id is similar to the one it represents in template_caches.
	id int
mut:
	// This value determines the number of ongoing requests using the cache specified by the ID.
	nbr_of_remaining_request int
	// The cache has become obsolete, and permission to dispose of it is granted to the cache manager
	need_to_delete bool
	// If the cache manager is unable to destroy an obsolete cache (due to it still being actively used),
	// then this boolean allows for the retransmission of the request, enabling action to be taken in the near future.
	need_to_send_delete_request bool
}

// TemplateCacheParams are used to specify cache expiration delay and provide placeholder data for substitution in templates.
@[params]
pub struct TemplateCacheParams {
	placeholders           &map[string]DtmMultiTypeMap = &map[string]DtmMultiTypeMap{}
	cache_delay_expiration i64 = dtm.cache_delay_expiration_by_default
}

// DynamicTemplateManagerInitialisationParams is used with 'initialize_dtm' function. (See below at initialize_dtm section)
@[params]
pub struct DynamicTemplateManagerInitialisationParams {
	compress_html        bool = true
	active_cache_server  bool = true
	max_size_data_in_mem int  = dtm.max_size_data_in_memory
	test_cache_dir       string
	test_template_dir    string
}

// create_dtm initializes and returns a reference to a new instance of DynamicTemplateManager on the heap.
pub fn create_dtm() &DynamicTemplateManager {
	return &DynamicTemplateManager{}
}

// initialize_dtm init the 'DynamicTemplateManager' with the storage mode, cache/templates path folders. The DTM requires this initialization function to be operational.
// A "vcache" directory must be created at the root of the project (where the executable is located) to use the DTM.
// Initalisation params are :
// - max_size_data_in_mem 'type int' Maximum size of data allowed in memory for caching. The value must be specified in kilobytes. ( Default is: 500KB / Limit max is : 500KB)	
// - compress_html: 'type bool' Light compress of the HTML ouput. ( default is true )
// - active_cache_server: 'type bool' Activate or not the template cache system. ( default is true )
// - test_cache_dir: 'type string' Used only for DTM internal test file, parameter is ignored otherwise.
// - test_template_dir: 'type string' Used only for DTM internal test file, parameter is ignored otherwise.
//
// vfmt off
pub fn initialize_dtm(mut dtmref &DynamicTemplateManager, dtm_init_params DynamicTemplateManagerInitialisationParams) ! {
	// vfmt on
	mut dir_path := ''
	mut dir_html_path := ''
	$if test {
		dir_path = dtm_init_params.test_cache_dir
		dir_html_path = dtm_init_params.test_template_dir
	} $else {
		dir_path = os.join_path('${os.dir(os.executable())}/vcache')
		dir_html_path = os.join_path('${os.dir(os.executable())}/templates')
	}
	// Control if 'vcache' folder exist in the root project
	if os.exists(dir_path) && os.is_dir(dir_path) {
		dtmref.template_cache_folder = dir_path
		// WARNING: When setting the directory for caching files and for testing purposes,
		// 'check_and_clear_cache_files' function will delete all "*.cache" or "*.tmp" files inside the specified 'vcache' directory in the project root's. Ensure that
		// directory used for the cache does not contain any important files.
		dtmref.check_and_clear_cache_files()!
		// Control if 'templates' folder exist in the root project
		if !os.exists(dir_html_path) && !os.is_dir(dir_html_path) {
			return error('${dtm.message_signature_error} The templates directory at the project root does not exist. Please create a "templates" directory at the root of your project with appropriate read permissions. This is a mandatory step for using the Dynamic Template Manager (DTM). Current path attempted for create the templates folder: "${dir_html_path}"')
		} else {
			dtmref.template_folder = dir_html_path
		}
	} else {
		return error('${dtm.message_signature_error} The cache storage directory at the project root does not exist. Please create a "vcache" directory at the root of your project with appropriate read/write permissions. This is a mandatory step for using the Dynamic Template Manager (DTM). Current path attempted for create the cache folder: "${dir_path}"')
	}
	// Validates the 'max_size_data_in_mem' setting in 'dtm_init_params'. If it's within the valid range, it's applied; otherwise, default value is used.
	if dtm_init_params.max_size_data_in_mem <= dtm.max_size_data_in_memory
		&& dtm_init_params.max_size_data_in_mem >= 0 {
		dtmref.max_size_data_in_memory = dtm_init_params.max_size_data_in_mem
	} else {
		mut type_error := 'exceeds'
		if dtm_init_params.max_size_data_in_mem < 0 {
			type_error = 'is invalid for define'
		}
		eprintln('${dtm.message_signature_info} The value "${dtm_init_params.max_size_data_in_mem}KB" ${type_error} the memory storage limit. It will not be considered, and the limit will be set to ${dtm.max_size_data_in_memory}KB.')
	}

	// Disable light HTML compression if user doesn't required. ( By default is ON )
	if !dtm_init_params.compress_html {
		dtmref.compress_html = false
	}
	// Disable cache handler if user doesn't required. Else, new thread is used to start the cache system. ( By default is ON )
	if !dtm_init_params.active_cache_server {
		dtmref.active_cache_server = false
	} else {
		spawn dtmref.cache_handler()
	}
	dtmref.dtm_init_is_ok = true
	println('${dtm.message_signature} Dynamic Template Manager mode activated')
}

/*
fn init_cache_block_middleware(cache_dir string, mut dtm &DynamicTemplateManager) {
    // Fonction locale correspondant à la signature MiddlewareFn
    dtm.cache_block_middleware = fn (mut ctx Context, cache_dir string) bool {
        request_path := ctx.req.url
        if request_path.contains(cache_dir) {
            ctx.text('Access to the cache directory is not allowed.')
            return false
        }
        return true
    }
}
*/

// serve_dynamic_template manages the cache and returns generated HTML.
// Requires an initialization via 'initialize_dtm' to running.
// To use this function, HTML templates must be located in the 'templates' directory at the project's root.
// However, it allows the use of subfolder paths within the 'templates' directory,
// enabling users to structure their templates in a way that best suits their project's organization.
pub fn (mut tm DynamicTemplateManager) serve_dynamic_template(tmpl_path string, tmpl_var TemplateCacheParams) string {
	if tm.dtm_init_is_ok {
		file_path, tmpl_name := tm.check_html_and_placeholders_size(tmpl_path, tmpl_var.placeholders) or {
			return err.msg()
		}
		converted_cache_delay_expiration := i64(tmpl_var.cache_delay_expiration) * dtm.convert_seconds
		// If cache exist, return necessary fields else, 'is_cache_exist' return false.
		is_cache_exist, id, path, mut last_template_mod, gen_at, cache_del_exp, c_time, content_checksum := tm.return_cache_info_isexistent(file_path)
		mut html := ''
		// Definition of several variables used to assess the need for cache updates.sss
		// This determination is based on modifications within the HTML template itself.
		// `last_template_mod` is set to 0 and `test_current_template_mod` to `i64(0)` when a new cache needs to be created.
		// `test_current_template_mod` is utilized for updating the cache.
		mut test_current_template_mod := i64(0)
		if last_template_mod == 0 {
			// Get last modification timestamp of HTML template to adding info for the creation of cache.
			last_template_mod = os.file_last_mod_unix(file_path)
		} else {
			// Get last modification timestamp of HTML template to compare with cache info already existant.
			test_current_template_mod = os.file_last_mod_unix(file_path)
		}
		// This function checks if the dynamic content ( If, however, it contains dynamic content ) of an HTML page has been updated.
		// If it has, it creates a checksum of this content for analysis.
		// The checksum is generated by concatenating all dynamic values and applying a fnv1a hash to the resulting string and generate a checksum.
		current_content_checksum := create_content_checksum(tmpl_var.placeholders, content_checksum)

		// From this point, all the previously encountered variables are used to determine the routing in rendering the HTML template and creating/using its cache.
		cash_req := cache_request_route(is_cache_exist, converted_cache_delay_expiration,
			last_template_mod, test_current_template_mod, cache_del_exp, gen_at, c_time,
			content_checksum, current_content_checksum)
		// Each of these match statements aims to provide HTML rendering, but each one sends a specific signal 'cash_req' of type CacheRequest
		// or calls the appropriate function for managing the cache of the provided HTML.
		match cash_req {
			.new {
				// Create a new cache
				html = tm.create_template_cache_and_display_html(cash_req, last_template_mod,
					c_time, file_path, tmpl_name, converted_cache_delay_expiration, tmpl_var.placeholders,
					current_content_checksum)
				//	println('create cache : ${cash_req}')
			}
			.update, .exp_update {
				// Update an existing cache
				tm.id_to_handlered = id
				html = tm.create_template_cache_and_display_html(cash_req, test_current_template_mod,
					c_time, file_path, tmpl_name, converted_cache_delay_expiration, tmpl_var.placeholders,
					current_content_checksum)
				//	println('update cache : ${cash_req}')
			}
			else {
				// Use the provided cache of html template.
				html = tm.get_cache(tmpl_name, path, tmpl_var.placeholders)
				//	println('get cache : ${cash_req}')
			}
		}
		return html
	} else {
		tm.stop_cache_handler()
		eprintln('${dtm.message_signature_error} The initialization phase of DTM has failed. Therefore, you cannot use it. Please address the errors and then restart the dtm server.')
		return dtm.internat_server_error
	}
}

// fn create_content_checksum(&map[string]DtmMultiTypeMap, string) return string
//
//	This function serves to monitor dynamic content for updates by generating a checksum that is compared against the cached version to verify any changes.
//
fn create_content_checksum(pl &map[string]DtmMultiTypeMap, content_checksum string) string {
	mut combined_str := ''
	mut res := ''
	if pl.len > 0 {
		for _, value in pl {
			match value {
				i8, i16, int, i64, u8, u16, u32, u64, f32, f64 {
					combined_str += value.str()
				}
				string {
					combined_str += value
				}
			}
		}
		res = fnv1a.sum64_string(combined_str).str()
	} else {
		res = content_checksum
	}
	return res
}

// fn (DynamicTemplateManager) check_and_clear_cache_files()
//
// Used exclusively during the initialization of the DTM (Dynamic Template Manager).
// Its primary purpose is to ensure a clean starting environment by clearing all files
// within the designated cache directory. It iterates through the directory, listing all files,
// and systematically removes each file found, ensuring that no residual cache data persists
// from previous executions. Additionally, this function also tests the read and write permissions
// of the cache directory to ensure that the application has the necessary access to properly manage the cache files.
//
// WARNING: When setting the directory for caching files and for testing purposes,
// this function will delete all "*.cache" or "*.tmp" files inside the 'vcache' directory in the project root's. Ensure that
// directory used for the cache does not contain any important files.
//
fn (tm DynamicTemplateManager) check_and_clear_cache_files() ! {
	//	println('${message_signature} WARNING! DTM needs to perform some file tests in the 'vcache' directory in your project. This operation will erase all "*.cache" or "*.tmp" files content in the folder : "${tm.template_cache_folder}"')
	//	println('Do you want to continue the operation? (yes/no)')
	//	mut user_response := os.input('>').to_lower()
	//	if user_response != 'yes' && user_response != 'y' {
	//		return error('${message_signature_error} Operation cancelled by the user. DTM initialization failed.')
	//	} else {
	file_p := os.join_path(tm.template_cache_folder, 'test.tmp')
	// Create a text file for test permission access
	mut f := os.create(file_p) or {
		return error('${dtm.message_signature_error} Files are not writable. Test fail, DTM initialization failed : ${err.msg()}')
	}
	f.close()
	// Read the previous text file for test permission access
	os.read_file(file_p) or {
		return error('${dtm.message_signature_error} Files are not readable. Test fail, DTM initialization failed : ${err.msg()}')
	}
	// List all files in the cache folder
	files_list := os.ls(tm.template_cache_folder) or {
		return error('${dtm.message_signature_error} While listing the cache directorie files, DTM initialization failed : ${err.msg()}')
	}
	// Delete one by one "*.cache" or "*.tmp" files in the previous file list
	for file in files_list {
		file_path := os.join_path(tm.template_cache_folder, file)
		file_extension := os.file_ext(file_path).to_lower()
		if file_extension in ['.tmp', '.cache'] {
			os.rm(file_path) or {
				eprintln('${dtm.message_signature_error} While deleting the cache file: ${file_path}. DTM initialization failed : ${err.msg()}')
				return
			}
		}
	}
	//	}
}

// fn (DynamicTemplateManager) check_html_and_placeholders_size(string, &map[string]DtmMultiTypeMap) return !(string, string)
//
// Used exclusively in the 'serve_dynamic_template' function, this check verifies if the HTML file exists and is located within the 'templates' directory at the project's root.
// It also ensures the file extension is HTML and controls the size of placeholder keys and values in the provided map,
// offering a basic validation and security measure against excessively long and potentially harmful inputs.
// Size limits are defined by the 'max_placeholders_key_size' and 'max_placeholders_value_size' constants.
//
fn (tm DynamicTemplateManager) check_html_and_placeholders_size(f_path string, tmpl_var &map[string]DtmMultiTypeMap) !(string, string) {
	mut html_file := ''
	$if test {
		html_file = f_path
	} $else {
		html_file = os.join_path(tm.template_folder, f_path)
	}
	if os.exists(html_file) {
		// Extracts the base file name with extension from the given file path.
		file_name_with_ext := os.base(html_file)
		// Removes the file extension, keeping only the name.
		file_name := file_name_with_ext.all_before_last('.')
		// Performs a basic check of the file extension.
		ext := os.file_ext(html_file)
		if ext != '.html' {
			eprintln('${dtm.message_signature_error} ${html_file}, is not a HTML file')
			return error(dtm.internat_server_error)
		}
		// Control placeholder key and value sizes
		for key, value in tmpl_var {
			if key.str().len > dtm.max_placeholders_key_size {
				eprintln('${dtm.message_signature_error} Length of placeholder key "${key}" exceeds the maximum allowed size for HTML content in file: ${html_file}. Max allowed size: ${dtm.max_placeholders_key_size} characters.')
				return error(dtm.internat_server_error)
			}
			if value.str().len > dtm.max_placeholders_value_size {
				eprintln('${dtm.message_signature_error} Length of placeholder value for key "${key}" exceeds the maximum allowed size for HTML content in file: ${html_file}. Max allowed size: ${dtm.max_placeholders_value_size} characters.')
				return error(dtm.internat_server_error)
			}
		}
		// If all is ok, return full path of HTML template file and HTML filename without extension
		return html_file, file_name
	} else {
		eprintln("${dtm.message_signature_error} Template : '${html_file}' not found. Ensure all HTML templates are located in the 'templates' directory at the project's root.")
		return error(dtm.internat_server_error)
	}
}

// fn (mut DynamicTemplateManager) create_template_cache_and_display_html(CacheRequest, i64, i64, string, string, i64, &map[string]DtmMultiTypeMap) return string
//
// Exclusively invoked from `serve_dynamic_template`.
// It role is generate the HTML rendering of a template and relaying informations
// to the cache manager for either the creation or updating of the HTML cache.
// It begin to starts by ensuring that the cache delay expiration is correctly set by user.
// It then parses the HTML file, replacing placeholders with actual dynamics/statics values.
// If caching is enabled (indicated by a cache delay expiration different from -1) and the HTML content is valid,
// the function constructs a `TemplateCache` request with all the necessary details.
// This request is then sent to the cache handler channel, signaling either the need for a new cache or an update to an existing one.
// The function returns the rendered HTML string immediately, without waiting for the cache to be created or updated.
//
fn (mut tm DynamicTemplateManager) create_template_cache_and_display_html(tcs CacheRequest, last_template_mod i64, c_time i64, file_path string, tmpl_name string, cache_delay_expiration i64, placeholders &map[string]DtmMultiTypeMap, current_content_checksum string) string {
	// Control if cache delay expiration is correctly setted. See the function itself for more details.
	check_if_cache_delay_iscorrect(cache_delay_expiration, tmpl_name) or {
		eprintln(err)
		return dtm.internat_server_error
	}
	// Parses the HTML and stores the rendered output in the variable. See the function itself for more details.
	mut html := tm.parse_html_file(file_path, tmpl_name, placeholders, tm.compress_html)
	// If caching is enabled and the HTML content is valid, this section creates a temporary cache file, which is then used by the cache manager.
	// If successfully temporary is created, a cache creation/update notification is sent through its dedicated channel to the cache manager
	if cache_delay_expiration != -1 && html != dtm.internat_server_error && tm.active_cache_server {
		op_success, tmp_name := tm.create_temp_cache(html, file_path, c_time)
		if op_success {
			tm.ch_cache_handler <- TemplateCache{
				id: tm.id_counter
				name: tmpl_name
				// 'path' field contains the full path, name and file extension of targeted HTML template.
				path: file_path
				content_checksum: current_content_checksum
				tmp_name_file: tmp_name
				// Last modified timestamp of HTML template
				last_template_mod: last_template_mod
				// Unix current local timestamp of cache generation request converted to UTC
				generate_at: c_time
				// Defines the cache expiration delay in seconds. This value is added to 'generate_at' to calculate the expiration time of the cache.
				cache_delay_expiration: cache_delay_expiration
				// The requested routing to define creation or updating cache.
				cache_request: tcs
			}
			// In the context of a cache update, this function is used to signal that the process has finished using the cache information. The 'nbr_of_remaining_request' counter is therefore updated."
			if tcs == .update || tcs == .exp_update {
				tm.remaining_template_request(false, tm.id_to_handlered)
			}
		}
	} else {
		// In the context of an HTML validity error, the 'nbr_of_remaining_request' counter is consistently updated to avoid anomalies in cache management.
		tm.remaining_template_request(false, tm.id_to_handlered)
	}

	return html
}

// fn (DynamicTemplateManager) create_temp_cache(&string, string, i64) return (bool, string)
//
// This function is responsible for creating a temporary cache file, which is subsequently used exclusively by the cache manager.
// It generates a temporary file name using a checksum based on the timestamp and the file path.
// The content is then written to this file, located in the designated cache folder.
// If the operation is successful, a boolean is returned to allow for the sending of a create or modify request to the cache manager.
//
fn (tm DynamicTemplateManager) create_temp_cache(html &string, f_path string, ts i64) (bool, string) {
	// Extracts the base file name with extension from the given file path.
	file_name_with_ext := os.base(f_path)
	// Removes the file extension, keeping only the name.
	file_name := file_name_with_ext.all_before_last('.')
	// Combines the timestamp and file path into a single string
	combined_str := ts.str() + f_path
	// Generates a md5 hash of the combined string for uniqueness
	tmp_checksum := md5.hexhash(combined_str)
	// Forms the temporary file name using the file name, checksum, and a .tmp extension
	tmp_name := '${file_name}_${tmp_checksum}.tmp'
	// Creates the full path for the temporary file in the cache folder
	cache_path := os.join_path(tm.template_cache_folder, tmp_name)
	// Converts the HTML content into a byte array
	html_bytes := html.bytes()
	mut f := os.create(cache_path) or {
		eprintln('${dtm.message_signature_error} Cannot create tempory cache file : ${err.msg()}')
		return false, ''
	}
	f.write(html_bytes) or {
		eprintln('${dtm.message_signature_error} Cannot write in temporary cache file : ${err.msg()}')
		f.close()
		return false, ''
	}
	f.close()
	return true, tmp_name
}

// fn (mut DynamicTemplateManager) get_cache(string, string, &map[string]DtmMultiTypeMap) return string
//
// Exclusively invoked from `serve_dynamic_template', retrieves the rendered HTML from the cache.
//
fn (mut tm DynamicTemplateManager) get_cache(name string, path string, placeholders &map[string]DtmMultiTypeMap) string {
	mut html := ''
	// Lock the cache database for writing.
	rlock tm.template_caches {
		for value in tm.template_caches {
			// If the cache for the specified HTML template is found, perform the following operations:
			if value.path == path {
				match value.cache_storage_mode {
					.memory {
						// Retrieve the HTML render from the memory cache and convert it to a string.
						html = value.html_data.bytestr()
					}
					.disk {
						// Retrieve the HTML render from the file cache in disk and convert it to a string.
						file_name := os.join_path(tm.template_cache_folder, '${value.name}_${value.checksum}.cache')
						r_b_html := os.read_bytes(file_name) or {
							eprintln('${dtm.message_signature_error} Get_cache() cannot read template cache file ${value.name} : ${err.msg()} ')
							return dtm.internat_server_error
						}
						html = r_b_html.bytestr()
					}
				}
				// Function is used to signal that the process has finished using the cache information. The 'nbr_of_remaining_request' counter is therefore updated."
				tm.remaining_template_request(false, value.id)
				return html
			}
		}
	}

	return html
}

// fn (mut DynamicTemplateManager) return_cache_info_isexistent(string) return (bool, int, string, i64, i64, i64, i64, string)
//
// Exclusively used in 'serve_dynamic_template' to determine whether a cache exists for the provided HTML template.
// If a cache exists, it returns the necessary information for its transformation. If not, it indicates the need to create a new cache.
//
fn (mut tm DynamicTemplateManager) return_cache_info_isexistent(tmpl_path string) (bool, int, string, i64, i64, i64, i64, string) {
	c_time := get_current_unix_timestamp()
	// Lock the cache database for writing.
	rlock tm.template_caches {
		for value in tm.template_caches {
			if value.path == tmpl_path {
				// This code section handles cache redirection.
				// If a cache redirection ID is found, it indicates that the currently used cache is outdated and there's a newer version available.
				// The process then seeks to retrieve information from this more recent cache.
				// This is done recursively: if the updated cache itself points to an even newer version, the process continues until the most up-to-date cache is found.
				// This recursive mechanism ensures that the latest cache data is always used.
				if value.id_redirection != 0 {
					mut need_goto := false
					mut id_value_recursion := value.id_redirection
					unsafe {
						re_loop:
						inner_loop: for val in tm.template_caches {
							if val.id == id_value_recursion {
								if val.id_redirection != 0 {
									id_value_recursion = val.id_redirection
									need_goto = true
									break inner_loop
								} else {
									// function is used to signal that the process has begun using the cache information.
									tm.remaining_template_request(true, val.id)
									return true, val.id, val.path, val.last_template_mod, val.generate_at, val.cache_delay_expiration, c_time, val.content_checksum
								}
							}
						}
						if need_goto {
							need_goto = false
							goto re_loop
						}
					}
					// No cache redirection, get cache current informations.
				} else {
					// function is used to signal that the process has begun using the cache information.
					tm.remaining_template_request(true, value.id)
					return true, value.id, value.path, value.last_template_mod, value.generate_at, value.cache_delay_expiration, c_time, value.content_checksum
				}
			}
		}
	}
	// No existing cache, need to create it.
	return false, 0, '', 0, 0, 0, c_time, ''
}

// fn (mut DynamicTemplateManager) remaining_template_request(bool, int)
//
// This function manages the counter in 'nbr_of_remaining_template_request', which tracks the number of requests that have started or finished for a specific cache.
// It updates this information accordingly.
// Moreover, this function sends a cache deletion callback request when the cache manager had previously been instructed to delete the cache but was unable to do because,
// it was still in use.
//
fn (mut tm DynamicTemplateManager) remaining_template_request(b bool, v int) {
	// Lock the remaining template request process for reading and writing.
	lock tm.nbr_of_remaining_template_request {
		for key, r_request in tm.nbr_of_remaining_template_request {
			if r_request.id == v {
				if b == true {
					// if true, indicating a new request for the cache, Increments the count of active requests.
					tm.nbr_of_remaining_template_request[key].nbr_of_remaining_request += 1
				} else {
					// if false, Decrements the count of active cache requests.
					tm.nbr_of_remaining_template_request[key].nbr_of_remaining_request -= 1
					// Checks if the number of active requests is zero or less and if there's a pending delete request for the cache.
					// If yes, request is sent to the cache handler on this own channel.
					if r_request.nbr_of_remaining_request <= 0
						&& r_request.need_to_send_delete_request {
						tm.ch_cache_handler <- TemplateCache{
							id: r_request.id
							cache_request: .delete
						}
					}
				}
				break
			}
		}
	}
}

// fn (mut DynamicTemplateManager) cache_handler()
//
// This function serves as the core handler for managing the cache within the DTM.
// It continuously listens for cache update requests and processes them accordingly.
// When a cache request is received, it either creates or updates or deletes the cache based on the request type.
// The function ensures that duplicate cache requests are avoided and handles the closing of the cache handler.
// It is necessary to restart the entire application in case the manager closes.
// The manager handles cache operations in a multithreaded context, accepting up to a list of 200 operation requests. (Define in 'cache_handler_channel_cap' constant)
// The HTML rendering is stored as a u8 array.
//
// TODO - Currently, the cache manager stops when it encounters an internal error requiring a restart of the program.
// ( it is designed to ignore external errors since these are already handled in a way that ensures no cache processing requests are affected ),
// A recovery system will need to be implemented to ensure service continuity.
//
fn (mut tm DynamicTemplateManager) cache_handler() {
	defer {
		// If cause is an internal cache handler error
		tm.active_cache_server = false
		// Close channel if handler is stopped
		tm.ch_cache_handler.close()
	}
	for {
		select {
			// Continuously listens until a request is received through the dedicated channel.
			mut tc := <-tm.ch_cache_handler {
				// Close handler if asked.
				if tm.close_cache_handler {
					eprintln('${dtm.message_signature_info} Cache manager has been successfully stopped. Please consider restarting the application if needed.')
					break
				}
				f_path_tmp := os.join_path(tm.template_cache_folder, tc.tmp_name_file)

				// determine if the requests passed to the manager are not duplicate requests. If so, the temporary file will be destroyed as part of a cache creation/update request.
				if !tm.chandler_prevent_cache_duplicate_request(tc) {
					if tc.cache_request != .delete {
						// Determines the size of the template content to decide where to store it (in memory or on disk).
						// It retrieves the content from the temporary file and then forms a unique checksum for the final name of the cache file.
						// The file size is compared to the maximum allowable data size in memory.
						// If the size is within the limit, the cache is stored in memory; otherwise, it's stored on disk.
						// The unique checksum is generated by hashing the file data, its path, and the cache generation timestamp using md5.
						tmp_file_size := os.file_size(f_path_tmp)
						if tmp_file_size <= (tm.max_size_data_in_memory * 1024) {
							tc.cache_storage_mode = .memory
						} else {
							tc.cache_storage_mode = .disk
						}
						file_data := os.read_bytes(f_path_tmp) or {
							eprintln('${dtm.message_signature_error} Cache Handler : Failed to read tmp file, cache server will be stopped, you need to fix and restart application: ${err.msg()}')
							break
						}

						combined_str := file_data.str() + tc.path + tc.generate_at.str()
						tc.checksum = md5.hexhash(combined_str)

						match tc.cache_storage_mode {
							.memory {
								// If the cache is stored in memory, the temporary file is destroyed.
								tc.html_data = file_data
								os.rm(f_path_tmp) or {
									eprintln('${dtm.message_signature_error} Cache Handler : While deleting the tmp cache file: "${f_path_tmp}", cache server will be stopped, you need to fix and restart application: ${err.msg()}')
									break
								}
							}
							.disk {
								// If the cache is stored on disk, the temporary file is renamed to become the definitive cache of the current version of the HTML template.
								new_cache_file_name := os.join_path(tm.template_cache_folder,
									'${tc.name}_${tc.checksum}.cache')
								os.mv(f_path_tmp, new_cache_file_name) or {
									eprintln('${dtm.message_signature_error} Cache Handler : Failed to rename tmp file, cache server will be stopped, you need to fix and restart application: ${err.msg()}')
									break
								}
							}
						}
					}
					// Lock the cache database for reading and writing.
					lock tm.template_caches {
						if tc.cache_request != .delete {
							// Include Cache information in database.
							tm.template_caches << tc
						}
						if tc.cache_request == .new {
							tm.chandler_remaining_cache_template_used(tc.cache_request,
								tc.id, tm.id_to_handlered)
							// Increment ID counter for the next creation/update cache request
							tm.id_counter++
						} else {
							if tc.cache_request != .delete {
								tm.id_counter++
							}
							// This function allows the cache manager to handle what happens in 'nbr_of_remaining_template_request' and
							// act accordingly for the creation, update, or destruction of the cache.
							test_b := tm.chandler_remaining_cache_template_used(tc.cache_request,
								tc.id, tm.id_to_handlered)
							if test_b {
								// Finding position of cache in database ( If disk mode, cache is erased )
								key, is_success := tm.chandler_clear_specific_cache(tm.id_to_handlered)
								if !is_success {
									break
								}
								// Delete in database.
								tm.template_caches.delete(key)
							}
						}
					}
				} else if tc.cache_request != .delete {
					os.rm(f_path_tmp) or {
						eprintln('${dtm.message_signature_warn} Cache Handler : Cannot deleting the unused tmp cache file: "${f_path_tmp}" : ${err.msg()}')
					}
				}
			}
		}
	}
}

// fn (DynamicTemplateManager) chandler_prevent_cache_duplicate_request(&TemplateCache) return bool
//
// Exclusively used by the cache handler, assesses whether a cache request is a duplicate,
// based on the type of cache request (.new, .update, .exp_update, .delete) and the existing data.
// Returns true to indicate a duplicate request, which will be ignored, and false otherwise.
//
fn (tm DynamicTemplateManager) chandler_prevent_cache_duplicate_request(tc &TemplateCache) bool {
	match tc.cache_request {
		.new {
			for value in tm.template_caches {
				// Evaluate full path
				if value.path == tc.path {
					return true
				}
			}
		}
		.update {
			for value in tm.template_caches {
				// Evaluate full path + The timestamp of the last html file modification + checksum of content.
				if value.path == tc.path && value.last_template_mod == tc.last_template_mod
					&& value.content_checksum == tc.content_checksum {
					return true
				}
			}
		}
		.exp_update {
			for value in tm.template_caches {
				// Evaluate full path + The timestamp of the last html modification file +
				// Checks if the current cache generation time is within the expiration window set from the last update.
				if value.path == tc.path && value.last_template_mod == tc.last_template_mod {
					if tc.generate_at < (value.generate_at + value.cache_delay_expiration) {
						return true
					}
				}
			}
		}
		.delete {
			for value in tm.template_caches {
				// Evaluate ID to test if cache is always in database.
				if value.id == tc.id {
					return false
				}
			}
			// Otherwise cache has already been deleted
			return true
		}
		else {}
	}

	return false
}

// fn (mut DynamicTemplateManager) chandler_clear_specific_cache(int) return (int, bool)
//
// Exclusively associated with the cache handler, is used to remove specific cache information from the database when necessary.
// It identifies the target 'TemplateCache' by its id in the array database and deletes its corresponding cache file in 'disk mode'.
//
fn (mut tm DynamicTemplateManager) chandler_clear_specific_cache(id int) (int, bool) {
	for key, value in tm.template_caches {
		if value.id == id {
			match value.cache_storage_mode {
				.memory {}
				.disk {
					file_path := os.join_path(tm.template_cache_folder, '${value.name}_${value.checksum}.cache')
					os.rm(file_path) or {
						eprintln('${dtm.message_signature_error} While deleting the specific cache file: ${file_path}, cache server will be stopped, you need to fix and restart application: : ${err.msg()}')
						break
					}
				}
			}
			return key, true
		}
	}
	return 0, false
}

// fn (mut DynamicTemplateManager) chandler_remaining_cache_template_used(CacheRequest, int, int) return bool
//
// Exclusively associated with the cache handler for managing the lifecycle of cache requests in 'nbr_of_remaining_template_request'.
// For each cache request (creation, update, or deletion), it updates the status of ongoing requests and decides on necessary actions.
// The function returns a boolean value: if true, it authorizes the destruction of the expired cache, ensuring that it's only removed when no longer in use.
// If false, it indicates that the cache cannot yet be destroyed due to ongoing usage.
//
fn (mut tm DynamicTemplateManager) chandler_remaining_cache_template_used(cr CacheRequest, id int, old_id int) bool {
	lock tm.nbr_of_remaining_template_request {
		match cr {
			// Adds a new request in 'nbr_of_remaining_template_request' to track the usage of the newly created cache
			.new {
				tm.nbr_of_remaining_template_request << RemainingTemplateRequest{
					id: id
				}
			}
			.update, .exp_update {
				// Marks the old cache as obsolete and adds a request for the new cache.
				// If the old cache is no longer in use, it is immediately deleted. Otherwise, a flag is set for deferred deletion of the cache as soon as feasible
				for key, mut value in tm.nbr_of_remaining_template_request {
					if value.id == old_id {
						value.need_to_delete = true
						tm.nbr_of_remaining_template_request << RemainingTemplateRequest{
							id: id
						}
						// If possible, immediately deleted request
						if value.nbr_of_remaining_request <= 0 && value.need_to_delete == true {
							tm.nbr_of_remaining_template_request.delete(key)
							return true
							// else, set for deferred deletion of the cache as soon as feasible
						} else {
							value.need_to_send_delete_request = true
						}
						break
					}
				}
				return false
			}
			.delete {
				// Removes the cache request from the list if it's no longer in use.
				for key, value in tm.nbr_of_remaining_template_request {
					if value.id == id {
						if value.nbr_of_remaining_request <= 0 && value.need_to_delete == true {
							tm.nbr_of_remaining_template_request.delete(key)
						}

						break
					}
				}
			}
			else {}
		}
	}
	return true
}

// fn (mut DynamicTemplateManager) stop_cache_handler()
//
// Signals the termination of the cache handler by setting 'close_cache_handler' to true and sending a signal through the channel.
//
fn (mut tm DynamicTemplateManager) stop_cache_handler() {
	if tm.active_cache_server {
		tm.active_cache_server = false
		tm.close_cache_handler = true
		tm.ch_cache_handler <- TemplateCache{
			id: 0
		}
	}
}

// fn (mut DynamicTemplateManager) parse_html_file(string, string, &map[string]DtmMultiTypeMap, bool) return (string, string)
//
// The V compiler's template parser 'vlib/v/parser/tmpl.v', parses and transforms HTML template file content.
// It ensures HTML format compatibility necessary for proper compilation and execution in its typical usage outside of DTM like managing various states,
// processing template tags, and supporting string interpolation...
// This function checks for the presence and validity of template directives '@include'
// to prevent runtime errors related to incorrect inclusion paths. Replaces placeholders with their actual values,
// including dynamic content with the possibility of adding HTML code but only for certain specified tags and can also light compress HTML if required ( Removing usless spaces ).
//
// TODO - This function does not perform an in-depth check to ensure the file is indeed a valid HTML file, which could lead to possible runtime crashes,
// Addressing this by adding a control mechanism is recommended for enhanced stability.
// For now, it is the user's responsibility to provide the correct HTML file(s) and HTML file(s)-content format.
//
const allowed_tags = ['<div>', '</div>', '<h1>', '</h1>', '<h2>', '</h2>', '<h3>', '</h3>', '<h4>',
	'</h4>', '<h5>', '</h5>', '<h6>', '</h6>', '<p>', '</p>', '<br>', '<hr>', '<span>', '</span>',
	'<ul>', '</ul>', '<ol>', '</ol>', '<li>', '</li>', '<dl>', '</dl>', '<dt>', '</dt>', '<dd>',
	'</dd>', '<menu>', '</menu>', '<table>', '</table>', '<caption>', '</caption>', '<th>', '</th>',
	'<tr>', '</tr>', '<td>', '</td>', '<thread>', '</thread>', '<tbody>', '</tbody>', '<tfoot>',
	'</tfoot>', '<col>', '</col>', '<colgroup>', '</colgroup>', '<header>', '</header>', '<footer>',
	'</footer>', '<main>', '</main>', '<section>', '</section>', '<article>', '</article>', '<aside>',
	'</aside>', '<details>', '</details>', '<dialog>', '</dialog>', '<data>', '</data>', '<summary>',
	'</summary>']

const include_html_key_tag = '_#includehtml'

fn (mut tm DynamicTemplateManager) parse_html_file(file_path string, tmpl_name string, placeholders &map[string]DtmMultiTypeMap, is_compressed bool) string {
	// To prevent runtime crashes related to template include directives error,
	// this code snippet ensures that the paths in include directives '@include' are correct.
	html_content := os.read_file(file_path) or {
		eprintln("${dtm.message_signature_error} Unable to read the file: '${file_path}' with HTML parser function.")
		return dtm.internat_server_error
	}
	mut re := regex.regex_opt('.*@include(?P<space_type>[ \t\r\n]+)(?P<quote_type_beg>[\'"])(?P<path>.*)(?P<quote_type_end>[\'"])') or {
		tm.stop_cache_handler()
		eprintln('${dtm.message_signature_error} with regular expression for template @inclusion in parse_html_file() function. Please check the syntax of the regex pattern : ${err.msg()}')
		return dtm.internat_server_error
	}
	// Find all occurrences of the compiled regular expression within the HTML content.
	matches := re.find_all(html_content)
	// Check if any matches were found.
	if matches.len > 0 {
		mut full_path := ''
		// Iterate through the matches. Since each match has a start and end index, increment by 2 for each iteration.
		for i := 0; i < matches.len; i += 2 {
			// Retrieve the start and end indices of the current match.
			start := matches[i]
			end := matches[i + 1]
			// Extract the substring from the HTML content that corresponds to the current match.
			match_text := html_content[start..end]
			// Apply the regex to the extracted substring to enable group capturing.
			re.match_string(match_text)
			// Extract the path from the current match.
			mut path := re.get_group_by_name(match_text, 'path')
			// Extract the type of quotation marks.
			quote_type_beg := re.get_group_by_name(match_text, 'quote_type_beg')
			quote_type_end := re.get_group_by_name(match_text, 'quote_type_end')
			// Extract the whitespace characters following the '@include' directive.
			space_type := re.get_group_by_name(match_text, 'space_type')
			// Check if double quotes are used or if the whitespace sequence contains newline (\n) or carriage return (\r) characters
			if quote_type_beg == '"' || quote_type_end == '"' || space_type.contains('\n')
				|| space_type.contains('\r') {
				eprintln("${dtm.message_signature_error} In the HTML template: '${file_path}', an error occurred in one of the '@include' directives. This could be due to the use of double quotes or unexpected newline/carriage return characters in the whitespace.")
				return dtm.internat_server_error
			}
			// Check if the 'path' string does not end with '.html'. If it doesn't, append '.html' to ensure the path has the correct extension.
			if !path.ends_with('.html') {
				path += '.html'
			}
			full_path = os.join_path(tm.template_folder, path)
			// Check if the 'path' is empty or if the HTML template path does not exist.
			if path.len < 1 || !os.exists(full_path) {
				eprintln("${dtm.message_signature_error} In the HTML template: '${file_path}', an error occurred in one of the '@include' directives. This could be due to the use of an invalid path: ${full_path}")
				return dtm.internat_server_error
			}
		}
	}
	mut p := parser.Parser{}
	// Parse/transform the HTML content, and a subsequent cleaning function restores the parsed content to a usable state for the DTM.
	// Refer to the comments in 'clean_parsed_html' for details.
	mut html := tm.clean_parsed_html(p.compile_template_file(file_path, tmpl_name), tmpl_name,
		placeholders)
	// If clean_parsed_html() return error
	if html == dtm.internat_server_error {
		return html
	}
	// This section completes the processing by replacing any placeholders that were not handled by the compiler template parser.
	// If there are placeholders present, it iterates through them, applying necessary filters and substituting their values into the HTML content.
	// dtm.filter function used here. 'escape_html_strings_in_templates.v'
	// Checks if there are any placeholders to process
	if placeholders.len > 0 {
		for key, value in placeholders {
			mut val := ''
			mut key_m := key
			match value {
				i8, i16, int, i64, u8, u16, u32, u64, f32, f64 {
					// Converts value to string
					temp_val := value.str()
					// Filters the string value for safe HTML insertion
					val = filter(temp_val)
				}
				string {
					// Checks if the placeholder allows HTML inclusion
					if key.ends_with(dtm.include_html_key_tag) {
						// Iterates over allowed HTML tags for inclusion
						for tag in dtm.allowed_tags {
							// Escapes the HTML tag
							escaped_tag := filter(tag)
							// Replaces the escaped tags with actual HTML tags in the value
							val = value.replace(escaped_tag, tag)
						}
						// Adjusts the placeholder key by removing the HTML inclusion tag
						key_m = key.all_before_last(dtm.include_html_key_tag)
					} else {
						// Filters the string value for safe HTML insertion
						val = filter(value)
					}
				}
			}
			// Forms the actual placeholder to be replaced in the HTML.
			placeholder := '$${key_m}'
			// Check if placeholder exist in the HTML
			if html.contains(placeholder) {
				// Replaces the placeholder if exist in the HTML with the actual value.
				html = html.replace(placeholder, val)
			}
		}
	}

	// Performs a light compression of the HTML output by removing usless spaces, newlines, and tabs if user selected this option.
	if is_compressed {
		html = html.replace_each(['\n', '', '\t', '', '  ', ' '])
		mut r := regex.regex_opt(r'>(\s+)<') or {
			tm.stop_cache_handler()
			eprintln('${dtm.message_signature_error} with regular expression for HTML light compression in parse_html_file() function. Please check the syntax of the regex pattern : ${err.msg()}')
			return dtm.internat_server_error
		}
		html = r.replace(html, '><')
		for html.contains('  ') {
			html = html.replace('  ', ' ')
		}
	}

	return html
}

// fn (mut DynamicTemplateManager) clean_parsed_html(string, string) return string
//
// Is specifically designed to clean the HTML output generated by V's compiler template parser.
// It addresses the fact that the parser prepares HTML content for integration into an executable, rather than for direct use in a web browser.
// The function adjusts markers and escapes specific characters to convert the parser's output into a format suitable for web browsers.
//
// TODO - Any changes to the HTML output made by the V lang compiler template parser in 'vlib/v/parser/tmpl.v'
// may necessitate corresponding adjustments in this function. Perhaps a more independent function is needed to clean the HTML rendering?
//
// TODO - This function does not currently handle the cleanup of all template directives typically managed by the Vlang compiler, such as conditional statements or loops....
// Implementation of these features will be necessary.
//
fn (mut tm DynamicTemplateManager) clean_parsed_html(tmpl string, tmpl_name string, provided_placeholders &map[string]DtmMultiTypeMap) string {
	// Defines the start marker to encapsulate HTML content
	start_marker := "sb_${tmpl_name}.write_string('"
	// Determines the end marker, signaling the end of HTML content
	end_marker := "')\n\n\t_tmpl_res_${tmpl_name} := sb_${tmpl_name}.str()"
	// Searches for the start marker in the processed HTML content. Triggers an error if the start marker is not found.
	start := tmpl.index(start_marker) or {
		eprintln("${dtm.message_signature_error} Start marker not found for '${tmpl_name}': ${err.msg()}")
		// dtm.filter function used here. 'escape_html_strings_in_templates.v'
		return filter(tmpl)
	}
	// Identifies the last occurrence of the end marker. Signals an error if it is missing.
	end := tmpl.index_last(end_marker) or {
		eprintln("${dtm.message_signature_error} End marker not found for '${tmpl_name}': ${err.msg()}")
		// dtm.filter function used here. 'escape_html_strings_in_templates.v'
		return filter(tmpl)
	}
	// Extracts the portion of HTML content between the start and end markers.
	mut html := tmpl[start + start_marker.len..end]

	// Utilizes a regular expression to identify placeholders within the HTML template output.
	// This process checks for placeholders that have no corresponding entries in the provided placeholders map.
	// Any unmatched placeholders found are then safely escaped
	mut r := regex.regex_opt(r'$([a-zA-Z_][a-zA-Z0-9_]*)') or {
		tm.stop_cache_handler()
		eprintln('${dtm.message_signature_error} with regular expression for identifying placeholders in the HTML template in clean_parsed_html() function. Please check the syntax of the regex pattern : ${err.msg()}')
		return dtm.internat_server_error
	}
	indices := r.find_all(html)
	// Iterates through each found placeholder.
	for i := 0; i < indices.len; i += 2 {
		// Retrieves the start and end indices of the current placeholder.
		beginning := indices[i]
		ending := indices[i + 1]
		// Extracts the placeholder from the HTML using the indices.
		placeholder := html[beginning..ending]
		// Removes the '$' symbol to get the placeholder name.
		placeholder_name := placeholder[1..]
		// Checks if the placeholder or its variant with '_#includehtml' is not in the provided placeholders map.
		if !(placeholder_name in provided_placeholders
			|| (placeholder_name + dtm.include_html_key_tag) in provided_placeholders) {
			// If so, escapes the unresolved placeholder and replaces the original placeholder with the escaped version
			escaped_placeholder := filter(placeholder)
			html = html.replace(placeholder, escaped_placeholder)
		}
	}

	// Transforms parser-specific escape sequences into their respective characters for proper HTML
	html = html.replace('\\n', '\n').replace("\\'", "'")
	return html
}

// fn check_if_cache_delay_iscorrect(i64, string) return !
//
// Validates the user-specified cache expiration delay for HTML templates.
// It enforces three permissible delay settings:
// - A minimum of five minutes and a maximum of one year for standard cache expiration. ( Define in constants )
// - A parameter of 0 for an infinite cache expiration delay
// - A parameter of -1 for no caching, meaning the HTML template is processed every time without being stored in the cache."
//
fn check_if_cache_delay_iscorrect(cde i64, tmpl_name string) ! {
	if (cde != 0 && cde != -1 && cde < dtm.converted_cache_delay_expiration_at_min)
		|| (cde != 0 && cde != -1 && cde > dtm.converted_cache_delay_expiration_at_max) {
		return error("${dtm.message_signature_error} The cache timeout for template '${tmpl_name}.html' cannot be set to a value less than '${dtm.cache_delay_expiration_at_min}' seconds and more than '${dtm.cache_delay_expiration_at_max}' seconds. Exception for the value '0' which means no cache expiration, and the value '-1' which means html generation without caching.")
	}
}

// fn cache_request_route(bool, i64, i64, i64, i64, i64, i64) return CacheRequest
//
// Used exclusively in 'serve_dynamic_template' function, determines the appropriate cache request action for an HTML template.
// It assesses various conditions such as cache existence, cache expiration settings, and last modification timestamps ( template or dynamic content )
// to decide whether to create a new cache, update an existing or delivered a valid cache content.
//
fn cache_request_route(is_cache_exist bool, neg_cache_delay_expiration i64, last_template_mod i64, test_current_template_mod i64, cache_del_exp i64, gen_at i64, c_time i64, content_checksum string, current_content_checksum string) CacheRequest {
	if !is_cache_exist || neg_cache_delay_expiration == -1 {
		// Requiere cache creation
		return CacheRequest.new
	} else if last_template_mod < test_current_template_mod
		|| content_checksum != current_content_checksum {
		// Requires cache update as the HTML template has been modified since the last time. it can be the template itself or its dynamic content.
		return CacheRequest.update
	} else if cache_del_exp != 0 && (gen_at + cache_del_exp) < c_time {
		// Requires cache update as the cache expiration delay has elapsed.
		return CacheRequest.exp_update
	} else {
		// Returns valid cached content, no update or creation necessary.
		return CacheRequest.cached
	}
}

// fn get_current_unix_timestamp() return i64
//
// This function is designed for handling timezone adjustments by converting the machine's local time at micro format to a universal micro format.
//
fn get_current_unix_timestamp() i64 {
	current_time := time.now()
	utc_time := current_time.local_to_utc()
	return utc_time.unix_time_micro()
}
