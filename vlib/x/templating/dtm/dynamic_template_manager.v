module dtm

import os
import crypto.md5
import hash.fnv1a
import time
import regex

// These are all the types of dynamic values that the DTM allows to be returned in the context of a map
pub type DtmMultiTypeMap = f32 | f64 | i16 | i64 | i8 | int | string | u16 | u32 | u64 | u8

// type MiddlewareFn = fn (mut Context, string) bool

// cache_delay_expiration_at_min is the minimum setting for cache expiration delay, fixed at 5 minutes (measured in seconds).
pub const cache_delay_expiration_at_min = 300
// cache_delay_expiration_at_max maximal is the maximal setting for cache expiration delay, fixed at 1 year (measured in seconds).
pub const cache_delay_expiration_at_max = 31536000
// cache_delay_expiration_by_default is the default setting for cache expiration delay, fixed at 1 day (measured in seconds).
pub const cache_delay_expiration_by_default = 86400
// Setting channel capacity of the cache handler.
const cache_handler_channel_cap = 200
// Setting the maximum data size (500 KB) to be stored at memory mode. If this limit is exceeded, cache handler switch to disk mode.
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

enum TemplateType {
	html
	text
}

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
	id_counter       int                = 1
	ch_cache_handler chan TemplateCache = chan TemplateCache{cap: cache_handler_channel_cap}
	// 'id_to_handlered' field is used exclusively by the cache handler to update or delete specific 'TemplateCache' in the cache database.
	id_to_handlered     int
	close_cache_handler bool
	// Initialisation params options for these two (Compress_html and active_cache_server)
	compress_html       bool = true
	active_cache_server bool = true
	// Initialisation of max data size in memory storage
	max_size_data_in_memory int = max_size_data_in_memory
	// This array is designed to store a control process that checks whether cached data is currently in use while simultaneously handling expiration.
	// This allows for the harmonious management of both aspects and facilitates the necessary actions.
	nbr_of_remaining_template_request shared []RemainingTemplateRequest = []RemainingTemplateRequest{}
	//	Dtm clock
	c_time            i64
	ch_stop_dtm_clock chan bool = chan bool{cap: 5}
	// Store small information about already cached pages to improve the verification speed of the check_tmpl_and_placeholders_size function.
	html_file_info shared map[string]HtmlFileInfo = map[string]HtmlFileInfo{}
	// Indicates whether the cache file storage directory is located in a temporary OS area
	cache_folder_is_temporary_storage bool
	// Handler for all threads used in the DTM
	threads_handler []thread = []thread{}
	// This channel used only for CI. Allows to check during CI tests in case of slowness in the creation/management of the cache to allow enough time for it to be done
	is_ready chan bool = chan bool{cap: 5}
	// If despite the synchronization attempt during the cache handler tests nothing happens, cancel the tests targeting the cached data
	abort_test bool
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
	// Contains the full path to a cache stored on disk.
	cache_full_path_name string
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

@[noinit]
struct HtmlFileInfo {
	file_full_path string
	file_name      string
	file_type      TemplateType
}

// TemplateCacheParams are used to specify cache expiration delay and provide placeholder data for substitution in templates.
@[params]
pub struct TemplateCacheParams {
pub:
	placeholders           &map[string]DtmMultiTypeMap = &map[string]DtmMultiTypeMap{}
	cache_delay_expiration i64                         = cache_delay_expiration_by_default
}

// DynamicTemplateManagerInitialisationParams is used with 'initialize' function. (See below at initialize section)
@[params]
pub struct DynamicTemplateManagerInitialisationParams {
pub:
	def_cache_path       string
	compress_html        bool = true
	active_cache_server  bool = true
	max_size_data_in_mem int  = max_size_data_in_memory
	test_cache_dir       string
	test_template_dir    string
}

// initialize create and init the 'DynamicTemplateManager' with the storage mode, cache/templates path folders.
// A cache directory can be created by the user for storage. If it is not defined or encounters issues such as permission problems,
// the DTM will attempt to create it in the OS's temporary area. If this proves impossible, the cache system will be deactivated and the user will be informed if cache system was required.
// Initialisation params are :
// - def_cache_path 'type string' User can define the path of cache folder.
// - max_size_data_in_mem 'type int' Maximum size of data allowed in memory for caching. The value must be specified in kilobytes. ( Default is: 500KB / Limit max is : 500KB)	
// - compress_html: 'type bool' Light compress of the HTML output. ( default is true )
// - active_cache_server: 'type bool' Activate or not the template cache system. ( default is true )
// - test_cache_dir: 'type string' Used only for DTM internal test file, parameter is ignored otherwise.
// - test_template_dir: 'type string' Used only for DTM internal test file, parameter is ignored otherwise.
pub fn initialize(dtm_init_params DynamicTemplateManagerInitialisationParams) &DynamicTemplateManager {
	mut dir_path := ''
	mut dir_html_path := ''
	mut max_size_memory := 0
	mut active_cache_handler := dtm_init_params.active_cache_server
	mut system_ready := true
	mut cache_temporary_bool := false
	$if test {
		dir_path = dtm_init_params.test_cache_dir
		dir_html_path = dtm_init_params.test_template_dir
	} $else {
		//	dir_path = os.join_path('${os.dir(os.executable())}/vcache_dtm')
		dir_path = dtm_init_params.def_cache_path
		dir_html_path = os.join_path('${os.dir(os.executable())}/templates')
	}
	if active_cache_handler {
		// Control if cache folder created by user exist
		if dir_path != '' && os.exists(dir_path) && os.is_dir(dir_path) {
			// WARNING: When setting the directory for caching files and for testing purposes,
			// 'check_and_clear_cache_files' function will delete all "*.cache" or "*.tmp" files inside the specified cache directory. Ensure that
			// directory used for the cache does not contain any important files.
			check_and_clear_cache_files(dir_path) or {
				system_ready = false
				eprintln(err.msg())
			}
		} else {
			// If the cache folder is not found or problems, the dtm will attempt to create it in the temporary OS area.
			dir_path = os.join_path(os.temp_dir(), 'vcache_dtm')
			if !os.exists(dir_path) || !os.is_dir(dir_path) {
				os.mkdir(dir_path) or {
					active_cache_handler = false
					eprintln(err.msg())
				}
			}
			if active_cache_handler {
				check_and_clear_cache_files(dir_path) or {
					active_cache_handler = false
					eprintln(err.msg())
				}
			}
			// If it is impossible to use a cache directory, the cache system is deactivated, and the user is warned."
			if !active_cache_handler {
				eprintln('${message_signature_warn} The cache storage directory does not exist or has a problem and it was also not possible to use a folder suitable for temporary storage. Therefore, the cache system will be disabled. It is recommended to address the aforementioned issues to utilize the cache system.')
			} else {
				cache_temporary_bool = true
			}
		}
	}
	// Control if 'templates' folder exist in the root project
	if !os.exists(dir_html_path) && !os.is_dir(dir_html_path) {
		system_ready = false
		eprintln('${message_signature_error} The templates directory at the project root does not exist. Please create a "templates" directory at the root of your project with appropriate read permissions. This is a mandatory step for using the Dynamic Template Manager (DTM). Current path attempted for create the templates folder: "${dir_html_path}"')
	}
	// Validates the 'max_size_data_in_mem' setting in 'dtm_init_params'. If it's within the valid range, it's applied; otherwise, default value is used.
	if dtm_init_params.max_size_data_in_mem <= max_size_data_in_memory
		&& dtm_init_params.max_size_data_in_mem >= 0 {
		max_size_memory = dtm_init_params.max_size_data_in_mem
	} else {
		max_size_memory = max_size_data_in_memory
		mut type_error := 'exceeds'
		if dtm_init_params.max_size_data_in_mem < 0 {
			type_error = 'is invalid for define'
		}
		eprintln('${message_signature_info} The value "${dtm_init_params.max_size_data_in_mem}KB" ${type_error} the memory storage limit. It will not be considered, and the limit will be set to ${max_size_data_in_memory}KB.')
	}

	mut dtmi := &DynamicTemplateManager{
		template_cache_folder:             dir_path
		template_folder:                   dir_html_path
		max_size_data_in_memory:           max_size_memory
		compress_html:                     dtm_init_params.compress_html
		active_cache_server:               active_cache_handler
		c_time:                            get_current_unix_micro_timestamp()
		dtm_init_is_ok:                    system_ready
		cache_folder_is_temporary_storage: cache_temporary_bool
	}
	if system_ready {
		// Disable cache handler if user doesn't required. Else, new thread is used to start the cache system. ( By default is ON )
		if active_cache_handler {
			dtmi.threads_handler << spawn dtmi.cache_handler()
			dtmi.threads_handler << spawn dtmi.handle_dtm_clock()
		}
		println('${message_signature} Dynamic Template Manager activated')
	} else {
		eprintln('${message_signature_error} Unable to use the Dynamic Template Manager, please refer to the above errors and correct them.')
	}

	return dtmi
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

// expand manages the cache and returns generated HTML.
// Requires an initialization via 'initialize' to running.
// To use this function, HTML templates must be located in the 'templates' directory at the project's root.
// However, it allows the use of subfolder paths within the 'templates' directory,
// enabling users to structure their templates in a way that best suits their project's organization.
pub fn (mut tm DynamicTemplateManager) expand(tmpl_path string, tmpl_var TemplateCacheParams) string {
	if tm.dtm_init_is_ok {
		file_path, tmpl_name, current_content_checksum, tmpl_type := tm.check_tmpl_and_placeholders_size(tmpl_path,
			tmpl_var.placeholders) or { return err.msg() }
		converted_cache_delay_expiration := i64(tmpl_var.cache_delay_expiration) * convert_seconds
		// If cache exist, return necessary fields else, 'is_cache_exist' return false.
		is_cache_exist, id, path, mut last_template_mod, gen_at, cache_del_exp, content_checksum := tm.return_cache_info_isexistent(file_path)
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
			// Get last modification timestamp of HTML template to compare with cache info already existent.
			test_current_template_mod = os.file_last_mod_unix(file_path)
		}

		// From this point, all the previously encountered variables are used to determine the routing in rendering the HTML template and creating/using its cache.
		cash_req, unique_time := tm.cache_request_route(is_cache_exist, converted_cache_delay_expiration,
			last_template_mod, test_current_template_mod, cache_del_exp, gen_at, tm.c_time,
			content_checksum, current_content_checksum)
		// Each of these match statements aims to provide HTML rendering, but each one sends a specific signal 'cash_req' of type CacheRequest
		// or calls the appropriate function for managing the cache of the provided HTML.
		match cash_req {
			.new {
				// Create a new cache
				html = tm.create_template_cache_and_display(cash_req, last_template_mod,
					unique_time, file_path, tmpl_name, converted_cache_delay_expiration,
					tmpl_var.placeholders, current_content_checksum, tmpl_type)
				//	println('create cache : ${cash_req}')
			}
			.update, .exp_update {
				// Update an existing cache
				tm.id_to_handlered = id
				html = tm.create_template_cache_and_display(cash_req, test_current_template_mod,
					unique_time, file_path, tmpl_name, converted_cache_delay_expiration,
					tmpl_var.placeholders, current_content_checksum, tmpl_type)
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
		eprintln('${message_signature_error} The initialization phase of DTM has failed. Therefore, you cannot use it. Please address the errors and then restart the dtm server.')
		return internat_server_error
	}
}

// stop_cache_handler signals the termination of the cache handler.
// It does so by setting 'close_cache_handler' to true, and sending a signal
// through the channel which will trigger a cascading effect to close the cache
// handler thread as well as the DTM clock thread.
pub fn (mut tm DynamicTemplateManager) stop_cache_handler() {
	if tm.active_cache_server {
		tm.active_cache_server = false
		tm.close_cache_handler = true
		tm.ch_cache_handler <- TemplateCache{
			id: 0
		}
		tm.threads_handler.wait()
	}
}

// check_and_clear_cache_files is used exclusively during the initialization of the DTM (Dynamic Template Manager).
// Its primary purpose is to ensure a clean starting environment by clearing all files
// within the designated cache directory. It iterates through the directory, listing all files,
// and systematically removes each file found, ensuring that no residual cache data persists
// from previous executions. Additionally, this function also tests the read and write permissions
// of the cache directory to ensure that the application has the necessary access to properly manage the cache files.
// WARNING: When setting the directory for caching files and for testing purposes,
// this function will delete all "*.cache" or "*.tmp" files inside the cache directory in the project root's. Ensure that
// directory used for the cache does not contain any important files.
fn check_and_clear_cache_files(c_folder string) ! {
	//	println('${message_signature} WARNING! DTM needs to perform some file tests in the cache directory. This operation will erase all "*.cache" or "*.tmp" files content in the folder : "${tm.template_cache_folder}"')
	//	println('Do you want to continue the operation? (yes/no)')
	//	mut user_response := os.input('>').to_lower()
	//	if user_response != 'yes' && user_response != 'y' {
	//		return error('${message_signature_error} Operation cancelled by the user. DTM initialization failed.')
	//	} else {
	file_p := os.join_path(c_folder, 'test.tmp')
	// Create a text file for test permission access
	mut f := os.create(file_p) or {
		return error('${message_signature_error} Files are not writable. Test fail, DTM initialization failed : ${err.msg()}')
	}
	f.close()
	// Read the previous text file for test permission access
	os.read_file(file_p) or {
		return error('${message_signature_error} Files are not readable. Test fail, DTM initialization failed : ${err.msg()}')
	}
	// List all files in the cache folder
	files_list := os.ls(c_folder) or {
		return error('${message_signature_error} While listing the cache directorie files, DTM initialization failed : ${err.msg()}')
	}
	// Delete one by one "*.cache" or "*.tmp" files in the previous file list
	for file in files_list {
		file_path := os.join_path(c_folder, file)
		file_extension := os.file_ext(file_path).to_lower()
		if file_extension in ['.tmp', '.cache'] {
			os.rm(file_path) or {
				eprintln('${message_signature_error} While deleting the cache file: ${file_path}. DTM initialization failed : ${err.msg()}')
				return
			}
		}
	}
	//	}
}

// check_tmpl_and_placeholders_size is used exclusively in the 'expand' function, this check verifies if the template file exists.
// It also ensures the file extension is correct ( like HTML or TXT ) and controls the size of placeholder keys and values in the provided map,
// offering a basic validation and security measure against excessively long and potentially harmful inputs.
// Size limits are defined by the 'max_placeholders_key_size' and 'max_placeholders_value_size' constants.
// Monitor dynamic content for updates by generating a checksum that is compared against the cached version to verify any changes.
fn (mut tm DynamicTemplateManager) check_tmpl_and_placeholders_size(f_path string, tmpl_var &map[string]DtmMultiTypeMap) !(string, string, string, TemplateType) {
	mut html_file := ''
	mut file_name := ''
	mut res_checksum_content := ''
	mut need_to_create_entry := false
	mut define_file_type := TemplateType.html

	rlock tm.html_file_info {
		if mapped_html_info := tm.html_file_info[f_path] {
			html_file = mapped_html_info.file_full_path
			file_name = mapped_html_info.file_name
		} else {
			need_to_create_entry = true
		}
	}
	if need_to_create_entry {
		$if test {
			html_file = f_path
		} $else {
			html_file = os.join_path(tm.template_folder, f_path)
		}
		if os.exists(html_file) {
			// Extracts the base file name with extension from the given file path.
			file_name_with_ext := os.base(html_file)
			// Removes the file extension, keeping only the name.
			file_name = file_name_with_ext.all_before_last('.')
			// Performs a basic check of the file extension.
			ext := os.file_ext(html_file)
			if ext != '.html' && ext != '.txt' {
				eprintln('${message_signature_error} ${html_file}, is not a valid template file like .html or .txt')
				return error(internat_server_error)
			}
			if ext == '.txt' {
				define_file_type = TemplateType.text
			}
			lock tm.html_file_info {
				tm.html_file_info[f_path] = HtmlFileInfo{
					file_full_path: html_file
					file_name:      file_name
					file_type:      define_file_type
				}
			}
		} else {
			eprintln("${message_signature_error} Template : '${html_file}' not found. Ensure all templates are located in the template directory.")
			return error(internat_server_error)
		}
	}

	// checks if the dynamic content ( If, however, it contains dynamic content ) of an template has been updated.
	// If it has, it creates a checksum of this content for analysis.
	// The checksum is generated by concatenating all dynamic values and applying a fnv1a hash to the resulting string and generate a checksum.
	if tmpl_var.len > 0 {
		mut combined_str := ''
		// Control placeholder key and value sizes
		for key, value in tmpl_var {
			if key.len > max_placeholders_key_size {
				eprintln('${message_signature_error} Length of placeholder key "${key}" exceeds the maximum allowed size for template content in file: ${html_file}. Max allowed size: ${max_placeholders_key_size} characters.')
				return error(internat_server_error)
			}
			match value {
				string {
					if value.len > max_placeholders_value_size {
						eprintln('${message_signature_error} Length of placeholder value for key "${key}" exceeds the maximum allowed size for template content in file: ${html_file}. Max allowed size: ${max_placeholders_value_size} characters.')
						return error(internat_server_error)
					}
					combined_str += value
				}
				else {
					casted_value := value.str()
					if casted_value.len > max_placeholders_value_size {
						eprintln('${message_signature_error} Length of placeholder value for key "${key}" exceeds the maximum allowed size for template content in file: ${html_file}. Max allowed size: ${max_placeholders_value_size} characters.')
						return error(internat_server_error)
					}

					combined_str += casted_value
				}
			}
		}

		res_checksum_content = fnv1a.sum64_string(combined_str).str()
	}

	// If all is ok, return full path of template file and filename without extension
	return html_file, file_name, res_checksum_content, define_file_type
}

// create_template_cache_and_display is exclusively invoked from `expand`.
// It generates the template rendering and relaying information
// to the cache manager for either the creation or updating of the template cache.
// It begin to starts by ensuring that the cache delay expiration is correctly set by user.
// It then parses the template file, replacing placeholders with actual dynamics/statics values.
// If caching is enabled (indicated by a cache delay expiration different from -1) and the template content is valid,
// the function constructs a `TemplateCache` request with all the necessary details.
// This request is then sent to the cache handler channel, signaling either the need for a new cache or an update to an existing one.
// The function returns the rendered immediately, without waiting for the cache to be created or updated.
fn (mut tm DynamicTemplateManager) create_template_cache_and_display(tcs CacheRequest, last_template_mod i64,
	unique_time i64, file_path string, tmpl_name string, cache_delay_expiration i64, placeholders &map[string]DtmMultiTypeMap,
	current_content_checksum string, tmpl_type TemplateType) string {
	// Control if cache delay expiration is correctly set. See the function itself for more details.
	check_if_cache_delay_iscorrect(cache_delay_expiration, tmpl_name) or {
		eprintln(err)
		return internat_server_error
	}
	// Parses the template and stores the rendered output in the variable. See the function itself for more details.
	mut html := tm.parse_tmpl_file(file_path, tmpl_name, placeholders, tm.compress_html,
		tmpl_type)
	// If caching is enabled and the template content is valid, this section creates a temporary cache file, which is then used by the cache manager.
	// If successfully temporary is created, a cache creation/update notification is sent through its dedicated channel to the cache manager
	if cache_delay_expiration != -1 && html != internat_server_error && tm.active_cache_server {
		op_success, tmp_name := tm.create_temp_cache(html, file_path, unique_time)
		if op_success {
			tm.ch_cache_handler <- TemplateCache{
				id:   tm.id_counter
				name: tmpl_name
				// 'path' field contains the full path, name and file extension of targeted HTML template.
				path:             file_path
				content_checksum: current_content_checksum
				tmp_name_file:    tmp_name
				// Last modified timestamp of HTML template
				last_template_mod: last_template_mod
				// Unix current local timestamp of cache generation request converted to UTC
				generate_at: unique_time
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
		// In the context of a template validity error, the 'nbr_of_remaining_request' counter is consistently updated to avoid anomalies in cache management.
		tm.remaining_template_request(false, tm.id_to_handlered)
	}

	return html
}

// create_temp_cache is responsible for creating a temporary cache file, which is subsequently used exclusively by the cache manager.
// It generates a temporary file name using a checksum based on the timestamp and the file path.
// The content is then written to this file, located in the designated cache folder.
// If the operation is successful, a boolean is returned to allow for the sending of a create or modify request to the cache manager.
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
		eprintln('${message_signature_error} Cannot create temporary cache file : ${err.msg()}')
		return false, ''
	}
	f.write(html_bytes) or {
		eprintln('${message_signature_error} Cannot write in temporary cache file : ${err.msg()}')
		f.close()
		return false, ''
	}
	f.close()
	return true, tmp_name
}

// get_cache is exclusively invoked from `expand', retrieves the rendered HTML from the cache.
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
						r_b_html := os.read_bytes(value.cache_full_path_name) or {
							eprintln('${message_signature_error} Get_cache() cannot read template cache file ${value.name} : ${err.msg()} ')
							return internat_server_error
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

// return_cache_info_isexistent is exclusively used in 'expand' to determine whether a cache exists for the provided HTML template.
// If a cache exists, it returns the necessary information for its transformation. If not, it indicates the need to create a new cache.
fn (mut tm DynamicTemplateManager) return_cache_info_isexistent(tmpl_path string) (bool, int, string, i64, i64, i64, string) {
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
									return true, val.id, val.path, val.last_template_mod, val.generate_at, val.cache_delay_expiration, val.content_checksum
								}
							}
						}
						if need_goto {
							need_goto = false
							goto re_loop
						}
					}
					// No cache redirection, get cache current information.
				} else {
					// function is used to signal that the process has begun using the cache information.
					tm.remaining_template_request(true, value.id)
					return true, value.id, value.path, value.last_template_mod, value.generate_at, value.cache_delay_expiration, value.content_checksum
				}
			}
		}
	}
	// No existing cache, need to create it.
	return false, 0, '', 0, 0, 0, ''
}

// remaining_template_request manages the counter in 'nbr_of_remaining_template_request', which tracks the number of requests that have started or finished for a specific cache.
// Moreover, this function sends a cache deletion callback request when the cache manager had previously been instructed to delete the cache but was unable to do because,
// it was still in use.
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
							id:            r_request.id
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
// TODO: Currently, the cache manager stops when it encounters an internal error requiring a restart of the program.
// ( it is designed to ignore external errors since these are already handled in a way that ensures no cache processing requests are affected ),
// A recovery system will need to be implemented to ensure service continuity.
//
fn (mut tm DynamicTemplateManager) cache_handler() {
	defer {
		// If cause is an internal cache handler error
		tm.active_cache_server = false
		// Close channel if handler is stopped
		tm.ch_cache_handler.close()
		tm.is_ready.close()
		tm.ch_stop_dtm_clock <- true
	}
	for {
		select {
			// Continuously listens until a request is received through the dedicated channel.
			mut tc := <-tm.ch_cache_handler {
				// Close handler if asked.
				if tm.close_cache_handler {
					eprintln('${message_signature_info} Cache manager has been successfully stopped. Please consider restarting the application if needed.')
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
							eprintln('${message_signature_error} Cache Handler : Failed to read tmp file, cache server will be stopped, you need to fix and restart application: ${err.msg()}')
							break
						}

						combined_str := file_data.str() + tc.path + tc.generate_at.str()
						tc.checksum = md5.hexhash(combined_str)

						match tc.cache_storage_mode {
							.memory {
								// If the cache is stored in memory, the temporary file is destroyed.
								tc.html_data = file_data
								os.rm(f_path_tmp) or {
									eprintln('${message_signature_error} Cache Handler : While deleting the tmp cache file: "${f_path_tmp}", cache server will be stopped, you need to fix and restart application: ${err.msg()}')
									break
								}
							}
							.disk {
								// If the cache is stored on disk, the temporary file is renamed to become the definitive cache of the current version of the HTML template.
								tc.cache_full_path_name = os.join_path(tm.template_cache_folder,
									'${tc.name}_${tc.checksum}.cache')
								os.mv(f_path_tmp, tc.cache_full_path_name) or {
									eprintln('${message_signature_error} Cache Handler : Failed to rename tmp file, cache server will be stopped, you need to fix and restart application: ${err.msg()}')
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

							$if test {
								tm.is_ready <- true
							}
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

								$if test {
									tm.is_ready <- true
								}
							}
						}
					}
				} else if tc.cache_request != .delete {
					os.rm(f_path_tmp) or {
						eprintln('${message_signature_warn} Cache Handler : Cannot deleting the unused tmp cache file: "${f_path_tmp}" : ${err.msg()}')
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
						eprintln('${message_signature_error} While deleting the specific cache file: ${file_path}, cache server will be stopped, you need to fix and restart application: : ${err.msg()}')
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

// fn (mut DynamicTemplateManager) parse_tmpl_file(string, string, &map[string]DtmMultiTypeMap, bool, TemplateType) return (string, string)
//
// Parses and generates template file content.
// It ensures template format compatibility necessary for proper compilation and execution in its typical usage outside of DTM like managing various states,
// processing template tags, and supporting string interpolation...
// including dynamic content with the possibility of adding HTML code but only for certain specified tags and can also light compress HTML if required ( Removing usless spaces ).
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

fn (mut tm DynamicTemplateManager) parse_tmpl_file(file_path string, tmpl_name string, placeholders &map[string]DtmMultiTypeMap,
	is_compressed bool, tmpl_type TemplateType) string {
	mut tmpl_ := compile_template_file(file_path, tmpl_name, placeholders)

	// Performs a light compression of the HTML output by removing usless spaces, newlines, and tabs if user selected this option.
	if is_compressed && tmpl_type == TemplateType.html && tmpl_ != internat_server_error {
		tmpl_ = tmpl_.replace_each(['\n', '', '\t', '', '  ', ' '])
		mut r := regex.regex_opt(r'>(\s+)<') or {
			tm.stop_cache_handler()
			eprintln('${message_signature_error} with regular expression for HTML light compression in parse_tmpl_file() function. Please check the syntax of the regex pattern : ${err.msg()}')
			return internat_server_error
		}
		tmpl_ = r.replace(tmpl_, '><')
		for tmpl_.contains('  ') {
			tmpl_ = tmpl_.replace('  ', ' ')
		}
	}

	return tmpl_
}

// fn check_if_cache_delay_iscorrect(i64, string) return !
//
// Validates the user-specified cache expiration delay for templates.
// It enforces three permissible delay settings:
// - A minimum of five minutes and a maximum of one year for standard cache expiration. ( Define in constants )
// - A parameter of 0 for an infinite cache expiration delay
// - A parameter of -1 for no caching, meaning the template is processed every time without being stored in the cache."
//
fn check_if_cache_delay_iscorrect(cde i64, tmpl_name string) ! {
	if (cde != 0 && cde != -1 && cde < converted_cache_delay_expiration_at_min)
		|| (cde != 0 && cde != -1 && cde > converted_cache_delay_expiration_at_max) {
		return error("${message_signature_error} The cache timeout for template '${tmpl_name}' cannot be set to a value less than '${cache_delay_expiration_at_min}' seconds and more than '${cache_delay_expiration_at_max}' seconds. Exception for the value '0' which means no cache expiration, and the value '-1' which means html generation without caching.")
	}
}

// fn cache_request_route(bool, i64, i64, i64, i64, i64, i64) return (CacheRequest, i64)
//
// Used exclusively in 'expand' function, determines the appropriate cache request action for a template.
// It assesses various conditions such as cache existence, cache expiration settings, and last modification timestamps ( template or dynamic content )
// to decide whether to create a new cache, update an existing or delivered a valid cache content.
//
fn (mut tm DynamicTemplateManager) cache_request_route(is_cache_exist bool, neg_cache_delay_expiration i64,
	last_template_mod i64, test_current_template_mod i64, cache_del_exp i64, gen_at i64, c_time i64, content_checksum string,
	current_content_checksum string) (CacheRequest, i64) {
	if !is_cache_exist || neg_cache_delay_expiration == -1 {
		// Require cache creation
		unique_ts := get_current_unix_micro_timestamp()
		tm.c_time = unique_ts
		return CacheRequest.new, unique_ts
	} else if last_template_mod < test_current_template_mod
		|| content_checksum != current_content_checksum {
		// Requires cache update as the template has been modified since the last time. it can be the template itself or its dynamic content.
		unique_ts := get_current_unix_micro_timestamp()
		tm.c_time = unique_ts
		return CacheRequest.update, unique_ts
	} else if cache_del_exp != 0 && (gen_at + cache_del_exp) < tm.c_time {
		unique_ts := get_current_unix_micro_timestamp()
		tm.c_time = unique_ts
		// Requires cache update as the cache expiration delay has elapsed.
		return CacheRequest.exp_update, unique_ts
	} else {
		// Returns valid cached content, no update or creation necessary.
		return CacheRequest.cached, 0
	}
}

// fn (mut tm DynamicTemplateManager) handle_dtm_clock()
//
// Manages the internal clock. It periodically updates ( 4 minutes minimum, if there has been a recent update elsewhere in the system, it will be taken into account by the clock handler. )
// The goal is ensuring that the DTM's internal time is maintained, especially during prolonged periods of inactivity on the website,
// this can potentially lead to cache expiration issues if there is zero traffic for a while."
//

// Minimum update interval ( in seconds ) set to 4 minutes minimum_wait_time_until_next_update
const update_duration = 240

fn (mut tm DynamicTemplateManager) handle_dtm_clock() {
	mut need_to_close := false
	defer {
		tm.ch_stop_dtm_clock.close()
		eprintln('${message_signature_info} DTM clock handler has been successfully stopped.')
	}

	for {
		// Calculate the remaining time until the next update.
		current_time := get_current_unix_micro_timestamp() / convert_seconds
		mut time_since_last_update := int(current_time - (tm.c_time / convert_seconds))
		mut minimum_wait_time_until_next_update := update_duration

		// Update DTM clock if update interval exceeded otherwise, set next check based on time since last update
		if time_since_last_update >= update_duration {
			tm.c_time = current_time * convert_seconds
		} else {
			if time_since_last_update < 0 {
				time_since_last_update = 0
			}
			minimum_wait_time_until_next_update = (update_duration - time_since_last_update) +
				update_duration
		}

		// Wait until the next update interval or until a stop signal is received.
		for elapsed_time := 0; elapsed_time < minimum_wait_time_until_next_update; elapsed_time++ {
			select {
				_ := <-tm.ch_stop_dtm_clock {
					need_to_close = true
					break
				}
				else {
					// Attendre une seconde
					time.sleep(1 * time.second)
				}
			}
		}
		if need_to_close {
			break
		}
		// Reset wait time for next cycle.
		minimum_wait_time_until_next_update = update_duration
	}
}

// fn get_current_unix_timestamp() return i64
//
// This function is designed for handling timezone adjustments by converting the machine's local time at micro format to a universal micro format.
//
fn get_current_unix_micro_timestamp() i64 {
	return time.now().unix_micro()
}
