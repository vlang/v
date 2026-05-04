@[deprecated: 'use x.templating.dtm2 for new code']
@[deprecated_after: '2999-01-01']
module dtm

import os
import crypto.md5
import hash.fnv1a
import time
import strings
import x.templating.dtm2

// These are all the types of dynamic values that the DTM allows to be returned in the context of a map

@[deprecated: 'use string placeholders with x.templating.dtm2.RenderParams for new code']
@[deprecated_after: '2999-01-01']
pub type DtmMultiTypeMap = f32 | f64 | i16 | i64 | i8 | int | string | u16 | u32 | u64 | u8

// type MiddlewareFn = fn (mut Context, string) bool

// cache_delay_expiration_at_min is the minimum setting for cache expiration delay, fixed at 5 minutes (measured in seconds).

@[deprecated: 'use x.templating.dtm2 for new code']
@[deprecated_after: '2999-01-01']
pub const cache_delay_expiration_at_min = 300
// cache_delay_expiration_at_max maximal is the maximal setting for cache expiration delay, fixed at 1 year (measured in seconds).

@[deprecated: 'use x.templating.dtm2 for new code']
@[deprecated_after: '2999-01-01']
pub const cache_delay_expiration_at_max = 31536000
// cache_delay_expiration_by_default is the default setting for cache expiration delay, fixed at 1 day (measured in seconds).

@[deprecated: 'use x.templating.dtm2 for new code']
@[deprecated_after: '2999-01-01']
pub const cache_delay_expiration_by_default = 86400
// Setting channel capacity of the legacy cache handler compatibility channel.
const cache_handler_channel_cap = 200
// Setting the maximum data size (500 KB) to be stored at memory mode. If this limit is exceeded, rendered cache switches to disk mode.
const max_size_data_in_memory = 500
// Defines the maximum character length for placeholder keys.
const max_placeholders_key_size = 50
// Sets the maximum character length for placeholder values.
const max_placeholders_value_size = 3000
// Internal DTM operations use microseconds to keep cache generation timestamps monotonic.
const convert_seconds = i64(1000000)
const converted_cache_delay_expiration_at_min = i64(cache_delay_expiration_at_min) * convert_seconds
const converted_cache_delay_expiration_at_max = i64(cache_delay_expiration_at_max) * convert_seconds

const internat_server_error = 'Internal Server Error'

const message_signature = '[Dynamic Template Manager]'
const message_signature_info = '[Dynamic Template Manager] Info :'
const message_signature_error = '[Dynamic Template Manager] Error :'
const message_signature_warn = '[Dynamic Template Manager] Warning :'

@[deprecated: 'use x.templating.dtm2 for new code']
@[deprecated_after: '2999-01-01']
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

@[deprecated: 'use x.templating.dtm2.Manager for new code']
@[deprecated_after: '2999-01-01']
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
	// Fast lookup from a full template path hash to the active rendered-cache id.
	template_cache_ids_by_path_hash map[u64]int = map[u64]int{}
	// counter for each individual TemplateCache created/updated
	id_counter       int                = 1
	ch_cache_handler chan TemplateCache = chan TemplateCache{cap: cache_handler_channel_cap}
	// 'id_to_handlered' is preserved for legacy callers that route cache update/delete work through the old API.
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
	// Handler for legacy cache-handler compatibility work.
	threads_handler []thread = []thread{}
	// This channel is used by legacy tests/callers to observe cache-handler completion.
	is_ready chan bool = chan bool{cap: 5}
	// If synchronization times out in legacy cache-handler tests, they set this flag.
	abort_test bool
	// Runtime rendering engine used by the compatibility layer. It caches parsed
	// template structures only, never rendered HTML responses.
	render_engine &dtm2.Manager = unsafe { nil }
}

// Represent individual template cache in database memory.
@[noinit]
struct TemplateCache {
mut:
	id   int
	name string
	// Previous cache id replaced by this cache request, only set for update requests.
	old_id int
	// 'path' field contains the full path, name and file extension of targeted HTML template.
	path string
	// Checksum of the cached template, which is constructed as follows: the complete HTML content plus its full file path and the timestamp of the cache generation.
	// This approach is utilized to create a unique identifier."
	checksum string
	// The checksum for the dynamic content of an HTML Template is constructed as follows: all dynamic values are concatenated into a single string,
	// and the checksum is then calculated based on this string.
	// Given that the value of this checksum is specific to its parent template, there is no issue of collision, and it does not need to be unique.
	content_checksum string
	// The timestamp of the last modification of the file (HTML template)
	last_template_mod i64
	// Timestamp of cache generation.
	generate_at i64
	// Timestamp of cache expiration define by user.
	cache_delay_expiration i64
	// Rendered HTML kept directly in memory when the cache entry uses memory storage.
	html_data          string
	cache_request      CacheRequest
	cache_storage_mode CacheStorageMode
	// This field is special as it determines if a cache is obsolete but still in use, allowing the system to redirect to the updated cache.
	// This enables the eventual deletion of the obsolete cache without causing issues for requests utilizing the cache.
	id_redirection int
	// Contains the full path to a cache stored on disk.
	cache_full_path_name string
}

// Represents controls stored in the 'nbr_of_remaining_template_request' of the DTM.
// It is used to monitor whether a rendered cache entry is in use and to manage
// deferred deletion when a cache entry is replaced.
@[noinit]
struct RemainingTemplateRequest {
	// The id is similar to the one it represents in template_caches.
	id int
mut:
	// This value determines the number of ongoing requests using the cache specified by the ID.
	nbr_of_remaining_request int
	// The cache has become obsolete, and permission to dispose of it is granted to the cache lifecycle.
	need_to_delete bool
	// If an obsolete cache cannot be destroyed because it is still in use, this
	// boolean allows deletion to be retried when the active request finishes.
	need_to_send_delete_request bool
}

@[noinit]
struct HtmlFileInfo {
	file_full_path string
	file_name      string
	file_type      TemplateType
}

struct PlaceholderChecksumValue {
	key   string
	value string
}

// TemplateCacheParams are used to specify cache expiration delay and provide placeholder data for substitution in templates.
@[deprecated: 'use x.templating.dtm2.RenderParams for new code']
@[deprecated_after: '2999-01-01']
@[params]
pub struct TemplateCacheParams {
pub:
	placeholders           &map[string]DtmMultiTypeMap = &map[string]DtmMultiTypeMap{}
	cache_delay_expiration i64                         = cache_delay_expiration_by_default
}

// DynamicTemplateManagerInitialisationParams is used with 'initialize' function. (See below at initialize section)
@[deprecated: 'use x.templating.dtm2.ManagerParams for new code']
@[deprecated_after: '2999-01-01']
@[params]
pub struct DynamicTemplateManagerInitialisationParams {
pub:
	def_cache_path       string
	compress_html        bool = true
	active_cache_server  bool = true
	max_size_data_in_mem int  = max_size_data_in_memory
	// Used by DTM internal tests to override the cache directory.
	test_cache_dir string
	// Used by DTM internal tests to override the templates directory.
	test_template_dir string
}

// initialize create and init the 'DynamicTemplateManager' with the storage mode, cache/templates path folders.
// A cache directory can be created by the user for storage. If it is not defined or encounters issues such as permission problems,
// the DTM will attempt to create it in the OS's temporary area. If this proves impossible, the cache system will be deactivated and the user will be informed if cache system was required.
// Initialisation params are :
// - def_cache_path 'type string' User can define the path of cache folder.
// - max_size_data_in_mem 'type int' Maximum size of data allowed in memory for caching. The value must be specified in kilobytes. ( Default is: 500KB / Limit max is : 500KB)	
// - compress_html: 'type bool' Light compress of the HTML output. ( default is true )
// - active_cache_server: 'type bool' Activate or not the template cache system. ( default is true )
// - test_cache_dir: 'type string' Used by DTM internal tests to override the cache directory.
// - test_template_dir: 'type string' Used by DTM internal tests to override the templates directory.
@[deprecated: 'use x.templating.dtm2.initialize for new code']
@[deprecated_after: '2999-01-01']
pub fn initialize(dtm_init_params DynamicTemplateManagerInitialisationParams) &DynamicTemplateManager {
	mut dir_path := dtm_init_params.def_cache_path
	mut dir_html_path := os.join_path('${os.dir(os.executable())}/templates')
	mut max_size_memory := 0
	mut active_rendered_cache := dtm_init_params.active_cache_server
	mut system_ready := true
	mut cache_temporary_bool := false

	if dtm_init_params.test_cache_dir != '' {
		dir_path = dtm_init_params.test_cache_dir
	}
	if dtm_init_params.test_template_dir != '' {
		dir_html_path = dtm_init_params.test_template_dir
	}
	if active_rendered_cache {
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
					active_rendered_cache = false
					eprintln(err.msg())
				}
			}
			if active_rendered_cache {
				check_and_clear_cache_files(dir_path) or {
					active_rendered_cache = false
					eprintln(err.msg())
				}
			}
			// If it is impossible to use a cache directory, the cache system is deactivated, and the user is warned."
			if !active_rendered_cache {
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
		template_cache_folder:             dir_path.clone()
		template_folder:                   dir_html_path.clone()
		template_cache_ids_by_path_hash:   map[u64]int{}
		max_size_data_in_memory:           max_size_memory
		compress_html:                     dtm_init_params.compress_html
		active_cache_server:               active_rendered_cache
		c_time:                            get_current_unix_micro_timestamp()
		dtm_init_is_ok:                    system_ready
		cache_folder_is_temporary_storage: cache_temporary_bool
		render_engine:                     new_dtm2_render_engine(dir_html_path,
			dtm_init_params.compress_html)
	}
	if system_ready {
		if active_rendered_cache {
			dtmi.threads_handler << spawn dtmi.cache_handler()
		}
		// Rendered-cache work is processed synchronously. This keeps prod/Boehm behavior
		// deterministic while preserving DTM's dynamic rendered-cache feature.
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
@[deprecated: 'use x.templating.dtm2.Manager.expand for new code']
@[deprecated_after: '2999-01-01']
pub fn (mut tm DynamicTemplateManager) expand(tmpl_path string, tmpl_var TemplateCacheParams) string {
	if !tm.dtm_init_is_ok {
		tm.disable_cache()
		eprintln('${message_signature_error} The initialization phase of DTM has failed. Therefore, you cannot use it. Please address the errors and then restart the dtm server.')
		return internat_server_error
	}
	return tm.expand_with_dtm2_render_engine(tmpl_path, tmpl_var)
}

// disable_cache disables future rendered-cache storage for this manager.
@[deprecated: 'legacy DTM compatibility only; use x.templating.dtm2 for new code']
@[deprecated_after: '2999-01-01']
pub fn (mut tm DynamicTemplateManager) disable_cache() {
	tm.stop_legacy_cache_handler()
}

// stop_cache_handler is kept for backward compatibility with the former cache-server
// API. Rendered-cache work is now synchronous, so stopping it simply disables future
// rendered-cache storage for this manager.
@[deprecated: 'use disable_cache while maintaining legacy DTM code; use x.templating.dtm2 for new code']
@[deprecated_after: '2999-01-01']
pub fn (mut tm DynamicTemplateManager) stop_cache_handler() {
	tm.stop_legacy_cache_handler()
}

fn (mut tm DynamicTemplateManager) stop_legacy_cache_handler() {
	if !tm.active_cache_server {
		return
	}
	tm.active_cache_server = false
	tm.close_cache_handler = true
	tm.ch_cache_handler <- TemplateCache{
		id: 0
	}
	if tm.threads_handler.len > 0 {
		tm.threads_handler.wait()
		tm.threads_handler = []thread{}
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
			html_file = mapped_html_info.file_full_path.clone()
			file_name = mapped_html_info.file_name.clone()
			define_file_type = mapped_html_info.file_type
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
				tm.html_file_info[f_path.clone()] = HtmlFileInfo{
					file_full_path: html_file.clone()
					file_name:      file_name.clone()
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
		mut placeholder_values := []PlaceholderChecksumValue{cap: tmpl_var.len}
		for key, value in tmpl_var {
			if key.len > max_placeholders_key_size {
				eprintln('${message_signature_error} Length of placeholder key "${key}" exceeds the maximum allowed size for template content in file: ${html_file}. Max allowed size: ${max_placeholders_key_size} characters.')
				return error(internat_server_error)
			}
			mut casted_value := ''
			match value {
				string {
					casted_value = value
				}
				else {
					casted_value = value.str()
				}
			}

			if casted_value.len > max_placeholders_value_size {
				eprintln('${message_signature_error} Length of placeholder value for key "${key}" exceeds the maximum allowed size for template content in file: ${html_file}. Max allowed size: ${max_placeholders_value_size} characters.')
				return error(internat_server_error)
			}
			placeholder_values << PlaceholderChecksumValue{
				key:   key.clone()
				value: casted_value.clone()
			}
		}
		placeholder_values.sort(a.key < b.key)
		mut combined_str := strings.new_builder(placeholder_values.len * 16)
		for placeholder_value in placeholder_values {
			combined_str.write_string(placeholder_value.value)
		}

		res_checksum_content = fnv1a.sum64_string(combined_str.str()).str()
	}

	// If all is ok, return full path of template file and filename without extension
	return html_file, file_name, res_checksum_content, define_file_type
}

// create_template_cache_and_display is exclusively invoked from `expand`.
// It generates the template rendering and prepares rendered-cache information
// for either the creation or updating of the template cache.
// It starts by ensuring that the cache delay expiration is correctly set by user.
// It then parses the template file, replacing placeholders with actual dynamics/statics values.
// If caching is enabled (indicated by a cache delay expiration different from -1) and the template content is valid,
// the function constructs a `TemplateCache` request with all the necessary details.
// This request is processed synchronously, so the rendered cache is ready when the function returns.
fn (mut tm DynamicTemplateManager) create_template_cache_and_display(tcs CacheRequest, last_template_mod i64,
	unique_time i64, file_path string, tmpl_name string, cache_delay_expiration i64, placeholders &map[string]DtmMultiTypeMap,
	current_content_checksum string, tmpl_type TemplateType) string {
	// Control if cache delay expiration is correctly set. See the function itself for more details.
	check_if_cache_delay_iscorrect(cache_delay_expiration, tmpl_name) or {
		eprintln(err)
		return internat_server_error
	}
	// Parses the template and stores the rendered output in the variable. See the function itself for more details.
	mut html := tm.parse_tmpl_file(file_path, tmpl_name, placeholders, tm.compress_html, tmpl_type)
	// If caching is enabled and the template content is valid, this section creates or updates
	// the rendered cache synchronously. Keeping the rendered string in the current call avoids
	// fragile tmp-file ownership/conversion paths in prod builds.
	if cache_delay_expiration != -1 && html != internat_server_error && tm.active_cache_server {
		cache_id := tm.id_counter
		tm.id_counter++
		old_cache_id := tm.id_to_handlered
		cache_request := TemplateCache{
			id:   cache_id
			name: tmpl_name.clone()
			// 'path' field contains the full path, name and file extension of targeted HTML template.
			path:             file_path.clone()
			content_checksum: current_content_checksum.clone()
			// Last modified timestamp of HTML template
			last_template_mod: last_template_mod
			// Unix current local timestamp of cache generation request converted to UTC
			generate_at: unique_time
			// Defines the cache expiration delay in seconds. This value is added to 'generate_at' to calculate the expiration time of the cache.
			cache_delay_expiration: cache_delay_expiration
			html_data:              html.clone()
			// The requested routing to define creation or updating cache.
			cache_request: tcs
			old_id:        old_cache_id
		}
		mut process_request := cache_request
		tm.process_cache_request(mut process_request)
		tm.signal_cache_ready()
		// In the context of a cache update, this function is used to signal that the process has finished using the cache information. The 'nbr_of_remaining_request' counter is therefore updated."
		if tcs == .update || tcs == .exp_update {
			tm.remaining_template_request(false, old_cache_id)
		}
	} else {
		// In the context of a template validity error, the 'nbr_of_remaining_request' counter is consistently updated to avoid anomalies in cache management.
		tm.remaining_template_request(false, tm.id_to_handlered)
	}

	return html
}

// get_cache is exclusively invoked from `expand', retrieves the rendered HTML from the cache.
fn (mut tm DynamicTemplateManager) get_cache(_name string, path string, _placeholders &map[string]DtmMultiTypeMap) string {
	mut cache_id := 0
	rlock tm.template_caches {
		for value in tm.template_caches {
			// If the cache for the specified HTML template is found, perform the following operations:
			if value.path == path {
				cache_id = value.id
				break
			}
		}
	}
	if cache_id != 0 {
		return tm.get_cache_by_id(cache_id)
	}
	return ''
}

fn (mut tm DynamicTemplateManager) get_cache_by_id(cache_id int) string {
	mut cache_exists := false
	mut html_data := ''
	mut cache_storage_mode := CacheStorageMode.memory
	mut disk_cache_path := ''
	mut cache_name := ''
	rlock tm.template_caches {
		for value in tm.template_caches {
			if value.id == cache_id {
				cache_exists = true
				html_data = value.html_data.clone()
				cache_storage_mode = value.cache_storage_mode
				cache_name = value.name.clone()
				disk_cache_path = if value.cache_full_path_name != '' {
					value.cache_full_path_name.clone()
				} else {
					tm.cache_disk_path(value.name, value.checksum)
				}
				break
			}
		}
	}
	defer {
		if cache_exists {
			tm.remaining_template_request(false, cache_id)
		}
	}
	if html_data.len > 0 {
		return html_data
	}
	match cache_storage_mode {
		.memory {
			return ''
		}
		.disk {
			cached_html := os.read_file(disk_cache_path) or {
				eprintln('${message_signature_error} Get_cache() cannot read template cache file ${cache_name} : ${err.msg()} ')
				return internat_server_error
			}
			return cached_html
		}
	}

	return ''
}

// return_cache_info_isexistent is exclusively used in 'expand' to determine whether a cache exists for the provided HTML template.
// If a cache exists, it returns the necessary information for its transformation. If not, it indicates the need to create a new cache.
fn (mut tm DynamicTemplateManager) return_cache_info_isexistent(tmpl_path string) (bool, int, string, i64, i64, i64, string) {
	cache_entries := tm.snapshot_template_caches()
	mut cache_found := false
	mut cache_id := 0
	mut cache_path := ''
	mut last_template_mod := i64(0)
	mut generate_at := i64(0)
	mut cache_delay_expiration := i64(0)
	mut content_checksum := ''
	path_hash := fnv1a.sum64_string(tmpl_path)
	active_cache_id := tm.template_cache_ids_by_path_hash[path_hash] or { 0 }
	for value in cache_entries {
		if active_cache_id != 0 {
			if value.id != active_cache_id {
				continue
			}
		} else if value.path != tmpl_path {
			continue
		}
		// This code section handles cache redirection.
		// If a cache redirection ID is found, it indicates that the currently used cache is outdated and there's a newer version available.
		// The process then seeks to retrieve information from this more recent cache.
		// This is done recursively: if the updated cache itself points to an even newer version, the process continues until the most up-to-date cache is found.
		// This recursive mechanism ensures that the latest cache data is always used.
		cache_found = true
		cache_id = value.id
		cache_path = value.path.clone()
		last_template_mod = value.last_template_mod
		generate_at = value.generate_at
		cache_delay_expiration = value.cache_delay_expiration
		content_checksum = value.content_checksum.clone()
		if value.id_redirection != 0 {
			mut id_value_recursion := value.id_redirection
			for {
				mut found_redirect := false
				for val in cache_entries {
					if val.id == id_value_recursion {
						cache_id = val.id
						cache_path = val.path.clone()
						last_template_mod = val.last_template_mod
						generate_at = val.generate_at
						cache_delay_expiration = val.cache_delay_expiration
						content_checksum = val.content_checksum.clone()
						found_redirect = true
						break
					}
				}
				if !found_redirect {
					break
				}
				mut next_redirection := 0
				for val in cache_entries {
					if val.id == cache_id {
						next_redirection = val.id_redirection
						break
					}
				}
				if next_redirection == 0 {
					break
				}
				id_value_recursion = next_redirection
			}
		}
		break
	}
	if cache_found {
		// Function is used to signal that the process has begun using the cache information.
		tm.remaining_template_request(true, cache_id)
		return true, cache_id, cache_path, last_template_mod, generate_at, cache_delay_expiration, content_checksum
	}
	// No existing cache, need to create it.
	return false, 0, '', 0, 0, 0, ''
}

// remaining_template_request manages the counter in 'nbr_of_remaining_template_request',
// which tracks the number of requests that have started or finished for a specific
// rendered-cache entry. If a replaced cache is no longer in use, it is deleted.
fn (mut tm DynamicTemplateManager) remaining_template_request(b bool, v int) {
	mut delete_request_id := 0
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
					if tm.nbr_of_remaining_template_request[key].nbr_of_remaining_request <= 0
						&& tm.nbr_of_remaining_template_request[key].need_to_send_delete_request {
						delete_request_id = r_request.id
					}
				}

				break
			}
		}
	}
	if delete_request_id != 0 {
		mut delete_request := TemplateCache{
			id:            delete_request_id
			cache_request: .delete
		}
		tm.process_cache_request(mut delete_request)
	}
}

// process_cache_request stores or deletes one rendered-cache entry synchronously.
// Small rendered outputs are kept in memory; larger outputs are written to disk.
fn (mut tm DynamicTemplateManager) process_cache_request(mut tc TemplateCache) bool {
	tc.ensure_owned_data()

	// Determine if the request is a duplicate. If so, the cache creation/update request is ignored.
	is_duplicate_request := tm.chandler_prevent_cache_duplicate_request(tc)
	if is_duplicate_request {
		return true
	}

	if tc.cache_request != .delete {
		rendered_html := tc.html_data.clone()
		if tc.html_data.len <= (tm.max_size_data_in_memory * 1024) {
			tc.cache_storage_mode = .memory
		} else {
			tc.cache_storage_mode = .disk
		}

		combined_str := rendered_html + tc.path + tc.generate_at.str()
		tc.checksum = md5.hexhash(combined_str).clone()

		match tc.cache_storage_mode {
			.memory {
				tc.html_data = rendered_html.clone()
			}
			.disk {
				cache_file_path := tm.cache_disk_path(tc.name, tc.checksum)
				os.write_file(cache_file_path, rendered_html) or {
					eprintln('${message_signature_error} Rendered cache: failed to write cache file: ${err.msg()}')
					return false
				}
				tc.cache_full_path_name = cache_file_path.clone()
				// Keep a hot in-memory copy even for disk-backed entries. The disk file
				// preserves compatibility, while cache hits avoid fragile string metadata
				// roundtrips in optimized prod builds.
				tc.html_data = rendered_html.clone()
			}
		}
	}

	if tc.cache_request != .delete {
		stable_cache_entry := tc.clone_cache_entry()
		lock tm.template_caches {
			tm.template_caches << stable_cache_entry
		}
		path_hash := fnv1a.sum64_string(stable_cache_entry.path)
		tm.template_cache_ids_by_path_hash[path_hash] = stable_cache_entry.id
	}
	if tc.cache_request == .new {
		tm.chandler_remaining_cache_template_used(tc.cache_request, tc.id, tc.old_id)
	} else {
		test_b := tm.chandler_remaining_cache_template_used(tc.cache_request, tc.id, tc.old_id)
		if test_b {
			clear_cache_id := if tc.cache_request == .delete { tc.id } else { tc.old_id }
			key, is_success := tm.chandler_clear_specific_cache(clear_cache_id)
			if !is_success {
				return false
			}
			lock tm.template_caches {
				tm.template_caches.delete(key)
			}
		}
	}
	return true
}

fn (tm &DynamicTemplateManager) cache_disk_path(name string, checksum string) string {
	cache_file_name := '${name}_${checksum}.cache'
	return os.join_path(tm.template_cache_folder, cache_file_name).clone()
}

fn (mut tc TemplateCache) ensure_owned_strings() {
	tc.name = tc.name.clone()
	tc.path = tc.path.clone()
	tc.checksum = tc.checksum.clone()
	tc.content_checksum = tc.content_checksum.clone()
	tc.cache_full_path_name = tc.cache_full_path_name.clone()
}

fn (mut tc TemplateCache) ensure_owned_data() {
	tc.ensure_owned_strings()
	tc.html_data = tc.html_data.clone()
}

fn (tc TemplateCache) clone_cache_entry() TemplateCache {
	return TemplateCache{
		id:                     tc.id
		name:                   tc.name.clone()
		old_id:                 tc.old_id
		path:                   tc.path.clone()
		checksum:               tc.checksum.clone()
		content_checksum:       tc.content_checksum.clone()
		last_template_mod:      tc.last_template_mod
		generate_at:            tc.generate_at
		cache_delay_expiration: tc.cache_delay_expiration
		html_data:              tc.html_data.clone()
		cache_request:          tc.cache_request
		cache_storage_mode:     tc.cache_storage_mode
		id_redirection:         tc.id_redirection
		cache_full_path_name:   tc.cache_full_path_name.clone()
	}
}

fn (tm &DynamicTemplateManager) snapshot_template_caches() []TemplateCache {
	rlock tm.template_caches {
		mut entries := []TemplateCache{cap: tm.template_caches.len}
		for entry in tm.template_caches {
			entries << entry.clone_cache_entry()
		}
		return entries
	}
}

fn (mut tm DynamicTemplateManager) signal_cache_ready() {
	$if test {
		select {
			tm.is_ready <- true {}
			else {}
		}
	}
}

fn (mut tm DynamicTemplateManager) cache_handler() {
	defer {
		tm.ch_cache_handler.close()
		tm.is_ready.close()
	}
	for {
		select {
			tc := <-tm.ch_cache_handler {
				if tm.close_cache_handler {
					eprintln('${message_signature_info} Cache manager has been successfully stopped. Please consider restarting the application if needed.')
					return
				}
				mut request := tc
				if request.cache_request != .delete && request.old_id == 0 {
					request.old_id = tm.id_to_handlered
				}
				tm.process_cache_request(mut request)
				tm.signal_cache_ready()
			}
		}
	}
}

// fn (DynamicTemplateManager) chandler_prevent_cache_duplicate_request(&TemplateCache) return bool
//
// Used by rendered-cache processing to assess whether a cache request is a duplicate,
// based on the type of cache request (.new, .update, .exp_update, .delete) and the existing data.
// Returns true to indicate a duplicate request, which will be ignored, and false otherwise.
//
fn (tm &DynamicTemplateManager) chandler_prevent_cache_duplicate_request(tc &TemplateCache) bool {
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
// Used to remove specific rendered-cache information from the database when necessary.
// It identifies the target 'TemplateCache' by its id in the array database and deletes its corresponding cache file in 'disk mode'.
//
fn (mut tm DynamicTemplateManager) chandler_clear_specific_cache(id int) (int, bool) {
	for key, value in tm.template_caches {
		if value.id == id {
			path_hash := fnv1a.sum64_string(value.path)
			if active_cache_id := tm.template_cache_ids_by_path_hash[path_hash] {
				if active_cache_id == id {
					tm.template_cache_ids_by_path_hash.delete(path_hash)
				}
			}
			match value.cache_storage_mode {
				.memory {}
				.disk {
					file_path := tm.cache_disk_path(value.name, value.checksum)
					os.rm(file_path) or {
						eprintln('${message_signature_error} While deleting the specific cache file: ${file_path}: ${err.msg()}')
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
// Manages the lifecycle of rendered-cache requests in 'nbr_of_remaining_template_request'.
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
				for key := 0; key < tm.nbr_of_remaining_template_request.len; key++ {
					if tm.nbr_of_remaining_template_request[key].id == old_id {
						tm.nbr_of_remaining_template_request[key].need_to_delete = true
						// If possible, immediately deleted request
						if tm.nbr_of_remaining_template_request[key].nbr_of_remaining_request <= 0
							&& tm.nbr_of_remaining_template_request[key].need_to_delete == true {
							tm.nbr_of_remaining_template_request.delete(key)
							tm.nbr_of_remaining_template_request << RemainingTemplateRequest{
								id: id
							}
							return true
							// else, set for deferred deletion of the cache as soon as feasible
						} else {
							tm.nbr_of_remaining_template_request[key].need_to_send_delete_request = true
							tm.nbr_of_remaining_template_request << RemainingTemplateRequest{
								id: id
							}
						}
						break
					}
				}
				return false
			}
			.delete {
				// Removes the cache request from the list if it's no longer in use.
				for key := 0; key < tm.nbr_of_remaining_template_request.len; key++ {
					if tm.nbr_of_remaining_template_request[key].id == id {
						if tm.nbr_of_remaining_template_request[key].nbr_of_remaining_request <= 0
							&& tm.nbr_of_remaining_template_request[key].need_to_delete == true {
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
		tmpl_ = compress_html_output(tmpl_)
	}

	return tmpl_
}

fn compress_html_output(html string) string {
	mut compressed := html.replace('\n', '').replace('\t', '')
	for compressed.contains('  ') {
		compressed = compressed.replace('  ', ' ')
	}
	mut result := strings.new_builder(compressed.len)
	mut i := 0
	for i < compressed.len {
		result.write_u8(compressed[i])
		if compressed[i] == `>` {
			mut j := i + 1
			for j < compressed.len && compressed[j] == ` ` {
				j++
			}
			if j < compressed.len && compressed[j] == `<` {
				i = j
				continue
			}
		}
		i++
	}
	return result.str()
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
	last_template_mod i64, test_current_template_mod i64, cache_del_exp i64, gen_at i64, c_time i64,
	content_checksum string, current_content_checksum string) (CacheRequest, i64) {
	current_ts := if c_time > 0 { c_time } else { tm.next_cache_timestamp() }
	if !is_cache_exist || neg_cache_delay_expiration == -1 {
		// Require cache creation
		unique_ts := tm.next_cache_timestamp()
		return CacheRequest.new, unique_ts
	} else if last_template_mod < test_current_template_mod
		|| content_checksum != current_content_checksum {
		// Requires cache update as the template has been modified since the last time. it can be the template itself or its dynamic content.
		unique_ts := tm.next_cache_timestamp()
		return CacheRequest.update, unique_ts
	} else if cache_del_exp != 0 && (gen_at + cache_del_exp) < current_ts {
		// Requires cache update as the cache expiration delay has elapsed.
		unique_ts := tm.next_cache_timestamp()
		return CacheRequest.exp_update, unique_ts
	} else {
		// Returns valid cached content, no update or creation necessary.
		return CacheRequest.cached, 0
	}
}

fn (mut tm DynamicTemplateManager) next_cache_timestamp() i64 {
	current_ts := get_current_unix_micro_timestamp()
	if current_ts <= tm.c_time {
		tm.c_time++
	} else {
		tm.c_time = current_ts
	}
	return tm.c_time
}

// fn get_current_unix_timestamp() return i64
//
// This function is designed for handling timezone adjustments by converting the machine's local time at micro format to a universal micro format.
//
fn get_current_unix_micro_timestamp() i64 {
	return time.now().unix_micro()
}
