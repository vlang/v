import os

// Example of C interop for a very handy task.
//
// wkhtmltopdf and wkhtmltoimage are open source (LGPLv3) command line tools to
// render HTML into PDF and various image formats using the Qt WebKit rendering
// engine. These run entirely "headless" and do not require a display or display
// service.
//
// https://github.com/wkhtmltopdf/wkhtmltopdf
// https://wkhtmltopdf.org/downloads.html
// https://wkhtmltopdf.org/libwkhtmltox/
#flag -lwkhtmltox
#include "wkhtmltox/pdf.h" # You can install the C package for your system from the wkhtmltopdf.org/downloads.html page

struct C.wkhtmltopdf_global_settings {}

struct C.wkhtmltopdf_object_settings {}

struct C.wkhtmltopdf_converter {}

fn C.wkhtmltopdf_init(use_graphics bool) int

fn C.wkhtmltopdf_deinit() int

fn C.wkhtmltopdf_version() &char

fn C.wkhtmltopdf_create_global_settings() &C.wkhtmltopdf_global_settings

fn C.wkhtmltopdf_destroy_global_settings(global_settings &C.wkhtmltopdf_global_settings)

fn C.wkhtmltopdf_set_global_setting(global_settings &C.wkhtmltopdf_global_settings, name &char, value &char) bool

fn C.wkhtmltopdf_create_object_settings() &C.wkhtmltopdf_object_settings

fn C.wkhtmltopdf_destroy_object_settings(object_settings &C.wkhtmltopdf_object_settings)

fn C.wkhtmltopdf_set_object_setting(object_settings &C.wkhtmltopdf_object_settings, name &char, value &char) bool

fn C.wkhtmltopdf_create_converter(global_settings &C.wkhtmltopdf_global_settings) &C.wkhtmltopdf_converter

fn C.wkhtmltopdf_destroy_converter(converter &C.wkhtmltopdf_converter)

fn C.wkhtmltopdf_add_object(converter &C.wkhtmltopdf_converter, object_settings &C.wkhtmltopdf_object_settings, data &char)

fn C.wkhtmltopdf_convert(converter &C.wkhtmltopdf_converter) bool

fn C.wkhtmltopdf_http_error_code(converter &C.wkhtmltopdf_converter) int

fn C.wkhtmltopdf_get_output(converter &C.wkhtmltopdf_converter, data &&char) int

fn main() {
	// init
	init := C.wkhtmltopdf_init(0)
	println('wkhtmltopdf_init: $init')
	version := unsafe { cstring_to_vstring(&char(C.wkhtmltopdf_version())) }
	println('wkhtmltopdf_version: $version')
	global_settings := C.wkhtmltopdf_create_global_settings()
	println('wkhtmltopdf_create_global_settings: ${voidptr(global_settings)}')
	object_settings := C.wkhtmltopdf_create_object_settings()
	println('wkhtmltopdf_create_object_settings')
	converter := C.wkhtmltopdf_create_converter(global_settings)
	println('wkhtmltopdf_create_converter: ${voidptr(converter)}')
	// convert
	mut result := C.wkhtmltopdf_set_object_setting(object_settings, c'page', c'http://www.google.com.br')
	println('wkhtmltopdf_set_object_setting: $result [page = http://www.google.com.br]')
	C.wkhtmltopdf_add_object(converter, object_settings, 0)
	println('wkhtmltopdf_add_object')
	result = C.wkhtmltopdf_convert(converter)
	println('wkhtmltopdf_convert: $result')
	error_code := C.wkhtmltopdf_http_error_code(converter)
	println('wkhtmltopdf_http_error_code: $error_code')
	if result {
		pdata := &char(0)
		ppdata := &pdata
		size := C.wkhtmltopdf_get_output(converter, voidptr(ppdata))
		println('wkhtmltopdf_get_output: $size bytes')
		mut file := os.open_file('./google.pdf', 'w+', 0o666) or {
			println('ERR: $err')
			return
		}
		wrote := unsafe { file.write_ptr(pdata, size) }
		println('write_bytes: $wrote [./google.pdf]')
		file.flush()
		file.close()
	}
	// destroy
	C.wkhtmltopdf_destroy_converter(converter)
	println('wkhtmltopdf_destroy_converter')
	C.wkhtmltopdf_destroy_object_settings(object_settings)
	println('wkhtmltopdf_destroy_object_settings: ${voidptr(object_settings)}')
	C.wkhtmltopdf_destroy_global_settings(global_settings)
	println('wkhtmltopdf_destroy_global_settings')
	deinit := C.wkhtmltopdf_deinit()
	println('wkhtmltopdf_deinit: $deinit')
}
