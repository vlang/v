module os

// Ref - winnt.h
const (
	success                   = 0x0000 // ERROR_SUCCESS
	error_insufficient_buffer = 0x0082
)

const (
	handle_generic_read  = 0x80000000
	handle_open_existing = 0x00000003
)

const (
	file_share_read   = 0x01
	file_share_write  = 0x02
	file_share_delete = 0x04
)

const (
	file_notify_change_file_name   = 0x01
	file_notify_change_dir_name    = 0x02
	file_notify_change_attributes  = 0x04
	file_notify_change_size        = 0x08
	file_notify_change_last_write  = 0x10
	file_notify_change_last_access = 0x20
	file_notify_change_creation    = 0x40
	file_notify_change_security    = 0x80
)

const (
	file_action_added            = 0x01
	file_action_removed          = 0x02
	file_action_modified         = 0x03
	file_action_renamed_old_name = 0x04
	file_action_renamed_new_name = 0x05
)

const (
	file_attr_readonly            = 0x00000001
	file_attr_hidden              = 0x00000002
	file_attr_system              = 0x00000004
	file_attr_directory           = 0x00000010
	file_attr_archive             = 0x00000020
	file_attr_device              = 0x00000040
	file_attr_normal              = 0x00000080
	file_attr_temporary           = 0x00000100
	file_attr_sparse_file         = 0x00000200
	file_attr_reparse_point       = 0x00000400
	file_attr_compressed          = 0x00000800
	file_attr_offline             = 0x00001000
	file_attr_not_content_indexed = 0x00002000
	file_attr_encrypted           = 0x00004000
	file_attr_integrity_stream    = 0x00008000
	file_attr_virtual             = 0x00010000
	file_attr_no_scrub_data       = 0x00020000
		// file_attr_recall_on_open        = u32(0x...)
		// file_attr_recall_on_data_access = u32(0x...)
)

const (
	file_type_unknown = 0x00
	file_type_disk    = 0x01
	file_type_char    = 0x02
	file_type_pipe    = 0x03
)

const (
	file_invalid_file_id = (-1)
)

const (
	invalid_handle_value = voidptr(-1)
)

// https://docs.microsoft.com/en-us/windows/console/setconsolemode
const (
	// Input Buffer
	enable_echo_input                  = 0x0004
	enable_extended_flags              = 0x0080
	enable_insert_mode                 = 0x0020
	enable_line_input                  = 0x0002
	enable_mouse_input                 = 0x0010
	enable_processed_input             = 0x0001
	enable_quick_edit_mode             = 0x0040
	enable_window_input                = 0x0008
	enable_virtual_terminal_input      = 0x0200
	// Output Screen Buffer
	enable_processed_output            = 0x01
	enable_wrap_at_eol_output          = 0x02
	enable_virtual_terminal_processing = 0x04
	disable_newline_auto_return        = 0x08
	enable_lvb_grid_worldwide          = 0x10
)

// File modes
const (
	o_rdonly   = 0x0000 // open the file read-only.
	o_wronly   = 0x0001 // open the file write-only.
	o_rdwr     = 0x0002 // open the file read-write.
	o_append   = 0x0008 // append data to the file when writing.
	o_create   = 0x0100 // create a new file if none exists.
	o_binary   = 0x8000 // input and output is not translated.
	o_trunc    = 0x0200 // truncate regular writable file when opened.
	o_excl     = 0x0400 // used with o_create, file must not exist.
	o_sync     = 0x0000 // open for synchronous I/O (ignored on Windows)
	o_noctty   = 0x0000 // make file non-controlling tty (ignored on Windows)
	o_nonblock = 0x0000 // don't block on opening file (ignored on Windows)
)

const (
	status_access_violation           = 0xC0000005
	status_in_page_error              = 0xC0000006
	status_invalid_handle             = 0xC0000008
	status_invalid_parameter          = 0xC000000D
	status_no_memory                  = 0xC0000017
	status_illegal_instruction        = 0xC000001D
	status_noncontinuable_exception   = 0xC0000025
	status_invalid_disposition        = 0xC0000026
	status_array_bounds_exceeded      = 0xC000008C
	status_float_denormal_operand     = 0xC000008D
	status_float_divide_by_zero       = 0xC000008E
	status_float_inexact_result       = 0xC000008F
	status_float_invalid_operation    = 0xC0000090
	status_float_overflow             = 0xC0000091
	status_float_stack_check          = 0xC0000092
	status_float_underflow            = 0xC0000093
	status_integer_divide_by_zero     = 0xC0000094
	status_integer_overflow           = 0xC0000095
	status_privileged_instruction     = 0xC0000096
	status_stack_overflow             = 0xC00000FD
	status_dll_not_found              = 0xC0000135
	status_ordinal_not_found          = 0xC0000138
	status_entrypoint_not_found       = 0xC0000139
	status_control_c_exit             = 0xC000013A
	status_dll_init_failed            = 0xC0000142
	status_float_multiple_faults      = 0xC00002B4
	status_float_multiple_traps       = 0xC00002B5
	status_reg_nat_consumption        = 0xC00002C9
	status_heap_corruption            = 0xC0000374
	status_stack_buffer_overrun       = 0xC0000409
	status_invalid_cruntime_parameter = 0xC0000417
	status_assertion_failure          = 0xC0000420
)

// Windows Registry Constants
pub const (
	hkey_local_machine     = voidptr(0x80000002)
	hkey_current_user      = voidptr(0x80000001)
	key_query_value        = 0x0001
	key_set_value          = 0x0002
	key_enumerate_sub_keys = 0x0008
	key_wow64_32key        = 0x0200
)

// Windows Messages
pub const (
	hwnd_broadcast   = voidptr(0xFFFF)
	wm_settingchange = 0x001A
	smto_abortifhung = 0x0002
)
