module os

const max_path_buffer_size = 2 * max_path_len

// Ref - winnt.h
const success = 0x0000 // ERROR_SUCCESS

const error_insufficient_buffer = 0x0082

const handle_generic_read = u32(0x80000000)
const handle_open_existing = 0x00000003

const file_share_read = 0x01
const file_share_write = 0x02
const file_share_delete = 0x04

const file_notify_change_file_name = 0x01
const file_notify_change_dir_name = 0x02
const file_notify_change_attributes = 0x04
const file_notify_change_size = 0x08
const file_notify_change_last_write = 0x10
const file_notify_change_last_access = 0x20
const file_notify_change_creation = 0x40
const file_notify_change_security = 0x80

const file_action_added = 0x01
const file_action_removed = 0x02
const file_action_modified = 0x03
const file_action_renamed_old_name = 0x04
const file_action_renamed_new_name = 0x05

const file_attr_readonly = 0x00000001
const file_attr_hidden = 0x00000002
const file_attr_system = 0x00000004
const file_attr_directory = 0x00000010
const file_attr_archive = 0x00000020
const file_attr_device = 0x00000040
const file_attr_normal = 0x00000080
const file_attr_temporary = 0x00000100
const file_attr_sparse_file = 0x00000200
const file_attr_reparse_point = 0x00000400
const file_attr_compressed = 0x00000800
const file_attr_offline = 0x00001000
const file_attr_not_content_indexed = 0x00002000
const file_attr_encrypted = 0x00004000
const file_attr_integrity_stream = 0x00008000
const file_attr_virtual = 0x00010000
const file_attr_no_scrub_data = 0x00020000

const file_type_unknown = 0x00
const file_type_disk = 0x01
const file_type_char = 0x02
const file_type_pipe = 0x03

const file_invalid_file_id = (-1)

const invalid_handle_value = voidptr(-1)

// https://docs.microsoft.com/en-us/windows/console/setconsolemode
// Input Buffer
const enable_echo_input = 0x0004
const enable_extended_flags = 0x0080
const enable_insert_mode = 0x0020
const enable_line_input = 0x0002
const enable_mouse_input = 0x0010
const enable_processed_input = 0x0001
const enable_quick_edit_mode = 0x0040
const enable_window_input = 0x0008
const enable_virtual_terminal_input = 0x0200
// Output Screen Buffer
const enable_processed_output = 0x01
const enable_wrap_at_eol_output = 0x02
const enable_virtual_terminal_processing = 0x04
const disable_newline_auto_return = 0x08
const enable_lvb_grid_worldwide = 0x10

// File modes
const o_rdonly = 0x0000 // open the file read-only.

const o_wronly = 0x0001 // open the file write-only.

const o_rdwr = 0x0002 // open the file read-write.

const o_append = 0x0008 // append data to the file when writing.

const o_create = 0x0100 // create a new file if none exists.

const o_binary = 0x8000 // input and output is not translated.

const o_trunc = 0x0200 // truncate regular writable file when opened.

const o_excl = 0x0400 // used with o_create, file must not exist.

const o_sync = 0x0000 // open for synchronous I/O (ignored on Windows)

const o_noctty = 0x0000 // make file non-controlling tty (ignored on Windows)

const o_nonblock = 0x0000

const status_access_violation = u32(0xC0000005)
const status_in_page_error = u32(0xC0000006)
const status_invalid_handle = u32(0xC0000008)
const status_invalid_parameter = u32(0xC000000D)
const status_no_memory = u32(0xC0000017)
const status_illegal_instruction = u32(0xC000001D)
const status_noncontinuable_exception = u32(0xC0000025)
const status_invalid_disposition = u32(0xC0000026)
const status_array_bounds_exceeded = u32(0xC000008C)
const status_float_denormal_operand = u32(0xC000008D)
const status_float_divide_by_zero = u32(0xC000008E)
const status_float_inexact_result = u32(0xC000008F)
const status_float_invalid_operation = u32(0xC0000090)
const status_float_overflow = u32(0xC0000091)
const status_float_stack_check = u32(0xC0000092)
const status_float_underflow = u32(0xC0000093)
const status_integer_divide_by_zero = u32(0xC0000094)
const status_integer_overflow = u32(0xC0000095)
const status_privileged_instruction = u32(0xC0000096)
const status_stack_overflow = u32(0xC00000FD)
const status_dll_not_found = u32(0xC0000135)
const status_ordinal_not_found = u32(0xC0000138)
const status_entrypoint_not_found = u32(0xC0000139)
const status_control_c_exit = u32(0xC000013A)
const status_dll_init_failed = u32(0xC0000142)
const status_float_multiple_faults = u32(0xC00002B4)
const status_float_multiple_traps = u32(0xC00002B5)
const status_reg_nat_consumption = u32(0xC00002C9)
const status_heap_corruption = u32(0xC0000374)
const status_stack_buffer_overrun = u32(0xC0000409)
const status_invalid_cruntime_parameter = u32(0xC0000417)
const status_assertion_failure = u32(0xC0000420)

// Windows Registry Constants
pub const hkey_local_machine = voidptr(0x80000002)
pub const hkey_current_user = voidptr(0x80000001)
pub const key_query_value = 0x0001
pub const key_set_value = 0x0002
pub const key_enumerate_sub_keys = 0x0008
pub const key_wow64_32key = 0x0200

// Windows Messages
pub const hwnd_broadcast = voidptr(0xFFFF)
pub const wm_settingchange = 0x001A
pub const smto_abortifhung = 0x0002
