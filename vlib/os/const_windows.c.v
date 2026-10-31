module os

const max_path_buffer_size = 2 * max_path_len

// Ref - winnt.h
const success = 0x0000 // ERROR_SUCCESS

const file_share_read = 0x01

const invalid_handle_value = voidptr(-1)

// https://docs.microsoft.com/en-us/windows/console/setconsolemode
// Output Screen Buffer
const enable_processed_output = 0x01
const enable_wrap_at_eol_output = 0x02
const enable_virtual_terminal_processing = 0x04

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
