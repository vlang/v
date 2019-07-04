module os

const (
	FILE_SHARE_READ   = 1
	FILE_SHARE_WRITE  = 2
	FILE_SHARE_DELETE = 4
)

const (
	FILE_NOTIFY_CHANGE_FILE_NAME   = 1
	FILE_NOTIFY_CHANGE_DIR_NAME    = 2
	FILE_NOTIFY_CHANGE_ATTRIBUTES  = 4
	FILE_NOTIFY_CHANGE_SIZE        = 8
	FILE_NOTIFY_CHANGE_LAST_WRITE  = 16
	FILE_NOTIFY_CHANGE_LAST_ACCESS = 32
	FILE_NOTIFY_CHANGE_CREATION    = 64
	FILE_NOTIFY_CHANGE_SECURITY    = 128
)

const (
	FILE_ACTION_ADDED              = 1
	FILE_ACTION_REMOVED            = 2
	FILE_ACTION_MODIFIED           = 3
	FILE_ACTION_RENAMED_OLD_NAME   = 4
	FILE_ACTION_RENAMED_NEW_NAME   = 5
)

const (
	FILE_ATTR_READONLY                 = 0x1
	FILE_ATTR_HIDDEN                   = 0x2
	FILE_ATTR_SYSTEM                   = 0x4
	FILE_ATTR_DIRECTORY                = 0x10
	FILE_ATTR_ARCHIVE                  = 0x20
	FILE_ATTR_DEVICE                   = 0x40
	FILE_ATTR_NORMAL                   = 0x80
	FILE_ATTR_TEMPORARY                = 0x100
	FILE_ATTR_SPARSE_FILE              = 0x200
	FILE_ATTR_REPARSE_POINT            = 0x400
	FILE_ATTR_COMPRESSED               = 0x800
	FILE_ATTR_OFFLINE                  = 0x1000
	FILE_ATTR_NOT_CONTENT_INDEXED      = 0x2000
	FILE_ATTR_ENCRYPTED                = 0x4000
	FILE_ATTR_INTEGRITY_STREAM         = 0x8000
	FILE_ATTR_VIRTUAL                  = 0x10000
	FILE_ATTR_NO_SCRUB_DATA            = 0x20000
	// FILE_ATTR_RECALL_ON_OPEN        = u32(0x...)
	// FILE_ATTR_RECALL_ON_DATA_ACCESS = u32(0x...)
)

const (
	FILE_TYPE_DISK = 0x1
	FILE_TYPE_CHAR = 0x2
	FILE_TYPE_PIPE = 0x3

	FILE_TYPE_UNKNOWN = 0x0
)

const (
	FILE_INVALID_FILE_ID = (-1)
)

// (Must be realized in Syscall) (Must be specified)
// File modes.

const (
    O_RDONLY = 1 // open the file read-only.
    O_WRONLY = 2 // open the file write-only.
    O_RDWR   = 3 // open the file read-write.

    O_APPEND = 8   // append data to the file when writing.
    O_CREATE = 16  // create a new file if none exists.
    O_EXCL   = 32  // used with O_CREATE, file must not exist.
    O_SYNC   = 64  // open for synchronous I/O.
    O_TRUNC  = 128 // truncate regular writable file when opened.
)

// Windows 
const(
	INVALID_HANDLE_VALUE = -1
)

const(
    STD_INPUT_HANDLE  = -10
    STD_OUTPUT_HANDLE = -11
    STD_ERROR_HANDLE  = -12
)

// https://docs.microsoft.com/en-us/windows/console/setconsolemode
const (
    // Input Buffer
    ENABLE_ECHO_INPUT                  = 0x0004
    ENABLE_EXTENDED_FLAGS              = 0x0080
    ENABLE_INSERT_MODE                 = 0x0020
    ENABLE_LINE_INPUT                  = 0x0002
    ENABLE_MOUSE_INPUT                 = 0x0010
    ENABLE_PROCESSED_INPUT             = 0x0001	
    ENABLE_QUICK_EDIT_MODE             = 0x0040
    ENABLE_WINDOW_INPUT                = 0x0008
    ENABLE_VIRTUAL_TERMINAL_INPUT      = 0x0200
    // Output Screen Buffer
    ENABLE_PROCESSED_OUTPUT            = 0x0001
    ENABLE_WRAP_AT_EOL_OUTPUT          = 0x0002
    ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004
    DISABLE_NEWLINE_AUTO_RETURN        = 0x0008
    ENABLE_LVB_GRID_WORLDWIDE          = 0x0010
)
// End Windows
