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

// Permission bits
const (
	S_IREAD	 = 0400	   /* Read by owner.    */
	S_IWRITE = 0200	   /* Write by owner.   */
	S_IEXEC	 = 0100	   /* Execute by owner. */

	S_IRUSR	= S_IREAD  /* Alias of S_IREAD  */
	S_IWUSR	= S_IWRITE /* Alias of S_IWRITE */
	S_IXUSR	= S_IEXEC  /* Alias of S_IEXEC  */

	S_IRWXU	= (S_IREAD|S_IWRITE|S_IEXEC)

	S_IRGRP	= (S_IRUSR >> 3)    /* Read by group.                     */
	S_IWGRP	= (S_IWUSR >> 3)    /* Write by group.                    */
	S_IXGRP	= (S_IXUSR >> 3)    /* Execute by group.                  */
	S_IRWXG	= (S_IRWXU >> 3)    /* Read, write, and execute by group. */

	S_IROTH	= (S_IRGRP >> 3)    /* Read by others.                     */
	S_IWOTH	= (S_IWGRP >> 3)    /* Write by others.                    */
	S_IXOTH	= (S_IXGRP >> 3)    /* Execute by others.                  */
	S_IRWXO	= (S_IRWXG >> 3)    /* Read, write, and execute by others. */

	ACCESSPERMS = (S_IRWXU|S_IRWXG|S_IRWXO)                         /* 0777 */
	ALLPERMS    = (S_ISUID|S_ISGID|S_ISVTX|S_IRWXU|S_IRWXG|S_IRWXO) /* 07777 */
	DEFFILEMODE = (S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH) /* 0666*/
)
