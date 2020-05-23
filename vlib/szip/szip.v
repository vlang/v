module szip

#flag -I @VROOT/thirdparty/zip

#include "zip.c"
#include "zip.h"

type zip      C.zip_t
type zip_ptr &zip

// Ref - miniz.h
const (
    no_compression      = 0
    best_speed          = 1
    best_compression    = 9
    uber_compression    = 10
    default_level       = 6
    default_compression = -1
)

// Ref - zip.h
const (
    default_compression_level = (default_level)
)

const (
    m_write  = 'w'
    m_ronly  = 'r'
    m_append = 'a'
)

/**
 * open opens zip archive with compression level using the given mode.
 *
 * @param zipname zip archive file name.
 * @param level compression level (0-9 are the standard zlib-style levels).
 * @param mode file access mode.
 *        - 'r': opens a file for reading/extracting (the file must exists).
 *        - 'w': creates an empty file for writing.
 *        - 'a': appends to an existing archive.
 *
 * @return the zip archive handler or NULL on error
 */
pub fn open(name string, level int, mode string) ?zip_ptr {
    mut _nlevel := level
    if (_nlevel & 0xF) > uber_compression {
        _nlevel = default_compression_level
    }
    n := name.len
    if n == 0 {
        return error('szip: name of file empty')
    }
    if mode != m_write && mode != m_ronly && mode != m_append {
        return error('szip: invalid provided open mode')
    }
    /* struct zip_t* */_p_zip := zip_ptr(C.zip_open(name.str,
                                 _nlevel, mode.str))
    if _p_zip == zip_ptr(0) {
        return error('szip: cannot open/create/append new zip archive')
    }
    return _p_zip
}

/**
 * close closes the zip archive, releases resources - always finalize.
 *
 * @param zip zip archive handler.
 */
pub fn (mut z zip_ptr) close() {
    C.zip_close(z)
}

/**
 * open_entry opens an entry by name in the zip archive.
 *
 * For zip archive opened in 'w' or 'a' mode the function will append
 * a new entry. In readonly mode the function tries to locate the entry
 * in global dictionary.
 *
 * @param zip zip archive handler.
 * @param entryname an entry name in local dictionary.
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
pub fn (mut zentry zip_ptr) open_entry(name string) /*?*/bool {
    res := C.zip_entry_open(zentry, name.str)
    return res != -1
}

/**
 * close_entry closes a zip entry, flushes buffer and releases resources.
 *
 * @param zip zip archive handler.
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
pub fn (mut zentry zip_ptr) close_entry() {
    C.zip_entry_close(zentry)
}

/**
 * name returns a local name of the current zip entry.
 *
 * The main difference between user's entry name and local entry name
 * is optional relative path.
 * Following .ZIP File Format Specification - the path stored MUST not contain
 * a drive or device letter, or a leading slash.
 * All slashes MUST be forward slashes '/' as opposed to backwards slashes '\'
 * for compatibility with Amiga and UNIX file systems etc.
 *
 * @param zip: zip archive handler.
 *
 * @return the pointer to the current zip entry name, or NULL on error.
 */
pub fn (mut zentry zip_ptr) name() string {
    _name := C.zip_entry_name(zentry)
    if _name == 0 {
        return ''
    }
    return tos_clone(_name)
}

/**
 * index returns an index of the current zip entry.
 *
 * @param zip zip archive handler.
 *
 * @return the index on success, negative number (< 0) on error.
 */
pub fn (mut zentry zip_ptr) index() ?int {
    _index := int(C.zip_entry_index(zentry))
    if _index == -1 {
        return error('szip: cannot get current index of zip entry')
    }
    return _index // must be check for INVALID_VALUE
}

/**
 * isdir determines if the current zip entry is a directory entry.
 *
 * @param zip zip archive handler.
 *
 * @return the return code - 1 (true), 0 (false), negative number (< 0) on
 *         error.
 */
pub fn (mut zentry zip_ptr) isdir() ?bool {
    _isdir := C.zip_entry_isdir(zentry)
    if _isdir == -1 {
        return error('szip: cannot check entry type')
    }
    dir := bool(_isdir) // wtf V , unary lvalue
    return dir
}

/**
 * size returns an uncompressed size of the current zip entry.
 *
 * @param zip zip archive handler.
 *
 * @return the uncompressed size in bytes.
 */
pub fn (mut zentry zip_ptr) size() i64 {
    _size := i64(C.zip_entry_size(zentry))
    return _size
}

/**
 * crc32 returns CRC-32 checksum of the current zip entry.
 *
 * @param zip zip archive handler.
 *
 * @return the CRC-32 checksum.
 */
pub fn (mut zentry zip_ptr) crc32() u32 {
    _checksum := u32(C.zip_entry_crc32(zentry))
    return _checksum // 0
}

/**
 * write_entry compresses an input buffer for the current zip entry.
 *
 * @param zip zip archive handler.
 * @param buf input buffer.
 * @param bufsize input buffer size (in bytes).
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
pub fn (mut zentry zip_ptr) write_entry(data []byte) bool {
    if (data[0] & 0xff) == -1 {
        return false
    }
    buf := data // alias of data
    res := C.zip_entry_write(zentry, buf.data, buf.len)
    return res == 0
}

/**
 * create_entry compresses a file for the current zip entry.
 *
 * @param zip zip archive handler.
 * @param filename input file.
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
pub fn (mut zentry zip_ptr) create_entry(name string) bool {
    res := C.zip_entry_fwrite(zentry, name.str)
    return res == 0
}

/**
 * read_entry extracts the current zip entry into output buffer.
 *
 * The function allocates sufficient memory for an output buffer.
 *
 * @param zip zip archive handler.
 * @param buf output buffer.
 * @param bufsize output buffer size (in bytes).
 *
 * @note remember to release the memory allocated for an output buffer.
 *       for large entries, please take a look at zip_entry_extract function.
 *
 * @return the return code - the number of bytes actually read on success.
 *         Otherwise a -1 on error.
 */
pub fn (mut zentry zip_ptr) read_entry() ?voidptr {
    mut _buf := voidptr(0)
    mut _bsize := i64(0)
    res := C.zip_entry_read(zentry, &_buf, &_bsize)
    if res == -1 {
        return error('szip: cannot read properly data from entry')
    }
    return _buf
}

/**
 * extract_entry extracts the current zip entry into output file.
 *
 * @param zip zip archive handler.
 * @param filename output file.
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
pub fn (mut zentry zip_ptr) extract_entry(path string) /*?*/bool {
    if C.access(path.str, 0) == -1 {
        return false
        //return error('Cannot open file for extracting, file not exists')
    }
    res := C.zip_entry_fread(zentry, path.str)
    return res == 0
}

/**
 * extract extracts the current zip entry using a callback function (on_extract).
 *
 * @param zip zip archive handler.
 * @param on_extract callback function.
 * @param arg opaque pointer (optional argument, which you can pass to the
 *        on_extract callback)
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
/*fn (mut zentry zip_ptr) extract(path string) bool {
    if C.access(path.str, 0) == -1 {
        return false
        //return error('Cannot open directory for extracting, directory not exists')
    }
    res := C.zip_extract(zentry, path.str, 0, 0)
    return res == 0
}*/

/**
 * total returns the number of all entries (files and directories) in the zip archive.
 *
 * @param zip zip archive handler.
 *
 * @return the return code - the number of entries on success, negative number
 *         (< 0) on error.
 */
pub fn (mut zentry zip_ptr) total() ?int {
    _tentry := int(C.zip_total_entries(zentry))
    if _tentry == -1 {
        return error('szip: cannot count total entries')
    }
    return _tentry
}
