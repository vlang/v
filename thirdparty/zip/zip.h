/*
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

#pragma once
#ifndef ZIP_H
#define ZIP_H

#include <string.h>
#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(_SSIZE_T_DEFINED) && !defined(_SSIZE_T_DEFINED_) &&               \
    !defined(__DEFINED_ssize_t) && !defined(__ssize_t_defined) &&              \
    !defined(_SSIZE_T) && !defined(_SSIZE_T_) && !defined(_SSIZE_T_DECLARED)

// 64-bit Windows is the only mainstream platform
// where sizeof(long) != sizeof(void*)
#ifdef _WIN64
typedef long long ssize_t; /* byte count or error */
#else
typedef long ssize_t; /* byte count or error */
#endif

#define _SSIZE_T_DEFINED
#define _SSIZE_T_DEFINED_
#define __DEFINED_ssize_t
#define __ssize_t_defined
#define _SSIZE_T
#define _SSIZE_T_
#define _SSIZE_T_DECLARED

#endif

#ifndef MAX_PATH
#define MAX_PATH 32767 /* # chars in a path name including NULL */
#endif

/**
 * @mainpage
 *
 * Documenation for @ref zip.
 */

/**
 * @addtogroup zip
 * @{
 */

/**
 * Default zip compression level.
 */

#define ZIP_DEFAULT_COMPRESSION_LEVEL 6

/**
 * @struct zip_t
 *
 * This data structure is used throughout the library to represent zip archive -
 * forward declaration.
 */
struct zip_t;

/**
 * Opens zip archive with compression level using the given mode.
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
extern struct zip_t *zip_open(const char *zipname, int level, char mode);

/**
 * Closes the zip archive, releases resources - always finalize.
 *
 * @param zip zip archive handler.
 */
extern void zip_close(struct zip_t *zip);

/**
 * Determines if the archive has a zip64 end of central directory headers.
 *
 * @param zip zip archive handler.
 *
 * @return the return code - 1 (true), 0 (false), negative number (< 0) on
 *         error.
 */
extern int zip_is64(struct zip_t *zip);

/**
 * Opens an entry by name in the zip archive.
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
extern int zip_entry_open(struct zip_t *zip, const char *entryname);

/**
 * Opens a new entry by index in the zip archive.
 *
 * This function is only valid if zip archive was opened in 'r' (readonly) mode.
 *
 * @param zip zip archive handler.
 * @param index index in local dictionary.
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
extern int zip_entry_openbyindex(struct zip_t *zip, int index);

/**
 * Closes a zip entry, flushes buffer and releases resources.
 *
 * @param zip zip archive handler.
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
extern int zip_entry_close(struct zip_t *zip);

/**
 * Returns a local name of the current zip entry.
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
extern const char *zip_entry_name(struct zip_t *zip);

/**
 * Returns an index of the current zip entry.
 *
 * @param zip zip archive handler.
 *
 * @return the index on success, negative number (< 0) on error.
 */
extern int zip_entry_index(struct zip_t *zip);

/**
 * Determines if the current zip entry is a directory entry.
 *
 * @param zip zip archive handler.
 *
 * @return the return code - 1 (true), 0 (false), negative number (< 0) on
 *         error.
 */
extern int zip_entry_isdir(struct zip_t *zip);

/**
 * Returns an uncompressed size of the current zip entry.
 *
 * @param zip zip archive handler.
 *
 * @return the uncompressed size in bytes.
 */
extern unsigned long long zip_entry_size(struct zip_t *zip);

/**
 * Returns CRC-32 checksum of the current zip entry.
 *
 * @param zip zip archive handler.
 *
 * @return the CRC-32 checksum.
 */
extern unsigned int zip_entry_crc32(struct zip_t *zip);

/**
 * Compresses an input buffer for the current zip entry.
 *
 * @param zip zip archive handler.
 * @param buf input buffer.
 * @param bufsize input buffer size (in bytes).
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
extern int zip_entry_write(struct zip_t *zip, const void *buf, size_t bufsize);

/**
 * Compresses a file for the current zip entry.
 *
 * @param zip zip archive handler.
 * @param filename input file.
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
extern int zip_entry_fwrite(struct zip_t *zip, const char *filename);

/**
 * Extracts the current zip entry into output buffer.
 *
 * The function allocates sufficient memory for a output buffer.
 *
 * @param zip zip archive handler.
 * @param buf output buffer.
 * @param bufsize output buffer size (in bytes).
 *
 * @note remember to release memory allocated for a output buffer.
 *       for large entries, please take a look at zip_entry_extract function.
 *
 * @return the return code - the number of bytes actually read on success.
 *         Otherwise a -1 on error.
 */
extern ssize_t zip_entry_read(struct zip_t *zip, void **buf, size_t *bufsize);

/**
 * Extracts the current zip entry into a memory buffer using no memory
 * allocation.
 *
 * @param zip zip archive handler.
 * @param buf preallocated output buffer.
 * @param bufsize output buffer size (in bytes).
 *
 * @note ensure supplied output buffer is large enough.
 *       zip_entry_size function (returns uncompressed size for the current
 *       entry) can be handy to estimate how big buffer is needed. for large
 * entries, please take a look at zip_entry_extract function.
 *
 * @return the return code - the number of bytes actually read on success.
 *         Otherwise a -1 on error (e.g. bufsize is not large enough).
 */
extern ssize_t zip_entry_noallocread(struct zip_t *zip, void *buf,
                                     size_t bufsize);

/**
 * Extracts the current zip entry into output file.
 *
 * @param zip zip archive handler.
 * @param filename output file.
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
extern int zip_entry_fread(struct zip_t *zip, const char *filename);

/**
 * Extracts the current zip entry using a callback function (on_extract).
 *
 * @param zip zip archive handler.
 * @param on_extract callback function.
 * @param arg opaque pointer (optional argument, which you can pass to the
 *        on_extract callback)
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
extern int
zip_entry_extract(struct zip_t *zip,
                  size_t (*on_extract)(void *arg, unsigned long long offset,
                                       const void *data, size_t size),
                  void *arg);

/**
 * Returns the number of all entries (files and directories) in the zip archive.
 *
 * @param zip zip archive handler.
 *
 * @return the return code - the number of entries on success, negative number
 *         (< 0) on error.
 */
extern int zip_total_entries(struct zip_t *zip);

/**
 * Creates a new archive and puts files into a single zip archive.
 *
 * @param zipname zip archive file.
 * @param filenames input files.
 * @param len: number of input files.
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
extern int zip_create(const char *zipname, const char *filenames[], size_t len);

/**
 * Extracts a zip archive file into directory.
 *
 * If on_extract_entry is not NULL, the callback will be called after
 * successfully extracted each zip entry.
 * Returning a negative value from the callback will cause abort and return an
 * error. The last argument (void *arg) is optional, which you can use to pass
 * data to the on_extract_entry callback.
 *
 * @param zipname zip archive file.
 * @param dir output directory.
 * @param on_extract_entry on extract callback.
 * @param arg opaque pointer.
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
extern int zip_extract(const char *zipname, const char *dir,
                       int (*on_extract_entry)(const char *filename, void *arg),
                       void *arg);
// temporary working unzip solution
extern int zip_extract_without_callback(const char *zipname, const char *dir);

/**
 * Extracts a zip archive stream into directory.
 *
 * If on_extract is not NULL, the callback will be called after
 * successfully extracted each zip entry.
 * Returning a negative value from the callback will cause abort and return an
 * error. The last argument (void *arg) is optional, which you can use to pass
 * data to the on_extract callback.
 *
 * @param stream zip archive stream.
 * @param size stream size.
 * @param dir output directory.
 * @param on_extract on extract callback.
 * @param arg opaque pointer.
 *
 * @return the return code - 0 on success, negative number (< 0) on error.
 */
extern int zip_extract_stream(const char *stream, size_t size, const char *dir,
                              int (*on_extract)(const char *filename,
                                                void *arg),
                              void *arg);
/** @} */

#ifdef __cplusplus
}
#endif

#endif
