module fastc

import crypto.sha256
import os

// TinyCC signs the executables it links on macOS by running Apple's
// `codesign -f -s - <file>`, and that tool costs about 50 ms per build,
// half of TinyCC's own time for the self-host. The drivers therefore put a
// no-op `codesign` first on TinyCC's PATH (fastc_codesign_shim_dir) and
// ad-hoc sign the Mach-O themselves (fastc_sign_macho_adhoc), in about a
// millisecond.

const macho_magic_64 = u32(0xfeedfacf)
const macho_lc_segment_64 = u32(0x19)
const macho_lc_code_signature = u32(0x1d)
const macho_lc_dyld_chained_fixups = u32(0x80000034)
const dyld_chained_fixups_header_size = 28
const dyld_chained_ptr_start_none = u16(0xffff)
const macho_header_size = 32
const macho_page_size = 16384
const cs_page_shift = 12
const cs_page_size = 4096
const cs_hash_size = 32
const cs_super_blob_magic = u32(0xfade0cc0)
const cs_code_directory_magic = u32(0xfade0c02)
const cs_requirements_magic = u32(0xfade0c01)
const cs_blob_wrapper_magic = u32(0xfade0b01)

// fastc_codesign_shim_name is the command name TinyCC runs after linking.
const fastc_codesign_shim_name = 'codesign'

// FastcCodesignShim owns the temporary codesign shim and the PATH value that
// must be restored when its use ends.
pub struct FastcCodesignShim {
pub:
	dir string
mut:
	previous_path     string
	previous_path_set bool
}

// fastc_codesign_shim_dir prepares a directory whose `codesign` is a link to
// `/usr/bin/true` and puts it first on PATH, so TinyCC's post-link `codesign`
// call (Apple's tool takes tens of milliseconds) does nothing; the driver
// then signs the executable itself with fastc_sign_macho_adhoc.
pub fn fastc_codesign_shim_dir() FastcCodesignShim {
	if os.user_os() != 'macos' || !os.is_executable(fastc_codesign_noop_command) {
		return FastcCodesignShim{}
	}
	dir := os.join_path_single(os.vtmp_dir(), 'fastc_codesign_shim_${os.getpid()}')
	os.mkdir_all(dir) or { return FastcCodesignShim{} }
	link := os.join_path_single(dir, fastc_codesign_shim_name)
	os.rm(link) or {}
	os.symlink(fastc_codesign_noop_command, link) or {
		os.rmdir_all(dir) or {}
		return FastcCodesignShim{}
	}
	previous_path := os.getenv_opt('PATH')
	path := previous_path or { '' }
	os.setenv('PATH', dir + ':' + path, true)
	return FastcCodesignShim{
		dir:               dir
		previous_path:     path
		previous_path_set: previous_path != none
	}
}

// fastc_codesign_noop_command stands in for `codesign` while TinyCC links.
const fastc_codesign_noop_command = '/usr/bin/true'

// fastc_remove_codesign_shim_dir restores PATH and removes the shim directory.
pub fn fastc_remove_codesign_shim_dir(shim FastcCodesignShim) {
	if shim.dir != '' {
		if shim.previous_path_set {
			os.setenv('PATH', shim.previous_path, true)
		} else {
			os.unsetenv('PATH')
		}
		os.rmdir_all(shim.dir) or {}
	}
}

struct FastcMachoPatch {
mut:
	original_len int
	tail_start   int
	load_end     int
	final_len    int
	// The chained fixups that fastc_trim_chained_fixup_pages changed, if any.
	fixups_start int
	fixups_end   int
}

fn fastc_macho_sign_capacity(original_len int, ident string) int {
	code_limit := (original_len + 15) & ~15
	return code_limit + fastc_adhoc_signature_size(ident, code_limit)
}

// fastc_sign_macho_adhoc gives the 64-bit Mach-O executable at `path` an
// ad-hoc code signature (a SHA-256 code directory over 4 KiB pages, an
// empty requirements blob and an empty CMS wrapper), replacing any signature
// it has. The identifier is the file name, as `codesign -s -` uses.
pub fn fastc_sign_macho_adhoc(path string) ! {
	$if macos {
		fastc_sign_macho_adhoc_mapped(path)!
	} $else {
		fastc_sign_macho_adhoc_buffered(path)!
	}
}

fn fastc_sign_macho_adhoc_buffered(path string) ! {
	original_len := int(os.file_size(path))
	capacity := fastc_macho_sign_capacity(original_len, path.all_after_last('/'))
	mut file := []u8{len: capacity}
	mut out := os.open_file(path, 'r+')!
	read := out.read_into_ptr(file.data, original_len) or {
		out.close()
		return err
	}
	if read != original_len {
		out.close()
		return error('`${path}`: could not read the complete Mach-O file')
	}
	patch := fastc_patch_macho_signature(mut file, original_len, path) or {
		out.close()
		return err
	}
	out.write_to(0, file[..patch.load_end]) or {
		out.close()
		return err
	}
	if patch.fixups_end > patch.fixups_start {
		out.write_to(u64(patch.fixups_start), file[patch.fixups_start..patch.fixups_end]) or {
			out.close()
			return err
		}
	}
	out.write_to(u64(patch.tail_start), file[patch.tail_start..patch.final_len]) or {
		out.close()
		return err
	}
	if patch.original_len > patch.final_len {
		// A larger old signature is cut off.
		out.flush()
		if fastc_truncate_file_descriptor(i32(out.fd), u64(patch.final_len)) != 0 {
			out.close()
			return error('`${path}`: could not truncate the old signature')
		}
	}
	out.close()
}

// fastc_patch_macho_signature validates and patches a buffer whose length is
// large enough for the replacement signature. It returns the ranges that a
// buffered caller must publish; the macOS path patches a shared mapping.
fn fastc_patch_macho_signature(mut file []u8, original_len int, path string) !FastcMachoPatch {
	if original_len < macho_header_size || fastc_read_u32_le(file, 0) != macho_magic_64 {
		return error('`${path}` is not a 64-bit Mach-O file')
	}
	mut ncmds := int(fastc_read_u32_le(file, 16))
	mut sizeofcmds := int(fastc_read_u32_le(file, 20))
	mut linkedit_cmd := -1
	mut text_limit := u64(0)
	mut signature_cmd := -1
	mut fixups_cmd := -1
	mut segment_filesizes := []u64{}
	mut offset := macho_header_size
	for _ in 0 .. ncmds {
		if offset + 8 > original_len {
			return error('`${path}`: truncated load commands')
		}
		cmd := fastc_read_u32_le(file, offset)
		cmdsize := int(fastc_read_u32_le(file, offset + 4))
		if cmdsize < 8 || offset + cmdsize > original_len {
			return error('`${path}`: bad load command size')
		}
		if cmd == macho_lc_segment_64 {
			segname := fastc_c_string_at(file, offset + 8, 16)
			if segname == '__LINKEDIT' {
				linkedit_cmd = offset
			} else if segname == '__TEXT' {
				text_limit = fastc_read_u64_le(file, offset + 48) // filesize
			}
			segment_filesizes << fastc_read_u64_le(file, offset + 48)
		} else if cmd == macho_lc_code_signature {
			signature_cmd = offset
		} else if cmd == macho_lc_dyld_chained_fixups && cmdsize >= 16 {
			fixups_cmd = offset
		}
		offset += cmdsize
	}
	if linkedit_cmd < 0 {
		return error('`${path}`: no __LINKEDIT segment')
	}
	// The fixups are hashed too, so they are settled before the signature.
	fixups_start, fixups_end := if fixups_cmd >= 0 {
		fastc_trim_chained_fixup_pages(mut file, original_len, fixups_cmd, segment_filesizes)
	} else {
		0, 0
	}
	// The signed range ends where the signature starts: at the old signature
	// when there is one, else at the (16-byte padded) end of the file.
	mut code_limit := original_len
	if signature_cmd >= 0 {
		code_limit = int(fastc_read_u32_le(file, signature_cmd + 8))
		if code_limit > original_len {
			return error('`${path}`: signature past the end of the file')
		}
	} else {
		load_end := macho_header_size + sizeofcmds
		if load_end + 16 > original_len {
			return error('`${path}`: no room for a signature load command')
		}
		for k in 0 .. 16 {
			if file[load_end + k] != 0 {
				return error('`${path}`: no room for a signature load command')
			}
		}
		signature_cmd = load_end
		ncmds++
		sizeofcmds += 16
		fastc_write_u32_le(mut file, 16, u32(ncmds))
		fastc_write_u32_le(mut file, 20, u32(sizeofcmds))
		fastc_write_u32_le(mut file, signature_cmd, macho_lc_code_signature)
		fastc_write_u32_le(mut file, signature_cmd + 4, 16)
	}
	// Only the load commands and the tail from here (padding and signature)
	// change; they are patched into the file rather than rewriting it all.
	tail_start := code_limit
	for code_limit % 16 != 0 {
		file[code_limit] = 0
		code_limit++
	}
	ident := path.all_after_last('/')
	signature_size := fastc_adhoc_signature_size(ident, code_limit)
	// The header and __LINKEDIT sizes are part of the hashed pages, so they
	// are settled before hashing.
	fastc_write_u32_le(mut file, signature_cmd + 8, u32(code_limit))
	fastc_write_u32_le(mut file, signature_cmd + 12, u32(signature_size))
	linkedit_fileoff := fastc_read_u64_le(file, linkedit_cmd + 40)
	linkedit_filesize := u64(code_limit + signature_size) - linkedit_fileoff
	linkedit_vmsize := (linkedit_filesize + u64(macho_page_size) - 1) / u64(macho_page_size) * u64(macho_page_size)
	fastc_write_u64_le(mut file, linkedit_cmd + 32, linkedit_vmsize)
	fastc_write_u64_le(mut file, linkedit_cmd + 48, linkedit_filesize)
	signature := fastc_adhoc_signature(file, code_limit, ident, text_limit)
	if signature.len != signature_size {
		return error('`${path}`: signature size mismatch')
	}
	unsafe { vmemcpy(&u8(file.data) + code_limit, signature.data, signature.len) }
	load_end := macho_header_size + sizeofcmds
	return FastcMachoPatch{
		original_len: original_len
		tail_start:   tail_start
		load_end:     load_end
		final_len:    code_limit + signature.len
		fixups_start: fixups_start
		fixups_end:   fixups_end
	}
}

// fastc_trim_chained_fixup_pages makes the chained fixups of each segment list
// only the pages that the file backs, as ld64 does. TinyCC lists the zero-fill
// `__bss` pages at the end of `__DATA` too, and the dyld of macOS 27 maps every
// listed page from the file, so those globals start out with the bytes of
// `__LINKEDIT` instead of zeros (https://github.com/vlang/v/issues/29744).
// `fixups_cmd` is the offset of the LC_DYLD_CHAINED_FIXUPS command, and
// `segment_filesizes` has the file size of each segment, in load command order.
// Pages with fixups stay listed. It returns the range of `file` that changed;
// an empty one when nothing did.
fn fastc_trim_chained_fixup_pages(mut file []u8, file_len int, fixups_cmd int, segment_filesizes []u64) (int, int) {
	data_start := int(fastc_read_u32_le(file, fixups_cmd + 8))
	data_size := int(fastc_read_u32_le(file, fixups_cmd + 12))
	if data_start < 0 || data_size < dyld_chained_fixups_header_size
		|| data_start > file_len - data_size {
		return 0, 0
	}
	data_end := data_start + data_size
	starts_offset := int(fastc_read_u32_le(file, data_start + 4))
	image := data_start + starts_offset
	if starts_offset < dyld_chained_fixups_header_size || image > data_end - 4 {
		return 0, 0
	}
	seg_count := int(fastc_read_u32_le(file, image))
	if seg_count < 0 || seg_count > segment_filesizes.len || image + 4 + seg_count * 4 > data_end {
		return 0, 0
	}
	mut changed_start := data_end
	mut changed_end := data_start
	for i in 0 .. seg_count {
		info_field := image + 4 + i * 4
		info_offset := int(fastc_read_u32_le(file, info_field))
		// dyld_chained_starts_in_segment: size u32, page_size u16, pointer_format
		// u16, segment_offset u64, max_valid_pointer u32, page_count u16,
		// page_start [page_count]u16.
		segment := image + info_offset
		if info_offset <= 0 || segment > data_end - 22 {
			continue
		}
		page_size := u64(fastc_read_u16_le(file, segment + 4))
		page_count := int(fastc_read_u16_le(file, segment + 20))
		if page_size == 0 || segment + 22 + page_count * 2 > data_end {
			continue
		}
		backed := int((segment_filesizes[i] + page_size - 1) / page_size)
		if backed >= page_count {
			continue
		}
		mut zero_fill_has_fixups := false
		for page in backed .. page_count {
			if fastc_read_u16_le(file, segment + 22 + page * 2) != dyld_chained_ptr_start_none {
				zero_fill_has_fixups = true
				break
			}
		}
		if zero_fill_has_fixups {
			continue
		}
		mut field := segment + 20
		mut field_end := field + 2
		if backed == 0 {
			// No page is backed by the file, so none has fixups: offset 0 says so.
			field = info_field
			field_end = info_field + 4
			fastc_write_u32_le(mut file, info_field, 0)
		} else {
			fastc_write_u16_le(mut file, field, u16(backed))
		}
		if field < changed_start {
			changed_start = field
		}
		if field_end > changed_end {
			changed_end = field_end
		}
	}
	if changed_end <= changed_start {
		return 0, 0
	}
	return changed_start, changed_end
}

fn fastc_adhoc_signature_size(ident string, code_limit int) int {
	n_pages := (code_limit + cs_page_size - 1) / cs_page_size
	cd_size := 88 + ident.len + 1 + 2 * cs_hash_size + n_pages * cs_hash_size
	cd_size_aligned := (cd_size + 3) & ~3
	return 12 + 24 + cd_size_aligned + 12 + 8
}

// fastc_adhoc_signature builds the embedded signature super blob over the
// first `code_limit` bytes of `file`.
fn fastc_adhoc_signature(file []u8, code_limit int, ident string, text_limit u64) []u8 {
	n_pages := (code_limit + cs_page_size - 1) / cs_page_size
	ident_len := ident.len + 1
	n_special_slots := 2
	cd_header_size := 88
	ident_offset := cd_header_size
	hash_offset := ident_offset + ident_len + n_special_slots * cs_hash_size
	cd_size := hash_offset + n_pages * cs_hash_size
	cd_size_aligned := (cd_size + 3) & ~3
	req_size := 12
	cms_size := 8
	cd_blob_offset := 12 + 24
	req_blob_offset := cd_blob_offset + cd_size_aligned
	cms_blob_offset := req_blob_offset + req_size
	total_size := cms_blob_offset + cms_size
	mut sig := []u8{cap: total_size}
	fastc_push_u32_be(mut sig, cs_super_blob_magic)
	fastc_push_u32_be(mut sig, u32(total_size))
	fastc_push_u32_be(mut sig, 3)
	fastc_push_u32_be(mut sig, 0) // CSSLOT_CODEDIRECTORY
	fastc_push_u32_be(mut sig, u32(cd_blob_offset))
	fastc_push_u32_be(mut sig, 2) // CSSLOT_REQUIREMENTS
	fastc_push_u32_be(mut sig, u32(req_blob_offset))
	fastc_push_u32_be(mut sig, 0x10000) // CSSLOT_SIGNATURESLOT
	fastc_push_u32_be(mut sig, u32(cms_blob_offset))
	fastc_push_u32_be(mut sig, cs_code_directory_magic)
	fastc_push_u32_be(mut sig, u32(cd_size))
	fastc_push_u32_be(mut sig, 0x20400) // version
	fastc_push_u32_be(mut sig, 0x2) // flags: CS_ADHOC
	fastc_push_u32_be(mut sig, u32(hash_offset))
	fastc_push_u32_be(mut sig, u32(ident_offset))
	fastc_push_u32_be(mut sig, u32(n_special_slots))
	fastc_push_u32_be(mut sig, u32(n_pages))
	fastc_push_u32_be(mut sig, u32(code_limit))
	sig << u8(cs_hash_size)
	sig << u8(2) // CS_HASHTYPE_SHA256
	sig << u8(0) // platform
	sig << u8(cs_page_shift)
	fastc_push_u32_be(mut sig, 0) // spare2
	fastc_push_u32_be(mut sig, 0) // scatterOffset
	fastc_push_u32_be(mut sig, 0) // teamOffset
	fastc_push_u32_be(mut sig, 0) // spare3
	fastc_push_u64_be(mut sig, 0) // codeLimit64
	fastc_push_u64_be(mut sig, 0) // execSegBase
	fastc_push_u64_be(mut sig, text_limit) // execSegLimit
	fastc_push_u64_be(mut sig, 1) // execSegFlags: CS_EXECSEG_MAIN_BINARY
	for b in ident.bytes() {
		sig << b
	}
	sig << u8(0)
	// Special slot -2: the requirements blob's hash; slot -1 (Info.plist): none.
	mut req_blob := []u8{cap: req_size}
	fastc_push_u32_be(mut req_blob, cs_requirements_magic)
	fastc_push_u32_be(mut req_blob, u32(req_size))
	fastc_push_u32_be(mut req_blob, 0)
	sig << sha256.sum256(req_blob)
	for _ in 0 .. cs_hash_size {
		sig << u8(0)
	}
	// The page hashes: the system digest on macOS (where the signature is
	// used); elsewhere V's SHA-256, which a self-hosted (TinyCC-built)
	// compiler runs at a few tens of MB/s, so the pages are split over
	// worker threads.
	mut page_hashes := []u8{len: n_pages * cs_hash_size}
	data_ptr := unsafe { &u8(file.data) }
	hashes_ptr := unsafe { &u8(page_hashes.data) }
	$if macos {
		fastc_hash_code_pages_native(data_ptr, hashes_ptr, n_pages, code_limit)
	} $else {
		if n_pages >= 64 {
			worker_count := 8
			mut workers := [
				spawn fastc_hash_code_pages(data_ptr, hashes_ptr, 0, n_pages / worker_count, code_limit),
			]
			for worker in 1 .. worker_count {
				start := n_pages * worker / worker_count
				end := n_pages * (worker + 1) / worker_count
				workers << spawn fastc_hash_code_pages(data_ptr, hashes_ptr, start, end, code_limit)
			}
			for worker in workers {
				worker.wait()
			}
		} else {
			fastc_hash_code_pages(data_ptr, hashes_ptr, 0, n_pages, code_limit)
		}
	}
	sig << page_hashes
	for sig.len < req_blob_offset {
		sig << u8(0)
	}
	sig << req_blob
	fastc_push_u32_be(mut sig, cs_blob_wrapper_magic)
	fastc_push_u32_be(mut sig, u32(cms_size))
	return sig
}

// fastc_hash_code_pages writes the SHA-256 of the pages [page_start, page_end)
// of the `code_limit` bytes at `data` into `hashes` (32 bytes per page).
fn fastc_hash_code_pages(data &u8, hashes &u8, page_start int, page_end int, code_limit int) bool {
	for page in page_start .. page_end {
		start := page * cs_page_size
		mut end := start + cs_page_size
		if end > code_limit {
			end = code_limit
		}
		page_bytes := unsafe { (data + start).vbytes(end - start) }
		digest := sha256.sum256(page_bytes)
		unsafe { vmemcpy(hashes + page * cs_hash_size, digest.data, cs_hash_size) }
	}
	return true
}

@[direct_array_access]
fn fastc_read_u16_le(data []u8, offset int) u16 {
	return u16(data[offset]) | (u16(data[offset + 1]) << 8)
}

@[direct_array_access]
fn fastc_write_u16_le(mut data []u8, offset int, value u16) {
	data[offset] = u8(value)
	data[offset + 1] = u8(value >> 8)
}

@[direct_array_access]
fn fastc_read_u32_le(data []u8, offset int) u32 {
	return u32(data[offset]) | (u32(data[offset + 1]) << 8) | (u32(data[offset + 2]) << 16) | (u32(data[offset + 3]) << 24)
}

@[direct_array_access]
fn fastc_read_u64_le(data []u8, offset int) u64 {
	return u64(fastc_read_u32_le(data, offset)) | (u64(fastc_read_u32_le(data, offset + 4)) << 32)
}

@[direct_array_access]
fn fastc_write_u32_le(mut data []u8, offset int, value u32) {
	data[offset] = u8(value)
	data[offset + 1] = u8(value >> 8)
	data[offset + 2] = u8(value >> 16)
	data[offset + 3] = u8(value >> 24)
}

fn fastc_write_u64_le(mut data []u8, offset int, value u64) {
	fastc_write_u32_le(mut data, offset, u32(value))
	fastc_write_u32_le(mut data, offset + 4, u32(value >> 32))
}

fn fastc_push_u32_be(mut data []u8, value u32) {
	data << u8(value >> 24)
	data << u8(value >> 16)
	data << u8(value >> 8)
	data << u8(value)
}

fn fastc_push_u64_be(mut data []u8, value u64) {
	fastc_push_u32_be(mut data, u32(value >> 32))
	fastc_push_u32_be(mut data, u32(value))
}

@[direct_array_access]
fn fastc_c_string_at(data []u8, offset int, max int) string {
	mut end := offset
	for end < offset + max && end < data.len && data[end] != 0 {
		end++
	}
	return data[offset..end].bytestr()
}
