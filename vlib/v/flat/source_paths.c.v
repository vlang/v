module flat

// The dirent entry types for an unreported type and a symbolic link. Their values
// are the same on every POSIX system, while some headers hide the macros.
const dirent_type_unknown = u8(0)
const dirent_type_symlink = u8(10)

// scan_listed_symlinks lists `dir` once and returns the names of its entries that
// are symbolic links, from the entry types the listing reports. The result is not
// known when the directory cannot be read, the file system does not report an
// entry's type, or the platform's `struct dirent` has no `d_type` member (Haiku,
// Solaris and illumos, for example); record_listed_source_path_in then asks
// lstat about each entry.
pub fn scan_listed_symlinks(dir string) ListedSymlinks {
	$if !(linux || android || termux || macos || darwin || ios || freebsd || openbsd || netbsd || dragonfly) {
		return ListedSymlinks{}
	} $else {
		dir_ptr := C.opendir(&char(dir.str))
		if isnil(dir_ptr) {
			return ListedSymlinks{}
		}
		mut names := map[string]bool{}
		mut known := true
		for {
			ent := C.readdir(dir_ptr)
			if isnil(ent) {
				break
			}
			if ent.d_type == dirent_type_symlink {
				names[unsafe { tos_clone(&u8(&ent.d_name[0])) }] = true
			} else if ent.d_type == dirent_type_unknown {
				known = false
				break
			}
		}
		C.closedir(dir_ptr)
		return ListedSymlinks{
			known: known
			names: names
		}
	}
}
