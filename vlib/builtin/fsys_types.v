module builtin

// fsys_types.v declares the normalized platform-agnostic result types of the
// uniform freestanding os-syscall surface (`bf_*`, one bf.v per *_bare
// directory). Declarations only; compiled on hosted builds as well, where the
// type declarations are inert because no bf_* implementation is included.

// FSysStat carries the fields of a successful file-stat, normalized to terms
// os.Stat uses. Errno is normalized to the Linux numbers at the bf_ boundary.
pub struct FSysStat {
pub:
	dev   u64
	ino   u64
	nlink u64
	mode  u32
	uid   u32
	gid   u32
	rdev  u64
	size  u64
	atime i64 // seconds since UNIX epoch
	mtime i64
	ctime i64
}
