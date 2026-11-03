## Description

`szip` reads and writes ZIP archives in V, using stored or DEFLATE-compressed entries,
including ZIP64 archives. Multi-disk and encrypted archives are unsupported and return an
error. Reading validates entry bounds, uncompressed sizes and CRC-32 checksums.
New entry names are written as UTF-8 with the ZIP language encoding flag. Names stored
without the flag are decoded as UTF-8 when they are valid UTF-8, and as code page 437
otherwise. Appending preserves the encoding flag and raw name bytes of existing entries.

ZIP64 records are written when an archive holds 65535 entries or more, or when a size or
offset does not fit in 32 bits, and read whenever an archive has them. An entry being
written is held in memory until it is closed; once closed it is in the archive being
written and can no longer be read back through the same `Zip`. In append mode, the
existing entries stay readable through `open_entry_by_index`, since `open_entry` adds an
entry.

Entries keep a modification time and Unix permissions. `create_entry`, `zip_files` and
`zip_folder` take them from the source files, other written entries get the current time
and mode 0644 (0755 for directories). They are stored in the MS-DOS fields and in an
extended timestamp, read back by `modified()` and `unix_mode()`, and applied to the files
that `extract_zip_to_dir` creates. Only permission bits are restored: set-user-ID,
set-group-ID and sticky bits are dropped.

An archive opened for writing or appending is written to a file in a private temporary
directory created next to it, and replaces the archive, keeping its permissions, when it is
closed. Until then the previous archive is left untouched; an archive that is never closed
leaves that directory behind.

Opening an archive for reading only reads its central directory. Each entry is read,
decompressed and checked when it is asked for, so damage to an entry's local header or
data is reported by that read rather than by `open`. `open_lazy(name)` is the same as
`open(name, .no_compression, .read_only)`. `read_entry_bytes` returns the checked bytes
of the current entry, without sizing a buffer from the central directory first.

Trailing bytes after the end record, as left by an interrupted download, are tolerated when
a plausible end record is found near the end of the file. This fallback ignores an empty end
record stored inside the archive.

Written entries are compressed once, when they are closed, at the level given to `open`:
`.best_speed` is DEFLATE level 1, `.default_compression` level 6 and `.best_compression`
level 9. An entry that deflate does not make smaller is stored.

## Reading untrusted archives

An archive states how many entries it holds and how much each one expands to. Reading
applies the bounds of `Limits`: the entry count and header sizes when the archive is opened,
the compressed and uncompressed entry sizes, expansion ratio and total size read when an
entry is read. Decompression
stops as soon as an entry passes its declared size. A bound that is hit returns a
`LimitExceeded` error, and the other entries remain readable. `open_with_limits` raises or
lowers the bounds:

```v
import compress.szip

fn main() {
	mut zp := szip.open_with_limits('archive.zip', .no_compression, .read_only,
		max_total_size: 64 * 1024 * 1024
		max_ratio:      100.0
	) or { return }
	zp.close()
}
```

Opening an entry by name in read-only mode returns an error when that entry is missing.
`extract_zip_to_dir` rejects paths outside the destination and paths containing symbolic links.
Extraction and the `zip_files` and `zip_folder` helpers propagate filesystem errors.
The entry API retains its non-failing `close()` signature; use the helpers when write errors
must be reported.
