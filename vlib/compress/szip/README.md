## Description

`szip` reads and writes ZIP archives in V, using stored or DEFLATE-compressed entries.
ZIP64, multi-disk and encrypted archives are unsupported and return an error.
Reading validates entry bounds, uncompressed sizes and CRC-32 checksums.
New entry names are written as UTF-8 with the ZIP language encoding flag.
Appending preserves the encoding flag and raw name bytes of existing entries.

Opening an archive for reading only reads its central directory. Each entry is read,
decompressed and checked when it is asked for, so damage to an entry's local header or
data is reported by that read rather than by `open`.

Written entries are compressed once, when they are closed, at the level given to `open`:
`.best_speed` is DEFLATE level 1, `.default_compression` level 6 and `.best_compression`
level 9. An entry that deflate does not make smaller is stored.

## Reading untrusted archives

An archive states how many entries it holds and how much each one expands to. Reading
applies the bounds of `Limits`: the entry count and header sizes when the archive is opened,
the entry size, expansion ratio and total size read when an entry is read. Decompression
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
