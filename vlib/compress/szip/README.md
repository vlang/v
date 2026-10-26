## Description

`szip` reads and writes ZIP archives in V, using stored or DEFLATE-compressed entries.
ZIP64, multi-disk and encrypted archives are unsupported and return an error.
Reading validates entry bounds, uncompressed sizes and CRC-32 checksums.

Opening an entry by name in read-only mode returns an error when that entry is missing.
`extract_zip_to_dir` rejects paths outside the destination and paths containing symbolic links.
Extraction and the `zip_files` and `zip_folder` helpers propagate filesystem errors.
The entry API retains its non-failing `close()` signature; use the helpers when write errors
must be reported.
