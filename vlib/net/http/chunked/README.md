# Chunked HTTP bodies

`chunked.decode` decodes the body of a chunked HTTP message. It accepts hexadecimal chunk sizes,
ignores chunk extensions, and returns an error for missing sizes, overflow, truncated chunks, or
invalid CRLF separators. Trailer fields are excluded from the decoded body.
