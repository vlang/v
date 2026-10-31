`net.mail` builds and parses MIME messages for use with transports such as `net.smtp`. Create a
message with `new_message()`, set `from`, recipients, `subject`, and text/HTML bodies, then
attach files with `add_attachment(filename, data, mime_type)` and call `encode()`. Encoding uses
CRLF endings, multipart/alternative for text plus HTML, and multipart/mixed when attachments are
present, nesting the text/HTML alternatives inside it. Every attachment uses base64, including
text attachments. Filenames are quoted and escaped. Header CR/LF/NUL characters, invalid
boundaries, and boundary lines present in text bodies return errors. Use `set_boundary()` to
choose another delimiter.

`parse(raw)` supports empty or folded headers, nested multiparts, text and HTML bodies, and
base64, 7bit, 8bit, or binary attachments. Delimiters match complete lines; preamble and
epilogue are ignored. Attachment bytes are preserved. Body text is normalized to LF; a single
final line ending on a plain single-part body is omitted. Multipart body text and decoded base64
text retain their payload line endings after normalization. Missing closing delimiters,
malformed base64, unsupported transfer encodings, and nesting beyond 32 levels return errors.
Quoted-printable and RFC 2047 encoded words are not decoded; header values pass through
unchanged. Recipient lists use comma-separated addresses and do not implement quoted
display-name address parsing.
