# scrypt

`scrypt.scrypt` derives a key of exactly `dk_len` bytes from a password and salt.
`r`, `p` and `dk_len` must be positive; `n` must be a nonzero power of two.

Buffer sizes are computed in `u64` and checked before allocation or conversion to `int`.
The derived key, parallel buffer, scratch buffer and work buffer must each fit V array
lengths, including SHA-256 block rounding in PBKDF2. Oversized parameters return an error
instead of wrapping a buffer length. These implementation limits are stricter than the
RFC maximum exposed by `max_buffer_length`.
