## Description

`crypto.pbkdf2` derives a key from a password, salt, iteration count, and HMAC hash.

`key` requires a positive `key_length` in bytes. Zero or negative key lengths return an error.
