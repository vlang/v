## Description

`crypto.sha3` implements SHA-3, legacy Keccak, and extended-output digests.

Incremental digests accept bytes with `write(data)`. Calling `checksum()` finalizes the current
message and resets the digest, so a later `write()` starts a new independent message.
Calling `reset()` explicitly discards pending input while retaining the digest variant and
output size. A checksum without a subsequent write is the checksum of the empty message.
