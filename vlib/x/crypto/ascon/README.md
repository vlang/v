# ascon

`ascon` is an implementation of Ascon-Based Cryptography module implemented in pure V language.
This module was mostly based on NIST Special Publication of 800 NIST SP 800-232 document.
Its describes an Ascon-Based Lightweight Cryptography Standards for Constrained Devices 
thats provides Authenticated Encryption, Hash, and Extendable Output Functions.
See the [NIST.SP.800-232 Standard](https://doi.org/10.6028/NIST.SP.800-232) for more detail.

This module mostly implements all the features availables on the document.
Its currently implements:
- `Ascon-Hash256`, Ascon-based hashing implementation that produces 256-bits output.
- `Ascon-XOF128`, Ascon-based eXtendable Output Function (XOF) where the output size of 
the hash of the message can be selected by the user.
- `Ascon-CXOF128`, a customized XOF that allows users to specify a customization
string and choose the output size of the message hash.
- `Ascon-AEAD128`, an Authenticated Encryption with Additional Data (AEAD) Scheme based
on Ascon-family crypto.
