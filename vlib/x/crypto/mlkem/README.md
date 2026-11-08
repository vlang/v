# mlkem

Experimental module for ML-KEM (FIPS 203) post-quantum key encapsulation.
Currently a scaffold: API defined, implementation pending.

Planned: ML-KEM-768 (and 512/1024) KeyGen/Encaps/Decaps, NIST ACVP vectors.
Used by `net.tls` for hybrid `X25519MLKEM768` key exchange.