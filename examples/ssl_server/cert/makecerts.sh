# generates the certificates used by the server_sni_advanced.v example

# default
gen_key type=rsa rsa_keysize=4096 filename=0x.dk.key
cert_write selfsign=1 issuer_key=0x.dk.key                    \
                         issuer_name=CN=0x.dk,O=0x,C=DK        \
                         not_before=20130101000000 not_after=20251231235959   \
                         is_ca=1 max_pathlen=0 output_file=0x.dk.crt

# 1x.dk
gen_key type=rsa rsa_keysize=4096 filename=1x.dk.key
cert_write selfsign=1 issuer_key=1x.dk.key                    \
                         issuer_name=CN=1x.dk,O=1x.dk,C=DK        \
                         not_before=20130101000000 not_after=20251231235959   \
                         is_ca=1 max_pathlen=0 output_file=1x.dk.crt

# 2x.dk
gen_key type=rsa rsa_keysize=4096 filename=2x.dk.key
cert_write selfsign=1 issuer_key=2x.dk.key                    \
                         issuer_name=CN=2x.dk,O=2x.dk,C=DK        \
                         not_before=20130101000000 not_after=20251231235959   \
                         is_ca=1 max_pathlen=0 output_file=2x.dk.crt
