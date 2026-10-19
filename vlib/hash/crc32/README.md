## CRC-32 variants

`sum_crc32q` and `sum_with_poly(crc32.crc32q, data)` calculate CRC-32Q, also called
CRC-32/AIXM. This algorithm uses polynomial `0x814141ab`, zero initial state, zero final
XOR, and non-reflected input and output. Its check value for `123456789` is `0x3010bf7f`.

The `crc32q_reflected` polynomial selects a separate reflected checksum with all-one initial
state and final XOR. Reflecting a polynomial alone does not preserve an algorithm's parameters.
See the [CRC catalogue](https://reveng.sourceforge.io/crc-catalogue/17plus.htm#crc.cat.crc-32-aixm)
and the EUROCONTROL AIXM Primer 4.5, section 6.2.