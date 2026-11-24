/* Exercise a nonzero PT_TLS first-byte offset with real linker relocations. */
SECTIONS {
    .tls_test_prefix : { BYTE(0); . = ALIGN(64); . += 32; }
    .tdata.test ALIGN(8) : { *(.tdata) *(.tdata.*) }
    .tbss.test ALIGN(64) : { *(.tbss) *(.tbss.*) }
}
INSERT AFTER .rodata;
