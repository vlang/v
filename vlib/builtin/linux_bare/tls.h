#ifndef V_LINUX_BARE_TLS_H
#define V_LINUX_BARE_TLS_H

/* GNU-compatible ELF linkers provide the runtime address of the image header. */
extern unsigned char __ehdr_start;

void *v_bare_loader_tls_allocate(void **thread_pointer, __SIZE_TYPE__ *mapping_size);
#endif
