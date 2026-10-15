#ifndef VPM_REGISTRY_LISTENER_H
#define VPM_REGISTRY_LISTENER_H

#include <winsock2.h>

static inline int vpm_registry_prepare_listener(int handle) {
    SOCKET socket = (SOCKET)handle;
    int disabled = 0;
    int enabled = 1;
    /* new_tcp_socket enables reuse by default; it conflicts with exclusivity. */
    if (setsockopt(socket, SOL_SOCKET, SO_REUSEADDR,
                   (const char *)&disabled, sizeof(disabled)) == SOCKET_ERROR) return SOCKET_ERROR;
    return setsockopt(socket, SOL_SOCKET, SO_EXCLUSIVEADDRUSE,
                      (const char *)&enabled, sizeof(enabled));
}

static inline int vpm_registry_listen(int handle) {
    return listen((SOCKET)handle, 128);
}

#endif
