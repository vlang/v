#include "kqueue.h"
#include <stddef.h>

size_t v_notify_kevent_size(void) { return sizeof(struct kevent); }
size_t v_notify_kevent_filter_offset(void) { return offsetof(struct kevent, filter); }
size_t v_notify_kevent_data_offset(void) { return offsetof(struct kevent, data); }
size_t v_notify_kevent_udata_offset(void) { return offsetof(struct kevent, udata); }
