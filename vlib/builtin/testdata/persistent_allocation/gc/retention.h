#ifndef V_PERSISTENT_RETENTION_H
#define V_PERSISTENT_RETENTION_H
int persistent_payload_create(void *parent);
int persistent_payload_collect(void *parent, int clear);
#endif
