#ifndef V_EXPORT_CONST_ARGUMENTS_SDK_H
#define V_EXPORT_CONST_ARGUMENTS_SDK_H

#include <stdint.h>
#include <stddef.h>

typedef struct { uint64_t value; } SDKRequest;
uint64_t sdk_read(const SDKRequest *request);
uint64_t sdk_bytes(const void *data, size_t count);
size_t sdk_text(const char *text);
void sdk_store(uint32_t *target, const uint32_t *source);
uint64_t sdk_scalar(uint64_t value);

static inline int sdk_verify(void) {
    static const SDKRequest request = {37};
    static const unsigned char bytes[] = {0, 127, 255};
    static const char text[] = "readonly SDK data";
    static const uint32_t source = 91;
    uint32_t target = 0;
    uint64_t (*volatile reader)(const SDKRequest *) = sdk_read;
    uint64_t (*volatile byte_reader)(const void *, size_t) = sdk_bytes;
    size_t (*volatile text_reader)(const char *) = sdk_text;
    void (*volatile store)(uint32_t *, const uint32_t *) = sdk_store;
    if (reader(&request) != 37) return 1;
    if (byte_reader(bytes, sizeof(bytes)) != 382) return 2;
    if (text_reader(text) != sizeof(text) - 1) return 3;
    store(&target, &source);
    if (target != 91 || source != 91) return 4;
    if (sdk_scalar(100) != 101) return 5;
    return 0;
}
#endif
