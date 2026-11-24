#ifndef V_C_UNION_TAG_SDK_H
#define V_C_UNION_TAG_SDK_H

#include <stdint.h>
#include <stddef.h>

/* V exposes only selected fields. The native header owns the full layout. */
typedef struct {
    uint64_t hidden;
    uint64_t value;
    uint32_t code;
} sdk_payload_t;

union __ReplyUnion__demo_subsystem {
    sdk_payload_t Reply;
    uint64_t wide;
    unsigned char hidden_storage[56];
};
union lowercase_reply {
    uint64_t word;
    unsigned char bytes[8];
};
union SdkReply {
    uint64_t word;
    unsigned char bytes[8];
};
typedef union {
    uint64_t word;
    unsigned char hidden_storage[24];
} __ReplyAlias_t;
struct __TaggedRecord {
    uint64_t value;
};

static inline size_t sdk_reply_size(void) {
    return sizeof(union __ReplyUnion__demo_subsystem);
}
static inline size_t sdk_payload_offset(void) {
    return offsetof(sdk_payload_t, value);
}
static inline union __ReplyUnion__demo_subsystem sdk_make_reply(uint64_t value) {
    union __ReplyUnion__demo_subsystem result = {0};
    result.Reply.value = value;
    result.Reply.code = 9;
    return result;
}
static inline uint64_t sdk_read_reply(union __ReplyUnion__demo_subsystem reply) {
    return reply.Reply.value + reply.Reply.code;
}
static inline void sdk_update_reply(union __ReplyUnion__demo_subsystem *reply, uint64_t value) {
    reply->Reply.value = value;
}

#endif
