#ifndef V_TEST_C_ESCAPED_FIELDS_H
#define V_TEST_C_ESCAPED_FIELDS_H

typedef union EscapedFieldEvent {
    unsigned int type;
    unsigned char padding[16];
} EscapedFieldEvent;

typedef struct EscapedFieldRecord {
    unsigned int type;
    unsigned int module;
    unsigned char bytes[4];
} EscapedFieldRecord;

#endif
