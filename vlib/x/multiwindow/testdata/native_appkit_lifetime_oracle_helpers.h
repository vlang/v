#ifndef V_MULTIWINDOW_NATIVE_APPKIT_LIFETIME_ORACLE_HELPERS_H
#define V_MULTIWINDOW_NATIVE_APPKIT_LIFETIME_ORACLE_HELPERS_H

#include <stdint.h>

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)

#define V_MULTIWINDOW_TEST_APPKIT_ORACLE_CAPACITY 512

enum VMultiwindowTestAppKitOracleKind {
	V_MULTIWINDOW_TEST_APPKIT_DEVICE_CREATE = 1,
	V_MULTIWINDOW_TEST_APPKIT_DEVICE_RELEASE = 2,
	V_MULTIWINDOW_TEST_APPKIT_WINDOW_CREATE = 3,
	V_MULTIWINDOW_TEST_APPKIT_WINDOW_CONFIGURE = 4,
	V_MULTIWINDOW_TEST_APPKIT_WINDOW_DESTROY = 5,
	V_MULTIWINDOW_TEST_APPKIT_WINDOW_RELEASE = 6,
	V_MULTIWINDOW_TEST_APPKIT_ANCHOR_CREATE = 7,
	V_MULTIWINDOW_TEST_APPKIT_ANCHOR_DESTROY = 8,
	V_MULTIWINDOW_TEST_APPKIT_DRAWABLE_ACQUIRE = 9,
	V_MULTIWINDOW_TEST_APPKIT_DRAWABLE_PRESENT = 10,
	V_MULTIWINDOW_TEST_APPKIT_DRAWABLE_ABORT = 11,
	V_MULTIWINDOW_TEST_APPKIT_DRAWABLE_RELEASE = 12,
	V_MULTIWINDOW_TEST_APPKIT_POOL_PUSH = 13,
	V_MULTIWINDOW_TEST_APPKIT_POOL_POP = 14
};

typedef struct VMultiwindowTestAppKitOracleRecord {
	uint64_t sequence;
	uint64_t kind;
	uint64_t identity;
	uint64_t parent_identity;
	uint64_t output_identity;
	uint64_t auxiliary_identity;
	uint64_t auxiliary_identity_1;
	uint64_t auxiliary_identity_2;
	uint64_t valid_mask;
	uint64_t thread_identity;
} VMultiwindowTestAppKitOracleRecord;

void v_multiwindow_test_appkit_oracle_reset(void);
uint64_t v_multiwindow_test_appkit_oracle_count_get(void);
int v_multiwindow_test_appkit_oracle_overflow_get(void);
VMultiwindowTestAppKitOracleRecord
v_multiwindow_test_appkit_oracle_record_get(uint64_t index);

#endif

#endif
