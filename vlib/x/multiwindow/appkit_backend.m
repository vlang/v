#include <Cocoa/Cocoa.h>
#include <IOKit/hidsystem/IOLLEvent.h>
#include <Metal/Metal.h>
#include <QuartzCore/CAMetalLayer.h>
#include <math.h>
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
#include <pthread.h>
#endif
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "appkit_backend_helpers.h"
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
#include "testdata/native_appkit_lifetime_oracle_helpers.h"
#define V_MULTIWINDOW_TEST_APPKIT_PROCESS_COMMIT UINT64_C(15)
#endif

extern void *objc_autoreleasePoolPush(void);
extern void objc_autoreleasePoolPop(void *pool);

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
typedef struct VMultiwindowAppKitObservedRoot {
	uint64_t identity;
	uint64_t subject;
} VMultiwindowAppKitObservedRoot;

static VMultiwindowAppKitSideEffectRecord
	v_multiwindow_appkit_side_effect_records[V_MULTIWINDOW_APPKIT_SIDE_EFFECT_CAPACITY];
static VMultiwindowAppKitObservedRoot
	v_multiwindow_appkit_observed_roots[V_MULTIWINDOW_APPKIT_SIDE_EFFECT_CAPACITY];
static uint64_t v_multiwindow_appkit_side_effect_generation_counter;
static uint64_t v_multiwindow_appkit_side_effect_active_generation;
static uint64_t v_multiwindow_appkit_side_effect_record_count;
static int v_multiwindow_appkit_side_effect_overflow_flag;

static void v_multiwindow_appkit_side_effect_append_generation(uint64_t generation,
		uint64_t kind, uint64_t subject, uint64_t identity,
		uint64_t parent_identity, uint64_t before_identity,
		uint64_t after_identity, uint64_t auxiliary_identity) {
	if (generation == 0) {
		return;
	}
	if (v_multiwindow_appkit_side_effect_record_count
			>= V_MULTIWINDOW_APPKIT_SIDE_EFFECT_CAPACITY) {
		v_multiwindow_appkit_side_effect_overflow_flag = 1;
		return;
	}
	VMultiwindowAppKitSideEffectRecord *record =
		&v_multiwindow_appkit_side_effect_records[v_multiwindow_appkit_side_effect_record_count];
	v_multiwindow_appkit_side_effect_record_count++;
	record->generation = generation;
	record->sequence = v_multiwindow_appkit_side_effect_record_count;
	record->kind = kind;
	record->subject = subject;
	record->identity = identity;
	record->parent_identity = parent_identity;
	record->before_identity = before_identity;
	record->after_identity = after_identity;
	record->auxiliary_identity = auxiliary_identity;
	record->thread_identity = (uint64_t)(uintptr_t)pthread_self();
	record->main_thread = [NSThread isMainThread] ? UINT64_C(1) : UINT64_C(0);
}

static void v_multiwindow_appkit_side_effect_append(uint64_t kind,
		uint64_t subject, uint64_t identity, uint64_t parent_identity,
		uint64_t before_identity, uint64_t after_identity,
		uint64_t auxiliary_identity) {
	v_multiwindow_appkit_side_effect_append_generation(
		v_multiwindow_appkit_side_effect_active_generation, kind, subject,
		identity, parent_identity, before_identity, after_identity,
		auxiliary_identity);
}

static void v_multiwindow_appkit_side_effect_register_root(uint64_t identity,
		uint64_t subject) {
	if (v_multiwindow_appkit_side_effect_active_generation == 0 || identity == 0) {
		return;
	}
	for (uint64_t index = 0; index < V_MULTIWINDOW_APPKIT_SIDE_EFFECT_CAPACITY; index++) {
		if (v_multiwindow_appkit_observed_roots[index].identity == identity) {
			v_multiwindow_appkit_observed_roots[index].subject = subject;
			return;
		}
		if (v_multiwindow_appkit_observed_roots[index].identity == 0) {
			v_multiwindow_appkit_observed_roots[index].identity = identity;
			v_multiwindow_appkit_observed_roots[index].subject = subject;
			return;
		}
	}
	v_multiwindow_appkit_side_effect_overflow_flag = 1;
}

static uint64_t v_multiwindow_appkit_side_effect_root_subject(uint64_t identity) {
	for (uint64_t index = 0; index < V_MULTIWINDOW_APPKIT_SIDE_EFFECT_CAPACITY; index++) {
		if (v_multiwindow_appkit_observed_roots[index].identity == identity) {
			return v_multiwindow_appkit_observed_roots[index].subject;
		}
	}
	return V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_NONE;
}

static void v_multiwindow_appkit_side_effect_retire_root(uint64_t identity) {
	for (uint64_t index = 0; index < V_MULTIWINDOW_APPKIT_SIDE_EFFECT_CAPACITY; index++) {
		if (v_multiwindow_appkit_observed_roots[index].identity == identity) {
			v_multiwindow_appkit_observed_roots[index].identity = 0;
			v_multiwindow_appkit_observed_roots[index].subject = 0;
			return;
		}
	}
}

@interface VMultiwindowAppKitSideEffectProbe : NSObject
@property(assign) uint64_t observerGeneration;
@property(assign) uint64_t observerSubject;
@property(assign) uint64_t observerParentIdentity;
@property(assign) BOOL observerPoolProbe;
- (instancetype)initWithGeneration:(uint64_t)generation
		 subject:(uint64_t)subject
		 parentIdentity:(uint64_t)parentIdentity
		 poolProbe:(BOOL)poolProbe;
@end

@implementation VMultiwindowAppKitSideEffectProbe
- (instancetype)initWithGeneration:(uint64_t)generation
		 subject:(uint64_t)subject
		 parentIdentity:(uint64_t)parentIdentity
		 poolProbe:(BOOL)poolProbe {
	self = [super init];
	if (self != nil) {
		_observerGeneration = generation;
		_observerSubject = subject;
		_observerParentIdentity = parentIdentity;
		_observerPoolProbe = poolProbe;
	}
	return self;
}
- (void)dealloc {
	uint64_t identity = (uint64_t)(uintptr_t)(__bridge void *)self;
	v_multiwindow_appkit_side_effect_append_generation(self.observerGeneration,
		self.observerPoolProbe
			? V_MULTIWINDOW_APPKIT_SIDE_EFFECT_POOL_PROBE_DEALLOC
			: V_MULTIWINDOW_APPKIT_SIDE_EFFECT_RELEASE_PROBE_DEALLOC,
		self.observerSubject, identity, self.observerParentIdentity,
		identity, UINT64_C(0), UINT64_C(0));
}
@end

uint64_t v_multiwindow_appkit_side_effect_reset(void) {
	memset(v_multiwindow_appkit_side_effect_records, 0,
		sizeof(v_multiwindow_appkit_side_effect_records));
	memset(v_multiwindow_appkit_observed_roots, 0,
		sizeof(v_multiwindow_appkit_observed_roots));
	v_multiwindow_appkit_side_effect_record_count = 0;
	v_multiwindow_appkit_side_effect_overflow_flag = 0;
	if (v_multiwindow_appkit_side_effect_generation_counter == UINT64_MAX) {
		v_multiwindow_appkit_side_effect_active_generation = 0;
		v_multiwindow_appkit_side_effect_overflow_flag = 1;
		return 0;
	}
	v_multiwindow_appkit_side_effect_generation_counter++;
	if (v_multiwindow_appkit_side_effect_generation_counter == 0) {
		v_multiwindow_appkit_side_effect_generation_counter = 1;
	}
	v_multiwindow_appkit_side_effect_active_generation =
		v_multiwindow_appkit_side_effect_generation_counter;
	return v_multiwindow_appkit_side_effect_active_generation;
}

uint64_t v_multiwindow_appkit_side_effect_generation(void) {
	return v_multiwindow_appkit_side_effect_active_generation;
}

uint64_t v_multiwindow_appkit_side_effect_count(void) {
	return v_multiwindow_appkit_side_effect_record_count;
}

int v_multiwindow_appkit_side_effect_overflow(void) {
	return v_multiwindow_appkit_side_effect_overflow_flag;
}

int v_multiwindow_appkit_side_effect_record(uint64_t index,
		VMultiwindowAppKitSideEffectRecord *out_record) {
	if (out_record == NULL || index >= v_multiwindow_appkit_side_effect_record_count) {
		return 0;
	}
	*out_record = v_multiwindow_appkit_side_effect_records[index];
	return 1;
}

void *v_multiwindow_appkit_side_effect_create_release_probe(uint64_t subject) {
	if (v_multiwindow_appkit_side_effect_active_generation == 0
			|| ![NSThread isMainThread]
			|| (subject != V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_DEVICE_ROOT
				&& subject != V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_WINDOW_ROOT
				&& subject != V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_ANCHOR_ROOT)) {
		return NULL;
	}
	VMultiwindowAppKitSideEffectProbe *probe =
		[[VMultiwindowAppKitSideEffectProbe alloc]
			initWithGeneration:v_multiwindow_appkit_side_effect_active_generation
			subject:subject parentIdentity:UINT64_C(0) poolProbe:NO];
	if (probe == nil) {
		return NULL;
	}
	void *retained = (void *)CFBridgingRetain(probe);
	uint64_t identity = (uint64_t)(uintptr_t)retained;
	v_multiwindow_appkit_side_effect_register_root(identity, subject);
	v_multiwindow_appkit_side_effect_append(
		V_MULTIWINDOW_APPKIT_SIDE_EFFECT_BRIDGE_RETAIN, subject, identity,
		UINT64_C(0), UINT64_C(0), identity, UINT64_C(1));
	return retained;
}

uint64_t v_multiwindow_appkit_side_effect_probe_generation(void *probe_ptr) {
	if (probe_ptr == NULL) {
		return 0;
	}
	VMultiwindowAppKitSideEffectProbe *probe =
		(__bridge VMultiwindowAppKitSideEffectProbe *)probe_ptr;
	return probe.observerGeneration;
}

uint64_t v_multiwindow_appkit_side_effect_probe_subject(void *probe_ptr) {
	if (probe_ptr == NULL) {
		return 0;
	}
	VMultiwindowAppKitSideEffectProbe *probe =
		(__bridge VMultiwindowAppKitSideEffectProbe *)probe_ptr;
	return probe.observerSubject;
}

static VMultiwindowTestAppKitOracleRecord
	v_multiwindow_test_appkit_oracle_records[V_MULTIWINDOW_TEST_APPKIT_ORACLE_CAPACITY];
static uint64_t v_multiwindow_test_appkit_oracle_count;
static int v_multiwindow_test_appkit_oracle_overflow;

static void v_multiwindow_test_appkit_oracle_append(uint64_t kind,
		uint64_t identity, uint64_t parent_identity,
		VMultiwindowNativePrimitive raw) {
	if (v_multiwindow_test_appkit_oracle_count
			>= V_MULTIWINDOW_TEST_APPKIT_ORACLE_CAPACITY) {
		v_multiwindow_test_appkit_oracle_overflow = 1;
		return;
	}
	VMultiwindowTestAppKitOracleRecord *record =
		&v_multiwindow_test_appkit_oracle_records[v_multiwindow_test_appkit_oracle_count];
	v_multiwindow_test_appkit_oracle_count++;
	record->sequence = v_multiwindow_test_appkit_oracle_count;
	record->kind = kind;
	record->identity = identity;
	record->parent_identity = parent_identity;
	record->output_identity = raw.handle;
	record->auxiliary_identity = raw.object_identity_0;
	record->auxiliary_identity_1 = raw.object_identity_1;
	record->auxiliary_identity_2 = raw.object_identity_2;
	record->valid_mask = raw.valid_mask;
	record->thread_identity = (uint64_t)(uintptr_t)pthread_self();
}

void v_multiwindow_test_appkit_oracle_reset(void) {
	memset(v_multiwindow_test_appkit_oracle_records, 0,
		sizeof(v_multiwindow_test_appkit_oracle_records));
	v_multiwindow_test_appkit_oracle_count = 0;
	v_multiwindow_test_appkit_oracle_overflow = 0;
}

uint64_t v_multiwindow_test_appkit_oracle_count_get(void) {
	return v_multiwindow_test_appkit_oracle_count;
}

int v_multiwindow_test_appkit_oracle_overflow_get(void) {
	return v_multiwindow_test_appkit_oracle_overflow;
}

VMultiwindowTestAppKitOracleRecord
v_multiwindow_test_appkit_oracle_record_get(uint64_t index) {
	if (index >= v_multiwindow_test_appkit_oracle_count) {
		VMultiwindowTestAppKitOracleRecord empty = {0};
		return empty;
	}
	return v_multiwindow_test_appkit_oracle_records[index];
}
#endif

#define V_MULTIWINDOW_CURSOR_SHAPE_DEFAULT 0
#define V_MULTIWINDOW_CURSOR_SHAPE_POINTER 1
#define V_MULTIWINDOW_CURSOR_SHAPE_MOVE 2
#define V_MULTIWINDOW_CURSOR_SHAPE_N_RESIZE 3
#define V_MULTIWINDOW_CURSOR_SHAPE_S_RESIZE 4
#define V_MULTIWINDOW_CURSOR_SHAPE_E_RESIZE 5
#define V_MULTIWINDOW_CURSOR_SHAPE_W_RESIZE 6
#define V_MULTIWINDOW_CURSOR_SHAPE_NE_RESIZE 7
#define V_MULTIWINDOW_CURSOR_SHAPE_NW_RESIZE 8
#define V_MULTIWINDOW_CURSOR_SHAPE_SE_RESIZE 9
#define V_MULTIWINDOW_CURSOR_SHAPE_SW_RESIZE 10
#define V_MULTIWINDOW_CURSOR_SHAPE_EW_RESIZE 11
#define V_MULTIWINDOW_CURSOR_SHAPE_NS_RESIZE 12
#define V_MULTIWINDOW_CURSOR_SHAPE_NESW_RESIZE 13
#define V_MULTIWINDOW_CURSOR_SHAPE_NWSE_RESIZE 14
#define V_MULTIWINDOW_CURSOR_SHAPE_GRAB 15
#define V_MULTIWINDOW_CURSOR_SHAPE_GRABBING 16

@class VMultiwindowAppKitWindowState;

@interface VMultiwindowAppKitView : NSView <NSDraggingDestination>
@property(weak) VMultiwindowAppKitWindowState *state;
@property(strong) NSTrackingArea *trackingArea;
@end

@interface VMultiwindowAppKitWindowState : NSObject <NSWindowDelegate>
{
	BOOL keyDown[512];
}
@property(strong) NSWindow *window;
@property(strong) VMultiwindowAppKitView *view;
@property(strong) CAMetalLayer *layer;
@property(strong) id<MTLTexture> depthTexture;
@property(strong) id<CAMetalDrawable> currentDrawable;
@property(strong) NSMutableArray<NSValue *> *queuedEvents;
@property(assign) BOOL highDpi;
@property(assign) BOOL suppressResizeEvent;
@property(assign) uint32_t flagsChangedStore;
@property(assign) uint8_t mouseButtons;
@property(assign) float mouseX;
@property(assign) float mouseY;
@property(assign) float mouseDx;
@property(assign) float mouseDy;
@property(assign) BOOL mousePosValid;
@property(assign) int width;
@property(assign) int height;
@property(assign) int framebufferWidth;
@property(assign) int framebufferHeight;
@property(assign) int cursorShape;
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
@property(assign) BOOL nativeProofHeadlessOcclusionOverride;
#endif
- (void)updateDimensions;
- (BOOL)ensureDepthTextureWithDevice:(id<MTLDevice>)device;
- (void)queueLifecycleEvent:(int)kind;
- (void)queueResizeEvents;
- (void)queueInputEvent:(VMultiwindowAppKitQueuedEvent)event;
- (void)queueKeyEvent:(int)kind keyCode:(int)keyCode repeat:(BOOL)repeat modifiers:(uint32_t)modifiers;
- (void)queueCharEventsFromString:(NSString *)characters repeat:(BOOL)repeat modifiers:(uint32_t)modifiers;
- (void)queueTouchEvent:(int)kind changedPhase:(NSTouchPhase)phase event:(NSEvent *)event view:(NSView *)view;
- (void)updateMousePositionFromEvent:(NSEvent *)event clearDelta:(BOOL)clearDelta;
- (void)updateMousePositionFromWindowPoint:(NSPoint)point clearDelta:(BOOL)clearDelta;
- (void)setKeyDown:(BOOL)down forKeyCode:(int)keyCode;
- (void)clearKeyDownState;
- (void)clearQueuedEvents;
- (void)applyCursorShape;
@end

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
@interface VMultiwindowAppKitPhysicalNilDrawableLayer : CAMetalLayer
@property(assign) uint64_t observerGeneration;
@property(assign) uint64_t observerStateIdentity;
@end

@interface VMultiwindowAppKitPhysicalNilDrawableLease : NSObject
@property(strong) VMultiwindowAppKitWindowState *state;
@property(strong) CAMetalLayer *originalLayer;
@property(strong) VMultiwindowAppKitPhysicalNilDrawableLayer *testLayer;
@property(strong) id<MTLDevice> device;
@property(assign) uint64_t ownerThread;
@property(assign) BOOL active;
@end
#endif

static uint64_t v_multiwindow_appkit_event_sequence = 1;
static int v_multiwindow_appkit_event_sequence_exhausted_flag = 0;

static uint64_t v_multiwindow_appkit_next_event_sequence(void) {
	if (v_multiwindow_appkit_event_sequence_exhausted_flag) {
		return 0;
	}
	uint64_t sequence = v_multiwindow_appkit_event_sequence;
	if (sequence == UINT64_MAX) {
		v_multiwindow_appkit_event_sequence = 0;
		v_multiwindow_appkit_event_sequence_exhausted_flag = 1;
	} else {
		v_multiwindow_appkit_event_sequence++;
	}
	return sequence;
}

int v_multiwindow_appkit_event_sequence_exhausted(void) {
	return v_multiwindow_appkit_event_sequence_exhausted_flag;
}

static VMultiwindowAppKitQueuedEvent v_multiwindow_appkit_zero_event(void) {
	VMultiwindowAppKitQueuedEvent event = {0};
	event.mouse_button = V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID;
	return event;
}

static NSCursor *v_multiwindow_appkit_cursor_for_shape(int shape) {
	switch (shape) {
	case V_MULTIWINDOW_CURSOR_SHAPE_POINTER:
		return [NSCursor pointingHandCursor];
	case V_MULTIWINDOW_CURSOR_SHAPE_MOVE:
	case V_MULTIWINDOW_CURSOR_SHAPE_GRAB:
		return [NSCursor openHandCursor];
	case V_MULTIWINDOW_CURSOR_SHAPE_GRABBING:
		return [NSCursor closedHandCursor];
	case V_MULTIWINDOW_CURSOR_SHAPE_N_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_S_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_NS_RESIZE:
		return [NSCursor resizeUpDownCursor];
	case V_MULTIWINDOW_CURSOR_SHAPE_E_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_W_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_EW_RESIZE:
		return [NSCursor resizeLeftRightCursor];
	case V_MULTIWINDOW_CURSOR_SHAPE_NE_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_SW_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_NESW_RESIZE:
		return [NSCursor resizeUpDownCursor];
	case V_MULTIWINDOW_CURSOR_SHAPE_NW_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_SE_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_NWSE_RESIZE:
		return [NSCursor resizeLeftRightCursor];
	default:
		return [NSCursor arrowCursor];
	}
}

static char *v_multiwindow_appkit_copy_cstring(const char *value) {
	if (value == NULL) {
		return NULL;
	}
	size_t len = strlen(value);
	char *copy = (char *)calloc(len + 1, 1);
	if (copy == NULL) {
		return NULL;
	}
	memcpy(copy, value, len);
	return copy;
}

void v_multiwindow_appkit_release_queued_event_resources(VMultiwindowAppKitQueuedEvent *event) {
	if (event == NULL || event->dropped_files == NULL) {
		return;
	}
	for (int i = 0; i < event->dropped_file_count; i++) {
		free(event->dropped_files[i]);
	}
	free(event->dropped_files);
	event->dropped_files = NULL;
	event->dropped_file_count = 0;
}

static uint32_t v_multiwindow_appkit_modifiers(NSEvent *event) {
	NSEventModifierFlags flags = event != nil ? event.modifierFlags : NSEvent.modifierFlags;
	NSUInteger buttons = NSEvent.pressedMouseButtons;
	uint32_t modifiers = 0;
	if (flags & NSEventModifierFlagShift) {
		modifiers |= 1;
	}
	if (flags & NSEventModifierFlagControl) {
		modifiers |= 2;
	}
	if (flags & NSEventModifierFlagOption) {
		modifiers |= 4;
	}
	if (flags & NSEventModifierFlagCommand) {
		modifiers |= 8;
	}
	if (buttons & (1u << 0)) {
		modifiers |= V_MULTIWINDOW_APPKIT_MODIFIER_LMB;
	}
	if (buttons & (1u << 1)) {
		modifiers |= V_MULTIWINDOW_APPKIT_MODIFIER_RMB;
	}
	if (buttons & (1u << 2)) {
		modifiers |= V_MULTIWINDOW_APPKIT_MODIFIER_MMB;
	}
	return modifiers;
}

static const int *v_multiwindow_appkit_keycodes(void) {
	static int keycodes[128];
	static int initialized = 0;
	if (initialized) {
		return keycodes;
	}
	initialized = 1;
	keycodes[0x1D] = 48;
	keycodes[0x12] = 49;
	keycodes[0x13] = 50;
	keycodes[0x14] = 51;
	keycodes[0x15] = 52;
	keycodes[0x17] = 53;
	keycodes[0x16] = 54;
	keycodes[0x1A] = 55;
	keycodes[0x1C] = 56;
	keycodes[0x19] = 57;
	keycodes[0x00] = 65;
	keycodes[0x0B] = 66;
	keycodes[0x08] = 67;
	keycodes[0x02] = 68;
	keycodes[0x0E] = 69;
	keycodes[0x03] = 70;
	keycodes[0x05] = 71;
	keycodes[0x04] = 72;
	keycodes[0x22] = 73;
	keycodes[0x26] = 74;
	keycodes[0x28] = 75;
	keycodes[0x25] = 76;
	keycodes[0x2E] = 77;
	keycodes[0x2D] = 78;
	keycodes[0x1F] = 79;
	keycodes[0x23] = 80;
	keycodes[0x0C] = 81;
	keycodes[0x0F] = 82;
	keycodes[0x01] = 83;
	keycodes[0x11] = 84;
	keycodes[0x20] = 85;
	keycodes[0x09] = 86;
	keycodes[0x0D] = 87;
	keycodes[0x07] = 88;
	keycodes[0x10] = 89;
	keycodes[0x06] = 90;
	keycodes[0x27] = 39;
	keycodes[0x2A] = 92;
	keycodes[0x2B] = 44;
	keycodes[0x18] = 61;
	keycodes[0x32] = 96;
	keycodes[0x21] = 91;
	keycodes[0x1B] = 45;
	keycodes[0x2F] = 46;
	keycodes[0x1E] = 93;
	keycodes[0x29] = 59;
	keycodes[0x2C] = 47;
	keycodes[0x0A] = 161;
	keycodes[0x33] = 259;
	keycodes[0x39] = 280;
	keycodes[0x75] = 261;
	keycodes[0x7D] = 264;
	keycodes[0x77] = 269;
	keycodes[0x24] = 257;
	keycodes[0x35] = 256;
	keycodes[0x7A] = 290;
	keycodes[0x78] = 291;
	keycodes[0x63] = 292;
	keycodes[0x76] = 293;
	keycodes[0x60] = 294;
	keycodes[0x61] = 295;
	keycodes[0x62] = 296;
	keycodes[0x64] = 297;
	keycodes[0x65] = 298;
	keycodes[0x6D] = 299;
	keycodes[0x67] = 300;
	keycodes[0x6F] = 301;
	keycodes[0x69] = 302;
	keycodes[0x6B] = 303;
	keycodes[0x71] = 304;
	keycodes[0x6A] = 305;
	keycodes[0x40] = 306;
	keycodes[0x4F] = 307;
	keycodes[0x50] = 308;
	keycodes[0x5A] = 309;
	keycodes[0x73] = 268;
	keycodes[0x72] = 260;
	keycodes[0x7B] = 263;
	keycodes[0x3A] = 342;
	keycodes[0x3B] = 341;
	keycodes[0x38] = 340;
	keycodes[0x37] = 343;
	keycodes[0x6E] = 348;
	keycodes[0x47] = 282;
	keycodes[0x79] = 267;
	keycodes[0x74] = 266;
	keycodes[0x7C] = 262;
	keycodes[0x3D] = 346;
	keycodes[0x3E] = 345;
	keycodes[0x3C] = 344;
	keycodes[0x36] = 347;
	keycodes[0x31] = 32;
	keycodes[0x30] = 258;
	keycodes[0x7E] = 265;
	keycodes[0x52] = 320;
	keycodes[0x53] = 321;
	keycodes[0x54] = 322;
	keycodes[0x55] = 323;
	keycodes[0x56] = 324;
	keycodes[0x57] = 325;
	keycodes[0x58] = 326;
	keycodes[0x59] = 327;
	keycodes[0x5B] = 328;
	keycodes[0x5C] = 329;
	keycodes[0x45] = 334;
	keycodes[0x41] = 330;
	keycodes[0x4B] = 331;
	keycodes[0x4C] = 335;
	keycodes[0x51] = 336;
	keycodes[0x43] = 332;
	keycodes[0x4E] = 333;
	return keycodes;
}

static int v_multiwindow_appkit_key_code(unsigned short native_key_code) {
	const int *keycodes = v_multiwindow_appkit_keycodes();
	if (native_key_code < 128) {
		return keycodes[native_key_code];
	}
	return 0;
}

static NSEventModifierFlags v_multiwindow_appkit_modifier_flag_for_key_code(int key_code) {
	switch (key_code) {
	case 280:
		return NSEventModifierFlagCapsLock;
	case 340:
	case 344:
		return NSEventModifierFlagShift;
	case 341:
	case 345:
		return NSEventModifierFlagControl;
	case 342:
	case 346:
		return NSEventModifierFlagOption;
	case 343:
	case 347:
		return NSEventModifierFlagCommand;
	default:
		return 0;
	}
}

static BOOL v_multiwindow_appkit_modifier_key_is_pressed(NSEventModifierFlags flags, int key_code, BOOL *out_pressed) {
	if (out_pressed == NULL) {
		return NO;
	}
	switch (key_code) {
	case 340:
		*out_pressed = (flags & NX_DEVICELSHIFTKEYMASK) != 0;
		return YES;
	case 344:
		*out_pressed = (flags & NX_DEVICERSHIFTKEYMASK) != 0;
		return YES;
	case 341:
		*out_pressed = (flags & NX_DEVICELCTLKEYMASK) != 0;
		return YES;
	case 345:
		*out_pressed = (flags & NX_DEVICERCTLKEYMASK) != 0;
		return YES;
	case 342:
		*out_pressed = (flags & NX_DEVICELALTKEYMASK) != 0;
		return YES;
	case 346:
		*out_pressed = (flags & NX_DEVICERALTKEYMASK) != 0;
		return YES;
	case 343:
		*out_pressed = (flags & NX_DEVICELCMDKEYMASK) != 0;
		return YES;
	case 347:
		*out_pressed = (flags & NX_DEVICERCMDKEYMASK) != 0;
		return YES;
	default:
		break;
	}
	NSEventModifierFlags modifierFlag = v_multiwindow_appkit_modifier_flag_for_key_code(key_code);
	if (modifierFlag == 0) {
		return NO;
	}
	*out_pressed = (flags & modifierFlag) != 0;
	return YES;
}

static BOOL v_multiwindow_appkit_touch_sets_share_identity(NSSet<NSTouch *> *touches, NSTouch *target) {
	if (target == nil) {
		return NO;
	}
	id targetIdentity = target.identity;
	for (NSTouch *touch in touches) {
		if (touch == target) {
			return YES;
		}
		id identity = touch.identity;
		if (identity != nil && targetIdentity != nil && identity == targetIdentity) {
			return YES;
		}
		if (identity != nil && targetIdentity != nil && [identity isEqual:targetIdentity]) {
			return YES;
		}
	}
	return NO;
}

static float v_multiwindow_appkit_clamp_unit(CGFloat value) {
	if (value < 0.0) {
		return 0.0f;
	}
	if (value > 1.0) {
		return 1.0f;
	}
	return (float)value;
}

static void v_multiwindow_appkit_fill_touch_point(VMultiwindowAppKitWindowState *state, VMultiwindowAppKitQueuedEvent *event, int index, NSTouch *touch, BOOL changed) {
	if (state == nil || event == NULL || touch == nil || index < 0 || index >= V_MULTIWINDOW_APPKIT_MAX_TOUCH_POINTS) {
		return;
	}
	id identity = touch.identity;
	const void *identity_ptr = identity != nil ? (__bridge const void *)identity : (__bridge const void *)touch;
	float width = state.framebufferWidth > 1 ? (float)(state.framebufferWidth - 1) : 0.0f;
	float height = state.framebufferHeight > 1 ? (float)(state.framebufferHeight - 1) : 0.0f;
	NSPoint normalized = touch.normalizedPosition;
	float x = v_multiwindow_appkit_clamp_unit(normalized.x) * width;
	float y = (1.0f - v_multiwindow_appkit_clamp_unit(normalized.y)) * height;
	event->touch_ids[index] = (uint64_t)(uintptr_t)identity_ptr;
	event->touch_x[index] = x;
	event->touch_y[index] = y;
	event->touch_changed[index] = changed ? 1 : 0;
}

@implementation VMultiwindowAppKitWindowState
- (instancetype)init {
	self = [super init];
	if (self != nil) {
		self.queuedEvents = [NSMutableArray array];
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		self.nativeProofHeadlessOcclusionOverride = NO;
#endif
	}
	return self;
}

- (void)updateDimensions {
	NSView *content = self.window.contentView != nil ? self.window.contentView : self.view;
	NSRect bounds = content != nil ? content.bounds : NSMakeRect(0, 0, 1, 1);
	int logical_width = (int)lrint(NSWidth(bounds));
	int logical_height = (int)lrint(NSHeight(bounds));
	if (logical_width <= 0) {
		logical_width = 1;
	}
	if (logical_height <= 0) {
		logical_height = 1;
	}
	int framebuffer_width = logical_width;
	int framebuffer_height = logical_height;
	if (self.highDpi && content != nil) {
		NSRect backing = [content convertRectToBacking:bounds];
		framebuffer_width = (int)lrint(NSWidth(backing));
		framebuffer_height = (int)lrint(NSHeight(backing));
	}
	if (framebuffer_width <= 0) {
		framebuffer_width = 1;
	}
	if (framebuffer_height <= 0) {
		framebuffer_height = 1;
	}
	self.width = logical_width;
	self.height = logical_height;
	self.framebufferWidth = framebuffer_width;
	self.framebufferHeight = framebuffer_height;
	if (self.layer != nil) {
		CGFloat scale = 1.0;
		if (self.highDpi && self.window.screen != nil) {
			scale = self.window.screen.backingScaleFactor;
		}
		self.layer.contentsScale = scale;
		self.layer.drawableSize = CGSizeMake((CGFloat)framebuffer_width, (CGFloat)framebuffer_height);
	}
}

- (BOOL)ensureDepthTextureWithDevice:(id<MTLDevice>)device {
	if (device == nil || self.framebufferWidth <= 0 || self.framebufferHeight <= 0) {
		return NO;
	}
	if (self.depthTexture != nil &&
	    self.depthTexture.width == (NSUInteger)self.framebufferWidth &&
	    self.depthTexture.height == (NSUInteger)self.framebufferHeight) {
		return YES;
	}
	MTLTextureDescriptor *desc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatDepth32Float_Stencil8
	                                                                                width:(NSUInteger)self.framebufferWidth
	                                                                               height:(NSUInteger)self.framebufferHeight
	                                                                            mipmapped:NO];
	desc.usage = MTLTextureUsageRenderTarget;
	desc.storageMode = MTLStorageModePrivate;
	self.depthTexture = [device newTextureWithDescriptor:desc];
	return self.depthTexture != nil;
}

- (void)fillDimensionsForEvent:(VMultiwindowAppKitQueuedEvent *)event {
	if (event == NULL) {
		return;
	}
	event->window_width = self.width;
	event->window_height = self.height;
	event->framebuffer_width = self.framebufferWidth;
	event->framebuffer_height = self.framebufferHeight;
}

- (void)queueEvent:(VMultiwindowAppKitQueuedEvent)event {
	if (self.queuedEvents == nil) {
		self.queuedEvents = [NSMutableArray array];
	}
	event.sequence = v_multiwindow_appkit_next_event_sequence();
	if (event.sequence == 0) {
		v_multiwindow_appkit_release_queued_event_resources(&event);
		return;
	}
	VMultiwindowAppKitQueuedEvent *queued = calloc(1, sizeof(VMultiwindowAppKitQueuedEvent));
	if (queued == NULL) {
		v_multiwindow_appkit_release_queued_event_resources(&event);
		return;
	}
	*queued = event;
	[self.queuedEvents addObject:[NSValue valueWithPointer:queued]];
}

- (void)queueLifecycleEvent:(int)kind {
	VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
	event.event_kind = V_MULTIWINDOW_APPKIT_EVENT_LIFECYCLE;
	event.lifecycle_kind = kind;
	[self fillDimensionsForEvent:&event];
	[self queueEvent:event];
}

- (void)queueResizeEvents {
	[self queueLifecycleEvent:V_MULTIWINDOW_APPKIT_LIFECYCLE_RESIZED];
	VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
	event.event_kind = V_MULTIWINDOW_APPKIT_EVENT_INPUT;
	event.input_kind = V_MULTIWINDOW_APPKIT_INPUT_RESIZED;
	[self fillDimensionsForEvent:&event];
	[self queueEvent:event];
}

- (void)queueInputEvent:(VMultiwindowAppKitQueuedEvent)event {
	event.event_kind = V_MULTIWINDOW_APPKIT_EVENT_INPUT;
	[self fillDimensionsForEvent:&event];
	[self queueEvent:event];
}

- (void)queueKeyEvent:(int)kind keyCode:(int)keyCode repeat:(BOOL)repeat modifiers:(uint32_t)modifiers {
	if (keyCode == 0) {
		return;
	}
	if (keyCode >= 0 && keyCode < 512) {
		keyDown[keyCode] = kind == V_MULTIWINDOW_APPKIT_INPUT_KEY_DOWN;
	}
	VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
	event.input_kind = kind;
	event.key_code = keyCode;
	event.key_repeat = repeat ? 1 : 0;
	event.modifiers = modifiers;
	[self queueInputEvent:event];
}

- (void)queueCharEventsFromString:(NSString *)characters repeat:(BOOL)repeat modifiers:(uint32_t)modifiers {
	NSUInteger length = characters.length;
	for (NSUInteger i = 0; i < length; i++) {
		unichar unit = [characters characterAtIndex:i];
		uint32_t codepoint = unit;
		if (unit >= 0xD800 && unit <= 0xDBFF) {
			if ((i + 1) >= length) {
				continue;
			}
			unichar low = [characters characterAtIndex:i + 1];
			if (low < 0xDC00 || low > 0xDFFF) {
				continue;
			}
			codepoint = 0x10000u + (((uint32_t)unit - 0xD800u) << 10) + ((uint32_t)low - 0xDC00u);
			i++;
		} else if (unit >= 0xDC00 && unit <= 0xDFFF) {
			continue;
		}
		if ((codepoint & 0xFF00u) == 0xF700u) {
			continue;
		}
		VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
		event.input_kind = V_MULTIWINDOW_APPKIT_INPUT_CHAR;
		event.char_code = codepoint;
		event.key_repeat = repeat ? 1 : 0;
		event.modifiers = modifiers;
		[self queueInputEvent:event];
	}
}

- (void)queueTouchEvent:(int)kind changedPhase:(NSTouchPhase)phase event:(NSEvent *)event view:(NSView *)view {
	if (event == nil || view == nil) {
		return;
	}
	NSSet<NSTouch *> *changedTouches = [event touchesMatchingPhase:phase inView:view];
	NSSet<NSTouch *> *allTouches = [event touchesMatchingPhase:NSTouchPhaseAny inView:view];
	if (allTouches.count == 0) {
		allTouches = changedTouches;
	}
	if (allTouches.count == 0) {
		return;
	}
	VMultiwindowAppKitQueuedEvent queued = v_multiwindow_appkit_zero_event();
	queued.input_kind = kind;
	queued.modifiers = v_multiwindow_appkit_modifiers(event);
	int index = 0;
	for (NSTouch *touch in allTouches) {
		if (index >= V_MULTIWINDOW_APPKIT_MAX_TOUCH_POINTS) {
			break;
		}
		BOOL changed = v_multiwindow_appkit_touch_sets_share_identity(changedTouches, touch);
		v_multiwindow_appkit_fill_touch_point(self, &queued, index, touch, changed);
		index++;
	}
	if (index == 0) {
		return;
	}
	queued.touch_count = index;
	queued.mouse_x = queued.touch_x[0];
	queued.mouse_y = queued.touch_y[0];
	[self queueInputEvent:queued];
}

- (void)updateMousePositionFromEvent:(NSEvent *)event clearDelta:(BOOL)clearDelta {
	NSPoint point = event.locationInWindow;
	[self updateMousePositionFromWindowPoint:point clearDelta:clearDelta];
}

- (void)updateMousePositionFromWindowPoint:(NSPoint)point clearDelta:(BOOL)clearDelta {
	NSView *content = self.window.contentView != nil ? self.window.contentView : self.view;
	if (content != nil) {
		point = [content convertPoint:point fromView:nil];
	}
	NSRect bounds = content != nil ? content.bounds : NSMakeRect(0, 0, self.width, self.height);
	CGFloat logicalWidth = NSWidth(bounds) > 0 ? NSWidth(bounds) : (CGFloat)self.width;
	CGFloat logicalHeight = NSHeight(bounds) > 0 ? NSHeight(bounds) : (CGFloat)self.height;
	CGFloat scaleX = logicalWidth > 0 ? ((CGFloat)self.framebufferWidth / logicalWidth) : 1.0;
	CGFloat scaleY = logicalHeight > 0 ? ((CGFloat)self.framebufferHeight / logicalHeight) : 1.0;
	float newX = (float)(point.x * scaleX);
	float newY = (float)((CGFloat)self.framebufferHeight - (point.y * scaleY) - 1.0);
	if (clearDelta || !self.mousePosValid) {
		self.mouseDx = 0.0f;
		self.mouseDy = 0.0f;
	} else {
		self.mouseDx = newX - self.mouseX;
		self.mouseDy = newY - self.mouseY;
	}
	self.mouseX = newX;
	self.mouseY = newY;
	self.mousePosValid = YES;
}

- (void)setKeyDown:(BOOL)down forKeyCode:(int)keyCode {
	if (keyCode < 0 || keyCode >= 512) {
		return;
	}
	keyDown[keyCode] = down;
}

- (void)clearKeyDownState {
	for (NSUInteger i = 0; i < sizeof(keyDown) / sizeof(keyDown[0]); i++) {
		keyDown[i] = NO;
	}
}

- (void)clearQueuedEvents {
	for (NSValue *value in self.queuedEvents) {
		VMultiwindowAppKitQueuedEvent *event = (VMultiwindowAppKitQueuedEvent *)value.pointerValue;
		v_multiwindow_appkit_release_queued_event_resources(event);
		free(event);
	}
	[self.queuedEvents removeAllObjects];
}

- (void)dealloc {
	[self clearQueuedEvents];
}

- (void)applyCursorShape {
	[v_multiwindow_appkit_cursor_for_shape(self.cursorShape) set];
}

- (BOOL)windowShouldClose:(id)sender {
	(void)sender;
	[self queueLifecycleEvent:V_MULTIWINDOW_APPKIT_LIFECYCLE_CLOSE_REQUESTED];
	return NO;
}

- (void)windowWillClose:(NSNotification *)notification {
	(void)notification;
	self.depthTexture = nil;
	[self queueLifecycleEvent:V_MULTIWINDOW_APPKIT_LIFECYCLE_DESTROYED];
}

- (void)windowDidResize:(NSNotification *)notification {
	(void)notification;
	int old_width = self.width;
	int old_height = self.height;
	int old_framebuffer_width = self.framebufferWidth;
	int old_framebuffer_height = self.framebufferHeight;
	[self updateDimensions];
	self.depthTexture = nil;
	if (self.width != old_width || self.height != old_height ||
	    self.framebufferWidth != old_framebuffer_width ||
	    self.framebufferHeight != old_framebuffer_height) {
		if (!self.suppressResizeEvent) {
			[self queueResizeEvents];
		}
	}
}

- (void)windowDidChangeBackingProperties:(NSNotification *)notification {
	(void)notification;
	int old_width = self.width;
	int old_height = self.height;
	int old_framebuffer_width = self.framebufferWidth;
	int old_framebuffer_height = self.framebufferHeight;
	[self updateDimensions];
	self.depthTexture = nil;
	if (self.width != old_width || self.height != old_height ||
	    self.framebufferWidth != old_framebuffer_width ||
	    self.framebufferHeight != old_framebuffer_height) {
		if (!self.suppressResizeEvent) {
			[self queueResizeEvents];
		}
	}
}

- (void)windowDidChangeScreen:(NSNotification *)notification {
	(void)notification;
	int old_width = self.width;
	int old_height = self.height;
	int old_framebuffer_width = self.framebufferWidth;
	int old_framebuffer_height = self.framebufferHeight;
	[self updateDimensions];
	self.depthTexture = nil;
	if (self.width != old_width || self.height != old_height ||
	    self.framebufferWidth != old_framebuffer_width ||
	    self.framebufferHeight != old_framebuffer_height) {
		if (!self.suppressResizeEvent) {
			[self queueResizeEvents];
		}
	}
}

- (void)windowDidBecomeKey:(NSNotification *)notification {
	(void)notification;
	[self clearKeyDownState];
	self.flagsChangedStore = (uint32_t)NSEvent.modifierFlags;
	VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
	event.input_kind = V_MULTIWINDOW_APPKIT_INPUT_FOCUSED;
	event.modifiers = v_multiwindow_appkit_modifiers(nil);
	[self queueInputEvent:event];
}

- (void)windowDidResignKey:(NSNotification *)notification {
	(void)notification;
	[self clearKeyDownState];
	self.flagsChangedStore = (uint32_t)NSEvent.modifierFlags;
	VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
	event.input_kind = V_MULTIWINDOW_APPKIT_INPUT_UNFOCUSED;
	event.modifiers = v_multiwindow_appkit_modifiers(nil);
	[self queueInputEvent:event];
}

- (void)windowDidMiniaturize:(NSNotification *)notification {
	(void)notification;
	VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
	event.input_kind = V_MULTIWINDOW_APPKIT_INPUT_ICONIFIED;
	event.modifiers = v_multiwindow_appkit_modifiers(nil);
	[self queueInputEvent:event];
}

- (void)windowDidDeminiaturize:(NSNotification *)notification {
	(void)notification;
	VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
	event.input_kind = V_MULTIWINDOW_APPKIT_INPUT_RESTORED;
	event.modifiers = v_multiwindow_appkit_modifiers(nil);
	[self queueInputEvent:event];
}
@end

@implementation VMultiwindowAppKitView
- (BOOL)acceptsFirstResponder {
	return YES;
}

- (BOOL)canBecomeKeyView {
	return YES;
}

- (BOOL)acceptsFirstMouse:(NSEvent *)event {
	(void)event;
	return YES;
}

- (void)updateTrackingAreas {
	if (self.trackingArea != nil) {
		[self removeTrackingArea:self.trackingArea];
		self.trackingArea = nil;
	}
	NSTrackingAreaOptions options = NSTrackingMouseEnteredAndExited |
		NSTrackingActiveInKeyWindow |
		NSTrackingEnabledDuringMouseDrag |
		NSTrackingCursorUpdate |
		NSTrackingInVisibleRect |
		NSTrackingAssumeInside;
	self.trackingArea = [[NSTrackingArea alloc] initWithRect:self.bounds
	                                                 options:options
	                                                   owner:self
	                                                userInfo:nil];
	[self addTrackingArea:self.trackingArea];
	[super updateTrackingAreas];
}

- (void)cursorUpdate:(NSEvent *)event {
	(void)event;
	VMultiwindowAppKitWindowState *state = self.state;
	if (state != nil) {
		[state applyCursorShape];
	}
}

- (void)queueMouseEvent:(NSEvent *)event kind:(int)kind button:(int)button clearDelta:(BOOL)clearDelta {
	VMultiwindowAppKitWindowState *state = self.state;
	if (state == nil) {
		return;
	}
	[state updateMousePositionFromEvent:event clearDelta:clearDelta];
	VMultiwindowAppKitQueuedEvent queued = v_multiwindow_appkit_zero_event();
	queued.input_kind = kind;
	queued.modifiers = v_multiwindow_appkit_modifiers(event);
	queued.mouse_button = button;
	queued.mouse_x = state.mouseX;
	queued.mouse_y = state.mouseY;
	queued.mouse_dx = state.mouseDx;
	queued.mouse_dy = state.mouseDy;
	[state queueInputEvent:queued];
}

- (NSDragOperation)draggingEntered:(id<NSDraggingInfo>)sender {
	NSPasteboard *pasteboard = sender.draggingPasteboard;
	if ([pasteboard canReadObjectForClasses:@[ NSURL.class ]
	                                options:@{ NSPasteboardURLReadingFileURLsOnlyKey: @YES }]) {
		return NSDragOperationCopy;
	}
	return NSDragOperationNone;
}

- (NSDragOperation)draggingUpdated:(id<NSDraggingInfo>)sender {
	return [self draggingEntered:sender];
}

- (BOOL)performDragOperation:(id<NSDraggingInfo>)sender {
	VMultiwindowAppKitWindowState *state = self.state;
	if (state == nil) {
		return NO;
	}
	NSPasteboard *pasteboard = sender.draggingPasteboard;
	NSArray<NSURL *> *urls = [pasteboard readObjectsForClasses:@[ NSURL.class ]
	                                                   options:@{ NSPasteboardURLReadingFileURLsOnlyKey: @YES }];
	if (urls.count == 0) {
		return NO;
	}
	[state updateMousePositionFromWindowPoint:sender.draggingLocation clearDelta:YES];
	VMultiwindowAppKitQueuedEvent queued = v_multiwindow_appkit_zero_event();
	queued.input_kind = V_MULTIWINDOW_APPKIT_INPUT_FILES_DROPPED;
	queued.modifiers = v_multiwindow_appkit_modifiers(nil);
	queued.mouse_x = state.mouseX;
	queued.mouse_y = state.mouseY;
	queued.dropped_files = (char **)calloc(urls.count, sizeof(char *));
	if (queued.dropped_files == NULL) {
		return NO;
	}
	for (NSURL *url in urls) {
		if (!url.fileURL) {
			continue;
		}
		const char *path = url.standardizedURL.path.UTF8String;
		char *copy = v_multiwindow_appkit_copy_cstring(path);
		if (copy == NULL) {
			continue;
		}
		queued.dropped_files[queued.dropped_file_count] = copy;
		queued.dropped_file_count++;
	}
	if (queued.dropped_file_count == 0) {
		v_multiwindow_appkit_release_queued_event_resources(&queued);
		return NO;
	}
	[state queueInputEvent:queued];
	return YES;
}

- (void)mouseEntered:(NSEvent *)event {
	if (self.state.mouseButtons == 0) {
		[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_ENTER button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID clearDelta:YES];
	}
}

- (void)mouseExited:(NSEvent *)event {
	if (self.state.mouseButtons == 0) {
		[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_LEAVE button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID clearDelta:YES];
	}
}

- (void)mouseDown:(NSEvent *)event {
	[self.window makeFirstResponder:self];
	[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_DOWN button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_LEFT clearDelta:NO];
	self.state.mouseButtons |= (1u << V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_LEFT);
}

- (void)mouseUp:(NSEvent *)event {
	[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_UP button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_LEFT clearDelta:NO];
	self.state.mouseButtons &= ~(1u << V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_LEFT);
}

- (void)rightMouseDown:(NSEvent *)event {
	[self.window makeFirstResponder:self];
	[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_DOWN button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_RIGHT clearDelta:NO];
	self.state.mouseButtons |= (1u << V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_RIGHT);
}

- (void)rightMouseUp:(NSEvent *)event {
	[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_UP button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_RIGHT clearDelta:NO];
	self.state.mouseButtons &= ~(1u << V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_RIGHT);
}

- (void)otherMouseDown:(NSEvent *)event {
	if (event.buttonNumber == 2) {
		[self.window makeFirstResponder:self];
		[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_DOWN button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_MIDDLE clearDelta:NO];
		self.state.mouseButtons |= (1u << V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_MIDDLE);
	}
}

- (void)otherMouseUp:(NSEvent *)event {
	if (event.buttonNumber == 2) {
		[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_UP button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_MIDDLE clearDelta:NO];
		self.state.mouseButtons &= ~(1u << V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_MIDDLE);
	}
}

- (void)mouseMoved:(NSEvent *)event {
	[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_MOVE button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID clearDelta:NO];
}

- (void)mouseDragged:(NSEvent *)event {
	[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_MOVE button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID clearDelta:NO];
}

- (void)rightMouseDragged:(NSEvent *)event {
	[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_MOVE button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID clearDelta:NO];
}

- (void)otherMouseDragged:(NSEvent *)event {
	if (event.buttonNumber == 2) {
		[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_MOVE button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID clearDelta:NO];
	}
}

- (void)scrollWheel:(NSEvent *)event {
	VMultiwindowAppKitWindowState *state = self.state;
	if (state == nil) {
		return;
	}
	[state updateMousePositionFromEvent:event clearDelta:YES];
	float dx = (float)event.scrollingDeltaX;
	float dy = (float)event.scrollingDeltaY;
	if (event.hasPreciseScrollingDeltas) {
		dx *= 0.1f;
		dy *= 0.1f;
	}
	if (dx == 0.0f && dy == 0.0f) {
		return;
	}
	VMultiwindowAppKitQueuedEvent queued = v_multiwindow_appkit_zero_event();
	queued.input_kind = V_MULTIWINDOW_APPKIT_INPUT_MOUSE_SCROLL;
	queued.modifiers = v_multiwindow_appkit_modifiers(event);
	queued.mouse_x = state.mouseX;
	queued.mouse_y = state.mouseY;
	queued.scroll_x = dx;
	queued.scroll_y = dy;
	[state queueInputEvent:queued];
}

- (void)touchesBeganWithEvent:(NSEvent *)event {
	[self.state queueTouchEvent:V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_BEGAN
	               changedPhase:NSTouchPhaseBegan
	                      event:event
	                       view:self];
}

- (void)touchesMovedWithEvent:(NSEvent *)event {
	[self.state queueTouchEvent:V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_MOVED
	               changedPhase:NSTouchPhaseMoved
	                      event:event
	                       view:self];
}

- (void)touchesEndedWithEvent:(NSEvent *)event {
	[self.state queueTouchEvent:V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_ENDED
	               changedPhase:NSTouchPhaseEnded
	                      event:event
	                       view:self];
}

- (void)touchesCancelledWithEvent:(NSEvent *)event {
	[self.state queueTouchEvent:V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_CANCELLED
	               changedPhase:NSTouchPhaseCancelled
	                      event:event
	                       view:self];
}

- (void)keyDown:(NSEvent *)event {
	VMultiwindowAppKitWindowState *state = self.state;
	if (state == nil) {
		return;
	}
	uint32_t modifiers = v_multiwindow_appkit_modifiers(event);
	int keyCode = v_multiwindow_appkit_key_code(event.keyCode);
	[state queueKeyEvent:V_MULTIWINDOW_APPKIT_INPUT_KEY_DOWN keyCode:keyCode repeat:event.isARepeat modifiers:modifiers];
	if ((modifiers & 8u) == 0) {
		[state queueCharEventsFromString:event.characters repeat:event.isARepeat modifiers:modifiers];
	}
	if (modifiers == 8 && keyCode == 86) {
		VMultiwindowAppKitQueuedEvent queued = v_multiwindow_appkit_zero_event();
		queued.input_kind = V_MULTIWINDOW_APPKIT_INPUT_CLIPBOARD_PASTED;
		queued.modifiers = modifiers;
		[state queueInputEvent:queued];
	}
}

- (BOOL)performKeyEquivalent:(NSEvent *)event {
	if (v_multiwindow_appkit_key_code(event.keyCode) == 86 &&
	    v_multiwindow_appkit_modifiers(event) == 8) {
		[self keyDown:event];
		return YES;
	}
	if (v_multiwindow_appkit_key_code(event.keyCode) == 258) {
		[self keyDown:event];
		return YES;
	}
	return NO;
}

- (void)keyUp:(NSEvent *)event {
	VMultiwindowAppKitWindowState *state = self.state;
	if (state == nil) {
		return;
	}
	int keyCode = v_multiwindow_appkit_key_code(event.keyCode);
	[state queueKeyEvent:V_MULTIWINDOW_APPKIT_INPUT_KEY_UP keyCode:keyCode repeat:event.isARepeat modifiers:v_multiwindow_appkit_modifiers(event)];
}

- (void)flagsChanged:(NSEvent *)event {
	VMultiwindowAppKitWindowState *state = self.state;
	if (state == nil) {
		return;
	}
	uint32_t oldFlags = state.flagsChangedStore;
	uint32_t newFlags = (uint32_t)event.modifierFlags;
	state.flagsChangedStore = newFlags;
	int keyCode = v_multiwindow_appkit_key_code(event.keyCode);
	BOOL wasPressed = NO;
	BOOL isPressed = NO;
	if (keyCode == 0 ||
	    !v_multiwindow_appkit_modifier_key_is_pressed((NSEventModifierFlags)oldFlags, keyCode, &wasPressed) ||
	    !v_multiwindow_appkit_modifier_key_is_pressed((NSEventModifierFlags)newFlags, keyCode, &isPressed)) {
		return;
	}

	if (wasPressed == isPressed) {
		[state setKeyDown:isPressed forKeyCode:keyCode];
		return;
	}
	[state queueKeyEvent:isPressed ? V_MULTIWINDOW_APPKIT_INPUT_KEY_DOWN : V_MULTIWINDOW_APPKIT_INPUT_KEY_UP
	             keyCode:keyCode
	              repeat:NO
	           modifiers:v_multiwindow_appkit_modifiers(event)];
}
@end

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
@implementation VMultiwindowAppKitPhysicalNilDrawableLayer
- (id<CAMetalDrawable>)nextDrawable {
	const uint64_t layer_identity =
		(uint64_t)(uintptr_t)(__bridge void *)self;
	v_multiwindow_appkit_side_effect_append_generation(self.observerGeneration,
		V_MULTIWINDOW_APPKIT_SIDE_EFFECT_NEXT_DRAWABLE,
		V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_LAYER,
		layer_identity, self.observerStateIdentity,
		UINT64_C(0), UINT64_C(0), layer_identity);
	return nil;
}
@end

@implementation VMultiwindowAppKitPhysicalNilDrawableLease
@end

static uint64_t v_multiwindow_appkit_native_proof_current_thread(void) {
	return (uint64_t)(uintptr_t)pthread_self();
}

static void v_multiwindow_appkit_native_proof_copy_layer(
		CAMetalLayer *source,
		VMultiwindowAppKitPhysicalNilDrawableLayer *destination) {
	destination.pixelFormat = source.pixelFormat;
	destination.framebufferOnly = source.framebufferOnly;
	destination.drawableSize = source.drawableSize;
	destination.presentsWithTransaction = source.presentsWithTransaction;
	destination.colorspace = source.colorspace;
	destination.device = source.device;
	if (@available(macOS 10.13, *)) {
		destination.maximumDrawableCount = source.maximumDrawableCount;
		destination.displaySyncEnabled = source.displaySyncEnabled;
		destination.allowsNextDrawableTimeout = source.allowsNextDrawableTimeout;
	}
	destination.frame = source.frame;
	destination.bounds = source.bounds;
	destination.position = source.position;
	destination.anchorPoint = source.anchorPoint;
	destination.zPosition = source.zPosition;
	destination.transform = source.transform;
	destination.sublayerTransform = source.sublayerTransform;
	destination.contentsScale = source.contentsScale;
	destination.contentsGravity = source.contentsGravity;
	destination.contentsRect = source.contentsRect;
	destination.contentsCenter = source.contentsCenter;
	destination.opaque = source.opaque;
	destination.hidden = source.hidden;
	destination.opacity = source.opacity;
	destination.masksToBounds = source.masksToBounds;
	destination.doubleSided = source.doubleSided;
	destination.geometryFlipped = source.geometryFlipped;
	destination.allowsEdgeAntialiasing = source.allowsEdgeAntialiasing;
	destination.edgeAntialiasingMask = source.edgeAntialiasingMask;
	destination.autoresizingMask = source.autoresizingMask;
	destination.backgroundColor = source.backgroundColor;
	destination.minificationFilter = source.minificationFilter;
	destination.magnificationFilter = source.magnificationFilter;
	destination.minificationFilterBias = source.minificationFilterBias;
	destination.delegate = source.delegate;
}

void *v_multiwindow_appkit_native_proof_install_physical_nil_drawable(
		void *state_ptr,
		void *expected_layer_ptr,
		void *expected_device_ptr,
		uint64_t expected_owner_thread) {
	if (state_ptr == NULL || expected_layer_ptr == NULL
			|| expected_device_ptr == NULL || expected_owner_thread == 0
			|| ![NSThread isMainThread]
			|| v_multiwindow_appkit_native_proof_current_thread()
				!= expected_owner_thread
			|| v_multiwindow_appkit_side_effect_active_generation == 0) {
		return NULL;
	}
	VMultiwindowAppKitWindowState *state =
		(__bridge VMultiwindowAppKitWindowState *)state_ptr;
	CAMetalLayer *original_layer = (__bridge CAMetalLayer *)expected_layer_ptr;
	id<MTLDevice> device = (__bridge id<MTLDevice>)expected_device_ptr;
	if (state.view == nil || !state.view.wantsLayer || state.layer != original_layer
			|| state.view.layer != original_layer
			|| original_layer.device != device
			|| state.currentDrawable != nil
			|| [original_layer isKindOfClass:
				[VMultiwindowAppKitPhysicalNilDrawableLayer class]]) {
		return NULL;
	}

	VMultiwindowAppKitPhysicalNilDrawableLayer *test_layer =
		[VMultiwindowAppKitPhysicalNilDrawableLayer layer];
	if (test_layer == nil) {
		return NULL;
	}
	test_layer.observerGeneration =
		v_multiwindow_appkit_side_effect_active_generation;
	test_layer.observerStateIdentity = (uint64_t)(uintptr_t)state_ptr;
	v_multiwindow_appkit_native_proof_copy_layer(original_layer, test_layer);
	if (test_layer.device != device) {
		test_layer.delegate = nil;
		test_layer.device = nil;
		return NULL;
	}

	VMultiwindowAppKitPhysicalNilDrawableLease *lease =
		[[VMultiwindowAppKitPhysicalNilDrawableLease alloc] init];
	if (lease == nil) {
		test_layer.delegate = nil;
		test_layer.device = nil;
		return NULL;
	}
	lease.state = state;
	lease.originalLayer = original_layer;
	lease.testLayer = test_layer;
	lease.device = device;
	lease.ownerThread = expected_owner_thread;
	lease.active = YES;

	state.view.layer = test_layer;
	state.layer = test_layer;
	if (state.view.layer != test_layer || state.layer != test_layer
			|| test_layer.device != device || state.currentDrawable != nil) {
		state.view.layer = original_layer;
		state.layer = original_layer;
		test_layer.delegate = nil;
		test_layer.device = nil;
		lease.active = NO;
		lease.device = nil;
		lease.testLayer = nil;
		lease.originalLayer = nil;
		lease.state = nil;
		return NULL;
	}
	return (void *)CFBridgingRetain(lease);
}

int v_multiwindow_appkit_native_proof_restore_physical_nil_drawable(
		void *lease_ptr,
		uint64_t expected_owner_thread) {
	if (lease_ptr == NULL || expected_owner_thread == 0
			|| ![NSThread isMainThread]
			|| v_multiwindow_appkit_native_proof_current_thread()
				!= expected_owner_thread) {
		return 0;
	}
	VMultiwindowAppKitPhysicalNilDrawableLease *lease =
		(__bridge VMultiwindowAppKitPhysicalNilDrawableLease *)lease_ptr;
	if (![lease isKindOfClass:[VMultiwindowAppKitPhysicalNilDrawableLease class]]
			|| !lease.active || lease.ownerThread != expected_owner_thread
			|| lease.state == nil || lease.originalLayer == nil
			|| lease.testLayer == nil || lease.device == nil
			|| lease.state.view == nil || !lease.state.view.wantsLayer
			|| lease.state.currentDrawable != nil
			|| lease.state.layer != lease.testLayer
			|| lease.state.view.layer != lease.testLayer
			|| lease.testLayer.device != lease.device
			|| lease.originalLayer.device != lease.device) {
		return 0;
	}

	VMultiwindowAppKitWindowState *state = lease.state;
	CAMetalLayer *original_layer = lease.originalLayer;
	VMultiwindowAppKitPhysicalNilDrawableLayer *test_layer = lease.testLayer;
	state.view.layer = original_layer;
	state.layer = original_layer;
	if (state.view.layer != original_layer || state.layer != original_layer
			|| state.currentDrawable != nil) {
		state.view.layer = test_layer;
		state.layer = test_layer;
		return 0;
	}

	test_layer.delegate = nil;
	test_layer.device = nil;
	test_layer.observerGeneration = 0;
	test_layer.observerStateIdentity = 0;
	lease.active = NO;
	lease.device = nil;
	lease.testLayer = nil;
	lease.originalLayer = nil;
	lease.state = nil;
	CFBridgingRelease(lease_ptr);
	return 1;
}

int v_multiwindow_test_appkit_admit_window(void *state_ptr) {
	@autoreleasepool {
		const int main_thread = [NSThread isMainThread] ? 1 : 0;
		if (state_ptr == NULL || !main_thread) {
			return 0;
		}
		VMultiwindowAppKitWindowState *state =
			(__bridge VMultiwindowAppKitWindowState *)state_ptr;
		state.nativeProofHeadlessOcclusionOverride = NO;
		if (state.window == nil) {
			return 0;
		}
		if (state.view == nil) {
			return 0;
		}
		if (state.layer == nil) {
			return 0;
		}
		if (state.layer.device == nil) {
			return 0;
		}
		if (state.width <= 0 || state.height <= 0) {
			return 0;
		}
		if (state.framebufferWidth <= 0 || state.framebufferHeight <= 0) {
			return 0;
		}
		NSNotificationCenter *center = [NSNotificationCenter defaultCenter];
		id observer = [center
			addObserverForName:NSWindowDidChangeOcclusionStateNotification
			object:state.window
			queue:[NSOperationQueue mainQueue]
			usingBlock:^(NSNotification *notification) {
				(void)notification;
			}];
		[NSApp activateIgnoringOtherApps:YES];
		[state.window orderFrontRegardless];
		[state.window makeKeyWindow];
		BOOL admitted = NO;
		int visible = -1;
		int miniaturized = -1;
		int occluded = -1;
		for (int poll = 0; poll < 128; poll++) {
			[NSApp updateWindows];
			visible = state.window.isVisible ? 1 : 0;
			miniaturized = state.window.isMiniaturized ? 1 : 0;
			occluded = visible
				&& ((state.window.occlusionState
					& NSWindowOcclusionStateVisible) == 0) ? 1 : 0;
			if (visible && !miniaturized && !occluded) {
				admitted = YES;
				break;
			}
			[[NSRunLoop mainRunLoop]
				runMode:NSDefaultRunLoopMode
				beforeDate:[NSDate dateWithTimeIntervalSinceNow:0.001]];
		}
		[center removeObserver:observer];
		if (admitted) {
			return 1;
		}
		if (state.window == nil) {
			return 0;
		}
		if (state.view == nil) {
			return 0;
		}
		if (state.layer == nil) {
			return 0;
		}
		if (state.layer.device == nil) {
			return 0;
		}
		if (state.width <= 0 || state.height <= 0) {
			return 0;
		}
		if (state.framebufferWidth <= 0 || state.framebufferHeight <= 0) {
			return 0;
		}
		if (!visible) {
			return 0;
		}
		if (miniaturized) {
			return 0;
		}
		if (occluded) {
			state.nativeProofHeadlessOcclusionOverride = YES;
			return 2;
		}
		return 0;
	}
}
#endif

static NSString *v_multiwindow_appkit_string(const char *value) {
	if (value == NULL) {
		return @"";
	}
	return [NSString stringWithUTF8String:value] ?: @"";
}

static int v_multiwindow_appkit_max_int(int value, int minimum) {
	return minimum > 0 && value < minimum ? minimum : value;
}

int v_multiwindow_appkit_is_main_thread(void) {
	return [NSThread isMainThread] ? 1 : 0;
}

int v_multiwindow_appkit_prepare_application(void) {
	@autoreleasepool {
		if (![NSThread isMainThread]) {
			return 0;
		}
		[NSApplication sharedApplication];
		if (NSApp.activationPolicy == NSApplicationActivationPolicyProhibited) {
			[NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
		}
		[NSApp finishLaunching];
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		VMultiwindowNativePrimitive observed = {0};
		v_multiwindow_test_appkit_oracle_append(
			V_MULTIWINDOW_TEST_APPKIT_PROCESS_COMMIT,
			(uint64_t)(uintptr_t)NSApp, 0, observed);
#endif
		return 1;
	}
}

static int v_multiwindow_appkit_create_metal_device_raw(void **out_device) {
	@autoreleasepool {
		if (out_device == NULL || ![NSThread isMainThread]) {
			return 0;
		}
		id<MTLDevice> device = MTLCreateSystemDefaultDevice();
		if (device == nil) {
			*out_device = NULL;
			return 0;
		}
		void *retained_device = (void *)CFBridgingRetain(device);
		*out_device = retained_device;
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		const uint64_t retained_identity = (uint64_t)(uintptr_t)retained_device;
		v_multiwindow_appkit_side_effect_register_root(retained_identity,
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_DEVICE_ROOT);
		v_multiwindow_appkit_side_effect_append(
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_BRIDGE_RETAIN,
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_DEVICE_ROOT,
			retained_identity, 0, 0, retained_identity, 0);
#endif
		return 1;
	}
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static VMultiwindowNativePrimitive v_multiwindow_appkit_create_metal_device_actual(void) {
#else
VMultiwindowNativePrimitive v_multiwindow_appkit_create_metal_device(void) {
#endif
	VMultiwindowNativePrimitive result = {0};
	void *device = NULL;
	(void)v_multiwindow_appkit_create_metal_device_raw(&device);
	result.handle = (uint64_t)(uintptr_t)device;
	result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_HANDLE;
	return result;
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static VMultiwindowNativePrimitive v_multiwindow_appkit_release_metal_device_actual(
		void *device) {
#else
VMultiwindowNativePrimitive v_multiwindow_appkit_release_metal_device(void *device) {
#endif
	VMultiwindowNativePrimitive result = {0};
	if (device != NULL && [NSThread isMainThread]) {
		result.object_identity_0 = (uint64_t)(uintptr_t)device;
		result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_0;
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		const uint64_t released_identity = (uint64_t)(uintptr_t)device;
		uint64_t released_subject =
			v_multiwindow_appkit_side_effect_root_subject(released_identity);
		if (released_subject == V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_NONE) {
			released_subject = V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_DEVICE_ROOT;
		}
#endif
		CFBridgingRelease(device);
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		v_multiwindow_appkit_side_effect_append(
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_BRIDGE_RELEASE,
			released_subject, released_identity, 0, released_identity, 0, 0);
		v_multiwindow_appkit_side_effect_retire_root(released_identity);
#endif
	}
	return result;
}

static CAMetalLayer *v_multiwindow_appkit_configure_layer(VMultiwindowAppKitWindowState *state,
		id<MTLDevice> device) {
	if (state == nil || state.window == nil || state.view == nil || device == nil
			|| ![NSThread isMainThread]) {
		return nil;
	}
	CAMetalLayer *layer = state.layer;
	if (layer == nil) {
		layer = [CAMetalLayer layer];
		if (layer == nil) {
			return nil;
		}
	}
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
	const uint64_t layer_identity = (uint64_t)(uintptr_t)(__bridge void *)layer;
	const uint64_t layer_device_before = (uint64_t)(uintptr_t)(__bridge void *)layer.device;
#endif
	layer.device = device;
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
	const uint64_t layer_device_after = (uint64_t)(uintptr_t)(__bridge void *)layer.device;
	v_multiwindow_appkit_side_effect_append(
		V_MULTIWINDOW_APPKIT_SIDE_EFFECT_LAYER_DEVICE_SET_READ,
		V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_LAYER,
		layer_identity, (uint64_t)(uintptr_t)(__bridge void *)state,
		layer_device_before, layer_device_after,
		(uint64_t)(uintptr_t)(__bridge void *)device);
#endif
	layer.pixelFormat = MTLPixelFormatBGRA8Unorm;
	layer.framebufferOnly = YES;
	layer.opaque = YES;
	state.view.wantsLayer = YES;
	state.view.layer = layer;
	state.layer = layer;
	[state updateDimensions];
	if (state.layer != layer || state.view.layer != layer || layer.device != device) {
		return nil;
	}
	return layer;
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static VMultiwindowNativePrimitive v_multiwindow_appkit_configure_window_device_actual(
		void *state_ptr, void *device_ptr) {
#else
VMultiwindowNativePrimitive v_multiwindow_appkit_configure_window_device(void *state_ptr,
		void *device_ptr) {
#endif
	VMultiwindowNativePrimitive result = {0};
	if (state_ptr == NULL || device_ptr == NULL || ![NSThread isMainThread]) {
		return result;
	}
	VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
	id<MTLDevice> device = (__bridge id<MTLDevice>)device_ptr;
	CAMetalLayer *layer = v_multiwindow_appkit_configure_layer(state, device);
	if (layer != nil) {
		result.object_identity_0 = (uint64_t)(uintptr_t)state_ptr;
		result.object_identity_1 = (uint64_t)(uintptr_t)device_ptr;
		result.object_identity_2 = (uint64_t)(uintptr_t)(__bridge void *)layer;
		result.observed_count = state.framebufferWidth > 0
			? (uint64_t)state.framebufferWidth : UINT64_C(0);
		result.selected_value = (int64_t)state.framebufferHeight;
		result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_0 |
			V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_1 |
			V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_2 |
			V_MULTIWINDOW_NATIVE_VALID_OBSERVED_COUNT |
			V_MULTIWINDOW_NATIVE_VALID_SELECTED_VALUE;
	}
	return result;
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static VMultiwindowNativePrimitive v_multiwindow_appkit_create_window_actual(void *device_ptr,
		const char *title, int width, int height, int min_width, int min_height,
		int resizable, int visible, int high_dpi, int borderless, int fullscreen,
		int *out_width, int *out_height, int *out_framebuffer_width,
		int *out_framebuffer_height) {
#else
VMultiwindowNativePrimitive v_multiwindow_appkit_create_window(void *device_ptr, const char *title,
		int width, int height, int min_width, int min_height, int resizable, int visible,
		int high_dpi, int borderless, int fullscreen, int *out_width, int *out_height,
		int *out_framebuffer_width, int *out_framebuffer_height) {
#endif
	VMultiwindowNativePrimitive result = {0};
	@autoreleasepool {
		if (![NSThread isMainThread] || width <= 0 || height <= 0) {
			result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_HANDLE;
			return result;
		}
		NSWindowStyleMask style = borderless ? NSWindowStyleMaskBorderless :
			(NSWindowStyleMaskTitled | NSWindowStyleMaskClosable | NSWindowStyleMaskMiniaturizable);
		if (resizable) {
			style |= NSWindowStyleMaskResizable;
		}
		int content_width = v_multiwindow_appkit_max_int(width, min_width);
		int content_height = v_multiwindow_appkit_max_int(height, min_height);
		NSRect rect = NSMakeRect(0, 0, (CGFloat)content_width, (CGFloat)content_height);
		NSWindow *window = [[NSWindow alloc] initWithContentRect:rect
		                                               styleMask:style
		                                                 backing:NSBackingStoreBuffered
		                                                   defer:NO];
		if (window == nil) {
			result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_HANDLE;
			return result;
		}
		window.releasedWhenClosed = NO;
		window.title = v_multiwindow_appkit_string(title);
		window.acceptsMouseMovedEvents = YES;
		window.restorable = NO;
		if (min_width > 0 || min_height > 0) {
			[window setMinSize:NSMakeSize((CGFloat)(min_width > 0 ? min_width : 1),
			                              (CGFloat)(min_height > 0 ? min_height : 1))];
		}
		VMultiwindowAppKitView *view = [[VMultiwindowAppKitView alloc] initWithFrame:rect];
		if (view == nil) {
			result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_HANDLE;
			return result;
		}
		view.acceptsTouchEvents = YES;
		VMultiwindowAppKitWindowState *state = [[VMultiwindowAppKitWindowState alloc] init];
		state.window = window;
		state.view = view;
		view.state = state;
		[view registerForDraggedTypes:@[ NSPasteboardTypeFileURL ]];
		state.highDpi = high_dpi ? YES : NO;
		state.flagsChangedStore = (uint32_t)NSEvent.modifierFlags;
		if (device_ptr != NULL) {
			id<MTLDevice> device = (__bridge id<MTLDevice>)device_ptr;
			if (v_multiwindow_appkit_configure_layer(state, device) == nil) {
				result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_HANDLE;
				return result;
			}
		}
		window.contentView = view;
		window.delegate = state;
		[window makeFirstResponder:view];
		[window center];
		[state updateDimensions];
		if (visible) {
			[window makeKeyAndOrderFront:nil];
			[NSApp activateIgnoringOtherApps:YES];
		} else {
			[window orderOut:nil];
		}
		if (fullscreen) {
			[window toggleFullScreen:nil];
		}
		if (out_width != NULL) {
			*out_width = state.width;
		}
		if (out_height != NULL) {
			*out_height = state.height;
		}
		if (out_framebuffer_width != NULL) {
			*out_framebuffer_width = state.framebufferWidth;
		}
		if (out_framebuffer_height != NULL) {
			*out_framebuffer_height = state.framebufferHeight;
		}
		void *retained_state = (void *)CFBridgingRetain(state);
		result.handle = (uint64_t)(uintptr_t)retained_state;
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		const uint64_t retained_identity = (uint64_t)(uintptr_t)retained_state;
		v_multiwindow_appkit_side_effect_register_root(retained_identity,
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_WINDOW_ROOT);
		v_multiwindow_appkit_side_effect_append(
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_BRIDGE_RETAIN,
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_WINDOW_ROOT,
			retained_identity, 0, 0, retained_identity, 0);
#endif
		result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_HANDLE;
		return result;
	}
}

static int v_multiwindow_appkit_create_renderer_anchor_raw(void *device_ptr, void **out_state) {
	@autoreleasepool {
		if (device_ptr == NULL || out_state == NULL || ![NSThread isMainThread]) {
			return 0;
		}
		id<MTLDevice> device = (__bridge id<MTLDevice>)device_ptr;
		NSRect rect = NSMakeRect(-10000.0, -10000.0, 1.0, 1.0);
		NSPanel *panel = [[NSPanel alloc] initWithContentRect:rect
		                                           styleMask:(NSWindowStyleMaskBorderless | NSWindowStyleMaskNonactivatingPanel)
		                                             backing:NSBackingStoreBuffered
		                                               defer:NO];
		if (panel == nil) {
			return 0;
		}
		panel.releasedWhenClosed = NO;
		panel.opaque = NO;
		panel.alphaValue = 0.0;
		panel.ignoresMouseEvents = YES;
		panel.hidesOnDeactivate = NO;
		VMultiwindowAppKitView *view = [[VMultiwindowAppKitView alloc] initWithFrame:rect];
		VMultiwindowAppKitWindowState *state = [[VMultiwindowAppKitWindowState alloc] init];
		CAMetalLayer *layer = [CAMetalLayer layer];
		if (view == nil || state == nil || layer == nil) {
			[panel close];
			return 0;
		}
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		const uint64_t layer_identity = (uint64_t)(uintptr_t)(__bridge void *)layer;
		const uint64_t layer_device_before =
			(uint64_t)(uintptr_t)(__bridge void *)layer.device;
#endif
		layer.device = device;
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		const uint64_t layer_device_after =
			(uint64_t)(uintptr_t)(__bridge void *)layer.device;
		v_multiwindow_appkit_side_effect_append(
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_LAYER_DEVICE_SET_READ,
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_LAYER,
			layer_identity, (uint64_t)(uintptr_t)(__bridge void *)state,
			layer_device_before, layer_device_after,
			(uint64_t)(uintptr_t)(__bridge void *)device);
#endif
		layer.pixelFormat = MTLPixelFormatBGRA8Unorm;
		layer.framebufferOnly = YES;
		layer.opaque = NO;
		view.wantsLayer = YES;
		view.layer = layer;
		state.window = panel;
		state.view = view;
		state.layer = layer;
		state.highDpi = NO;
		view.state = state;
		panel.contentView = view;
		[state updateDimensions];
		[panel orderBack:nil];
		void *retained_state = (void *)CFBridgingRetain(state);
		*out_state = retained_state;
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		const uint64_t retained_identity = (uint64_t)(uintptr_t)retained_state;
		v_multiwindow_appkit_side_effect_register_root(retained_identity,
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_ANCHOR_ROOT);
		v_multiwindow_appkit_side_effect_append(
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_BRIDGE_RETAIN,
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_ANCHOR_ROOT,
			retained_identity, 0, 0, retained_identity, 0);
#endif
		return 1;
	}
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static VMultiwindowNativePrimitive v_multiwindow_appkit_destroy_window_actual(void *state_ptr);

static VMultiwindowNativePrimitive v_multiwindow_appkit_create_renderer_anchor_actual(
		void *device_ptr) {
#else
VMultiwindowNativePrimitive v_multiwindow_appkit_create_renderer_anchor(void *device_ptr) {
#endif
	VMultiwindowNativePrimitive result = {0};
	void *state = NULL;
	(void)v_multiwindow_appkit_create_renderer_anchor_raw(device_ptr, &state);
	result.handle = (uint64_t)(uintptr_t)state;
	result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_HANDLE;
	return result;
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static VMultiwindowNativePrimitive v_multiwindow_appkit_destroy_renderer_anchor_actual(
		void *state_ptr) {
#else
VMultiwindowNativePrimitive v_multiwindow_appkit_destroy_renderer_anchor(void *state_ptr) {
#endif
	VMultiwindowNativePrimitive result = {0};
	if (state_ptr == NULL) {
		return result;
	}
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
	(void)v_multiwindow_appkit_destroy_window_actual(state_ptr);
#else
	(void)v_multiwindow_appkit_destroy_window(state_ptr);
#endif
	return result;
}

int v_multiwindow_appkit_render_snapshot(void *state_ptr, int *out_visible, int *out_miniaturized, int *out_occluded, int *out_width, int *out_height, int *out_framebuffer_width, int *out_framebuffer_height, float *out_scale) {
	@autoreleasepool {
		if (state_ptr == NULL || ![NSThread isMainThread]) {
			return 0;
		}
		VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
		if (state.window == nil || state.view == nil) {
			return 0;
		}
		[state updateDimensions];
		BOOL visible = state.window.isVisible;
		BOOL miniaturized = state.window.isMiniaturized;
		BOOL occluded = visible && ((state.window.occlusionState & NSWindowOcclusionStateVisible) == 0);
		if (out_visible != NULL) *out_visible = visible ? 1 : 0;
		if (out_miniaturized != NULL) *out_miniaturized = miniaturized ? 1 : 0;
		if (out_occluded != NULL) {
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
			*out_occluded = state.nativeProofHeadlessOcclusionOverride
				? 0 : (occluded ? 1 : 0);
#else
			*out_occluded = occluded ? 1 : 0;
#endif
		}
		if (out_width != NULL) *out_width = state.width;
		if (out_height != NULL) *out_height = state.height;
		if (out_framebuffer_width != NULL) *out_framebuffer_width = state.framebufferWidth;
		if (out_framebuffer_height != NULL) *out_framebuffer_height = state.framebufferHeight;
		if (out_scale != NULL) {
			NSRect logical = NSMakeRect(0, 0, 1, 1);
			NSRect backing = [state.view convertRectToBacking:logical];
			*out_scale = (float)NSWidth(backing);
		}
		return 1;
	}
}

int v_multiwindow_appkit_logical_to_pixel_rect(void *state_ptr, float x, float y, float width, float height, int *out_x, int *out_y, int *out_width, int *out_height) {
	@autoreleasepool {
		if (state_ptr == NULL || ![NSThread isMainThread]) return 0;
		VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
		if (state.view == nil) return 0;
		NSRect backing = [state.view convertRectToBacking:NSMakeRect(x, y, width, height)];
		if (out_x) *out_x = (int)floor(NSMinX(backing));
		if (out_y) *out_y = (int)floor(NSMinY(backing));
		if (out_width) *out_width = (int)ceil(NSMaxX(backing)) - (int)floor(NSMinX(backing));
		if (out_height) *out_height = (int)ceil(NSMaxY(backing)) - (int)floor(NSMinY(backing));
		return 1;
	}
}

int v_multiwindow_appkit_pixel_to_logical_rect(void *state_ptr, int x, int y, int width, int height, float *out_x, float *out_y, float *out_width, float *out_height) {
	@autoreleasepool {
		if (state_ptr == NULL || ![NSThread isMainThread]) return 0;
		VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
		if (state.view == nil) return 0;
		NSRect logical = [state.view convertRectFromBacking:NSMakeRect(x, y, width, height)];
		if (out_x) *out_x = (float)NSMinX(logical);
		if (out_y) *out_y = (float)NSMinY(logical);
		if (out_width) *out_width = (float)NSWidth(logical);
		if (out_height) *out_height = (float)NSHeight(logical);
		return 1;
	}
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static VMultiwindowNativePrimitive v_multiwindow_appkit_destroy_window_actual(void *state_ptr) {
#else
VMultiwindowNativePrimitive v_multiwindow_appkit_destroy_window(void *state_ptr) {
#endif
	VMultiwindowNativePrimitive result = {0};
	@autoreleasepool {
		if (state_ptr == NULL || ![NSThread isMainThread]) {
			return result;
		}
		VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		const uint64_t drawable_before =
			(uint64_t)(uintptr_t)(__bridge void *)state.currentDrawable;
#endif
		state.currentDrawable = nil;
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		const uint64_t drawable_after =
			(uint64_t)(uintptr_t)(__bridge void *)state.currentDrawable;
		if (drawable_before != 0) {
			v_multiwindow_appkit_side_effect_append(
				V_MULTIWINDOW_APPKIT_SIDE_EFFECT_CURRENT_DRAWABLE_CLEAR,
				V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_DRAWABLE,
				drawable_before, (uint64_t)(uintptr_t)state_ptr,
				drawable_before, drawable_after, 0);
		}
#endif
		state.depthTexture = nil;
		[state clearQueuedEvents];
		if (state.window != nil) {
			state.window.delegate = nil;
			[state.window orderOut:nil];
			[state.window close];
		}
		state.view.state = nil;
		state.window = nil;
		state.view = nil;
		state.layer = nil;
	}
	return result;
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static VMultiwindowNativePrimitive v_multiwindow_appkit_release_window_actual(void *state_ptr) {
#else
VMultiwindowNativePrimitive v_multiwindow_appkit_release_window(void *state_ptr) {
#endif
	VMultiwindowNativePrimitive result = {0};
	if (state_ptr != NULL && [NSThread isMainThread]) {
		result.object_identity_0 = (uint64_t)(uintptr_t)state_ptr;
		result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_0;
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		const uint64_t released_identity = (uint64_t)(uintptr_t)state_ptr;
		const uint64_t released_subject =
			v_multiwindow_appkit_side_effect_root_subject(released_identity);
#endif
		CFBridgingRelease(state_ptr);
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		v_multiwindow_appkit_side_effect_append(
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_BRIDGE_RELEASE,
			released_subject, released_identity, 0, released_identity, 0, 0);
		v_multiwindow_appkit_side_effect_retire_root(released_identity);
#endif
	}
	return result;
}

int v_multiwindow_appkit_set_window_title(void *state_ptr, const char *title) {
	@autoreleasepool {
		if (state_ptr == NULL || ![NSThread isMainThread]) {
			return 0;
		}
		VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
		if (state.window == nil) {
			return 0;
		}
		state.window.title = v_multiwindow_appkit_string(title);
		return 1;
	}
}

int v_multiwindow_appkit_set_cursor_shape(void *state_ptr, int shape) {
	@autoreleasepool {
		if (state_ptr == NULL || ![NSThread isMainThread] ||
		    shape < V_MULTIWINDOW_CURSOR_SHAPE_DEFAULT ||
		    shape > V_MULTIWINDOW_CURSOR_SHAPE_GRABBING) {
			return 0;
		}
		VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
		if (state.window == nil) {
			return 0;
		}
		state.cursorShape = shape;
		[state applyCursorShape];
		return 1;
	}
}

int v_multiwindow_appkit_resize_window(void *state_ptr, int width, int height, int *out_width, int *out_height, int *out_framebuffer_width, int *out_framebuffer_height) {
	@autoreleasepool {
		if (state_ptr == NULL || ![NSThread isMainThread] || width <= 0 || height <= 0) {
			return 0;
		}
		VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
		if (state.window == nil) {
			return 0;
		}
		state.suppressResizeEvent = YES;
		[state.window setContentSize:NSMakeSize((CGFloat)width, (CGFloat)height)];
		[state updateDimensions];
		state.suppressResizeEvent = NO;
		state.depthTexture = nil;
		if (out_width != NULL) {
			*out_width = state.width;
		}
		if (out_height != NULL) {
			*out_height = state.height;
		}
		if (out_framebuffer_width != NULL) {
			*out_framebuffer_width = state.framebufferWidth;
		}
		if (out_framebuffer_height != NULL) {
			*out_framebuffer_height = state.framebufferHeight;
		}
		return 1;
	}
}

void v_multiwindow_appkit_poll_events(void) {
	@autoreleasepool {
		if (![NSThread isMainThread] || NSApp == nil) {
			return;
		}
		for (;;) {
			NSEvent *event = [NSApp nextEventMatchingMask:NSEventMaskAny
			                                    untilDate:[NSDate distantPast]
			                                       inMode:NSDefaultRunLoopMode
			                                      dequeue:YES];
			if (event == nil) {
				break;
			}
			[NSApp sendEvent:event];
		}
		[NSApp updateWindows];
	}
}

int v_multiwindow_appkit_take_queued_event(void *state_ptr, VMultiwindowAppKitQueuedEvent *out_event) {
	if (state_ptr == NULL || out_event == NULL || ![NSThread isMainThread]) {
		return 0;
	}
	VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
	if (state.queuedEvents.count == 0) {
		return 0;
	}
	NSValue *value = state.queuedEvents[0];
	VMultiwindowAppKitQueuedEvent *event = (VMultiwindowAppKitQueuedEvent *)value.pointerValue;
	[state.queuedEvents removeObjectAtIndex:0];
	if (event == NULL) {
		return 0;
	}
	*out_event = *event;
	free(event);
	return 1;
}

static int v_multiwindow_appkit_clear_frame(void *state_ptr, void *drawable_ptr) {
	if (state_ptr == NULL || drawable_ptr == NULL || ![NSThread isMainThread]) {
		return 0;
	}
	VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
	if (state.currentDrawable == nil || (__bridge void *)state.currentDrawable != drawable_ptr) {
		return 0;
	}
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
	const uint64_t drawable_before =
		(uint64_t)(uintptr_t)(__bridge void *)state.currentDrawable;
#endif
	state.currentDrawable = nil;
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
	const uint64_t drawable_after =
		(uint64_t)(uintptr_t)(__bridge void *)state.currentDrawable;
	v_multiwindow_appkit_side_effect_append(
		V_MULTIWINDOW_APPKIT_SIDE_EFFECT_CURRENT_DRAWABLE_CLEAR,
		V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_DRAWABLE,
		(uint64_t)(uintptr_t)drawable_ptr, (uint64_t)(uintptr_t)state_ptr,
		drawable_before, drawable_after, 0);
#endif
	return 1;
}

static void *v_multiwindow_appkit_begin_render_batch_raw(void) {
	if (![NSThread isMainThread]) {
		return NULL;
	}
	void *pool = objc_autoreleasePoolPush();
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
	if (pool != NULL && v_multiwindow_appkit_side_effect_active_generation != 0) {
		const uint64_t pool_identity = (uint64_t)(uintptr_t)pool;
		v_multiwindow_appkit_side_effect_append(
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_POOL_PUSH,
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_POOL,
			pool_identity, 0, 0, pool_identity, 0);
		VMultiwindowAppKitSideEffectProbe *__autoreleasing probe =
			[[VMultiwindowAppKitSideEffectProbe alloc]
				initWithGeneration:v_multiwindow_appkit_side_effect_active_generation
				subject:V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_POOL
				parentIdentity:pool_identity poolProbe:YES];
		if (probe != nil) {
			const uint64_t probe_identity =
				(uint64_t)(uintptr_t)(__bridge void *)probe;
			v_multiwindow_appkit_side_effect_append(
				V_MULTIWINDOW_APPKIT_SIDE_EFFECT_POOL_PROBE_CREATE,
				V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_POOL,
				probe_identity, pool_identity, 0, probe_identity, 0);
		}
	}
#endif
	return pool;
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static VMultiwindowNativePrimitive v_multiwindow_appkit_begin_render_batch_actual(void) {
#else
VMultiwindowNativePrimitive v_multiwindow_appkit_begin_render_batch(void) {
#endif
	VMultiwindowNativePrimitive result = {0};
	void *pool = v_multiwindow_appkit_begin_render_batch_raw();
	result.handle = (uint64_t)(uintptr_t)pool;
	result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_HANDLE;
	return result;
}

static void v_multiwindow_appkit_end_render_batch_raw(void *pool) {
	if (pool == NULL || ![NSThread isMainThread]) {
		return;
	}
	objc_autoreleasePoolPop(pool);
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
	v_multiwindow_appkit_side_effect_append(
		V_MULTIWINDOW_APPKIT_SIDE_EFFECT_POOL_POP,
		V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_POOL,
		(uint64_t)(uintptr_t)pool, 0, (uint64_t)(uintptr_t)pool, 0, 0);
#endif
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static VMultiwindowNativePrimitive v_multiwindow_appkit_end_render_batch_actual(void *pool) {
#else
VMultiwindowNativePrimitive v_multiwindow_appkit_end_render_batch(void *pool) {
#endif
	VMultiwindowNativePrimitive result = {0};
	if (pool != NULL && [NSThread isMainThread]) {
		result.object_identity_0 = (uint64_t)(uintptr_t)pool;
		result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_0;
	}
	v_multiwindow_appkit_end_render_batch_raw(pool);
	return result;
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static VMultiwindowNativePrimitive v_multiwindow_appkit_release_drawable_actual(
		void *state_ptr, void *drawable_ptr) {
#else
VMultiwindowNativePrimitive v_multiwindow_appkit_release_drawable(void *state_ptr,
		void *drawable_ptr) {
#endif
	VMultiwindowNativePrimitive result = {0};
	VMultiwindowAppKitWindowState *state = state_ptr != NULL
		? (__bridge VMultiwindowAppKitWindowState *)state_ptr : nil;
	if ([NSThread isMainThread] && state.currentDrawable != nil
		&& (__bridge void *)state.currentDrawable == drawable_ptr) {
		result.object_identity_0 = (uint64_t)(uintptr_t)drawable_ptr;
		result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_0;
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		const uint64_t drawable_before =
			(uint64_t)(uintptr_t)(__bridge void *)state.currentDrawable;
#endif
		state.currentDrawable = nil;
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
		const uint64_t drawable_after =
			(uint64_t)(uintptr_t)(__bridge void *)state.currentDrawable;
		v_multiwindow_appkit_side_effect_append(
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_CURRENT_DRAWABLE_CLEAR,
			V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_DRAWABLE,
			(uint64_t)(uintptr_t)drawable_ptr, (uint64_t)(uintptr_t)state_ptr,
			drawable_before, drawable_after, 0);
#endif
	}
	return result;
}

static int v_multiwindow_appkit_begin_frame_raw(void *state_ptr, void *device_ptr, void **out_drawable, void **out_depth_texture, int *out_framebuffer_width, int *out_framebuffer_height) {
	if (state_ptr == NULL || device_ptr == NULL || out_drawable == NULL || out_depth_texture == NULL ||
	    ![NSThread isMainThread]) {
		return 0;
	}
	VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
	id<MTLDevice> device = (__bridge id<MTLDevice>)device_ptr;
	if (state.layer == nil || device == nil || state.currentDrawable != nil) {
		return 0;
	}
	[state updateDimensions];
	if (![state ensureDepthTextureWithDevice:device]) {
		return 0;
	}
	state.currentDrawable = [state.layer nextDrawable];
#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
	const uint64_t drawable_identity =
		(uint64_t)(uintptr_t)(__bridge void *)state.currentDrawable;
	v_multiwindow_appkit_side_effect_append(
		V_MULTIWINDOW_APPKIT_SIDE_EFFECT_NEXT_DRAWABLE,
		V_MULTIWINDOW_APPKIT_SIDE_EFFECT_SUBJECT_DRAWABLE,
		drawable_identity, (uint64_t)(uintptr_t)state_ptr,
		0, drawable_identity,
		(uint64_t)(uintptr_t)(__bridge void *)state.layer);
#endif
	if (state.currentDrawable == nil) {
		return 0;
	}
	*out_drawable = (__bridge void *)state.currentDrawable;
	*out_depth_texture = (__bridge void *)state.depthTexture;
	if (out_framebuffer_width != NULL) {
		*out_framebuffer_width = state.framebufferWidth;
	}
	if (out_framebuffer_height != NULL) {
		*out_framebuffer_height = state.framebufferHeight;
	}
	return 1;
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static VMultiwindowNativePrimitive v_multiwindow_appkit_begin_frame_actual(
		void *state_ptr, void *device_ptr) {
#else
VMultiwindowNativePrimitive v_multiwindow_appkit_begin_frame(void *state_ptr, void *device_ptr) {
#endif
	VMultiwindowNativePrimitive result = {0};
	void *drawable = NULL;
	void *depth_texture = NULL;
	int framebuffer_width = 0;
	int framebuffer_height = 0;
	(void)v_multiwindow_appkit_begin_frame_raw(state_ptr, device_ptr,
		&drawable, &depth_texture, &framebuffer_width, &framebuffer_height);
	result.handle = (uint64_t)(uintptr_t)drawable;
	result.object_identity_0 = (uint64_t)(uintptr_t)depth_texture;
	result.observed_count = framebuffer_width > 0 ? (uint64_t)framebuffer_width : UINT64_C(0);
	result.selected_value = (int64_t)framebuffer_height;
	result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_HANDLE |
		V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_0 |
		V_MULTIWINDOW_NATIVE_VALID_OBSERVED_COUNT |
		V_MULTIWINDOW_NATIVE_VALID_SELECTED_VALUE;
	return result;
}

static int v_multiwindow_appkit_end_frame_raw(void *state_ptr, void *drawable_ptr) {
	return v_multiwindow_appkit_clear_frame(state_ptr, drawable_ptr);
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static VMultiwindowNativePrimitive v_multiwindow_appkit_end_frame_actual(
		void *state_ptr, void *drawable_ptr) {
#else
VMultiwindowNativePrimitive v_multiwindow_appkit_end_frame(void *state_ptr, void *drawable_ptr) {
#endif
	VMultiwindowNativePrimitive result = {0};
	if (v_multiwindow_appkit_end_frame_raw(state_ptr, drawable_ptr)) {
		result.object_identity_0 = (uint64_t)(uintptr_t)drawable_ptr;
		result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_0;
	}
	return result;
}

static int v_multiwindow_appkit_abort_frame_raw(void *state_ptr, void *drawable_ptr) {
	return v_multiwindow_appkit_clear_frame(state_ptr, drawable_ptr);
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
static VMultiwindowNativePrimitive v_multiwindow_appkit_abort_frame_actual(
		void *state_ptr, void *drawable_ptr) {
#else
VMultiwindowNativePrimitive v_multiwindow_appkit_abort_frame(void *state_ptr, void *drawable_ptr) {
#endif
	VMultiwindowNativePrimitive result = {0};
	if (v_multiwindow_appkit_abort_frame_raw(state_ptr, drawable_ptr)) {
		result.object_identity_0 = (uint64_t)(uintptr_t)drawable_ptr;
		result.valid_mask = V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_0;
	}
	return result;
}

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST)
VMultiwindowNativePrimitive v_multiwindow_appkit_create_metal_device(void) {
	VMultiwindowNativePrimitive raw =
		v_multiwindow_appkit_create_metal_device_actual();
	v_multiwindow_test_appkit_oracle_append(
		V_MULTIWINDOW_TEST_APPKIT_DEVICE_CREATE, 0, 0, raw);
	return raw;
}

VMultiwindowNativePrimitive v_multiwindow_appkit_release_metal_device(void *device) {
	const uint64_t identity = (uint64_t)(uintptr_t)device;
	VMultiwindowNativePrimitive raw =
		v_multiwindow_appkit_release_metal_device_actual(device);
	v_multiwindow_test_appkit_oracle_append(
		V_MULTIWINDOW_TEST_APPKIT_DEVICE_RELEASE,
		identity, 0, raw);
	return raw;
}

VMultiwindowNativePrimitive v_multiwindow_appkit_create_window(void *device_ptr,
		const char *title, int width, int height, int min_width, int min_height,
		int resizable, int visible, int high_dpi, int borderless, int fullscreen,
		int *out_width, int *out_height, int *out_framebuffer_width,
		int *out_framebuffer_height) {
	VMultiwindowNativePrimitive raw = v_multiwindow_appkit_create_window_actual(
		device_ptr, title, width, height, min_width, min_height, resizable,
		visible, high_dpi, borderless, fullscreen, out_width, out_height,
		out_framebuffer_width, out_framebuffer_height);
	v_multiwindow_test_appkit_oracle_append(
		V_MULTIWINDOW_TEST_APPKIT_WINDOW_CREATE,
		(uint64_t)(uintptr_t)device_ptr, 0, raw);
	return raw;
}

VMultiwindowNativePrimitive v_multiwindow_appkit_configure_window_device(
		void *state_ptr, void *device_ptr) {
	VMultiwindowNativePrimitive raw =
		v_multiwindow_appkit_configure_window_device_actual(state_ptr, device_ptr);
	v_multiwindow_test_appkit_oracle_append(
		V_MULTIWINDOW_TEST_APPKIT_WINDOW_CONFIGURE,
		(uint64_t)(uintptr_t)state_ptr, (uint64_t)(uintptr_t)device_ptr, raw);
	return raw;
}

VMultiwindowNativePrimitive v_multiwindow_appkit_destroy_window(void *state_ptr) {
	const uint64_t identity = (uint64_t)(uintptr_t)state_ptr;
	VMultiwindowNativePrimitive raw =
		v_multiwindow_appkit_destroy_window_actual(state_ptr);
	v_multiwindow_test_appkit_oracle_append(
		V_MULTIWINDOW_TEST_APPKIT_WINDOW_DESTROY,
		identity, 0, raw);
	return raw;
}

VMultiwindowNativePrimitive v_multiwindow_appkit_release_window(void *state_ptr) {
	const uint64_t identity = (uint64_t)(uintptr_t)state_ptr;
	VMultiwindowNativePrimitive raw =
		v_multiwindow_appkit_release_window_actual(state_ptr);
	v_multiwindow_test_appkit_oracle_append(
		V_MULTIWINDOW_TEST_APPKIT_WINDOW_RELEASE,
		identity, 0, raw);
	return raw;
}

VMultiwindowNativePrimitive v_multiwindow_appkit_create_renderer_anchor(void *device_ptr) {
	const uint64_t identity = (uint64_t)(uintptr_t)device_ptr;
	VMultiwindowNativePrimitive raw =
		v_multiwindow_appkit_create_renderer_anchor_actual(device_ptr);
	v_multiwindow_test_appkit_oracle_append(
		V_MULTIWINDOW_TEST_APPKIT_ANCHOR_CREATE,
		identity, 0, raw);
	return raw;
}

VMultiwindowNativePrimitive v_multiwindow_appkit_destroy_renderer_anchor(void *state_ptr) {
	const uint64_t identity = (uint64_t)(uintptr_t)state_ptr;
	VMultiwindowNativePrimitive raw =
		v_multiwindow_appkit_destroy_renderer_anchor_actual(state_ptr);
	v_multiwindow_test_appkit_oracle_append(
		V_MULTIWINDOW_TEST_APPKIT_ANCHOR_DESTROY,
		identity, 0, raw);
	return raw;
}

VMultiwindowNativePrimitive v_multiwindow_appkit_begin_frame(void *state_ptr,
		void *device_ptr) {
	const uint64_t state_identity = (uint64_t)(uintptr_t)state_ptr;
	const uint64_t device_identity = (uint64_t)(uintptr_t)device_ptr;
	VMultiwindowNativePrimitive raw =
		v_multiwindow_appkit_begin_frame_actual(state_ptr, device_ptr);
	v_multiwindow_test_appkit_oracle_append(
		V_MULTIWINDOW_TEST_APPKIT_DRAWABLE_ACQUIRE,
		state_identity, device_identity, raw);
	return raw;
}

VMultiwindowNativePrimitive v_multiwindow_appkit_end_frame(void *state_ptr,
		void *drawable_ptr) {
	VMultiwindowNativePrimitive raw =
		v_multiwindow_appkit_end_frame_actual(state_ptr, drawable_ptr);
	v_multiwindow_test_appkit_oracle_append(
		V_MULTIWINDOW_TEST_APPKIT_DRAWABLE_PRESENT,
		(uint64_t)(uintptr_t)drawable_ptr, (uint64_t)(uintptr_t)state_ptr, raw);
	return raw;
}

VMultiwindowNativePrimitive v_multiwindow_appkit_abort_frame(void *state_ptr,
		void *drawable_ptr) {
	VMultiwindowNativePrimitive raw =
		v_multiwindow_appkit_abort_frame_actual(state_ptr, drawable_ptr);
	v_multiwindow_test_appkit_oracle_append(
		V_MULTIWINDOW_TEST_APPKIT_DRAWABLE_ABORT,
		(uint64_t)(uintptr_t)drawable_ptr, (uint64_t)(uintptr_t)state_ptr, raw);
	return raw;
}

VMultiwindowNativePrimitive v_multiwindow_appkit_release_drawable(void *state_ptr,
		void *drawable_ptr) {
	const uint64_t identity = (uint64_t)(uintptr_t)drawable_ptr;
	VMultiwindowNativePrimitive raw =
		v_multiwindow_appkit_release_drawable_actual(state_ptr, drawable_ptr);
	v_multiwindow_test_appkit_oracle_append(
		V_MULTIWINDOW_TEST_APPKIT_DRAWABLE_RELEASE,
		identity, (uint64_t)(uintptr_t)state_ptr, raw);
	return raw;
}

VMultiwindowNativePrimitive v_multiwindow_appkit_begin_render_batch(void) {
	VMultiwindowNativePrimitive raw =
		v_multiwindow_appkit_begin_render_batch_actual();
	v_multiwindow_test_appkit_oracle_append(
		V_MULTIWINDOW_TEST_APPKIT_POOL_PUSH, 0, 0, raw);
	return raw;
}

VMultiwindowNativePrimitive v_multiwindow_appkit_end_render_batch(void *pool) {
	const uint64_t identity = (uint64_t)(uintptr_t)pool;
	VMultiwindowNativePrimitive raw =
		v_multiwindow_appkit_end_render_batch_actual(pool);
	v_multiwindow_test_appkit_oracle_append(
		V_MULTIWINDOW_TEST_APPKIT_POOL_POP,
		identity, 0, raw);
	return raw;
}
#endif
