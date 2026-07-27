#ifndef V_MULTIWINDOW_NATIVE_WIN32_LIFETIME_ORACLE_HELPERS_H
#define V_MULTIWINDOW_NATIVE_WIN32_LIFETIME_ORACLE_HELPERS_H

#if defined(SOKOL_TRACE_HOOKS) && defined(V_MULTIWINDOW_NATIVE_PROOF_TEST) && defined(SOKOL_D3D11)

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif

#include <stdint.h>
#include <string.h>
#include <windows.h>
#include "../win32_d3d11_backend_helpers.h"

#define V_MULTIWINDOW_TEST_WIN32_ORACLE_CAPACITY 512

enum VMultiwindowTestWin32OracleKind {
	V_MULTIWINDOW_TEST_WIN32_RELEASE = 1,
	V_MULTIWINDOW_TEST_WIN32_DEVICE_CREATE = 2,
	V_MULTIWINDOW_TEST_WIN32_DEVICE_QUERY = 3,
	V_MULTIWINDOW_TEST_WIN32_ADAPTER_ACQUIRE = 4,
	V_MULTIWINDOW_TEST_WIN32_FACTORY_ACQUIRE = 5,
	V_MULTIWINDOW_TEST_WIN32_SWAPCHAIN_CREATE = 6,
	V_MULTIWINDOW_TEST_WIN32_WINDOW_ASSOCIATION = 7,
	V_MULTIWINDOW_TEST_WIN32_BACKBUFFER_ACQUIRE = 8,
	V_MULTIWINDOW_TEST_WIN32_RENDER_VIEW_CREATE = 9,
	V_MULTIWINDOW_TEST_WIN32_TEXTURE_CREATE = 10,
	V_MULTIWINDOW_TEST_WIN32_DEPTH_VIEW_CREATE = 11,
	V_MULTIWINDOW_TEST_WIN32_CLEAR_STATE = 12,
	V_MULTIWINDOW_TEST_WIN32_RESIZE_BUFFERS = 13,
	V_MULTIWINDOW_TEST_WIN32_PRESENT = 14,
	V_MULTIWINDOW_TEST_WIN32_DEVICE_STATUS = 15,
	V_MULTIWINDOW_TEST_WIN32_REMOVAL_QUERY = 16
};

typedef struct VMultiwindowTestWin32OracleRecord {
	uint64_t sequence;
	uint64_t kind;
	uint64_t identity;
	uint64_t parent_identity;
	uint64_t output_identity;
	uint64_t auxiliary_identity;
	uint64_t observed_count;
	uint64_t valid_mask;
	int64_t return_value;
	uint64_t thread_identity;
} VMultiwindowTestWin32OracleRecord;

static VMultiwindowTestWin32OracleRecord
	v_multiwindow_test_win32_oracle_records[V_MULTIWINDOW_TEST_WIN32_ORACLE_CAPACITY];
static uint64_t v_multiwindow_test_win32_oracle_count;
static int v_multiwindow_test_win32_oracle_overflow;
static int v_multiwindow_test_win32_resize_invalid_call_armed;
static uint64_t v_multiwindow_test_win32_resize_invalid_call_consumed;

static inline void v_multiwindow_test_win32_oracle_record(uint64_t kind,
		uint64_t identity, uint64_t parent_identity,
		const VMultiwindowNativePrimitive *raw) {
	if (v_multiwindow_test_win32_oracle_count
			>= V_MULTIWINDOW_TEST_WIN32_ORACLE_CAPACITY) {
		v_multiwindow_test_win32_oracle_overflow = 1;
		return;
	}
	VMultiwindowTestWin32OracleRecord *record =
		&v_multiwindow_test_win32_oracle_records[v_multiwindow_test_win32_oracle_count++];
	record->sequence = v_multiwindow_test_win32_oracle_count;
	record->kind = kind;
	record->identity = identity;
	record->parent_identity = parent_identity;
	record->thread_identity = (uint64_t)GetCurrentThreadId();
	if (raw != NULL) {
		record->output_identity = raw->handle;
		record->auxiliary_identity = raw->object_identity_0;
		record->observed_count = raw->observed_count;
		record->valid_mask = raw->valid_mask;
		record->return_value = raw->return_value;
	}
}

static inline void v_multiwindow_test_win32_release(uint64_t identity,
		VMultiwindowNativePrimitive *out) {
	VMultiwindowNativePrimitive raw;
	memset(&raw, 0, sizeof(raw));
	if (identity != UINT64_C(0)) {
		ULONG remaining = V_MULTIWINDOW_COM_RELEASE((IUnknown *)(uintptr_t)identity);
		raw.observed_count = (uint64_t)remaining;
		raw.valid_mask = V_MULTIWINDOW_NATIVE_VALID_OBSERVED_COUNT;
	}
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_RELEASE,
		identity, UINT64_C(0), &raw);
	if (out != NULL) {
		*out = raw;
	}
}

static inline void v_multiwindow_test_win32_device_create_attempt(int64_t driver,
		int64_t feature_list, VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_d3d11_create_device_attempt(driver, feature_list, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_DEVICE_CREATE,
		(uint64_t)driver, (uint64_t)feature_list, out);
}

static inline void v_multiwindow_test_win32_query_dxgi_device(uint64_t device,
		VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_d3d11_query_dxgi_device(device, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_DEVICE_QUERY,
		device, UINT64_C(0), out);
}

static inline void v_multiwindow_test_win32_get_adapter(uint64_t device,
		uint64_t dxgi_device, VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_dxgi_get_adapter(device, dxgi_device, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_ADAPTER_ACQUIRE,
		dxgi_device, device, out);
}

static inline void v_multiwindow_test_win32_get_factory(uint64_t device,
		uint64_t adapter, VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_dxgi_get_factory(device, adapter, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_FACTORY_ACQUIRE,
		adapter, device, out);
}

static inline void v_multiwindow_test_win32_create_swapchain(uint64_t factory,
		uint64_t device, uint64_t hwnd, int64_t width, int64_t height,
		VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_dxgi_create_swapchain(factory, device, hwnd, width, height, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_SWAPCHAIN_CREATE,
		factory, hwnd, out);
}

static inline void v_multiwindow_test_win32_window_association(uint64_t factory,
		uint64_t device, uint64_t hwnd, VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_dxgi_make_window_association(factory, device, hwnd, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_WINDOW_ASSOCIATION,
		factory, hwnd, out);
}

static inline void v_multiwindow_test_win32_get_backbuffer(uint64_t device,
		uint64_t swapchain, VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_dxgi_get_backbuffer(device, swapchain, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_BACKBUFFER_ACQUIRE,
		swapchain, device, out);
}

static inline void v_multiwindow_test_win32_create_render_view(uint64_t device,
		uint64_t resource, VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_d3d11_create_render_view(device, resource, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_RENDER_VIEW_CREATE,
		resource, device, out);
}

static inline void v_multiwindow_test_win32_create_texture(uint64_t device,
		int64_t width, int64_t height, int64_t depth,
		VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_d3d11_create_texture(device, width, height, depth, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_TEXTURE_CREATE,
		device, (uint64_t)depth, out);
}

static inline void v_multiwindow_test_win32_create_depth_view(uint64_t device,
		uint64_t texture, VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_d3d11_create_depth_view(device, texture, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_DEPTH_VIEW_CREATE,
		texture, device, out);
}

static inline void v_multiwindow_test_win32_clear_state(uint64_t context,
		VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_d3d11_clear_state(context, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_CLEAR_STATE,
		context, UINT64_C(0), out);
}

static inline void v_multiwindow_test_win32_resize_buffers(uint64_t device,
		uint64_t swapchain, int64_t width, int64_t height,
		VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_dxgi_resize_buffers(device, swapchain, width, height, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_RESIZE_BUFFERS,
		swapchain, device, out);
	if (v_multiwindow_test_win32_resize_invalid_call_armed && out != NULL) {
		v_multiwindow_test_win32_resize_invalid_call_armed = 0;
		v_multiwindow_test_win32_resize_invalid_call_consumed++;
		v_multiwindow_win32_result(out, DXGI_ERROR_INVALID_CALL);
	}
}

static inline void v_multiwindow_test_win32_present(uint64_t device,
		uint64_t swapchain, VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_dxgi_present(device, swapchain, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_PRESENT,
		swapchain, device, out);
}

static inline void v_multiwindow_test_win32_device_status(uint64_t device,
		VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_d3d11_device_status(device, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_DEVICE_STATUS,
		device, UINT64_C(0), out);
}

static inline void v_multiwindow_test_win32_removal_query(uint64_t device,
		VMultiwindowNativePrimitive *out) {
	v_multiwindow_win32_d3d11_get_removed_reason(device, out);
	v_multiwindow_test_win32_oracle_record(V_MULTIWINDOW_TEST_WIN32_REMOVAL_QUERY,
		device, UINT64_C(0), out);
}

#define v_multiwindow_win32_release v_multiwindow_test_win32_release
#define v_multiwindow_win32_d3d11_create_device_attempt v_multiwindow_test_win32_device_create_attempt
#define v_multiwindow_win32_d3d11_query_dxgi_device v_multiwindow_test_win32_query_dxgi_device
#define v_multiwindow_win32_dxgi_get_adapter v_multiwindow_test_win32_get_adapter
#define v_multiwindow_win32_dxgi_get_factory v_multiwindow_test_win32_get_factory
#define v_multiwindow_win32_dxgi_create_swapchain v_multiwindow_test_win32_create_swapchain
#define v_multiwindow_win32_dxgi_make_window_association v_multiwindow_test_win32_window_association
#define v_multiwindow_win32_dxgi_get_backbuffer v_multiwindow_test_win32_get_backbuffer
#define v_multiwindow_win32_d3d11_create_render_view v_multiwindow_test_win32_create_render_view
#define v_multiwindow_win32_d3d11_create_texture v_multiwindow_test_win32_create_texture
#define v_multiwindow_win32_d3d11_create_depth_view v_multiwindow_test_win32_create_depth_view
#define v_multiwindow_win32_d3d11_clear_state v_multiwindow_test_win32_clear_state
#define v_multiwindow_win32_dxgi_resize_buffers v_multiwindow_test_win32_resize_buffers
#define v_multiwindow_win32_dxgi_present v_multiwindow_test_win32_present
#define v_multiwindow_win32_d3d11_device_status v_multiwindow_test_win32_device_status
#define v_multiwindow_win32_d3d11_get_removed_reason v_multiwindow_test_win32_removal_query

typedef struct VMultiwindowTestIUnknown {
	IUnknown iface;
	volatile LONG references;
} VMultiwindowTestIUnknown;

static HRESULT STDMETHODCALLTYPE v_multiwindow_test_iunknown_query_interface(
		IUnknown *self, REFIID iid, void **out) {
	(void)iid;
	if (out == NULL) {
		return E_POINTER;
	}
	VMultiwindowTestIUnknown *object = (VMultiwindowTestIUnknown *)self;
	InterlockedIncrement(&object->references);
	*out = self;
	return S_OK;
}

static ULONG STDMETHODCALLTYPE v_multiwindow_test_iunknown_add_ref(IUnknown *self) {
	VMultiwindowTestIUnknown *object = (VMultiwindowTestIUnknown *)self;
	return (ULONG)InterlockedIncrement(&object->references);
}

static ULONG STDMETHODCALLTYPE v_multiwindow_test_iunknown_release(IUnknown *self) {
	VMultiwindowTestIUnknown *object = (VMultiwindowTestIUnknown *)self;
	return (ULONG)InterlockedDecrement(&object->references);
}

static IUnknownVtbl v_multiwindow_test_iunknown_vtable = {
	v_multiwindow_test_iunknown_query_interface,
	v_multiwindow_test_iunknown_add_ref,
	v_multiwindow_test_iunknown_release
};

static VMultiwindowTestIUnknown v_multiwindow_test_iunknown = {
	{&v_multiwindow_test_iunknown_vtable}, 0
};

static inline void v_multiwindow_test_win32_fake_unknown_reset(uint64_t references) {
	v_multiwindow_test_iunknown.references = (LONG)references;
}

static inline uint64_t v_multiwindow_test_win32_fake_unknown_identity(void) {
	return (uint64_t)(uintptr_t)&v_multiwindow_test_iunknown.iface;
}

static inline uint64_t v_multiwindow_test_win32_fake_unknown_references(void) {
	return (uint64_t)v_multiwindow_test_iunknown.references;
}

#define V_MULTIWINDOW_TEST_WIN32_FAKE_D3D_CAPACITY 16

enum VMultiwindowTestWin32FakeD3DKind {
	V_MULTIWINDOW_TEST_WIN32_FAKE_DEVICE_RELEASE = 1,
	V_MULTIWINDOW_TEST_WIN32_FAKE_CONTEXT_RELEASE = 2,
	V_MULTIWINDOW_TEST_WIN32_FAKE_CONTEXT_CLEAR_STATE = 3,
	V_MULTIWINDOW_TEST_WIN32_FAKE_CONTEXT_FLUSH = 4,
	V_MULTIWINDOW_TEST_WIN32_FAKE_DEVICE_REMOVED_REASON = 5
};

typedef struct VMultiwindowTestWin32FakeD3DRecord {
	uint64_t kind;
	uint64_t identity;
	uint64_t remaining;
} VMultiwindowTestWin32FakeD3DRecord;

typedef struct VMultiwindowTestWin32FakeDevice {
	ID3D11Device iface;
	volatile LONG references;
} VMultiwindowTestWin32FakeDevice;

typedef struct VMultiwindowTestWin32FakeContext {
	ID3D11DeviceContext iface;
	volatile LONG references;
} VMultiwindowTestWin32FakeContext;

static ID3D11DeviceVtbl v_multiwindow_test_win32_fake_device_vtable;
static ID3D11DeviceContextVtbl v_multiwindow_test_win32_fake_context_vtable;
static VMultiwindowTestWin32FakeDevice v_multiwindow_test_win32_fake_device;
static VMultiwindowTestWin32FakeContext v_multiwindow_test_win32_fake_context;
static VMultiwindowTestWin32FakeD3DRecord
	v_multiwindow_test_win32_fake_d3d_records[V_MULTIWINDOW_TEST_WIN32_FAKE_D3D_CAPACITY];
static uint64_t v_multiwindow_test_win32_fake_d3d_count_value;
static int v_multiwindow_test_win32_fake_d3d_overflow_value;
static int v_multiwindow_test_win32_fake_d3d_initialized;

static inline void v_multiwindow_test_win32_fake_d3d_record(uint64_t kind,
		uint64_t identity, uint64_t remaining) {
	if (v_multiwindow_test_win32_fake_d3d_count_value
			>= V_MULTIWINDOW_TEST_WIN32_FAKE_D3D_CAPACITY) {
		v_multiwindow_test_win32_fake_d3d_overflow_value = 1;
		return;
	}
	VMultiwindowTestWin32FakeD3DRecord *record =
		&v_multiwindow_test_win32_fake_d3d_records[
			v_multiwindow_test_win32_fake_d3d_count_value++];
	record->kind = kind;
	record->identity = identity;
	record->remaining = remaining;
}

static HRESULT STDMETHODCALLTYPE v_multiwindow_test_win32_fake_device_query_interface(
		ID3D11Device *self, REFIID iid, void **out) {
	(void)iid;
	if (out == NULL) {
		return E_POINTER;
	}
	VMultiwindowTestWin32FakeDevice *object =
		(VMultiwindowTestWin32FakeDevice *)self;
	InterlockedIncrement(&object->references);
	*out = self;
	return S_OK;
}

static ULONG STDMETHODCALLTYPE v_multiwindow_test_win32_fake_device_add_ref(
		ID3D11Device *self) {
	VMultiwindowTestWin32FakeDevice *object =
		(VMultiwindowTestWin32FakeDevice *)self;
	return (ULONG)InterlockedIncrement(&object->references);
}

static ULONG STDMETHODCALLTYPE v_multiwindow_test_win32_fake_device_release(
		ID3D11Device *self) {
	VMultiwindowTestWin32FakeDevice *object =
		(VMultiwindowTestWin32FakeDevice *)self;
	ULONG remaining = (ULONG)InterlockedDecrement(&object->references);
	v_multiwindow_test_win32_fake_d3d_record(
		V_MULTIWINDOW_TEST_WIN32_FAKE_DEVICE_RELEASE,
		(uint64_t)(uintptr_t)self, (uint64_t)remaining);
	return remaining;
}

static HRESULT STDMETHODCALLTYPE v_multiwindow_test_win32_fake_device_removed_reason(
		ID3D11Device *self) {
	v_multiwindow_test_win32_fake_d3d_record(
		V_MULTIWINDOW_TEST_WIN32_FAKE_DEVICE_REMOVED_REASON,
		(uint64_t)(uintptr_t)self, UINT64_C(0));
	return DXGI_ERROR_DEVICE_REMOVED;
}

static HRESULT STDMETHODCALLTYPE v_multiwindow_test_win32_fake_context_query_interface(
		ID3D11DeviceContext *self, REFIID iid, void **out) {
	(void)iid;
	if (out == NULL) {
		return E_POINTER;
	}
	VMultiwindowTestWin32FakeContext *object =
		(VMultiwindowTestWin32FakeContext *)self;
	InterlockedIncrement(&object->references);
	*out = self;
	return S_OK;
}

static ULONG STDMETHODCALLTYPE v_multiwindow_test_win32_fake_context_add_ref(
		ID3D11DeviceContext *self) {
	VMultiwindowTestWin32FakeContext *object =
		(VMultiwindowTestWin32FakeContext *)self;
	return (ULONG)InterlockedIncrement(&object->references);
}

static ULONG STDMETHODCALLTYPE v_multiwindow_test_win32_fake_context_release(
		ID3D11DeviceContext *self) {
	VMultiwindowTestWin32FakeContext *object =
		(VMultiwindowTestWin32FakeContext *)self;
	ULONG remaining = (ULONG)InterlockedDecrement(&object->references);
	v_multiwindow_test_win32_fake_d3d_record(
		V_MULTIWINDOW_TEST_WIN32_FAKE_CONTEXT_RELEASE,
		(uint64_t)(uintptr_t)self, (uint64_t)remaining);
	return remaining;
}

static void STDMETHODCALLTYPE v_multiwindow_test_win32_fake_context_clear_state(
		ID3D11DeviceContext *self) {
	v_multiwindow_test_win32_fake_d3d_record(
		V_MULTIWINDOW_TEST_WIN32_FAKE_CONTEXT_CLEAR_STATE,
		(uint64_t)(uintptr_t)self, UINT64_C(0));
}

static void STDMETHODCALLTYPE v_multiwindow_test_win32_fake_context_flush(
		ID3D11DeviceContext *self) {
	v_multiwindow_test_win32_fake_d3d_record(
		V_MULTIWINDOW_TEST_WIN32_FAKE_CONTEXT_FLUSH,
		(uint64_t)(uintptr_t)self, UINT64_C(0));
}

static inline void v_multiwindow_test_win32_fake_d3d_initialize(void) {
	if (v_multiwindow_test_win32_fake_d3d_initialized) {
		return;
	}
	memset(&v_multiwindow_test_win32_fake_device_vtable, 0,
		sizeof(v_multiwindow_test_win32_fake_device_vtable));
	v_multiwindow_test_win32_fake_device_vtable.QueryInterface =
		v_multiwindow_test_win32_fake_device_query_interface;
	v_multiwindow_test_win32_fake_device_vtable.AddRef =
		v_multiwindow_test_win32_fake_device_add_ref;
	v_multiwindow_test_win32_fake_device_vtable.Release =
		v_multiwindow_test_win32_fake_device_release;
	v_multiwindow_test_win32_fake_device_vtable.GetDeviceRemovedReason =
		v_multiwindow_test_win32_fake_device_removed_reason;
	memset(&v_multiwindow_test_win32_fake_context_vtable, 0,
		sizeof(v_multiwindow_test_win32_fake_context_vtable));
	v_multiwindow_test_win32_fake_context_vtable.QueryInterface =
		v_multiwindow_test_win32_fake_context_query_interface;
	v_multiwindow_test_win32_fake_context_vtable.AddRef =
		v_multiwindow_test_win32_fake_context_add_ref;
	v_multiwindow_test_win32_fake_context_vtable.Release =
		v_multiwindow_test_win32_fake_context_release;
	v_multiwindow_test_win32_fake_context_vtable.ClearState =
		v_multiwindow_test_win32_fake_context_clear_state;
	v_multiwindow_test_win32_fake_context_vtable.Flush =
		v_multiwindow_test_win32_fake_context_flush;
	v_multiwindow_test_win32_fake_device.iface.lpVtbl =
		&v_multiwindow_test_win32_fake_device_vtable;
	v_multiwindow_test_win32_fake_context.iface.lpVtbl =
		&v_multiwindow_test_win32_fake_context_vtable;
	v_multiwindow_test_win32_fake_d3d_initialized = 1;
}

static inline void v_multiwindow_test_win32_fake_d3d_reset(void) {
	v_multiwindow_test_win32_fake_d3d_initialize();
	memset(v_multiwindow_test_win32_fake_d3d_records, 0,
		sizeof(v_multiwindow_test_win32_fake_d3d_records));
	v_multiwindow_test_win32_fake_d3d_count_value = UINT64_C(0);
	v_multiwindow_test_win32_fake_d3d_overflow_value = 0;
	v_multiwindow_test_win32_fake_device.references = 1;
	v_multiwindow_test_win32_fake_context.references = 1;
}

static inline uint64_t v_multiwindow_test_win32_fake_d3d_device_identity(void) {
	v_multiwindow_test_win32_fake_d3d_initialize();
	return (uint64_t)(uintptr_t)&v_multiwindow_test_win32_fake_device.iface;
}

static inline uint64_t v_multiwindow_test_win32_fake_d3d_context_identity(void) {
	v_multiwindow_test_win32_fake_d3d_initialize();
	return (uint64_t)(uintptr_t)&v_multiwindow_test_win32_fake_context.iface;
}

static inline uint64_t v_multiwindow_test_win32_fake_d3d_count(void) {
	return v_multiwindow_test_win32_fake_d3d_count_value;
}

static inline uint64_t v_multiwindow_test_win32_fake_d3d_kind(uint64_t index) {
	return index < v_multiwindow_test_win32_fake_d3d_count_value
		? v_multiwindow_test_win32_fake_d3d_records[index].kind : UINT64_C(0);
}

static inline uint64_t v_multiwindow_test_win32_fake_d3d_identity(uint64_t index) {
	return index < v_multiwindow_test_win32_fake_d3d_count_value
		? v_multiwindow_test_win32_fake_d3d_records[index].identity : UINT64_C(0);
}

static inline uint64_t v_multiwindow_test_win32_fake_d3d_remaining(uint64_t index) {
	return index < v_multiwindow_test_win32_fake_d3d_count_value
		? v_multiwindow_test_win32_fake_d3d_records[index].remaining : UINT64_C(0);
}

static inline int v_multiwindow_test_win32_fake_d3d_overflow(void) {
	return v_multiwindow_test_win32_fake_d3d_overflow_value;
}

static inline int v_multiwindow_test_win32_resize_invalid_call_arm(void) {
	if (v_multiwindow_test_win32_resize_invalid_call_armed || v_multiwindow_test_win32_resize_invalid_call_consumed != UINT64_C(0)) {
		return 0;
	}
	v_multiwindow_test_win32_resize_invalid_call_armed = 1;
	return 1;
}

static inline void v_multiwindow_test_win32_resize_invalid_call_reset(void) {
	v_multiwindow_test_win32_resize_invalid_call_armed = 0;
	v_multiwindow_test_win32_resize_invalid_call_consumed = UINT64_C(0);
}

static inline int v_multiwindow_test_win32_resize_invalid_call_consumed_once(void) {
	return !v_multiwindow_test_win32_resize_invalid_call_armed && v_multiwindow_test_win32_resize_invalid_call_consumed == UINT64_C(1);
}
static inline int64_t v_multiwindow_test_win32_resize_invalid_call_code(void) { return (int64_t)(int32_t)DXGI_ERROR_INVALID_CALL; }
static inline uint64_t v_multiwindow_test_win32_release_kind(void) { return (uint64_t)V_MULTIWINDOW_TEST_WIN32_RELEASE; }
static inline uint64_t v_multiwindow_test_win32_resize_buffers_kind(void) { return (uint64_t)V_MULTIWINDOW_TEST_WIN32_RESIZE_BUFFERS; }

static inline void v_multiwindow_test_win32_oracle_reset(void) {
	memset(v_multiwindow_test_win32_oracle_records, 0,
		sizeof(v_multiwindow_test_win32_oracle_records));
	v_multiwindow_test_win32_oracle_count = UINT64_C(0);
	v_multiwindow_test_win32_oracle_overflow = 0;
	v_multiwindow_test_win32_resize_invalid_call_reset();
}

static inline uint64_t v_multiwindow_test_win32_oracle_count_get(void) {
	return v_multiwindow_test_win32_oracle_count;
}

static inline int v_multiwindow_test_win32_oracle_overflow_get(void) {
	return v_multiwindow_test_win32_oracle_overflow;
}

static inline VMultiwindowTestWin32OracleRecord
v_multiwindow_test_win32_oracle_record_get(uint64_t index) {
	if (index >= v_multiwindow_test_win32_oracle_count) {
		VMultiwindowTestWin32OracleRecord empty;
		memset(&empty, 0, sizeof(empty));
		return empty;
	}
	return v_multiwindow_test_win32_oracle_records[index];
}

#endif

#endif
