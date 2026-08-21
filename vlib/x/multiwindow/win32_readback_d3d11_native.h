#ifndef V_MULTIWINDOW_WIN32_READBACK_D3D11_NATIVE_H
#define V_MULTIWINDOW_WIN32_READBACK_D3D11_NATIVE_H

#include <d3d11.h>
#include <dxgi1_2.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "win32_readback_d3d11_diagnostics.h"
#include "win32_readback_d3d11_helpers.h"

#define V_MULTIWINDOW_WIN32_D3D11_READBACK_NATIVE_MAGIC UINT64_C(0x564D574433443131)

typedef struct VMultiwindowWin32D3D11ReadbackNative {
	uint64_t magic;
	ID3D11Device *device;
	ID3D11DeviceContext *context;
	VMultiwindowWin32ReadbackMachine *machine;
	int32_t last_hresult;
	int32_t device_removed_reason;
} VMultiwindowWin32D3D11ReadbackNative;

static const IID v_multiwindow_win32_readback_iid_texture2d = {
	0x6f15aaf2, 0xd208, 0x4e89,
	{0x9a, 0xb4, 0x48, 0x95, 0x35, 0xd3, 0x4f, 0x9c}};

static ULONG v_multiwindow_win32_readback_com_add_ref(void *object) {
	if (object == NULL) {
		return 0;
	}
#if defined(__cplusplus)
	return ((IUnknown *)object)->AddRef();
#else
	IUnknown *unknown = (IUnknown *)object;
	return unknown->lpVtbl->AddRef(unknown);
#endif
}

static ULONG v_multiwindow_win32_readback_com_release(void *object) {
	if (object == NULL) {
		return 0;
	}
#if defined(__cplusplus)
	return ((IUnknown *)object)->Release();
#else
	IUnknown *unknown = (IUnknown *)object;
	return unknown->lpVtbl->Release(unknown);
#endif
}

static void v_multiwindow_win32_readback_texture_desc(
		ID3D11Texture2D *texture, D3D11_TEXTURE2D_DESC *desc) {
#if defined(__cplusplus)
	texture->GetDesc(desc);
#else
	texture->lpVtbl->GetDesc(texture, desc);
#endif
}

static void v_multiwindow_win32_readback_texture_device(
		ID3D11Texture2D *texture, ID3D11Device **device) {
#if defined(__cplusplus)
	texture->GetDevice(device);
#else
	texture->lpVtbl->GetDevice(texture, device);
#endif
}

static void v_multiwindow_win32_readback_context_device(
		ID3D11DeviceContext *context, ID3D11Device **device) {
#if defined(__cplusplus)
	context->GetDevice(device);
#else
	context->lpVtbl->GetDevice(context, device);
#endif
}

static D3D11_DEVICE_CONTEXT_TYPE v_multiwindow_win32_readback_context_type(
		ID3D11DeviceContext *context) {
#if defined(__cplusplus)
	return context->GetType();
#else
	return context->lpVtbl->GetType(context);
#endif
}

static HRESULT v_multiwindow_win32_readback_create_texture(
		ID3D11Device *device, const D3D11_TEXTURE2D_DESC *desc,
		ID3D11Texture2D **texture) {
#if defined(__cplusplus)
	return device->CreateTexture2D(desc, NULL, texture);
#else
	return device->lpVtbl->CreateTexture2D(device, desc, NULL, texture);
#endif
}

static HRESULT v_multiwindow_win32_readback_create_query(
		ID3D11Device *device, const D3D11_QUERY_DESC *desc,
		ID3D11Query **query) {
#if defined(__cplusplus)
	return device->CreateQuery(desc, query);
#else
	return device->lpVtbl->CreateQuery(device, desc, query);
#endif
}

static void v_multiwindow_win32_readback_copy_region(
		ID3D11DeviceContext *context, ID3D11Resource *destination,
		ID3D11Resource *source, const D3D11_BOX *box) {
#if defined(__cplusplus)
	context->CopySubresourceRegion(destination, 0, 0, 0, 0, source, 0, box);
#else
	context->lpVtbl->CopySubresourceRegion(context, destination, 0, 0, 0, 0,
		source, 0, box);
#endif
}

static void v_multiwindow_win32_readback_end_query(
		ID3D11DeviceContext *context, ID3D11Query *query) {
#if defined(__cplusplus)
	context->End(query);
#else
	context->lpVtbl->End(context, (ID3D11Asynchronous *)query);
#endif
}

static HRESULT v_multiwindow_win32_readback_get_data(
		ID3D11DeviceContext *context, ID3D11Query *query) {
#if defined(__cplusplus)
	return context->GetData(query, NULL, 0, D3D11_ASYNC_GETDATA_DONOTFLUSH);
#else
	return context->lpVtbl->GetData(context, (ID3D11Asynchronous *)query, NULL,
		0, D3D11_ASYNC_GETDATA_DONOTFLUSH);
#endif
}

static HRESULT v_multiwindow_win32_readback_map_texture(
		ID3D11DeviceContext *context, ID3D11Texture2D *texture,
		D3D11_MAPPED_SUBRESOURCE *mapped) {
#if defined(__cplusplus)
	return context->Map(texture, 0, D3D11_MAP_READ,
		D3D11_MAP_FLAG_DO_NOT_WAIT, mapped);
#else
	return context->lpVtbl->Map(context, (ID3D11Resource *)texture, 0,
		D3D11_MAP_READ, D3D11_MAP_FLAG_DO_NOT_WAIT, mapped);
#endif
}

static void v_multiwindow_win32_readback_unmap_texture(
		ID3D11DeviceContext *context, ID3D11Texture2D *texture) {
#if defined(__cplusplus)
	context->Unmap(texture, 0);
#else
	context->lpVtbl->Unmap(context, (ID3D11Resource *)texture, 0);
#endif
}

static HRESULT v_multiwindow_win32_readback_removed_reason(
		ID3D11Device *device) {
#if defined(__cplusplus)
	return device->GetDeviceRemovedReason();
#else
	return device->lpVtbl->GetDeviceRemovedReason(device);
#endif
}

static HRESULT v_multiwindow_win32_readback_swapchain_buffer(
		IDXGISwapChain *swapchain, ID3D11Texture2D **texture) {
#if defined(__cplusplus)
	return swapchain->GetBuffer(0, v_multiwindow_win32_readback_iid_texture2d,
		(void **)texture);
#else
	return swapchain->lpVtbl->GetBuffer(swapchain, 0,
		&v_multiwindow_win32_readback_iid_texture2d, (void **)texture);
#endif
}

static int v_multiwindow_win32_readback_native_valid(
		const VMultiwindowWin32D3D11ReadbackNative *native) {
	return native != NULL
		&& native->magic == V_MULTIWINDOW_WIN32_D3D11_READBACK_NATIVE_MAGIC
		&& native->device != NULL && native->context != NULL
		&& native->machine != NULL;
}

static int v_multiwindow_win32_readback_source_device_matches(
		VMultiwindowWin32D3D11ReadbackNative *native,
		ID3D11Texture2D *texture) {
	ID3D11Device *source_device = NULL;
	v_multiwindow_win32_readback_texture_device(texture, &source_device);
	int matches = source_device != NULL && source_device == native->device;
	v_multiwindow_win32_readback_com_release(source_device);
	return matches;
}

static int v_multiwindow_win32_readback_native_format(
		DXGI_FORMAT format) {
	switch (format) {
	case DXGI_FORMAT_B8G8R8A8_UNORM:
	case DXGI_FORMAT_B8G8R8A8_UNORM_SRGB:
		return V_MW_D3D11_TEST_FORMAT_BGRA8;
	case DXGI_FORMAT_R8G8B8A8_UNORM:
	case DXGI_FORMAT_R8G8B8A8_UNORM_SRGB:
		return V_MW_D3D11_TEST_FORMAT_RGBA8;
	default:
		return 0;
	}
}

static int v_multiwindow_win32_readback_native_retain_source(
		void *world, void *source) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)world;
	if (!v_multiwindow_win32_readback_native_valid(native) || source == NULL
			|| !v_multiwindow_win32_readback_source_device_matches(native,
				(ID3D11Texture2D *)source)) {
		if (native != NULL) {
			native->last_hresult = (int32_t)E_INVALIDARG;
		}
		return 0;
	}
	v_multiwindow_win32_readback_com_add_ref(source);
	native->last_hresult = (int32_t)S_OK;
	return 1;
}

static void v_multiwindow_win32_readback_native_release_source(
		void *world, void *source) {
	(void)world;
	v_multiwindow_win32_readback_com_release(source);
}

static int v_multiwindow_win32_readback_native_describe_source(
		void *world, void *source, uint32_t *width, uint32_t *height,
		uint32_t *format) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)world;
	if (!v_multiwindow_win32_readback_native_valid(native) || source == NULL
			|| width == NULL || height == NULL || format == NULL
			|| !v_multiwindow_win32_readback_source_device_matches(native,
				(ID3D11Texture2D *)source)) {
		if (native != NULL) {
			native->last_hresult = (int32_t)E_INVALIDARG;
		}
		return (int32_t)E_INVALIDARG;
	}
	D3D11_TEXTURE2D_DESC desc;
	memset(&desc, 0, sizeof(desc));
	v_multiwindow_win32_readback_texture_desc((ID3D11Texture2D *)source, &desc);
	int native_format = v_multiwindow_win32_readback_native_format(desc.Format);
	if (native_format == 0 || desc.Width == 0 || desc.Height == 0
			|| desc.MipLevels == 0 || desc.ArraySize != 1
			|| desc.SampleDesc.Count != 1) {
		native->last_hresult = (int32_t)DXGI_ERROR_UNSUPPORTED;
		return (int32_t)DXGI_ERROR_UNSUPPORTED;
	}
	*width = desc.Width;
	*height = desc.Height;
	*format = (uint32_t)native_format;
	native->last_hresult = (int32_t)S_OK;
	return V_MW_D3D11_TEST_OK;
}

static int v_multiwindow_win32_readback_native_copy_region(
		void *world, void *source, uint32_t x, uint32_t y, uint32_t width,
		uint32_t height, void **out_staging) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)world;
	if (!v_multiwindow_win32_readback_native_valid(native) || source == NULL
			|| out_staging == NULL || width == 0 || height == 0) {
		if (native != NULL) {
			native->last_hresult = (int32_t)E_INVALIDARG;
		}
		return (int32_t)E_INVALIDARG;
	}
	*out_staging = NULL;
	D3D11_TEXTURE2D_DESC source_desc;
	memset(&source_desc, 0, sizeof(source_desc));
	v_multiwindow_win32_readback_texture_desc((ID3D11Texture2D *)source,
		&source_desc);
	if (v_multiwindow_win32_readback_native_format(source_desc.Format) == 0
			|| source_desc.ArraySize != 1 || source_desc.MipLevels == 0
			|| source_desc.SampleDesc.Count != 1 || x > source_desc.Width
			|| y > source_desc.Height || width > source_desc.Width - x
			|| height > source_desc.Height - y) {
		native->last_hresult = (int32_t)DXGI_ERROR_UNSUPPORTED;
		return (int32_t)DXGI_ERROR_UNSUPPORTED;
	}
	D3D11_TEXTURE2D_DESC staging_desc;
	memset(&staging_desc, 0, sizeof(staging_desc));
	staging_desc.Width = width;
	staging_desc.Height = height;
	staging_desc.MipLevels = 1;
	staging_desc.ArraySize = 1;
	staging_desc.Format = source_desc.Format;
	staging_desc.SampleDesc.Count = 1;
	staging_desc.Usage = D3D11_USAGE_STAGING;
	staging_desc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
	ID3D11Texture2D *staging = NULL;
	HRESULT result = v_multiwindow_win32_readback_create_texture(native->device,
		&staging_desc, &staging);
	native->last_hresult = (int32_t)result;
	if (FAILED(result) || staging == NULL) {
		v_multiwindow_win32_readback_com_release(staging);
		return (int32_t)(FAILED(result) ? result : E_FAIL);
	}
	D3D11_BOX box;
	box.left = x;
	box.top = y;
	box.front = 0;
	box.right = x + width;
	box.bottom = y + height;
	box.back = 1;
	v_multiwindow_win32_readback_copy_region(native->context,
		(ID3D11Resource *)staging, (ID3D11Resource *)source, &box);
	*out_staging = staging;
	native->last_hresult = (int32_t)S_OK;
	return V_MW_D3D11_TEST_OK;
}

static int v_multiwindow_win32_readback_native_end_event_query(
		void *world, void **out_query) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)world;
	if (!v_multiwindow_win32_readback_native_valid(native)
			|| out_query == NULL) {
		if (native != NULL) {
			native->last_hresult = (int32_t)E_INVALIDARG;
		}
		return (int32_t)E_INVALIDARG;
	}
	*out_query = NULL;
	D3D11_QUERY_DESC desc;
	memset(&desc, 0, sizeof(desc));
	desc.Query = D3D11_QUERY_EVENT;
	ID3D11Query *query = NULL;
	HRESULT result = v_multiwindow_win32_readback_create_query(native->device,
		&desc, &query);
	native->last_hresult = (int32_t)result;
	if (FAILED(result) || query == NULL) {
		v_multiwindow_win32_readback_com_release(query);
		return (int32_t)(FAILED(result) ? result : E_FAIL);
	}
	v_multiwindow_win32_readback_end_query(native->context, query);
	*out_query = query;
	native->last_hresult = (int32_t)S_OK;
	return V_MW_D3D11_TEST_OK;
}

static int v_multiwindow_win32_readback_native_get_event_data(
		void *world, void *query) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)world;
	if (!v_multiwindow_win32_readback_native_valid(native) || query == NULL) {
		if (native != NULL) {
			native->last_hresult = (int32_t)E_INVALIDARG;
		}
		return (int32_t)E_INVALIDARG;
	}
	HRESULT result = v_multiwindow_win32_readback_get_data(native->context,
		(ID3D11Query *)query);
	native->last_hresult = (int32_t)result;
	return (int32_t)result;
}

static int v_multiwindow_win32_readback_native_map_staging(
		void *world, void *staging, const uint8_t **data, size_t *row_pitch) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)world;
	if (!v_multiwindow_win32_readback_native_valid(native) || staging == NULL
			|| data == NULL || row_pitch == NULL) {
		if (native != NULL) {
			native->last_hresult = (int32_t)E_INVALIDARG;
		}
		return (int32_t)E_INVALIDARG;
	}
	*data = NULL;
	*row_pitch = 0;
	D3D11_MAPPED_SUBRESOURCE mapped;
	memset(&mapped, 0, sizeof(mapped));
	HRESULT result = v_multiwindow_win32_readback_map_texture(native->context,
		(ID3D11Texture2D *)staging, &mapped);
	native->last_hresult = (int32_t)result;
	if (FAILED(result)) {
		return (int32_t)result;
	}
	if (mapped.pData == NULL || mapped.RowPitch == 0) {
		v_multiwindow_win32_readback_unmap_texture(native->context,
			(ID3D11Texture2D *)staging);
		native->last_hresult = (int32_t)E_FAIL;
		return (int32_t)E_FAIL;
	}
	*data = (const uint8_t *)mapped.pData;
	*row_pitch = (size_t)mapped.RowPitch;
	return V_MW_D3D11_TEST_OK;
}

static void v_multiwindow_win32_readback_native_unmap_staging(
		void *world, void *staging) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)world;
	if (v_multiwindow_win32_readback_native_valid(native) && staging != NULL) {
		v_multiwindow_win32_readback_unmap_texture(native->context,
			(ID3D11Texture2D *)staging);
	}
}

static void v_multiwindow_win32_readback_native_release_staging(
		void *world, void *staging) {
	(void)world;
	v_multiwindow_win32_readback_com_release(staging);
}

static void v_multiwindow_win32_readback_native_release_query(
		void *world, void *query) {
	(void)world;
	v_multiwindow_win32_readback_com_release(query);
}

static int v_multiwindow_win32_readback_native_device_removed_reason(
		void *world) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)world;
	if (!v_multiwindow_win32_readback_native_valid(native)) {
		return (int32_t)E_INVALIDARG;
	}
	HRESULT result = v_multiwindow_win32_readback_removed_reason(native->device);
	v_multiwindow_win32_readback_record_device_removed_reason(
		native->last_hresult, &native->device_removed_reason, (int32_t)result);
	return (int32_t)result;
}

static const VMultiwindowWin32ReadbackOps
v_multiwindow_win32_readback_native_ops = {
	v_multiwindow_win32_readback_native_retain_source,
	v_multiwindow_win32_readback_native_release_source,
	v_multiwindow_win32_readback_native_describe_source,
	v_multiwindow_win32_readback_native_copy_region,
	v_multiwindow_win32_readback_native_end_event_query,
	v_multiwindow_win32_readback_native_get_event_data,
	v_multiwindow_win32_readback_native_map_staging,
	v_multiwindow_win32_readback_native_unmap_staging,
	v_multiwindow_win32_readback_native_release_staging,
	v_multiwindow_win32_readback_native_release_query,
	v_multiwindow_win32_readback_native_device_removed_reason,
};

static void *v_multiwindow_win32_d3d11_readback_native_create(
		uint64_t device_identity, uint64_t context_identity,
		uint64_t state_identity, uint64_t renderer_generation) {
	ID3D11Device *device = (ID3D11Device *)(uintptr_t)device_identity;
	ID3D11DeviceContext *context =
		(ID3D11DeviceContext *)(uintptr_t)context_identity;
	if (device == NULL || context == NULL || state_identity == 0
			|| renderer_generation == 0
			|| v_multiwindow_win32_readback_context_type(context)
				!= D3D11_DEVICE_CONTEXT_IMMEDIATE) {
		return NULL;
	}
	ID3D11Device *context_device = NULL;
	v_multiwindow_win32_readback_context_device(context, &context_device);
	int device_matches = context_device != NULL && context_device == device;
	v_multiwindow_win32_readback_com_release(context_device);
	if (!device_matches) {
		return NULL;
	}
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)calloc(1, sizeof(*native));
	if (native == NULL) {
		return NULL;
	}
	native->magic = V_MULTIWINDOW_WIN32_D3D11_READBACK_NATIVE_MAGIC;
	native->device = device;
	native->context = context;
	native->last_hresult = (int32_t)S_OK;
	v_multiwindow_win32_readback_com_add_ref(native->device);
	v_multiwindow_win32_readback_com_add_ref(native->context);
	native->machine = v_multiwindow_win32_readback_create(
		&v_multiwindow_win32_readback_native_ops, native);
	if (native->machine == NULL
			|| !v_multiwindow_win32_readback_bind(native->machine,
				state_identity, renderer_generation)) {
		if (native->machine != NULL) {
			(void)v_multiwindow_win32_readback_destroy(native->machine);
		}
		v_multiwindow_win32_readback_com_release(native->context);
		v_multiwindow_win32_readback_com_release(native->device);
		native->magic = 0;
		free(native);
		return NULL;
	}
	return native;
}

static int v_multiwindow_win32_d3d11_readback_native_stage_source(
		VMultiwindowWin32D3D11ReadbackNative *native,
		ID3D11Texture2D *source, uint64_t request, uint64_t state_identity,
		uint64_t renderer_generation, uint64_t window_slot,
		uint32_t window_generation, uint32_t x, uint32_t y, uint32_t width,
		uint32_t height, uint64_t producing_frame, int resize_pending) {
	if (!v_multiwindow_win32_readback_native_valid(native) || source == NULL) {
		return 0;
	}
	native->last_hresult = (int32_t)S_OK;
	int staged = v_multiwindow_win32_readback_stage(native->machine, request,
		state_identity, renderer_generation, window_slot, window_generation,
		source, x, y, width, height, producing_frame, resize_pending);
	if (!staged && FAILED((HRESULT)native->last_hresult)) {
		(void)v_multiwindow_win32_readback_notify_failure(native->machine,
			state_identity, renderer_generation, 1);
	}
	return staged;
}

static int v_multiwindow_win32_d3d11_readback_native_stage_window(
		void *state, uint64_t swapchain_identity, uint64_t request,
		uint64_t state_identity, uint64_t renderer_generation,
		uint64_t window_slot, uint32_t window_generation, uint32_t x,
		uint32_t y, uint32_t width, uint32_t height,
		uint64_t producing_frame, int resize_pending) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	IDXGISwapChain *swapchain =
		(IDXGISwapChain *)(uintptr_t)swapchain_identity;
	if (!v_multiwindow_win32_readback_native_valid(native)
			|| !v_multiwindow_win32_readback_is_owner(native->machine)
			|| swapchain == NULL) {
		return 0;
	}
	ID3D11Texture2D *source = NULL;
	HRESULT result = v_multiwindow_win32_readback_swapchain_buffer(swapchain,
		&source);
	native->last_hresult = (int32_t)result;
	if (FAILED(result) || source == NULL) {
		v_multiwindow_win32_readback_com_release(source);
		(void)v_multiwindow_win32_readback_notify_failure(native->machine,
			state_identity, renderer_generation, 1);
		return 0;
	}
	int staged = v_multiwindow_win32_d3d11_readback_native_stage_source(native,
		source, request, state_identity, renderer_generation, window_slot,
		window_generation, x, y, width, height, producing_frame,
		resize_pending);
	v_multiwindow_win32_readback_com_release(source);
	return staged;
}

static int v_multiwindow_win32_d3d11_readback_native_stage_image(
		void *state, uint64_t texture_identity, uint64_t request,
		uint64_t state_identity, uint64_t renderer_generation,
		uint64_t window_slot, uint32_t window_generation, uint32_t x,
		uint32_t y, uint32_t width, uint32_t height,
		uint64_t producing_frame, int resize_pending) {
	return v_multiwindow_win32_d3d11_readback_native_stage_source(
		(VMultiwindowWin32D3D11ReadbackNative *)state,
		(ID3D11Texture2D *)(uintptr_t)texture_identity, request,
		state_identity, renderer_generation, window_slot, window_generation,
		x, y, width, height, producing_frame, resize_pending);
}

static int v_multiwindow_win32_d3d11_readback_native_resolve(
		void *state, uint64_t window_slot, uint32_t window_generation,
		uint64_t producing_frame, int succeeded, int failure_origin) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	return v_multiwindow_win32_readback_native_valid(native)
		? v_multiwindow_win32_readback_resolve(native->machine, window_slot,
			window_generation, producing_frame, succeeded, failure_origin)
		: 0;
}

static int v_multiwindow_win32_d3d11_readback_native_resize_submitted(
		void *state, uint64_t window_slot, uint32_t window_generation) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	return v_multiwindow_win32_readback_native_valid(native)
		? v_multiwindow_win32_readback_resize_submitted(native->machine,
			window_slot, window_generation)
		: 0;
}

static int v_multiwindow_win32_d3d11_readback_native_notify_failure(
		void *state, uint64_t state_identity, uint64_t renderer_generation,
		int failure_origin) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	return v_multiwindow_win32_readback_native_valid(native)
		? v_multiwindow_win32_readback_notify_failure(native->machine,
			state_identity, renderer_generation, failure_origin)
		: 0;
}

static int v_multiwindow_win32_d3d11_readback_native_poll(void *state) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	return v_multiwindow_win32_readback_native_valid(native)
		? v_multiwindow_win32_readback_poll(native->machine)
		: 0;
}

static int v_multiwindow_win32_d3d11_readback_native_take(
		void *state, VMultiwindowWin32ReadbackResult *result) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	return v_multiwindow_win32_readback_native_valid(native)
		? v_multiwindow_win32_readback_take(native->machine, result)
		: 0;
}

static int v_multiwindow_win32_d3d11_readback_native_copy(
		void *state, uint64_t request, uint8_t *pixels, size_t capacity) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	return v_multiwindow_win32_readback_native_valid(native)
		? v_multiwindow_win32_readback_copy(native->machine, request, pixels,
			capacity)
		: 0;
}

static int v_multiwindow_win32_d3d11_readback_native_release(
		void *state, uint64_t request) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	return v_multiwindow_win32_readback_native_valid(native)
		? v_multiwindow_win32_readback_release(native->machine, request)
		: 0;
}

static int v_multiwindow_win32_d3d11_readback_native_cancel(
		void *state, uint64_t request) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	return v_multiwindow_win32_readback_native_valid(native)
		? v_multiwindow_win32_readback_cancel(native->machine, request)
		: 0;
}

static int v_multiwindow_win32_d3d11_readback_native_quiesce_window(
		void *state, uint64_t window_slot, uint32_t window_generation) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	return v_multiwindow_win32_readback_native_valid(native)
		? v_multiwindow_win32_readback_quiesce_window(native->machine,
			window_slot, window_generation)
		: 0;
}

static int v_multiwindow_win32_d3d11_readback_native_restart(
		void *state, uint64_t state_identity,
		uint64_t renderer_generation) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	return v_multiwindow_win32_readback_native_valid(native)
		? v_multiwindow_win32_readback_restart(native->machine,
			state_identity, renderer_generation)
		: 0;
}

static int v_multiwindow_win32_d3d11_readback_native_stop(void *state) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	return v_multiwindow_win32_readback_native_valid(native)
		? v_multiwindow_win32_readback_stop(native->machine)
		: 0;
}

static int v_multiwindow_win32_d3d11_readback_native_active(void *state) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	return v_multiwindow_win32_readback_native_valid(native)
		? v_multiwindow_win32_readback_active(native->machine)
		: 0;
}

static int32_t v_multiwindow_win32_d3d11_readback_native_last_hresult(
		void *state) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	return v_multiwindow_win32_readback_native_valid(native)
		? native->last_hresult
		: (int32_t)E_INVALIDARG;
}

static int32_t
v_multiwindow_win32_d3d11_readback_native_device_removed_reason(
		void *state) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	return v_multiwindow_win32_readback_native_valid(native)
		? native->device_removed_reason
		: (int32_t)E_INVALIDARG;
}

static int v_multiwindow_win32_d3d11_readback_native_destroy(void *state) {
	VMultiwindowWin32D3D11ReadbackNative *native =
		(VMultiwindowWin32D3D11ReadbackNative *)state;
	if (!v_multiwindow_win32_readback_native_valid(native)
			|| !v_multiwindow_win32_readback_destroy(native->machine)) {
		return 0;
	}
	native->machine = NULL;
	v_multiwindow_win32_readback_com_release(native->context);
	v_multiwindow_win32_readback_com_release(native->device);
	native->context = NULL;
	native->device = NULL;
	native->magic = 0;
	free(native);
	return 1;
}

#endif
