#ifndef V_MULTIWINDOW_WIN32_D3D11_BACKEND_HELPERS_H
#define V_MULTIWINDOW_WIN32_D3D11_BACKEND_HELPERS_H

#include <d3d11.h>
#include <dxgi1_2.h>
#include <stdint.h>
#include <string.h>
#include <windows.h>
#include "native_render_result.h"

static const IID v_multiwindow_IID_ID3D11Texture2D = {
	0x6f15aaf2, 0xd208, 0x4e89,
	{0x9a, 0xb4, 0x48, 0x95, 0x35, 0xd3, 0x4f, 0x9c}};
static const IID v_multiwindow_IID_IDXGIDevice1 = {
	0x77db970f, 0x6276, 0x48ba,
	{0xba, 0x28, 0x07, 0x01, 0x43, 0xb4, 0x39, 0x2c}};
static const IID v_multiwindow_IID_IDXGIFactory2 = {
	0x50c83a1c, 0xe072, 0x4c48,
	{0x87, 0xb0, 0x36, 0x30, 0xfa, 0x36, 0xa6, 0xd0}};

#if defined(__cplusplus)
#define V_MULTIWINDOW_WIN32_REFIID(iid) iid
#define V_MULTIWINDOW_COM_RELEASE(obj) (obj)->Release()
#else
#define V_MULTIWINDOW_WIN32_REFIID(iid) &iid
#define V_MULTIWINDOW_COM_RELEASE(obj) (obj)->lpVtbl->Release(obj)
#endif

static inline uint64_t v_multiwindow_win32_identity(const void *value) {
	return (uint64_t)(uintptr_t)value;
}

static inline void v_multiwindow_win32_release(uint64_t identity,
		VMultiwindowNativePrimitive *out) {
	if (out != NULL) {
		memset(out, 0, sizeof(*out));
	}
	if (identity != UINT64_C(0)) {
		ULONG remaining = V_MULTIWINDOW_COM_RELEASE((IUnknown *)(uintptr_t)identity);
		if (out != NULL) {
			out->observed_count = (uint64_t)remaining;
			out->valid_mask = V_MULTIWINDOW_NATIVE_VALID_OBSERVED_COUNT;
		}
	}
}

static inline HRESULT v_multiwindow_d3d11_query_interface(ID3D11Device *self,
	REFIID riid, void **out_object) {
#if defined(__cplusplus)
	return self->QueryInterface(riid, out_object);
#else
	return self->lpVtbl->QueryInterface(self, riid, out_object);
#endif
}

static inline HRESULT v_multiwindow_dxgi_get_adapter(IDXGIDevice1 *self,
	IDXGIAdapter **out_adapter) {
#if defined(__cplusplus)
	return self->GetAdapter(out_adapter);
#else
	return self->lpVtbl->GetAdapter(self, out_adapter);
#endif
}

static inline HRESULT v_multiwindow_dxgi_get_parent(IDXGIObject *self, REFIID riid,
	void **out_parent) {
#if defined(__cplusplus)
	return self->GetParent(riid, out_parent);
#else
	return self->lpVtbl->GetParent(self, riid, out_parent);
#endif
}

static inline HRESULT v_multiwindow_d3d11_removed_reason(ID3D11Device *device) {
#if defined(__cplusplus)
	return device->GetDeviceRemovedReason();
#else
	return device->lpVtbl->GetDeviceRemovedReason(device);
#endif
}

static inline void v_multiwindow_win32_result(VMultiwindowNativePrimitive *out,
		HRESULT result) {
	if (out == NULL) {
		return;
	}
	memset(out, 0, sizeof(*out));
	out->valid_mask = V_MULTIWINDOW_NATIVE_VALID_RETURN_VALUE;
	out->return_value = (int64_t)(int32_t)result;
}

static inline void v_multiwindow_win32_d3d11_get_removed_reason(uint64_t device_identity,
		VMultiwindowNativePrimitive *out) {
	if (out == NULL) {
		return;
	}
	memset(out, 0, sizeof(*out));
	if (device_identity == UINT64_C(0)) {
		return;
	}
	HRESULT reason = v_multiwindow_d3d11_removed_reason(
		(ID3D11Device *)(uintptr_t)device_identity);
	out->valid_mask = V_MULTIWINDOW_NATIVE_VALID_DXGI_REMOVAL_REASON;
	out->dxgi_removal_reason = (int64_t)(int32_t)reason;
}

static inline void v_multiwindow_win32_d3d11_create_device_attempt(int64_t driver,
	int64_t feature_list, VMultiwindowNativePrimitive *out) {
	D3D_FEATURE_LEVEL with_11_1[] = {
		D3D_FEATURE_LEVEL_11_1, D3D_FEATURE_LEVEL_11_0,
		D3D_FEATURE_LEVEL_10_1, D3D_FEATURE_LEVEL_10_0,
	};
	D3D_FEATURE_LEVEL legacy[] = {
		D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_10_1, D3D_FEATURE_LEVEL_10_0,
	};
	D3D_FEATURE_LEVEL selected = D3D_FEATURE_LEVEL_10_0;
	ID3D11Device *device = NULL;
	ID3D11DeviceContext *context = NULL;
	D3D_DRIVER_TYPE driver_type = driver == 1 ? D3D_DRIVER_TYPE_WARP
		: D3D_DRIVER_TYPE_HARDWARE;
	D3D_FEATURE_LEVEL *levels = feature_list == 1 ? legacy : with_11_1;
	UINT level_count = feature_list == 1 ? 3 : 4;
	UINT flags = D3D11_CREATE_DEVICE_BGRA_SUPPORT | D3D11_CREATE_DEVICE_SINGLETHREADED;
	HRESULT result = D3D11CreateDevice(NULL, driver_type, NULL, flags, levels, level_count,
		D3D11_SDK_VERSION, &device, &selected, &context);
	v_multiwindow_win32_result(out, result);
	if (out != NULL) {
		out->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_HANDLE |
			V_MULTIWINDOW_NATIVE_VALID_OBJECT_IDENTITY_0 |
			V_MULTIWINDOW_NATIVE_VALID_SELECTED_VALUE;
		out->handle = v_multiwindow_win32_identity(device);
		out->object_identity_0 = v_multiwindow_win32_identity(context);
		out->selected_value = (int64_t)selected;
	}
}

static inline void v_multiwindow_win32_d3d11_query_dxgi_device(uint64_t device_identity,
	VMultiwindowNativePrimitive *out) {
	ID3D11Device *device = (ID3D11Device *)(uintptr_t)device_identity;
	IDXGIDevice1 *dxgi_device = NULL;
	HRESULT result = v_multiwindow_d3d11_query_interface(device,
		V_MULTIWINDOW_WIN32_REFIID(v_multiwindow_IID_IDXGIDevice1),
		(void **)&dxgi_device);
	v_multiwindow_win32_result(out, result);
	if (out != NULL) {
		out->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_HANDLE;
		out->handle = v_multiwindow_win32_identity(dxgi_device);
	}
}

static inline void v_multiwindow_win32_dxgi_get_adapter(uint64_t device_identity,
	uint64_t dxgi_device_identity, VMultiwindowNativePrimitive *out) {
	ID3D11Device *device = (ID3D11Device *)(uintptr_t)device_identity;
	IDXGIAdapter *adapter = NULL;
	HRESULT result = v_multiwindow_dxgi_get_adapter(
		(IDXGIDevice1 *)(uintptr_t)dxgi_device_identity, &adapter);
	v_multiwindow_win32_result(out, result);
	if (out != NULL) {
		out->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_HANDLE;
		out->handle = v_multiwindow_win32_identity(adapter);
	}
}

static inline void v_multiwindow_win32_dxgi_get_factory(uint64_t device_identity,
	uint64_t adapter_identity, VMultiwindowNativePrimitive *out) {
	ID3D11Device *device = (ID3D11Device *)(uintptr_t)device_identity;
	IDXGIFactory2 *factory = NULL;
	HRESULT result = v_multiwindow_dxgi_get_parent(
		(IDXGIObject *)(uintptr_t)adapter_identity,
		V_MULTIWINDOW_WIN32_REFIID(v_multiwindow_IID_IDXGIFactory2), (void **)&factory);
	v_multiwindow_win32_result(out, result);
	if (out != NULL) {
		out->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_HANDLE;
		out->handle = v_multiwindow_win32_identity(factory);
	}
}

static inline void v_multiwindow_win32_dxgi_create_swapchain(uint64_t factory_identity,
	uint64_t device_identity, uint64_t hwnd_identity, int64_t width, int64_t height,
	VMultiwindowNativePrimitive *out) {
	DXGI_SWAP_CHAIN_DESC1 desc;
	ZeroMemory(&desc, sizeof(desc));
	desc.Width = (UINT)width;
	desc.Height = (UINT)height;
	desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
	desc.SampleDesc.Count = 1;
	desc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
	desc.BufferCount = 2;
	desc.Scaling = DXGI_SCALING_STRETCH;
	desc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL;
	desc.AlphaMode = DXGI_ALPHA_MODE_IGNORE;
	IDXGIFactory2 *factory = (IDXGIFactory2 *)(uintptr_t)factory_identity;
	IDXGISwapChain1 *swapchain = NULL;
#if defined(__cplusplus)
	HRESULT result = factory->CreateSwapChainForHwnd(
		(IUnknown *)(uintptr_t)device_identity, (HWND)(uintptr_t)hwnd_identity, &desc,
		NULL, NULL, &swapchain);
#else
	HRESULT result = factory->lpVtbl->CreateSwapChainForHwnd(factory,
		(IUnknown *)(uintptr_t)device_identity, (HWND)(uintptr_t)hwnd_identity, &desc,
		NULL, NULL, &swapchain);
#endif
	v_multiwindow_win32_result(out, result);
	if (out != NULL) {
		out->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_HANDLE;
		out->handle = v_multiwindow_win32_identity(swapchain);
	}
}

static inline void v_multiwindow_win32_dxgi_make_window_association(
	uint64_t factory_identity, uint64_t device_identity, uint64_t hwnd_identity,
	VMultiwindowNativePrimitive *out) {
	IDXGIFactory2 *factory = (IDXGIFactory2 *)(uintptr_t)factory_identity;
#if defined(__cplusplus)
	HRESULT result = factory->MakeWindowAssociation((HWND)(uintptr_t)hwnd_identity,
		DXGI_MWA_NO_ALT_ENTER | DXGI_MWA_NO_PRINT_SCREEN);
#else
	HRESULT result = factory->lpVtbl->MakeWindowAssociation(factory,
		(HWND)(uintptr_t)hwnd_identity,
		DXGI_MWA_NO_ALT_ENTER | DXGI_MWA_NO_PRINT_SCREEN);
#endif
	v_multiwindow_win32_result(out, result);
}

static inline void v_multiwindow_win32_dxgi_get_backbuffer(uint64_t device_identity,
	uint64_t swapchain_identity, VMultiwindowNativePrimitive *out) {
	IDXGISwapChain *swapchain = (IDXGISwapChain *)(uintptr_t)swapchain_identity;
	ID3D11Texture2D *backbuffer = NULL;
#if defined(__cplusplus)
	HRESULT result = swapchain->GetBuffer(0,
		V_MULTIWINDOW_WIN32_REFIID(v_multiwindow_IID_ID3D11Texture2D),
		(void **)&backbuffer);
#else
	HRESULT result = swapchain->lpVtbl->GetBuffer(swapchain, 0,
		V_MULTIWINDOW_WIN32_REFIID(v_multiwindow_IID_ID3D11Texture2D),
		(void **)&backbuffer);
#endif
	v_multiwindow_win32_result(out, result);
	if (out != NULL) {
		out->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_HANDLE;
		out->handle = v_multiwindow_win32_identity(backbuffer);
	}
}

static inline void v_multiwindow_win32_d3d11_create_render_view(uint64_t device_identity,
	uint64_t resource_identity, VMultiwindowNativePrimitive *out) {
	ID3D11Device *device = (ID3D11Device *)(uintptr_t)device_identity;
	ID3D11RenderTargetView *view = NULL;
#if defined(__cplusplus)
	HRESULT result = device->CreateRenderTargetView(
		(ID3D11Resource *)(uintptr_t)resource_identity, NULL, &view);
#else
	HRESULT result = device->lpVtbl->CreateRenderTargetView(device,
		(ID3D11Resource *)(uintptr_t)resource_identity, NULL, &view);
#endif
	v_multiwindow_win32_result(out, result);
	if (out != NULL) {
		out->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_HANDLE;
		out->handle = v_multiwindow_win32_identity(view);
	}
}

static inline void v_multiwindow_win32_d3d11_create_texture(uint64_t device_identity,
	int64_t width, int64_t height, int64_t depth, VMultiwindowNativePrimitive *out) {
	ID3D11Device *device = (ID3D11Device *)(uintptr_t)device_identity;
	D3D11_TEXTURE2D_DESC desc;
	ZeroMemory(&desc, sizeof(desc));
	desc.Width = (UINT)width;
	desc.Height = (UINT)height;
	desc.MipLevels = 1;
	desc.ArraySize = 1;
	desc.Format = depth != 0 ? DXGI_FORMAT_D24_UNORM_S8_UINT
		: DXGI_FORMAT_B8G8R8A8_UNORM;
	desc.SampleDesc.Count = 1;
	desc.Usage = D3D11_USAGE_DEFAULT;
	desc.BindFlags = depth != 0 ? D3D11_BIND_DEPTH_STENCIL : D3D11_BIND_RENDER_TARGET;
	ID3D11Texture2D *texture = NULL;
#if defined(__cplusplus)
	HRESULT result = device->CreateTexture2D(&desc, NULL, &texture);
#else
	HRESULT result = device->lpVtbl->CreateTexture2D(device, &desc, NULL, &texture);
#endif
	v_multiwindow_win32_result(out, result);
	if (out != NULL) {
		out->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_HANDLE;
		out->handle = v_multiwindow_win32_identity(texture);
	}
}

static inline void v_multiwindow_win32_d3d11_create_depth_view(uint64_t device_identity,
	uint64_t texture_identity, VMultiwindowNativePrimitive *out) {
	ID3D11Device *device = (ID3D11Device *)(uintptr_t)device_identity;
	ID3D11DepthStencilView *view = NULL;
#if defined(__cplusplus)
	HRESULT result = device->CreateDepthStencilView(
		(ID3D11Resource *)(uintptr_t)texture_identity, NULL, &view);
#else
	HRESULT result = device->lpVtbl->CreateDepthStencilView(device,
		(ID3D11Resource *)(uintptr_t)texture_identity, NULL, &view);
#endif
	v_multiwindow_win32_result(out, result);
	if (out != NULL) {
		out->valid_mask |= V_MULTIWINDOW_NATIVE_VALID_HANDLE;
		out->handle = v_multiwindow_win32_identity(view);
	}
}

static inline void v_multiwindow_win32_d3d11_clear_state(uint64_t context_identity,
	VMultiwindowNativePrimitive *out) {
	ID3D11DeviceContext *context = (ID3D11DeviceContext *)(uintptr_t)context_identity;
#if defined(__cplusplus)
	context->ClearState();
#else
	context->lpVtbl->ClearState(context);
#endif
	if (out != NULL) {
		memset(out, 0, sizeof(*out));
	}
}

static inline void v_multiwindow_win32_dxgi_resize_buffers(uint64_t device_identity,
	uint64_t swapchain_identity, int64_t width, int64_t height,
	VMultiwindowNativePrimitive *out) {
	IDXGISwapChain *swapchain = (IDXGISwapChain *)(uintptr_t)swapchain_identity;
#if defined(__cplusplus)
	HRESULT result = swapchain->ResizeBuffers(0, (UINT)width, (UINT)height,
		DXGI_FORMAT_B8G8R8A8_UNORM, 0);
#else
	HRESULT result = swapchain->lpVtbl->ResizeBuffers(swapchain, 0, (UINT)width,
		(UINT)height, DXGI_FORMAT_B8G8R8A8_UNORM, 0);
#endif
	v_multiwindow_win32_result(out, result);
}

static inline void v_multiwindow_win32_dxgi_present(uint64_t device_identity,
	uint64_t swapchain_identity, VMultiwindowNativePrimitive *out) {
	IDXGISwapChain *swapchain = (IDXGISwapChain *)(uintptr_t)swapchain_identity;
#if defined(__cplusplus)
	HRESULT result = swapchain->Present(1, 0);
#else
	HRESULT result = swapchain->lpVtbl->Present(swapchain, 1, 0);
#endif
	v_multiwindow_win32_result(out, result);
}

static inline void v_multiwindow_win32_d3d11_device_status(uint64_t device_identity,
	VMultiwindowNativePrimitive *out) {
	ID3D11Device *device = (ID3D11Device *)(uintptr_t)device_identity;
	HRESULT reason = v_multiwindow_d3d11_removed_reason(device);
	if (out != NULL) {
		memset(out, 0, sizeof(*out));
		out->valid_mask = V_MULTIWINDOW_NATIVE_VALID_DXGI_REMOVAL_REASON;
		out->dxgi_removal_reason = (int64_t)(int32_t)reason;
	}
}

#endif
