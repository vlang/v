#ifndef V_MULTIWINDOW_WIN32_D3D11_BACKEND_HELPERS_H
#define V_MULTIWINDOW_WIN32_D3D11_BACKEND_HELPERS_H

#include <d3d11.h>
#include <dxgi1_2.h>
#include <stdint.h>
#include <windows.h>

static const IID v_multiwindow_IID_ID3D11Texture2D = {
	0x6f15aaf2,
	0xd208,
	0x4e89,
	{0x9a, 0xb4, 0x48, 0x95, 0x35, 0xd3, 0x4f, 0x9c}};
static const IID v_multiwindow_IID_IDXGIDevice1 = {
	0x77db970f,
	0x6276,
	0x48ba,
	{0xba, 0x28, 0x07, 0x01, 0x43, 0xb4, 0x39, 0x2c}};
static const IID v_multiwindow_IID_IDXGIFactory2 = {
	0x50c83a1c,
	0xe072,
	0x4c48,
	{0x87, 0xb0, 0x36, 0x30, 0xfa, 0x36, 0xa6, 0xd0}};

#if defined(__cplusplus)
#define V_MULTIWINDOW_WIN32_REFIID(iid) iid
#define V_MULTIWINDOW_COM_RELEASE(obj) (obj)->Release()
#else
#define V_MULTIWINDOW_WIN32_REFIID(iid) &iid
#define V_MULTIWINDOW_COM_RELEASE(obj) (obj)->lpVtbl->Release(obj)
#endif

static inline void v_multiwindow_win32_safe_release(void **obj) {
	if (obj && *obj) {
		V_MULTIWINDOW_COM_RELEASE((IUnknown *)*obj);
		*obj = 0;
	}
}

static inline HRESULT v_multiwindow_d3d11_query_interface(ID3D11Device *self, REFIID riid, void **out_object) {
#if defined(__cplusplus)
	return self->QueryInterface(riid, out_object);
#else
	return self->lpVtbl->QueryInterface(self, riid, out_object);
#endif
}

static inline HRESULT v_multiwindow_dxgi_get_adapter(IDXGIDevice1 *self, IDXGIAdapter **out_adapter) {
#if defined(__cplusplus)
	return self->GetAdapter(out_adapter);
#else
	return self->lpVtbl->GetAdapter(self, out_adapter);
#endif
}

static inline HRESULT v_multiwindow_dxgi_get_parent(IDXGIObject *self, REFIID riid, void **out_parent) {
#if defined(__cplusplus)
	return self->GetParent(riid, out_parent);
#else
	return self->lpVtbl->GetParent(self, riid, out_parent);
#endif
}

static inline HRESULT v_multiwindow_dxgi_make_window_association(IDXGIFactory2 *self, HWND hwnd, UINT flags) {
#if defined(__cplusplus)
	return self->MakeWindowAssociation(hwnd, flags);
#else
	return self->lpVtbl->MakeWindowAssociation(self, hwnd, flags);
#endif
}

static inline HRESULT v_multiwindow_dxgi_create_swapchain_for_hwnd(IDXGIFactory2 *self, IUnknown *device, HWND hwnd, const DXGI_SWAP_CHAIN_DESC1 *desc, IDXGISwapChain1 **out_swapchain) {
#if defined(__cplusplus)
	return self->CreateSwapChainForHwnd(device, hwnd, desc, NULL, NULL, out_swapchain);
#else
	return self->lpVtbl->CreateSwapChainForHwnd(self, device, hwnd, desc, NULL, NULL, out_swapchain);
#endif
}

static inline HRESULT v_multiwindow_dxgi_get_buffer(IDXGISwapChain *self, UINT buffer, REFIID riid, void **out_surface) {
#if defined(__cplusplus)
	return self->GetBuffer(buffer, riid, out_surface);
#else
	return self->lpVtbl->GetBuffer(self, buffer, riid, out_surface);
#endif
}

static inline HRESULT v_multiwindow_dxgi_resize_buffers(IDXGISwapChain *self, UINT buffer_count, UINT width, UINT height, DXGI_FORMAT format, UINT flags) {
#if defined(__cplusplus)
	return self->ResizeBuffers(buffer_count, width, height, format, flags);
#else
	return self->lpVtbl->ResizeBuffers(self, buffer_count, width, height, format, flags);
#endif
}

static inline HRESULT v_multiwindow_dxgi_present(IDXGISwapChain *self, UINT sync_interval, UINT flags) {
#if defined(__cplusplus)
	return self->Present(sync_interval, flags);
#else
	return self->lpVtbl->Present(self, sync_interval, flags);
#endif
}

static inline HRESULT v_multiwindow_d3d11_create_render_target_view(ID3D11Device *self, ID3D11Resource *resource, ID3D11RenderTargetView **out_view) {
#if defined(__cplusplus)
	return self->CreateRenderTargetView(resource, NULL, out_view);
#else
	return self->lpVtbl->CreateRenderTargetView(self, resource, NULL, out_view);
#endif
}

static inline HRESULT v_multiwindow_d3d11_create_texture2d(ID3D11Device *self, const D3D11_TEXTURE2D_DESC *desc, ID3D11Texture2D **out_texture) {
#if defined(__cplusplus)
	return self->CreateTexture2D(desc, NULL, out_texture);
#else
	return self->lpVtbl->CreateTexture2D(self, desc, NULL, out_texture);
#endif
}

static inline HRESULT v_multiwindow_d3d11_create_depth_stencil_view(ID3D11Device *self, ID3D11Resource *resource, ID3D11DepthStencilView **out_view) {
#if defined(__cplusplus)
	return self->CreateDepthStencilView(resource, NULL, out_view);
#else
	return self->lpVtbl->CreateDepthStencilView(self, resource, NULL, out_view);
#endif
}

static inline void v_multiwindow_d3d11_clear_state(ID3D11DeviceContext *self) {
#if defined(__cplusplus)
	self->ClearState();
#else
	self->lpVtbl->ClearState(self);
#endif
}

static inline int v_multiwindow_win32_d3d11_create_device(void **out_device, void **out_context, void **out_factory) {
	ID3D11Device *device = 0;
	ID3D11DeviceContext *context = 0;
	D3D_FEATURE_LEVEL feature_level;
	UINT flags = D3D11_CREATE_DEVICE_BGRA_SUPPORT | D3D11_CREATE_DEVICE_SINGLETHREADED;
	D3D_FEATURE_LEVEL levels_with_11_1[] = {
		D3D_FEATURE_LEVEL_11_1,
		D3D_FEATURE_LEVEL_11_0,
		D3D_FEATURE_LEVEL_10_1,
		D3D_FEATURE_LEVEL_10_0,
	};
	HRESULT hr = D3D11CreateDevice(NULL, D3D_DRIVER_TYPE_HARDWARE, NULL, flags, levels_with_11_1, 4, D3D11_SDK_VERSION, &device, &feature_level, &context);
	if (hr == E_INVALIDARG) {
		D3D_FEATURE_LEVEL levels[] = {
			D3D_FEATURE_LEVEL_11_0,
			D3D_FEATURE_LEVEL_10_1,
			D3D_FEATURE_LEVEL_10_0,
		};
		hr = D3D11CreateDevice(NULL, D3D_DRIVER_TYPE_HARDWARE, NULL, flags, levels, 3, D3D11_SDK_VERSION, &device, &feature_level, &context);
	}
	if (FAILED(hr) || !device || !context) {
		v_multiwindow_win32_safe_release((void **)&context);
		v_multiwindow_win32_safe_release((void **)&device);
		return 0;
	}
	IDXGIDevice1 *dxgi_device = 0;
	hr = v_multiwindow_d3d11_query_interface(device, V_MULTIWINDOW_WIN32_REFIID(v_multiwindow_IID_IDXGIDevice1), (void **)&dxgi_device);
	if (FAILED(hr) || !dxgi_device) {
		v_multiwindow_win32_safe_release((void **)&context);
		v_multiwindow_win32_safe_release((void **)&device);
		return 0;
	}
	IDXGIAdapter *adapter = 0;
	hr = v_multiwindow_dxgi_get_adapter(dxgi_device, &adapter);
	v_multiwindow_win32_safe_release((void **)&dxgi_device);
	if (FAILED(hr) || !adapter) {
		v_multiwindow_win32_safe_release((void **)&context);
		v_multiwindow_win32_safe_release((void **)&device);
		return 0;
	}
	IDXGIFactory2 *factory = 0;
	hr = v_multiwindow_dxgi_get_parent((IDXGIObject *)adapter, V_MULTIWINDOW_WIN32_REFIID(v_multiwindow_IID_IDXGIFactory2), (void **)&factory);
	v_multiwindow_win32_safe_release((void **)&adapter);
	if (FAILED(hr) || !factory) {
		v_multiwindow_win32_safe_release((void **)&context);
		v_multiwindow_win32_safe_release((void **)&device);
		return 0;
	}
	*out_device = device;
	*out_context = context;
	*out_factory = factory;
	return 1;
}

static inline int v_multiwindow_win32_d3d11_create_swapchain(void *factory_ptr, void *device_ptr, void *hwnd, int width, int height, void **out_swapchain) {
	DXGI_SWAP_CHAIN_DESC1 desc;
	ZeroMemory(&desc, sizeof(desc));
	desc.Width = (UINT)width;
	desc.Height = (UINT)height;
	desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
	desc.SampleDesc.Count = 1;
	desc.SampleDesc.Quality = 0;
	desc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
	desc.BufferCount = 2;
	desc.Scaling = DXGI_SCALING_STRETCH;
	desc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL;
	desc.AlphaMode = DXGI_ALPHA_MODE_IGNORE;
	IDXGISwapChain1 *swapchain = 0;
	HRESULT hr = v_multiwindow_dxgi_create_swapchain_for_hwnd((IDXGIFactory2 *)factory_ptr, (IUnknown *)device_ptr, (HWND)hwnd, &desc, &swapchain);
	if (FAILED(hr) || !swapchain) {
		return 0;
	}
	v_multiwindow_dxgi_make_window_association((IDXGIFactory2 *)factory_ptr, (HWND)hwnd, DXGI_MWA_NO_ALT_ENTER | DXGI_MWA_NO_PRINT_SCREEN);
	*out_swapchain = swapchain;
	return 1;
}

static inline void v_multiwindow_win32_d3d11_release_views(void **render_view, void **depth_texture, void **depth_view) {
	v_multiwindow_win32_safe_release(depth_view);
	v_multiwindow_win32_safe_release(depth_texture);
	v_multiwindow_win32_safe_release(render_view);
}

static inline int v_multiwindow_win32_d3d11_create_views(void *device_ptr, void *swapchain_ptr, int width, int height, void **out_render_view, void **out_depth_texture, void **out_depth_view) {
	ID3D11Device *device = (ID3D11Device *)device_ptr;
	IDXGISwapChain *swapchain = (IDXGISwapChain *)swapchain_ptr;
	ID3D11Texture2D *backbuffer = 0;
	HRESULT hr = v_multiwindow_dxgi_get_buffer(swapchain, 0, V_MULTIWINDOW_WIN32_REFIID(v_multiwindow_IID_ID3D11Texture2D), (void **)&backbuffer);
	if (FAILED(hr) || !backbuffer) {
		return 0;
	}
	ID3D11RenderTargetView *rtv = 0;
	hr = v_multiwindow_d3d11_create_render_target_view(device, (ID3D11Resource *)backbuffer, &rtv);
	v_multiwindow_win32_safe_release((void **)&backbuffer);
	if (FAILED(hr) || !rtv) {
		return 0;
	}
	D3D11_TEXTURE2D_DESC depth_desc;
	ZeroMemory(&depth_desc, sizeof(depth_desc));
	depth_desc.Width = (UINT)width;
	depth_desc.Height = (UINT)height;
	depth_desc.MipLevels = 1;
	depth_desc.ArraySize = 1;
	depth_desc.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
	depth_desc.SampleDesc.Count = 1;
	depth_desc.SampleDesc.Quality = 0;
	depth_desc.Usage = D3D11_USAGE_DEFAULT;
	depth_desc.BindFlags = D3D11_BIND_DEPTH_STENCIL;
	ID3D11Texture2D *depth_texture = 0;
	hr = v_multiwindow_d3d11_create_texture2d(device, &depth_desc, &depth_texture);
	if (FAILED(hr) || !depth_texture) {
		v_multiwindow_win32_safe_release((void **)&rtv);
		return 0;
	}
	ID3D11DepthStencilView *dsv = 0;
	hr = v_multiwindow_d3d11_create_depth_stencil_view(device, (ID3D11Resource *)depth_texture, &dsv);
	if (FAILED(hr) || !dsv) {
		v_multiwindow_win32_safe_release((void **)&depth_texture);
		v_multiwindow_win32_safe_release((void **)&rtv);
		return 0;
	}
	*out_render_view = rtv;
	*out_depth_texture = depth_texture;
	*out_depth_view = dsv;
	return 1;
}

static inline int v_multiwindow_win32_d3d11_resize_swapchain(void *device, void *context, void *swapchain, int width, int height, void **render_view, void **depth_texture, void **depth_view) {
	v_multiwindow_d3d11_clear_state((ID3D11DeviceContext *)context);
	v_multiwindow_win32_d3d11_release_views(render_view, depth_texture, depth_view);
	HRESULT hr = v_multiwindow_dxgi_resize_buffers((IDXGISwapChain *)swapchain, 0, (UINT)width, (UINT)height, DXGI_FORMAT_B8G8R8A8_UNORM, 0);
	if (FAILED(hr)) {
		return 0;
	}
	return v_multiwindow_win32_d3d11_create_views(device, swapchain, width, height, render_view, depth_texture, depth_view);
}

#define V_MULTIWINDOW_D3D11_PRESENT_OK 0
#define V_MULTIWINDOW_D3D11_PRESENT_FAILED 1
#define V_MULTIWINDOW_D3D11_PRESENT_DEVICE_REMOVED 2
#define V_MULTIWINDOW_D3D11_PRESENT_DEVICE_RESET 3
#define V_MULTIWINDOW_D3D11_PRESENT_OCCLUDED 4

static inline int v_multiwindow_win32_d3d11_present(void *swapchain) {
	HRESULT hr = v_multiwindow_dxgi_present((IDXGISwapChain *)swapchain, 1, 0);
	if (hr == DXGI_STATUS_OCCLUDED) {
		return V_MULTIWINDOW_D3D11_PRESENT_OCCLUDED;
	}
	if (hr == DXGI_ERROR_DEVICE_REMOVED) {
		return V_MULTIWINDOW_D3D11_PRESENT_DEVICE_REMOVED;
	}
	if (hr == DXGI_ERROR_DEVICE_RESET) {
		return V_MULTIWINDOW_D3D11_PRESENT_DEVICE_RESET;
	}
	if (SUCCEEDED(hr)) {
		return V_MULTIWINDOW_D3D11_PRESENT_OK;
	}
	return V_MULTIWINDOW_D3D11_PRESENT_FAILED;
}

#endif
