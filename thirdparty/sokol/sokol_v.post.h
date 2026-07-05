#if defined(SOKOL_GLCORE) || defined(SOKOL_GLES3)
	int v_sapp_read_rgba_pixels(int x, int y, int width, int height, unsigned char* pixels) {
		glReadPixels(x, y, width, height, GL_RGBA, GL_UNSIGNED_BYTE, pixels);
		return 0;
	}
#elif defined(SOKOL_D3D11) && defined(_SAPP_WIN32) && defined(SOKOL_APP_INCLUDED)
	enum {
		V_SAPP_READBACK_OK = 0,
		V_SAPP_READBACK_INVALID_ARGS = -1,
		V_SAPP_READBACK_NO_DEVICE = -2,
		V_SAPP_READBACK_GET_BUFFER_FAILED = -3,
		V_SAPP_READBACK_UNSUPPORTED_SOURCE = -4,
		V_SAPP_READBACK_CREATE_STAGING_FAILED = -5,
		V_SAPP_READBACK_MAP_FAILED = -6,
		V_SAPP_READBACK_UNSUPPORTED_FORMAT = -7,
	};

	static const IID v_sapp_IID_ID3D11Texture2D = {
		0x6f15aaf2,
		0xd208,
		0x4e89,
		{0x9a, 0xb4, 0x48, 0x95, 0x35, 0xd3, 0x4f, 0x9c}
	};

	#if defined(__cplusplus)
		#define v_sapp_d3d11_refiid(iid) iid
		#define v_sapp_d3d11_release(obj) do { if (obj) { (obj)->Release(); (obj) = 0; } } while (0)
		static inline HRESULT v_sapp_dxgi_get_buffer(IDXGISwapChain* self, UINT buffer, REFIID riid, void** surface) {
			return self->GetBuffer(buffer, riid, surface);
		}
		static inline void v_sapp_d3d11_get_desc(ID3D11Texture2D* self, D3D11_TEXTURE2D_DESC* desc) {
			self->GetDesc(desc);
		}
		static inline HRESULT v_sapp_d3d11_create_texture_2d(ID3D11Device* self, const D3D11_TEXTURE2D_DESC* desc, ID3D11Texture2D** texture) {
			return self->CreateTexture2D(desc, NULL, texture);
		}
		static inline void v_sapp_d3d11_copy_resource(ID3D11DeviceContext* self, ID3D11Resource* dst, ID3D11Resource* src) {
			self->CopyResource(dst, src);
		}
		static inline HRESULT v_sapp_d3d11_map(ID3D11DeviceContext* self, ID3D11Resource* resource, D3D11_MAPPED_SUBRESOURCE* mapped) {
			return self->Map(resource, 0, D3D11_MAP_READ, 0, mapped);
		}
		static inline void v_sapp_d3d11_unmap(ID3D11DeviceContext* self, ID3D11Resource* resource) {
			self->Unmap(resource, 0);
		}
	#else
		#define v_sapp_d3d11_refiid(iid) &iid
		#define v_sapp_d3d11_release(obj) do { if (obj) { (obj)->lpVtbl->Release(obj); (obj) = 0; } } while (0)
		static inline HRESULT v_sapp_dxgi_get_buffer(IDXGISwapChain* self, UINT buffer, REFIID riid, void** surface) {
			return self->lpVtbl->GetBuffer(self, buffer, riid, surface);
		}
		static inline void v_sapp_d3d11_get_desc(ID3D11Texture2D* self, D3D11_TEXTURE2D_DESC* desc) {
			self->lpVtbl->GetDesc(self, desc);
		}
		static inline HRESULT v_sapp_d3d11_create_texture_2d(ID3D11Device* self, const D3D11_TEXTURE2D_DESC* desc, ID3D11Texture2D** texture) {
			return self->lpVtbl->CreateTexture2D(self, desc, NULL, texture);
		}
		static inline void v_sapp_d3d11_copy_resource(ID3D11DeviceContext* self, ID3D11Resource* dst, ID3D11Resource* src) {
			self->lpVtbl->CopyResource(self, dst, src);
		}
		static inline HRESULT v_sapp_d3d11_map(ID3D11DeviceContext* self, ID3D11Resource* resource, D3D11_MAPPED_SUBRESOURCE* mapped) {
			return self->lpVtbl->Map(self, resource, 0, D3D11_MAP_READ, 0, mapped);
		}
		static inline void v_sapp_d3d11_unmap(ID3D11DeviceContext* self, ID3D11Resource* resource) {
			self->lpVtbl->Unmap(self, resource, 0);
		}
	#endif

	int v_sapp_read_rgba_pixels(int x, int y, int width, int height, unsigned char* pixels) {
		if ((0 == pixels) || (x < 0) || (y < 0) || (width <= 0) || (height <= 0)) {
			return V_SAPP_READBACK_INVALID_ARGS;
		}

		sapp_environment env = sapp_get_environment();
		ID3D11Device* device = (ID3D11Device*) env.d3d11.device;
		ID3D11DeviceContext* ctx = (ID3D11DeviceContext*) env.d3d11.device_context;
		IDXGISwapChain* swap_chain = (IDXGISwapChain*) sapp_d3d11_get_swap_chain();
		if ((0 == device) || (0 == ctx) || (0 == swap_chain)) {
			return V_SAPP_READBACK_NO_DEVICE;
		}

		ID3D11Texture2D* back_buffer = 0;
		HRESULT hr = v_sapp_dxgi_get_buffer(swap_chain, 0, v_sapp_d3d11_refiid(v_sapp_IID_ID3D11Texture2D), (void**) &back_buffer);
		if (FAILED(hr) || (0 == back_buffer)) {
			return V_SAPP_READBACK_GET_BUFFER_FAILED;
		}

		D3D11_TEXTURE2D_DESC desc;
		v_sapp_d3d11_get_desc(back_buffer, &desc);
		if ((desc.SampleDesc.Count != 1) || (desc.ArraySize != 1) || (desc.MipLevels < 1) ||
			(((UINT) x + (UINT) width) > desc.Width) ||
			(((UINT) y + (UINT) height) > desc.Height)) {
			v_sapp_d3d11_release(back_buffer);
			return V_SAPP_READBACK_UNSUPPORTED_SOURCE;
		}

		const bool src_bgra =
			(desc.Format == DXGI_FORMAT_B8G8R8A8_UNORM) ||
			(desc.Format == DXGI_FORMAT_B8G8R8A8_UNORM_SRGB);
		const bool src_rgba =
			(desc.Format == DXGI_FORMAT_R8G8B8A8_UNORM) ||
			(desc.Format == DXGI_FORMAT_R8G8B8A8_UNORM_SRGB);
		if (!src_bgra && !src_rgba) {
			v_sapp_d3d11_release(back_buffer);
			return V_SAPP_READBACK_UNSUPPORTED_FORMAT;
		}

		D3D11_TEXTURE2D_DESC staging_desc = desc;
		staging_desc.Usage = D3D11_USAGE_STAGING;
		staging_desc.BindFlags = 0;
		staging_desc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
		staging_desc.MiscFlags = 0;

		ID3D11Texture2D* staging = 0;
		hr = v_sapp_d3d11_create_texture_2d(device, &staging_desc, &staging);
		if (FAILED(hr) || (0 == staging)) {
			v_sapp_d3d11_release(back_buffer);
			return V_SAPP_READBACK_CREATE_STAGING_FAILED;
		}

		v_sapp_d3d11_copy_resource(ctx, (ID3D11Resource*) staging, (ID3D11Resource*) back_buffer);

		D3D11_MAPPED_SUBRESOURCE mapped;
		hr = v_sapp_d3d11_map(ctx, (ID3D11Resource*) staging, &mapped);
		if (FAILED(hr)) {
			v_sapp_d3d11_release(staging);
			v_sapp_d3d11_release(back_buffer);
			return V_SAPP_READBACK_MAP_FAILED;
		}

		const UINT src_top = desc.Height - (UINT) y - (UINT) height;
		const UINT src_left = (UINT) x;
		const size_t dst_row_pitch = (size_t) width * 4;
		for (int row = 0; row < height; row++) {
			const unsigned char* src = ((const unsigned char*) mapped.pData) +
				(((size_t) src_top + (size_t) row) * (size_t) mapped.RowPitch) +
				((size_t) src_left * 4);
			unsigned char* dst = pixels + ((size_t) (height - 1 - row) * dst_row_pitch);
			for (int col = 0; col < width; col++) {
				if (src_bgra) {
					dst[0] = src[2];
					dst[1] = src[1];
					dst[2] = src[0];
					dst[3] = src[3];
				} else {
					dst[0] = src[0];
					dst[1] = src[1];
					dst[2] = src[2];
					dst[3] = src[3];
				}
				src += 4;
				dst += 4;
			}
		}

		v_sapp_d3d11_unmap(ctx, (ID3D11Resource*) staging);
		v_sapp_d3d11_release(staging);
		v_sapp_d3d11_release(back_buffer);
		return V_SAPP_READBACK_OK;
	}
#else
	int v_sapp_read_rgba_pixels(int x, int y, int width, int height, unsigned char* pixels) {
		(void) x;
		(void) y;
		(void) width;
		(void) height;
		(void) pixels;
		return -100;
	}
#endif
