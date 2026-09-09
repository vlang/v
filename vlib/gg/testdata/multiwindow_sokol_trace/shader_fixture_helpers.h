#ifndef V_MULTIWINDOW_SHADER_FIXTURE_HELPERS_H
#define V_MULTIWINDOW_SHADER_FIXTURE_HELPERS_H

#include <stddef.h>
#include <stdint.h>

#if defined(SOKOL_D3D11)
/* These are the pinned sokol_gl SM4 blobs compiled into the same translation unit. */
static const uint8_t _sgl_vs_bytecode_hlsl4[1032];
static const uint8_t _sgl_fs_bytecode_hlsl4[608];

static const void* v_multiwindow_fixture_hlsl4_vs(void) {
    return _sgl_vs_bytecode_hlsl4;
}

static size_t v_multiwindow_fixture_hlsl4_vs_size(void) {
    return sizeof(_sgl_vs_bytecode_hlsl4);
}

static const void* v_multiwindow_fixture_hlsl4_fs(void) {
    return _sgl_fs_bytecode_hlsl4;
}

static size_t v_multiwindow_fixture_hlsl4_fs_size(void) {
    return sizeof(_sgl_fs_bytecode_hlsl4);
}
#else
static const void* v_multiwindow_fixture_hlsl4_vs(void) { return NULL; }
static size_t v_multiwindow_fixture_hlsl4_vs_size(void) { return 0; }
static const void* v_multiwindow_fixture_hlsl4_fs(void) { return NULL; }
static size_t v_multiwindow_fixture_hlsl4_fs_size(void) { return 0; }
#endif

#endif
