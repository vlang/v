#ifndef V_MULTIWINDOW_WIN32_READBACK_D3D11_FAKE_COM_H
#define V_MULTIWINDOW_WIN32_READBACK_D3D11_FAKE_COM_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define V_MW_D3D11_TEST_OK 0
#define V_MW_D3D11_TEST_S_FALSE 1
#define V_MW_D3D11_TEST_DEVICE_LOST (-2005270523)
#define V_MW_D3D11_TEST_WAS_STILL_DRAWING ((int32_t)UINT32_C(0x887A000A))
#define V_MW_D3D11_TEST_FORMAT_BGRA8 1
#define V_MW_D3D11_TEST_FORMAT_RGBA8 2
#define V_MW_D3D11_TEST_STATUS_READY 1
#define V_MW_D3D11_TEST_STATUS_FAILED 2
#define V_MW_D3D11_TEST_FAILURE_PRESENT 1
#define V_MW_D3D11_TEST_FAILURE_RESIZE 2
#define V_MW_D3D11_TEST_OP_RETAIN 1
#define V_MW_D3D11_TEST_OP_DESCRIBE 2
#define V_MW_D3D11_TEST_OP_COPY 3
#define V_MW_D3D11_TEST_OP_END_QUERY 4
#define V_MW_D3D11_TEST_OP_RELEASE_SOURCE 5
#define V_MW_D3D11_TEST_OP_GET_DATA 6
#define V_MW_D3D11_TEST_OP_MAP 7
#define V_MW_D3D11_TEST_OP_UNMAP 8
#define V_MW_D3D11_TEST_OP_RELEASE_STAGING 9
#define V_MW_D3D11_TEST_OP_RELEASE_QUERY 10
#define V_MW_D3D11_TEST_OP_DEVICE_REASON 11

typedef struct VMultiwindowD3D11TestSource {
	uint32_t width;
	uint32_t height;
	uint32_t format;
	size_t row_pitch;
	uint8_t *pixels;
	int refs;
	int add_refs;
	int releases;
	int release_calls;
} VMultiwindowD3D11TestSource;

typedef struct VMultiwindowD3D11TestStaging {
	uint32_t width;
	uint32_t height;
	uint32_t format;
	size_t row_pitch;
	uint8_t *pixels;
	int releases;
	int release_calls;
} VMultiwindowD3D11TestStaging;

typedef struct VMultiwindowD3D11TestQuery {
	int releases;
	int release_calls;
} VMultiwindowD3D11TestQuery;

typedef struct VMultiwindowD3D11TestWorld {
	VMultiwindowD3D11TestStaging *staging[32];
	VMultiwindowD3D11TestQuery *queries[32];
	int staging_count;
	int query_count;
	int query_results[8];
	int query_result_count;
	int query_result_index;
	int map_result;
	int map_results[8];
	int map_result_count;
	int map_result_index;
	int map_calls;
	int unmap_calls;
	int copy_calls;
	int end_query_calls;
	int get_data_calls;
	int device_reason_calls;
	int journal[256];
	int journal_count;
} VMultiwindowD3D11TestWorld;

typedef struct VMultiwindowD3D11TestOps {
	int (*retain_source)(void *world, void *source);
	void (*release_source)(void *world, void *source);
	int (*describe_source)(void *world, void *source, uint32_t *width,
		uint32_t *height, uint32_t *format);
	int (*copy_region)(void *world, void *source, uint32_t x, uint32_t y,
		uint32_t width, uint32_t height, void **out_staging);
	int (*end_event_query)(void *world, void **out_query);
	int (*get_event_data)(void *world, void *query);
	int (*map_staging)(void *world, void *staging, const uint8_t **data,
		size_t *row_pitch);
	void (*unmap_staging)(void *world, void *staging);
	void (*release_staging)(void *world, void *staging);
	void (*release_query)(void *world, void *query);
	int (*device_removed_reason)(void *world);
} VMultiwindowD3D11TestOps;

typedef struct VMultiwindowD3D11TestResult {
	uint64_t request;
	uint64_t state_identity;
	uint64_t renderer_generation;
	uint64_t window_slot;
	uint32_t window_generation;
	uint64_t submitted_frame;
	int status;
	uint32_t width;
	uint32_t height;
	size_t stride;
	size_t byte_length;
} VMultiwindowD3D11TestResult;

#if defined(__has_include)
#if __has_include("../win32_readback_d3d11_helpers.h")
#define V_MULTIWINDOW_WIN32_D3D11_READBACK_TEST 1
#include "../win32_readback_d3d11_helpers.h"
#define V_MULTIWINDOW_WIN32_D3D11_READBACK_CANDIDATE 1
#endif
#endif

#if !defined(V_MULTIWINDOW_WIN32_D3D11_READBACK_CANDIDATE)
static void *v_multiwindow_win32_d3d11_readback_test_create(
		const VMultiwindowD3D11TestOps *ops, void *world) {
	(void)ops;
	(void)world;
	return NULL;
}

static int v_multiwindow_win32_d3d11_readback_test_bind(void *state,
		uint64_t state_identity, uint64_t renderer_generation) {
	(void)state;
	(void)state_identity;
	(void)renderer_generation;
	return 0;
}

static int v_multiwindow_win32_d3d11_readback_test_stage(void *state,
		uint64_t request, uint64_t state_identity, uint64_t renderer_generation,
		uint64_t window_slot, uint32_t window_generation, void *source,
		uint32_t x, uint32_t y, uint32_t width, uint32_t height,
		uint64_t producing_frame, int resize_pending) {
	(void)state;
	(void)request;
	(void)state_identity;
	(void)renderer_generation;
	(void)window_slot;
	(void)window_generation;
	(void)source;
	(void)x;
	(void)y;
	(void)width;
	(void)height;
	(void)producing_frame;
	(void)resize_pending;
	return 0;
}

static int v_multiwindow_win32_d3d11_readback_test_resolve(void *state,
		uint64_t window_slot, uint32_t window_generation,
		uint64_t producing_frame, int succeeded, int failure_origin) {
	(void)state;
	(void)window_slot;
	(void)window_generation;
	(void)producing_frame;
	(void)succeeded;
	(void)failure_origin;
	return 0;
}

static int v_multiwindow_win32_d3d11_readback_test_resize_submitted(void *state,
		uint64_t window_slot, uint32_t window_generation) {
	(void)state;
	(void)window_slot;
	(void)window_generation;
	return 0;
}

static int v_multiwindow_win32_d3d11_readback_test_notify_failure(void *state,
		uint64_t state_identity, uint64_t renderer_generation, int failure_origin) {
	(void)state;
	(void)state_identity;
	(void)renderer_generation;
	(void)failure_origin;
	return 0;
}

static int v_multiwindow_win32_d3d11_readback_test_poll(void *state) {
	(void)state;
	return 0;
}

static int v_multiwindow_win32_d3d11_readback_test_take(void *state,
		VMultiwindowD3D11TestResult *result) {
	(void)state;
	(void)result;
	return 0;
}

static int v_multiwindow_win32_d3d11_readback_test_copy(void *state,
		uint64_t request, uint8_t *pixels, size_t capacity) {
	(void)state;
	(void)request;
	(void)pixels;
	(void)capacity;
	return 0;
}

static int v_multiwindow_win32_d3d11_readback_test_release(void *state,
		uint64_t request) {
	(void)state;
	(void)request;
	return 0;
}

static int v_multiwindow_win32_d3d11_readback_test_cancel(void *state,
		uint64_t request) {
	(void)state;
	(void)request;
	return 0;
}

static int v_multiwindow_win32_d3d11_readback_test_quiesce_window(void *state,
		uint64_t window_slot, uint32_t window_generation) {
	(void)state;
	(void)window_slot;
	(void)window_generation;
	return 0;
}

static int v_multiwindow_win32_d3d11_readback_test_restart(void *state,
		uint64_t state_identity, uint64_t renderer_generation) {
	(void)state;
	(void)state_identity;
	(void)renderer_generation;
	return 0;
}

static int v_multiwindow_win32_d3d11_readback_test_stop(void *state) {
	(void)state;
	return 0;
}

static int v_multiwindow_win32_d3d11_readback_test_active(void *state) {
	(void)state;
	return 0;
}

static uintptr_t v_multiwindow_win32_d3d11_readback_test_machine_identity(
		void *state) {
	(void)state;
	return 0;
}

static uintptr_t v_multiwindow_win32_d3d11_readback_native_machine_identity(
		void *state) {
	(void)state;
	return 0;
}

static void v_multiwindow_win32_d3d11_readback_test_destroy(void *state) {
	(void)state;
}
#endif

static void v_mw_d3d11_fake_log(VMultiwindowD3D11TestWorld *world, int operation) {
	if (world != NULL && world->journal_count < 256) {
		world->journal[world->journal_count++] = operation;
	}
}

static int v_mw_d3d11_fake_retain_source(void *world_ptr, void *source_ptr) {
	VMultiwindowD3D11TestWorld *world =
		(VMultiwindowD3D11TestWorld *)world_ptr;
	VMultiwindowD3D11TestSource *source =
		(VMultiwindowD3D11TestSource *)source_ptr;
	if (source == NULL || source->refs <= 0) {
		return 0;
	}
	v_mw_d3d11_fake_log(world, V_MW_D3D11_TEST_OP_RETAIN);
	source->refs++;
	source->add_refs++;
	return 1;
}

static void v_mw_d3d11_fake_release_source(void *world_ptr, void *source_ptr) {
	VMultiwindowD3D11TestWorld *world =
		(VMultiwindowD3D11TestWorld *)world_ptr;
	VMultiwindowD3D11TestSource *source =
		(VMultiwindowD3D11TestSource *)source_ptr;
	if (source != NULL) {
		source->release_calls++;
		v_mw_d3d11_fake_log(world, V_MW_D3D11_TEST_OP_RELEASE_SOURCE);
	}
	if (source != NULL && source->refs > 0) {
		source->refs--;
		source->releases++;
	}
}

static int v_mw_d3d11_fake_describe_source(void *world_ptr, void *source_ptr,
		uint32_t *width, uint32_t *height, uint32_t *format) {
	VMultiwindowD3D11TestWorld *world =
		(VMultiwindowD3D11TestWorld *)world_ptr;
	VMultiwindowD3D11TestSource *source =
		(VMultiwindowD3D11TestSource *)source_ptr;
	if (source == NULL || width == NULL || height == NULL || format == NULL) {
		return V_MW_D3D11_TEST_DEVICE_LOST;
	}
	v_mw_d3d11_fake_log(world, V_MW_D3D11_TEST_OP_DESCRIBE);
	*width = source->width;
	*height = source->height;
	*format = source->format;
	return V_MW_D3D11_TEST_OK;
}

static int v_mw_d3d11_fake_copy_region(void *world_ptr, void *source_ptr,
		uint32_t x, uint32_t y, uint32_t width, uint32_t height,
		void **out_staging) {
	VMultiwindowD3D11TestWorld *world =
		(VMultiwindowD3D11TestWorld *)world_ptr;
	VMultiwindowD3D11TestSource *source =
		(VMultiwindowD3D11TestSource *)source_ptr;
	if (world == NULL || source == NULL || out_staging == NULL
			|| world->staging_count >= 32 || width == 0 || height == 0
			|| x > source->width || y > source->height
			|| width > source->width - x || height > source->height - y
			|| width > (SIZE_MAX - 13) / 4
			|| height > SIZE_MAX / (width * 4 + 13)) {
		return V_MW_D3D11_TEST_DEVICE_LOST;
	}
	VMultiwindowD3D11TestStaging *staging =
		(VMultiwindowD3D11TestStaging *)calloc(1, sizeof(*staging));
	if (staging == NULL) {
		return V_MW_D3D11_TEST_DEVICE_LOST;
	}
	staging->width = width;
	staging->height = height;
	staging->format = source->format;
	staging->row_pitch = (size_t)width * 4 + 13;
	staging->pixels = (uint8_t *)calloc(height, staging->row_pitch);
	if (staging->pixels == NULL) {
		free(staging);
		return V_MW_D3D11_TEST_DEVICE_LOST;
	}
	for (uint32_t row = 0; row < height; row++) {
		memcpy(staging->pixels + (size_t)row * staging->row_pitch,
			source->pixels + (size_t)(y + row) * source->row_pitch + (size_t)x * 4,
			(size_t)width * 4);
	}
	world->staging[world->staging_count++] = staging;
	world->copy_calls++;
	v_mw_d3d11_fake_log(world, V_MW_D3D11_TEST_OP_COPY);
	*out_staging = staging;
	return V_MW_D3D11_TEST_OK;
}

static int v_mw_d3d11_fake_end_event_query(void *world_ptr, void **out_query) {
	VMultiwindowD3D11TestWorld *world =
		(VMultiwindowD3D11TestWorld *)world_ptr;
	if (world == NULL || out_query == NULL || world->query_count >= 32) {
		return V_MW_D3D11_TEST_DEVICE_LOST;
	}
	VMultiwindowD3D11TestQuery *query =
		(VMultiwindowD3D11TestQuery *)calloc(1, sizeof(*query));
	if (query == NULL) {
		return V_MW_D3D11_TEST_DEVICE_LOST;
	}
	world->queries[world->query_count++] = query;
	world->end_query_calls++;
	v_mw_d3d11_fake_log(world, V_MW_D3D11_TEST_OP_END_QUERY);
	*out_query = query;
	return V_MW_D3D11_TEST_OK;
}

static int v_mw_d3d11_fake_get_event_data(void *world_ptr, void *query_ptr) {
	VMultiwindowD3D11TestWorld *world =
		(VMultiwindowD3D11TestWorld *)world_ptr;
	if (world == NULL || query_ptr == NULL) {
		return V_MW_D3D11_TEST_DEVICE_LOST;
	}
	world->get_data_calls++;
	v_mw_d3d11_fake_log(world, V_MW_D3D11_TEST_OP_GET_DATA);
	if (world->query_result_index < world->query_result_count) {
		return world->query_results[world->query_result_index++];
	}
	return V_MW_D3D11_TEST_OK;
}

static int v_mw_d3d11_fake_map_staging(void *world_ptr, void *staging_ptr,
		const uint8_t **data, size_t *row_pitch) {
	VMultiwindowD3D11TestWorld *world =
		(VMultiwindowD3D11TestWorld *)world_ptr;
	VMultiwindowD3D11TestStaging *staging =
		(VMultiwindowD3D11TestStaging *)staging_ptr;
	if (world == NULL || staging == NULL || data == NULL || row_pitch == NULL) {
		return V_MW_D3D11_TEST_DEVICE_LOST;
	}
	world->map_calls++;
	v_mw_d3d11_fake_log(world, V_MW_D3D11_TEST_OP_MAP);
	if (world->map_result_index < world->map_result_count) {
		int result = world->map_results[world->map_result_index++];
		if (result != V_MW_D3D11_TEST_OK) {
			return result;
		}
	}
	if (world->map_result != V_MW_D3D11_TEST_OK) {
		return world->map_result;
	}
	*data = staging->pixels;
	*row_pitch = staging->row_pitch;
	return V_MW_D3D11_TEST_OK;
}

static void v_mw_d3d11_fake_unmap_staging(void *world_ptr, void *staging_ptr) {
	VMultiwindowD3D11TestWorld *world =
		(VMultiwindowD3D11TestWorld *)world_ptr;
	if (world != NULL && staging_ptr != NULL) {
		world->unmap_calls++;
		v_mw_d3d11_fake_log(world, V_MW_D3D11_TEST_OP_UNMAP);
	}
}

static void v_mw_d3d11_fake_release_staging(void *world_ptr, void *staging_ptr) {
	VMultiwindowD3D11TestWorld *world =
		(VMultiwindowD3D11TestWorld *)world_ptr;
	VMultiwindowD3D11TestStaging *staging =
		(VMultiwindowD3D11TestStaging *)staging_ptr;
	if (staging != NULL) {
		staging->release_calls++;
		v_mw_d3d11_fake_log(world, V_MW_D3D11_TEST_OP_RELEASE_STAGING);
	}
	if (staging != NULL && staging->releases == 0) {
		staging->releases++;
		free(staging->pixels);
		staging->pixels = NULL;
	}
}

static void v_mw_d3d11_fake_release_query(void *world_ptr, void *query_ptr) {
	VMultiwindowD3D11TestWorld *world =
		(VMultiwindowD3D11TestWorld *)world_ptr;
	VMultiwindowD3D11TestQuery *query =
		(VMultiwindowD3D11TestQuery *)query_ptr;
	if (query != NULL) {
		query->release_calls++;
		v_mw_d3d11_fake_log(world, V_MW_D3D11_TEST_OP_RELEASE_QUERY);
	}
	if (query != NULL && query->releases == 0) {
		query->releases++;
	}
}

static int v_mw_d3d11_fake_device_removed_reason(void *world_ptr) {
	VMultiwindowD3D11TestWorld *world =
		(VMultiwindowD3D11TestWorld *)world_ptr;
	if (world != NULL) {
		world->device_reason_calls++;
		v_mw_d3d11_fake_log(world, V_MW_D3D11_TEST_OP_DEVICE_REASON);
	}
	return V_MW_D3D11_TEST_DEVICE_LOST;
}

static const VMultiwindowD3D11TestOps v_mw_d3d11_fake_ops = {
	v_mw_d3d11_fake_retain_source,
	v_mw_d3d11_fake_release_source,
	v_mw_d3d11_fake_describe_source,
	v_mw_d3d11_fake_copy_region,
	v_mw_d3d11_fake_end_event_query,
	v_mw_d3d11_fake_get_event_data,
	v_mw_d3d11_fake_map_staging,
	v_mw_d3d11_fake_unmap_staging,
	v_mw_d3d11_fake_release_staging,
	v_mw_d3d11_fake_release_query,
	v_mw_d3d11_fake_device_removed_reason,
};

static VMultiwindowD3D11TestSource *v_mw_d3d11_fake_source(uint32_t width,
		uint32_t height, uint32_t format, uint8_t seed) {
	if (width == 0 || height == 0 || width > (SIZE_MAX - 7) / 4
			|| height > SIZE_MAX / (width * 4 + 7)) {
		return NULL;
	}
	VMultiwindowD3D11TestSource *source =
		(VMultiwindowD3D11TestSource *)calloc(1, sizeof(*source));
	if (source == NULL) {
		return NULL;
	}
	source->width = width;
	source->height = height;
	source->format = format;
	source->row_pitch = (size_t)width * 4 + 7;
	source->pixels = (uint8_t *)calloc(height, source->row_pitch);
	source->refs = 1;
	if (source->pixels == NULL) {
		free(source);
		return NULL;
	}
	for (uint32_t row = 0; row < height; row++) {
		for (uint32_t column = 0; column < width; column++) {
			size_t offset = (size_t)row * source->row_pitch + (size_t)column * 4;
			uint8_t base = (uint8_t)(seed + row * 13 + column * 3);
			if (format == V_MW_D3D11_TEST_FORMAT_BGRA8) {
				source->pixels[offset + 0] = (uint8_t)(base + 2);
				source->pixels[offset + 1] = (uint8_t)(base + 1);
				source->pixels[offset + 2] = base;
			} else {
				source->pixels[offset + 0] = base;
				source->pixels[offset + 1] = (uint8_t)(base + 1);
				source->pixels[offset + 2] = (uint8_t)(base + 2);
			}
			source->pixels[offset + 3] = (uint8_t)(base + 3);
		}
	}
	return source;
}

static void v_mw_d3d11_fake_source_destroy(VMultiwindowD3D11TestSource *source) {
	if (source != NULL) {
		free(source->pixels);
		free(source);
	}
}

static int v_mw_d3d11_fake_world_released_once(
		const VMultiwindowD3D11TestWorld *world) {
	if (world == NULL) {
		return 0;
	}
	for (int index = 0; index < world->staging_count; index++) {
		if (world->staging[index] == NULL
				|| world->staging[index]->releases != 1
				|| world->staging[index]->release_calls != 1) {
			return 0;
		}
	}
	for (int index = 0; index < world->query_count; index++) {
		if (world->queries[index] == NULL
				|| world->queries[index]->releases != 1
				|| world->queries[index]->release_calls != 1) {
			return 0;
		}
	}
	return 1;
}

static int v_mw_d3d11_test_result_pixel(uint8_t *pixels, size_t stride,
		uint32_t row, uint32_t column, uint8_t seed) {
	size_t offset = (size_t)row * stride + (size_t)column * 4;
	uint8_t base = (uint8_t)(seed + row * 13 + column * 3);
	return pixels[offset + 0] == base
		&& pixels[offset + 1] == (uint8_t)(base + 1)
		&& pixels[offset + 2] == (uint8_t)(base + 2)
		&& pixels[offset + 3] == (uint8_t)(base + 3);
}

static void *v_mw_d3d11_test_new_state(VMultiwindowD3D11TestWorld *world,
		uint64_t state_identity, uint64_t renderer_generation) {
	void *state = v_multiwindow_win32_d3d11_readback_test_create(
		&v_mw_d3d11_fake_ops, world);
	if (state == NULL
			|| !v_multiwindow_win32_d3d11_readback_test_bind(state,
				state_identity, renderer_generation)) {
		v_multiwindow_win32_d3d11_readback_test_destroy(state);
		return NULL;
	}
	return state;
}

static int v_multiwindow_win32_d3d11_readback_probe_addref_release(void) {
	VMultiwindowD3D11TestWorld world = {0};
	VMultiwindowD3D11TestSource *source =
		v_mw_d3d11_fake_source(3, 2, V_MW_D3D11_TEST_FORMAT_BGRA8, 7);
	void *state = v_mw_d3d11_test_new_state(&world, 11, 1);
	if (source == NULL || state == NULL
			|| !v_multiwindow_win32_d3d11_readback_test_stage(state, 101, 11, 1,
				1, 3, source, 0, 0, 3, 2, 9, 0)
			|| source->refs != 1 || source->add_refs != 1
			|| source->releases != 1 || source->release_calls != 1
			|| !v_multiwindow_win32_d3d11_readback_test_resolve(state,
				1, 3, 9, 1, 0)
			|| !v_multiwindow_win32_d3d11_readback_test_poll(state)) {
		v_multiwindow_win32_d3d11_readback_test_destroy(state);
		v_mw_d3d11_fake_source_destroy(source);
		return 0;
	}
	VMultiwindowD3D11TestResult result = {0};
	int ok = v_multiwindow_win32_d3d11_readback_test_take(state, &result)
		&& v_multiwindow_win32_d3d11_readback_test_release(state, result.request)
		&& !v_multiwindow_win32_d3d11_readback_test_release(state, result.request)
		&& source->refs == 1 && source->releases == 1 && source->release_calls == 1
		&& world.staging[0]->releases == 1
		&& world.staging[0]->release_calls == 1
		&& world.queries[0]->releases == 1
		&& world.queries[0]->release_calls == 1;
	v_multiwindow_win32_d3d11_readback_test_stop(state);
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	v_mw_d3d11_fake_source_destroy(source);
	return ok;
}

static int v_multiwindow_win32_d3d11_readback_probe_query_progression(void) {
	VMultiwindowD3D11TestWorld world = {0};
	world.query_results[0] = V_MW_D3D11_TEST_S_FALSE;
	world.query_results[1] = V_MW_D3D11_TEST_S_FALSE;
	world.query_results[2] = V_MW_D3D11_TEST_OK;
	world.query_result_count = 3;
	VMultiwindowD3D11TestSource *source =
		v_mw_d3d11_fake_source(1, 1, V_MW_D3D11_TEST_FORMAT_RGBA8, 17);
	void *state = v_mw_d3d11_test_new_state(&world, 12, 1);
	int ok = source != NULL && state != NULL
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, 102, 12, 1,
			1, 1, source, 0, 0, 1, 1, 10, 0)
		&& v_multiwindow_win32_d3d11_readback_test_resolve(state,
			1, 1, 10, 1, 0)
		&& !v_multiwindow_win32_d3d11_readback_test_poll(state)
		&& !v_multiwindow_win32_d3d11_readback_test_poll(state)
		&& world.map_calls == 0
		&& v_multiwindow_win32_d3d11_readback_test_poll(state)
		&& world.get_data_calls == 3 && world.map_calls == 1 && world.unmap_calls == 1;
	VMultiwindowD3D11TestResult result = {0};
	ok = ok && v_multiwindow_win32_d3d11_readback_test_take(state, &result)
		&& !v_multiwindow_win32_d3d11_readback_test_take(state, &result);
	v_multiwindow_win32_d3d11_readback_test_release(state, 102);
	v_multiwindow_win32_d3d11_readback_test_stop(state);
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	v_mw_d3d11_fake_source_destroy(source);
	return ok;
}

static int v_multiwindow_win32_d3d11_readback_probe_row_pitch_rgba(void) {
	const uint32_t widths[] = {1, 3, 257};
	for (size_t index = 0; index < 3; index++) {
		VMultiwindowD3D11TestWorld world = {0};
		uint32_t width = widths[index];
		uint8_t seed = (uint8_t)(21 + index * 11);
		VMultiwindowD3D11TestSource *source =
			v_mw_d3d11_fake_source(width, 2,
				V_MW_D3D11_TEST_FORMAT_BGRA8, seed);
		void *state = v_mw_d3d11_test_new_state(&world, 20 + index, 1);
		if (source == NULL || state == NULL
				|| !v_multiwindow_win32_d3d11_readback_test_stage(state,
					200 + index, 20 + index, 1, 2 + index, 1, source,
					0, 0, width, 2, 20 + index, 0)
				|| !v_multiwindow_win32_d3d11_readback_test_resolve(state,
					2 + index, 1, 20 + index, 1, 0)
				|| !v_multiwindow_win32_d3d11_readback_test_poll(state)) {
			v_multiwindow_win32_d3d11_readback_test_destroy(state);
			v_mw_d3d11_fake_source_destroy(source);
			return 0;
		}
		VMultiwindowD3D11TestResult result = {0};
		size_t expected_stride = (size_t)width * 4;
		size_t byte_length = expected_stride * 2;
		uint8_t *pixels = (uint8_t *)calloc(1, byte_length);
		int ok = pixels != NULL
			&& v_multiwindow_win32_d3d11_readback_test_take(state, &result)
			&& result.width == width && result.height == 2
			&& result.stride == expected_stride
			&& result.byte_length == byte_length
			&& v_multiwindow_win32_d3d11_readback_test_copy(state,
				result.request, pixels, byte_length)
			&& v_mw_d3d11_test_result_pixel(pixels, expected_stride,
				0, width - 1, seed)
			&& v_mw_d3d11_test_result_pixel(pixels, expected_stride,
				1, 0, seed);
		free(pixels);
		v_multiwindow_win32_d3d11_readback_test_release(state, result.request);
		v_multiwindow_win32_d3d11_readback_test_stop(state);
		v_multiwindow_win32_d3d11_readback_test_destroy(state);
		v_mw_d3d11_fake_source_destroy(source);
		if (!ok) {
			return 0;
		}
	}
	return 1;
}

static int v_multiwindow_win32_d3d11_readback_probe_producing_frame(void) {
	VMultiwindowD3D11TestWorld world = {0};
	VMultiwindowD3D11TestSource *source =
		v_mw_d3d11_fake_source(2, 1, V_MW_D3D11_TEST_FORMAT_RGBA8, 31);
	void *state = v_mw_d3d11_test_new_state(&world, 31, 4);
	if (source == NULL || state == NULL
			|| !v_multiwindow_win32_d3d11_readback_test_stage(state, 301, 31, 4,
				1, 1, source, 0, 0, 2, 1, 41, 0)
			|| v_multiwindow_win32_d3d11_readback_test_resolve(state,
				1, 1, 40, 1, 0)
			|| !v_multiwindow_win32_d3d11_readback_test_resolve(state,
				1, 1, 41, 1, 0)
			|| !v_multiwindow_win32_d3d11_readback_test_poll(state)) {
		v_multiwindow_win32_d3d11_readback_test_destroy(state);
		v_mw_d3d11_fake_source_destroy(source);
		return 0;
	}
	VMultiwindowD3D11TestResult result = {0};
	int ok = v_multiwindow_win32_d3d11_readback_test_take(state, &result)
		&& result.submitted_frame == 41;
	v_multiwindow_win32_d3d11_readback_test_release(state, result.request);
	ok = ok && v_multiwindow_win32_d3d11_readback_test_stage(state, 302, 31, 4,
		1, 1, source, 0, 0, 2, 1, 42, 0)
		&& v_multiwindow_win32_d3d11_readback_test_resolve(state,
			1, 1, 42, 0, V_MW_D3D11_TEST_FAILURE_PRESENT)
		&& v_multiwindow_win32_d3d11_readback_test_take(state, &result)
		&& result.status == V_MW_D3D11_TEST_STATUS_FAILED
		&& result.submitted_frame == 0
		&& !v_multiwindow_win32_d3d11_readback_test_take(state, &result);
	v_multiwindow_win32_d3d11_readback_test_release(state, 302);
	v_multiwindow_win32_d3d11_readback_test_stop(state);
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	v_mw_d3d11_fake_source_destroy(source);
	return ok;
}

static int v_multiwindow_win32_d3d11_readback_probe_two_windows(void) {
	VMultiwindowD3D11TestWorld world = {0};
	VMultiwindowD3D11TestSource *source_a =
		v_mw_d3d11_fake_source(1, 1, V_MW_D3D11_TEST_FORMAT_BGRA8, 41);
	VMultiwindowD3D11TestSource *source_b =
		v_mw_d3d11_fake_source(1, 1, V_MW_D3D11_TEST_FORMAT_BGRA8, 91);
	void *state = v_mw_d3d11_test_new_state(&world, 51, 2);
	int ok = source_a != NULL && source_b != NULL && state != NULL
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, 501, 51, 2,
			1, 8, source_a, 0, 0, 1, 1, 61, 0)
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, 502, 51, 2,
			2, 5, source_b, 0, 0, 1, 1, 61, 0)
		&& v_multiwindow_win32_d3d11_readback_test_resolve(state,
			1, 8, 61, 1, 0)
		&& v_multiwindow_win32_d3d11_readback_test_resolve(state,
			2, 5, 61, 1, 0)
		&& v_multiwindow_win32_d3d11_readback_test_poll(state);
	for (int index = 0; ok && index < 2; index++) {
		VMultiwindowD3D11TestResult result = {0};
		uint8_t pixels[4] = {0};
		ok = v_multiwindow_win32_d3d11_readback_test_take(state, &result)
			&& v_multiwindow_win32_d3d11_readback_test_copy(state,
				result.request, pixels, sizeof(pixels));
		if (result.request == 501) {
			ok = ok && result.window_slot == 1 && result.window_generation == 8
				&& v_mw_d3d11_test_result_pixel(pixels, 4, 0, 0, 41);
		} else if (result.request == 502) {
			ok = ok && result.window_slot == 2 && result.window_generation == 5
				&& v_mw_d3d11_test_result_pixel(pixels, 4, 0, 0, 91);
		} else {
			ok = 0;
		}
		v_multiwindow_win32_d3d11_readback_test_release(state, result.request);
	}
	ok = ok && !v_multiwindow_win32_d3d11_readback_test_take(state,
		&(VMultiwindowD3D11TestResult){0});
	v_multiwindow_win32_d3d11_readback_test_stop(state);
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	v_mw_d3d11_fake_source_destroy(source_a);
	v_mw_d3d11_fake_source_destroy(source_b);
	return ok;
}

static int v_multiwindow_win32_d3d11_readback_probe_resize_pending(void) {
	VMultiwindowD3D11TestWorld world = {0};
	VMultiwindowD3D11TestSource *source =
		v_mw_d3d11_fake_source(2, 2, V_MW_D3D11_TEST_FORMAT_RGBA8, 51);
	void *state = v_mw_d3d11_test_new_state(&world, 61, 1);
	int ok = source != NULL && state != NULL
		&& !v_multiwindow_win32_d3d11_readback_test_stage(state, 601, 61, 1,
			1, 4, source, 0, 0, 2, 2, 71, 1)
		&& source->add_refs == 0
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, 602, 61, 1,
			1, 4, source, 0, 0, 2, 2, 72, 0)
		&& v_multiwindow_win32_d3d11_readback_test_quiesce_window(state, 1, 4)
		&& !v_multiwindow_win32_d3d11_readback_test_quiesce_window(state, 1, 4)
		&& source->refs == 1 && source->releases == 1
		&& v_multiwindow_win32_d3d11_readback_test_active(state) == 0;
	VMultiwindowD3D11TestResult result = {0};
	ok = ok && !v_multiwindow_win32_d3d11_readback_test_take(state, &result);
	v_multiwindow_win32_d3d11_readback_test_stop(state);
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	v_mw_d3d11_fake_source_destroy(source);
	return ok;
}

static int v_multiwindow_win32_d3d11_readback_probe_device_lost(void) {
	for (int failure = 0; failure < 4; failure++) {
		VMultiwindowD3D11TestWorld world = {0};
		VMultiwindowD3D11TestSource *source =
			v_mw_d3d11_fake_source(2, 1, V_MW_D3D11_TEST_FORMAT_BGRA8,
				(uint8_t)(61 + failure));
		void *state = v_mw_d3d11_test_new_state(&world, 71 + failure, 3);
		if (source == NULL || state == NULL
				|| !v_multiwindow_win32_d3d11_readback_test_stage(state,
					701 + failure, 71 + failure, 3, 1, 1, source,
					0, 0, 2, 1, 81 + failure, 0)) {
			v_multiwindow_win32_d3d11_readback_test_destroy(state);
			v_mw_d3d11_fake_source_destroy(source);
			return 0;
		}
		if (failure == 0) {
			if (!v_multiwindow_win32_d3d11_readback_test_resolve(state,
					1, 1, 81, 0, V_MW_D3D11_TEST_FAILURE_PRESENT)) {
				return 0;
			}
		} else if (failure == 1) {
			if (!v_multiwindow_win32_d3d11_readback_test_notify_failure(state,
					72, 3, V_MW_D3D11_TEST_FAILURE_RESIZE)) {
				return 0;
			}
		} else {
			if (!v_multiwindow_win32_d3d11_readback_test_resolve(state,
					1, 1, 81 + failure, 1, 0)) {
				return 0;
			}
			if (failure == 2) {
				world.query_results[0] = V_MW_D3D11_TEST_DEVICE_LOST;
				world.query_result_count = 1;
			} else {
				world.map_result = V_MW_D3D11_TEST_DEVICE_LOST;
			}
			(void)v_multiwindow_win32_d3d11_readback_test_poll(state);
		}
		VMultiwindowD3D11TestResult result = {0};
		int ok = v_multiwindow_win32_d3d11_readback_test_take(state, &result)
			&& result.status == V_MW_D3D11_TEST_STATUS_FAILED
			&& result.submitted_frame == 0
			&& world.device_reason_calls == 1
			&& !v_multiwindow_win32_d3d11_readback_test_take(state, &result);
		v_multiwindow_win32_d3d11_readback_test_release(state, result.request);
		v_multiwindow_win32_d3d11_readback_test_stop(state);
		v_multiwindow_win32_d3d11_readback_test_destroy(state);
		ok = ok && source->refs == 1
			&& source->release_calls == source->add_refs
			&& v_mw_d3d11_fake_world_released_once(&world);
		if (!ok) {
			v_mw_d3d11_fake_source_destroy(source);
			return 0;
		}
		v_mw_d3d11_fake_source_destroy(source);
	}
	return 1;
}

static int v_multiwindow_win32_d3d11_readback_probe_poll_before_resolve(void) {
	VMultiwindowD3D11TestWorld world = {0};
	VMultiwindowD3D11TestSource *source =
		v_mw_d3d11_fake_source(2, 1, V_MW_D3D11_TEST_FORMAT_RGBA8, 63);
	void *state = v_mw_d3d11_test_new_state(&world, 75, 2);
	int ok = source != NULL && state != NULL
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, 751, 75, 2,
			1, 1, source, 0, 0, 2, 1, 85, 0)
		&& !v_multiwindow_win32_d3d11_readback_test_poll(state)
		&& world.get_data_calls == 0 && world.map_calls == 0;
	VMultiwindowD3D11TestResult result = {0};
	ok = ok && !v_multiwindow_win32_d3d11_readback_test_take(state, &result)
		&& v_multiwindow_win32_d3d11_readback_test_resolve(state,
			1, 1, 85, 1, 0)
		&& v_multiwindow_win32_d3d11_readback_test_poll(state)
		&& world.get_data_calls == 1 && world.map_calls == 1
		&& v_multiwindow_win32_d3d11_readback_test_take(state, &result)
		&& result.status == V_MW_D3D11_TEST_STATUS_READY;
	v_multiwindow_win32_d3d11_readback_test_release(state, 751);
	v_multiwindow_win32_d3d11_readback_test_stop(state);
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	ok = ok && source->refs == 1
		&& source->release_calls == source->add_refs
		&& v_mw_d3d11_fake_world_released_once(&world);
	v_mw_d3d11_fake_source_destroy(source);
	return ok;
}

static int v_multiwindow_win32_d3d11_readback_probe_map_still_drawing(void) {
	VMultiwindowD3D11TestWorld world = {0};
	world.map_results[0] = V_MW_D3D11_TEST_WAS_STILL_DRAWING;
	world.map_results[1] = V_MW_D3D11_TEST_OK;
	world.map_result_count = 2;
	VMultiwindowD3D11TestSource *source =
		v_mw_d3d11_fake_source(3, 1, V_MW_D3D11_TEST_FORMAT_BGRA8, 67);
	void *state = v_mw_d3d11_test_new_state(&world, 76, 2);
	int ok = source != NULL && state != NULL
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, 761, 76, 2,
			1, 1, source, 0, 0, 3, 1, 86, 0)
		&& v_multiwindow_win32_d3d11_readback_test_resolve(state,
			1, 1, 86, 1, 0)
		&& !v_multiwindow_win32_d3d11_readback_test_poll(state)
		&& world.map_calls == 1 && world.unmap_calls == 0;
	VMultiwindowD3D11TestResult result = {0};
	ok = ok && !v_multiwindow_win32_d3d11_readback_test_take(state, &result)
		&& v_multiwindow_win32_d3d11_readback_test_poll(state)
		&& world.map_calls == 2 && world.unmap_calls == 1
		&& v_multiwindow_win32_d3d11_readback_test_take(state, &result)
		&& result.status == V_MW_D3D11_TEST_STATUS_READY;
	v_multiwindow_win32_d3d11_readback_test_release(state, 761);
	v_multiwindow_win32_d3d11_readback_test_stop(state);
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	v_mw_d3d11_fake_source_destroy(source);
	return ok;
}

static int v_multiwindow_win32_d3d11_readback_probe_rgba_passthrough(void) {
	VMultiwindowD3D11TestWorld world = {0};
	VMultiwindowD3D11TestSource *source =
		v_mw_d3d11_fake_source(3, 2, V_MW_D3D11_TEST_FORMAT_RGBA8, 73);
	void *state = v_mw_d3d11_test_new_state(&world, 77, 2);
	if (source == NULL || state == NULL
			|| !v_multiwindow_win32_d3d11_readback_test_stage(state, 771, 77, 2,
				1, 1, source, 0, 0, 3, 2, 87, 0)
			|| !v_multiwindow_win32_d3d11_readback_test_resolve(state,
				1, 1, 87, 1, 0)
			|| !v_multiwindow_win32_d3d11_readback_test_poll(state)) {
		v_multiwindow_win32_d3d11_readback_test_destroy(state);
		v_mw_d3d11_fake_source_destroy(source);
		return 0;
	}
	VMultiwindowD3D11TestResult result = {0};
	uint8_t pixels[24] = {0};
	int ok = v_multiwindow_win32_d3d11_readback_test_take(state, &result)
		&& result.stride == 12 && result.byte_length == sizeof(pixels)
		&& v_multiwindow_win32_d3d11_readback_test_copy(state, 771,
			pixels, sizeof(pixels))
		&& v_mw_d3d11_test_result_pixel(pixels, 12, 0, 0, 73)
		&& v_mw_d3d11_test_result_pixel(pixels, 12, 0, 2, 73)
		&& v_mw_d3d11_test_result_pixel(pixels, 12, 1, 1, 73);
	v_multiwindow_win32_d3d11_readback_test_release(state, 771);
	v_multiwindow_win32_d3d11_readback_test_stop(state);
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	v_mw_d3d11_fake_source_destroy(source);
	return ok;
}

static int v_mw_d3d11_readback_probe_destroy_phase(int phase) {
	VMultiwindowD3D11TestWorld world = {0};
	if (phase == 1) {
		world.query_results[0] = V_MW_D3D11_TEST_S_FALSE;
		world.query_result_count = 1;
	}
	VMultiwindowD3D11TestSource *source =
		v_mw_d3d11_fake_source(1, 1, V_MW_D3D11_TEST_FORMAT_RGBA8,
			(uint8_t)(81 + phase));
	void *state = v_mw_d3d11_test_new_state(&world, 78 + phase, 5);
	if (source == NULL || state == NULL
			|| !v_multiwindow_win32_d3d11_readback_test_stage(state,
				781 + phase, 78 + phase, 5, 1, 1, source,
				0, 0, 1, 1, 88 + phase, 0)) {
		v_multiwindow_win32_d3d11_readback_test_destroy(state);
		v_mw_d3d11_fake_source_destroy(source);
		return 0;
	}
	if (phase >= 1
			&& !v_multiwindow_win32_d3d11_readback_test_resolve(state,
				1, 1, 88 + phase, 1, 0)) {
		v_multiwindow_win32_d3d11_readback_test_destroy(state);
		v_mw_d3d11_fake_source_destroy(source);
		return 0;
	}
	if (phase == 1 && v_multiwindow_win32_d3d11_readback_test_poll(state)) {
		v_multiwindow_win32_d3d11_readback_test_destroy(state);
		v_mw_d3d11_fake_source_destroy(source);
		return 0;
	}
	if (phase == 2 && !v_multiwindow_win32_d3d11_readback_test_poll(state)) {
		v_multiwindow_win32_d3d11_readback_test_destroy(state);
		v_mw_d3d11_fake_source_destroy(source);
		return 0;
	}
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	int ok = source->refs == 1 && source->add_refs == 1 && source->releases == 1
		&& world.staging_count == 1 && world.staging[0]->releases == 1
		&& world.query_count == 1 && world.queries[0]->releases == 1;
	v_mw_d3d11_fake_source_destroy(source);
	return ok;
}

static int v_multiwindow_win32_d3d11_readback_probe_destroy_direct(void) {
	return v_mw_d3d11_readback_probe_destroy_phase(0)
		&& v_mw_d3d11_readback_probe_destroy_phase(1)
		&& v_mw_d3d11_readback_probe_destroy_phase(2);
}

static int v_multiwindow_win32_d3d11_readback_probe_targeted_resolve(void) {
	VMultiwindowD3D11TestWorld world = {0};
	VMultiwindowD3D11TestSource *source_a =
		v_mw_d3d11_fake_source(1, 1, V_MW_D3D11_TEST_FORMAT_RGBA8, 91);
	VMultiwindowD3D11TestSource *source_b =
		v_mw_d3d11_fake_source(1, 1, V_MW_D3D11_TEST_FORMAT_RGBA8, 101);
	void *state = v_mw_d3d11_test_new_state(&world, 90, 3);
	int ok = source_a != NULL && source_b != NULL && state != NULL
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, 901, 90, 3,
			1, 7, source_a, 0, 0, 1, 1, 100, 0)
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, 902, 90, 3,
			2, 9, source_b, 0, 0, 1, 1, 100, 0)
		&& v_multiwindow_win32_d3d11_readback_test_resolve(state,
			1, 7, 100, 1, 0)
		&& v_multiwindow_win32_d3d11_readback_test_resolve(state,
			2, 9, 100, 0, 0)
		&& v_multiwindow_win32_d3d11_readback_test_poll(state);
	int ready = 0;
	int failed = 0;
	for (int index = 0; ok && index < 2; index++) {
		VMultiwindowD3D11TestResult result = {0};
		ok = v_multiwindow_win32_d3d11_readback_test_take(state, &result);
		if (result.request == 901 && result.window_slot == 1
				&& result.window_generation == 7
				&& result.status == V_MW_D3D11_TEST_STATUS_READY) {
			ready++;
		} else if (result.request == 902 && result.window_slot == 2
				&& result.window_generation == 9
				&& result.status == V_MW_D3D11_TEST_STATUS_FAILED) {
			failed++;
		} else {
			ok = 0;
		}
		v_multiwindow_win32_d3d11_readback_test_release(state, result.request);
	}
	ok = ok && ready == 1 && failed == 1
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, 903, 90, 3,
			1, 7, source_a, 0, 0, 1, 1, 101, 0)
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, 904, 90, 3,
			2, 9, source_b, 0, 0, 1, 1, 101, 0)
		&& v_multiwindow_win32_d3d11_readback_test_resolve(state,
			1, 7, 101, 0, 0)
		&& v_multiwindow_win32_d3d11_readback_test_resolve(state,
			2, 9, 101, 1, 0)
		&& v_multiwindow_win32_d3d11_readback_test_poll(state);
	ready = 0;
	failed = 0;
	for (int index = 0; ok && index < 2; index++) {
		VMultiwindowD3D11TestResult result = {0};
		ok = v_multiwindow_win32_d3d11_readback_test_take(state, &result);
		if (result.request == 903 && result.window_slot == 1
				&& result.status == V_MW_D3D11_TEST_STATUS_FAILED) {
			failed++;
		} else if (result.request == 904 && result.window_slot == 2
				&& result.status == V_MW_D3D11_TEST_STATUS_READY) {
			ready++;
		} else {
			ok = 0;
		}
		v_multiwindow_win32_d3d11_readback_test_release(state, result.request);
	}
	ok = ok && ready == 1 && failed == 1;
	v_multiwindow_win32_d3d11_readback_test_stop(state);
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	v_mw_d3d11_fake_source_destroy(source_a);
	v_mw_d3d11_fake_source_destroy(source_b);
	return ok;
}

static int v_multiwindow_win32_d3d11_readback_probe_operation_journal(void) {
	const int expected[] = {
		V_MW_D3D11_TEST_OP_RETAIN,
		V_MW_D3D11_TEST_OP_DESCRIBE,
		V_MW_D3D11_TEST_OP_COPY,
		V_MW_D3D11_TEST_OP_END_QUERY,
		V_MW_D3D11_TEST_OP_RELEASE_SOURCE,
		V_MW_D3D11_TEST_OP_GET_DATA,
		V_MW_D3D11_TEST_OP_MAP,
		V_MW_D3D11_TEST_OP_UNMAP,
		V_MW_D3D11_TEST_OP_RELEASE_STAGING,
		V_MW_D3D11_TEST_OP_RELEASE_QUERY,
	};
	VMultiwindowD3D11TestWorld world = {0};
	VMultiwindowD3D11TestSource *source =
		v_mw_d3d11_fake_source(1, 1, V_MW_D3D11_TEST_FORMAT_BGRA8, 111);
	void *state = v_mw_d3d11_test_new_state(&world, 91, 1);
	int ok = source != NULL && state != NULL
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, 911, 91, 1,
			1, 1, source, 0, 0, 1, 1, 111, 0)
		&& source->release_calls == 1
		&& v_multiwindow_win32_d3d11_readback_test_resolve(state,
			1, 1, 111, 1, 0)
		&& v_multiwindow_win32_d3d11_readback_test_poll(state)
		&& world.journal_count == (int)(sizeof(expected) / sizeof(expected[0]));
	for (size_t index = 0; ok && index < sizeof(expected) / sizeof(expected[0]);
			index++) {
		ok = world.journal[index] == expected[index];
	}
	VMultiwindowD3D11TestResult result = {0};
	ok = ok && v_multiwindow_win32_d3d11_readback_test_take(state, &result)
		&& v_multiwindow_win32_d3d11_readback_test_release(state, result.request)
		&& world.journal_count == (int)(sizeof(expected) / sizeof(expected[0]));
	v_multiwindow_win32_d3d11_readback_test_stop(state);
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	v_mw_d3d11_fake_source_destroy(source);
	return ok;
}

static int v_multiwindow_win32_d3d11_readback_probe_subregion_canaries(void) {
	VMultiwindowD3D11TestWorld world = {0};
	const uint8_t seed = 121;
	VMultiwindowD3D11TestSource *source =
		v_mw_d3d11_fake_source(5, 4, V_MW_D3D11_TEST_FORMAT_BGRA8, seed);
	void *state = v_mw_d3d11_test_new_state(&world, 92, 1);
	if (source == NULL || state == NULL
			|| !v_multiwindow_win32_d3d11_readback_test_stage(state, 921, 92, 1,
				1, 1, source, 1, 1, 3, 2, 112, 0)
			|| !v_multiwindow_win32_d3d11_readback_test_resolve(state,
				1, 1, 112, 1, 0)
			|| !v_multiwindow_win32_d3d11_readback_test_poll(state)) {
		v_multiwindow_win32_d3d11_readback_test_destroy(state);
		v_mw_d3d11_fake_source_destroy(source);
		return 0;
	}
	VMultiwindowD3D11TestResult result = {0};
	uint8_t storage[40];
	memset(storage, 0xA5, sizeof(storage));
	int ok = v_multiwindow_win32_d3d11_readback_test_take(state, &result)
		&& result.width == 3 && result.height == 2
		&& result.stride == 12 && result.byte_length == 24
		&& !v_multiwindow_win32_d3d11_readback_test_copy(state, 921,
			storage + 8, 23);
	for (size_t index = 0; ok && index < sizeof(storage); index++) {
		ok = storage[index] == 0xA5;
	}
	ok = ok && v_multiwindow_win32_d3d11_readback_test_copy(state, 921,
		storage + 8, 24);
	for (size_t index = 0; ok && index < 8; index++) {
		ok = storage[index] == 0xA5 && storage[32 + index] == 0xA5;
	}
	for (uint32_t row = 0; ok && row < 2; row++) {
		for (uint32_t column = 0; ok && column < 3; column++) {
			size_t offset = 8 + (size_t)row * 12 + (size_t)column * 4;
			uint8_t base = (uint8_t)(seed + (row + 1) * 13 + (column + 1) * 3);
			ok = storage[offset + 0] == base
				&& storage[offset + 1] == (uint8_t)(base + 1)
				&& storage[offset + 2] == (uint8_t)(base + 2)
				&& storage[offset + 3] == (uint8_t)(base + 3);
		}
	}
	v_multiwindow_win32_d3d11_readback_test_release(state, 921);
	v_multiwindow_win32_d3d11_readback_test_stop(state);
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	v_mw_d3d11_fake_source_destroy(source);
	return ok;
}

static int v_multiwindow_win32_d3d11_readback_probe_resize_after_submit(void) {
	VMultiwindowD3D11TestWorld world = {0};
	world.query_results[0] = V_MW_D3D11_TEST_S_FALSE;
	world.query_results[1] = V_MW_D3D11_TEST_OK;
	world.query_result_count = 2;
	VMultiwindowD3D11TestSource *source =
		v_mw_d3d11_fake_source(3, 1, V_MW_D3D11_TEST_FORMAT_BGRA8, 131);
	void *state = v_mw_d3d11_test_new_state(&world, 93, 1);
	int ok = source != NULL && state != NULL
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, 931, 93, 1,
			1, 6, source, 0, 0, 3, 1, 113, 0)
		&& source->refs == 1 && source->release_calls == 1
		&& v_multiwindow_win32_d3d11_readback_test_resolve(state,
			1, 6, 113, 1, 0)
		&& !v_multiwindow_win32_d3d11_readback_test_poll(state)
		&& v_multiwindow_win32_d3d11_readback_test_resize_submitted(state,
			1, 6)
		&& v_multiwindow_win32_d3d11_readback_test_poll(state);
	VMultiwindowD3D11TestResult result = {0};
	ok = ok && v_multiwindow_win32_d3d11_readback_test_take(state, &result)
		&& result.status == V_MW_D3D11_TEST_STATUS_READY
		&& result.submitted_frame == 113;
	v_multiwindow_win32_d3d11_readback_test_release(state, 931);
	v_multiwindow_win32_d3d11_readback_test_stop(state);
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	v_mw_d3d11_fake_source_destroy(source);
	return ok;
}

static int v_multiwindow_win32_d3d11_readback_probe_device_loss_latch(void) {
	VMultiwindowD3D11TestWorld world_a = {0};
	VMultiwindowD3D11TestWorld world_b = {0};
	VMultiwindowD3D11TestSource *source_a =
		v_mw_d3d11_fake_source(1, 1, V_MW_D3D11_TEST_FORMAT_RGBA8, 141);
	VMultiwindowD3D11TestSource *source_b =
		v_mw_d3d11_fake_source(1, 1, V_MW_D3D11_TEST_FORMAT_RGBA8, 151);
	void *state_a = v_mw_d3d11_test_new_state(&world_a, 94, 7);
	void *state_b = v_mw_d3d11_test_new_state(&world_b, 95, 7);
	int ok = source_a != NULL && source_b != NULL
		&& state_a != NULL && state_b != NULL
		&& v_multiwindow_win32_d3d11_readback_test_stage(state_a, 941, 94, 7,
			1, 1, source_a, 0, 0, 1, 1, 114, 0)
		&& v_multiwindow_win32_d3d11_readback_test_stage(state_a, 942, 94, 7,
			2, 1, source_a, 0, 0, 1, 1, 114, 0)
		&& v_multiwindow_win32_d3d11_readback_test_stage(state_b, 951, 95, 7,
			1, 1, source_b, 0, 0, 1, 1, 114, 0)
		&& v_multiwindow_win32_d3d11_readback_test_notify_failure(state_a,
			94, 7, V_MW_D3D11_TEST_FAILURE_PRESENT);
	for (int index = 0; ok && index < 2; index++) {
		VMultiwindowD3D11TestResult result = {0};
		ok = v_multiwindow_win32_d3d11_readback_test_take(state_a, &result)
			&& result.status == V_MW_D3D11_TEST_STATUS_FAILED
			&& result.submitted_frame == 0;
		v_multiwindow_win32_d3d11_readback_test_release(state_a, result.request);
	}
	VMultiwindowD3D11TestResult result = {0};
	ok = ok && world_a.device_reason_calls == 1
		&& !v_multiwindow_win32_d3d11_readback_test_take(state_a, &result)
		&& !v_multiwindow_win32_d3d11_readback_test_stage(state_a, 943, 94, 7,
			1, 1, source_a, 0, 0, 1, 1, 115, 0)
		&& v_multiwindow_win32_d3d11_readback_test_resolve(state_b,
			1, 1, 114, 1, 0)
		&& v_multiwindow_win32_d3d11_readback_test_poll(state_b)
		&& v_multiwindow_win32_d3d11_readback_test_take(state_b, &result)
		&& result.status == V_MW_D3D11_TEST_STATUS_READY
		&& world_b.device_reason_calls == 0;
	v_multiwindow_win32_d3d11_readback_test_release(state_b, result.request);
	ok = ok && v_multiwindow_win32_d3d11_readback_test_restart(state_a, 96, 8)
		&& v_multiwindow_win32_d3d11_readback_test_stage(state_a, 944, 96, 8,
			1, 2, source_a, 0, 0, 1, 1, 116, 0)
		&& v_multiwindow_win32_d3d11_readback_test_resolve(state_a,
			1, 2, 116, 1, 0)
		&& v_multiwindow_win32_d3d11_readback_test_poll(state_a)
		&& v_multiwindow_win32_d3d11_readback_test_take(state_a, &result)
		&& result.status == V_MW_D3D11_TEST_STATUS_READY;
	v_multiwindow_win32_d3d11_readback_test_release(state_a, result.request);
	v_multiwindow_win32_d3d11_readback_test_stop(state_a);
	v_multiwindow_win32_d3d11_readback_test_stop(state_b);
	v_multiwindow_win32_d3d11_readback_test_destroy(state_a);
	v_multiwindow_win32_d3d11_readback_test_destroy(state_b);
	ok = ok && source_a->refs == 1
		&& source_a->release_calls == source_a->add_refs
		&& source_b->refs == 1
		&& source_b->release_calls == source_b->add_refs
		&& v_mw_d3d11_fake_world_released_once(&world_a)
		&& v_mw_d3d11_fake_world_released_once(&world_b);
	v_mw_d3d11_fake_source_destroy(source_a);
	v_mw_d3d11_fake_source_destroy(source_b);
	return ok;
}

static int v_mw_d3d11_readback_lifecycle_case(int operation, int phase) {
	VMultiwindowD3D11TestWorld world = {0};
	if (phase == 1) {
		world.query_results[0] = V_MW_D3D11_TEST_S_FALSE;
		world.query_result_count = 1;
	}
	VMultiwindowD3D11TestSource *source =
		v_mw_d3d11_fake_source(1, 1, V_MW_D3D11_TEST_FORMAT_RGBA8,
			(uint8_t)(161 + operation * 7 + phase));
	void *state = v_mw_d3d11_test_new_state(&world,
		100 + operation * 3 + phase, 1);
	uint64_t request = (uint64_t)(1000 + operation * 10 + phase);
	int ok = source != NULL && state != NULL
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, request,
			100 + operation * 3 + phase, 1, 1, 1, source,
			0, 0, 1, 1, 120 + phase, 0);
	if (ok && phase >= 1) {
		ok = v_multiwindow_win32_d3d11_readback_test_resolve(state,
			1, 1, 120 + phase, 1, 0);
	}
	if (ok && phase == 1) {
		ok = !v_multiwindow_win32_d3d11_readback_test_poll(state);
	}
	if (ok && phase == 2) {
		ok = v_multiwindow_win32_d3d11_readback_test_poll(state);
	}
	if (ok && operation == 0) {
		ok = v_multiwindow_win32_d3d11_readback_test_cancel(state, request)
			&& !v_multiwindow_win32_d3d11_readback_test_cancel(state, request);
	} else if (ok && operation == 1) {
		ok = v_multiwindow_win32_d3d11_readback_test_quiesce_window(state, 1, 1)
			&& !v_multiwindow_win32_d3d11_readback_test_quiesce_window(state, 1, 1);
	} else if (ok && operation == 2) {
		ok = v_multiwindow_win32_d3d11_readback_test_stop(state)
			&& !v_multiwindow_win32_d3d11_readback_test_stop(state);
	} else if (ok) {
		v_multiwindow_win32_d3d11_readback_test_destroy(state);
		state = NULL;
	}
	VMultiwindowD3D11TestResult result = {0};
	if (ok && state != NULL) {
		ok = !v_multiwindow_win32_d3d11_readback_test_take(state, &result)
			&& v_multiwindow_win32_d3d11_readback_test_active(state) == 0;
		v_multiwindow_win32_d3d11_readback_test_destroy(state);
	}
	ok = ok && source->refs == 1 && source->release_calls == 1
		&& world.staging_count == 1 && world.staging[0]->release_calls == 1
		&& world.query_count == 1 && world.queries[0]->release_calls == 1;
	v_mw_d3d11_fake_source_destroy(source);
	return ok;
}

static int v_multiwindow_win32_d3d11_readback_probe_lifecycle_matrix(void) {
	for (int operation = 0; operation < 4; operation++) {
		for (int phase = 0; phase < 3; phase++) {
			if (!v_mw_d3d11_readback_lifecycle_case(operation, phase)) {
				return 0;
			}
		}
	}
	return 1;
}

static int v_multiwindow_win32_d3d11_readback_probe_shared_machine(void) {
	VMultiwindowD3D11TestWorld world = {0};
	void *state = v_mw_d3d11_test_new_state(&world, 150, 1);
	uintptr_t test_identity =
		v_multiwindow_win32_d3d11_readback_test_machine_identity(state);
	uintptr_t native_identity =
		v_multiwindow_win32_d3d11_readback_native_machine_identity(state);
	int ok = state != NULL && test_identity != 0 && test_identity == native_identity;
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	return ok;
}

static int v_multiwindow_win32_d3d11_readback_probe_restart_teardown(void) {
	VMultiwindowD3D11TestWorld world = {0};
	VMultiwindowD3D11TestSource *source_a =
		v_mw_d3d11_fake_source(1, 1, V_MW_D3D11_TEST_FORMAT_RGBA8, 71);
	VMultiwindowD3D11TestSource *source_b =
		v_mw_d3d11_fake_source(1, 1, V_MW_D3D11_TEST_FORMAT_RGBA8, 81);
	void *state = v_mw_d3d11_test_new_state(&world, 81, 1);
	int ok = source_a != NULL && source_b != NULL && state != NULL
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, 801, 81, 1,
			1, 1, source_a, 0, 0, 1, 1, 91, 0)
		&& v_multiwindow_win32_d3d11_readback_test_restart(state, 82, 2)
		&& source_a->refs == 1 && source_a->releases == 1
		&& v_multiwindow_win32_d3d11_readback_test_active(state) == 0
		&& !v_multiwindow_win32_d3d11_readback_test_resolve(state,
			1, 1, 91, 1, 0)
		&& v_multiwindow_win32_d3d11_readback_test_stage(state, 802, 82, 2,
			2, 1, source_b, 0, 0, 1, 1, 92, 0)
		&& v_multiwindow_win32_d3d11_readback_test_stop(state)
		&& !v_multiwindow_win32_d3d11_readback_test_stop(state)
		&& source_b->refs == 1 && source_b->releases == 1
		&& v_multiwindow_win32_d3d11_readback_test_active(state) == 0;
	VMultiwindowD3D11TestResult result = {0};
	ok = ok && !v_multiwindow_win32_d3d11_readback_test_take(state, &result);
	v_multiwindow_win32_d3d11_readback_test_destroy(state);
	v_mw_d3d11_fake_source_destroy(source_a);
	v_mw_d3d11_fake_source_destroy(source_b);
	return ok;
}

#endif
