#ifndef V_MULTIWINDOW_WIN32_READBACK_D3D11_HELPERS_H
#define V_MULTIWINDOW_WIN32_READBACK_D3D11_HELPERS_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32)
#include <windows.h>
#endif

#ifndef V_MW_D3D11_TEST_OK
#define V_MW_D3D11_TEST_OK 0
#endif
#ifndef V_MW_D3D11_TEST_S_FALSE
#define V_MW_D3D11_TEST_S_FALSE 1
#endif
#ifndef V_MW_D3D11_TEST_WAS_STILL_DRAWING
#define V_MW_D3D11_TEST_WAS_STILL_DRAWING ((int32_t)UINT32_C(0x887A000A))
#endif
#ifndef V_MW_D3D11_TEST_FORMAT_BGRA8
#define V_MW_D3D11_TEST_FORMAT_BGRA8 1
#endif
#ifndef V_MW_D3D11_TEST_FORMAT_RGBA8
#define V_MW_D3D11_TEST_FORMAT_RGBA8 2
#endif
#ifndef V_MW_D3D11_TEST_STATUS_READY
#define V_MW_D3D11_TEST_STATUS_READY 1
#endif
#ifndef V_MW_D3D11_TEST_STATUS_FAILED
#define V_MW_D3D11_TEST_STATUS_FAILED 2
#endif

#define V_MULTIWINDOW_WIN32_READBACK_MAX_RECORDS 64

#if defined(V_MULTIWINDOW_WIN32_D3D11_READBACK_TEST)
typedef VMultiwindowD3D11TestOps VMultiwindowWin32ReadbackOps;
#else
typedef struct VMultiwindowWin32ReadbackOps {
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
} VMultiwindowWin32ReadbackOps;
#endif

typedef struct VMultiwindowWin32ReadbackResult {
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
} VMultiwindowWin32ReadbackResult;

typedef enum VMultiwindowWin32ReadbackPhase {
	V_MULTIWINDOW_WIN32_READBACK_FREE = 0,
	V_MULTIWINDOW_WIN32_READBACK_WAITING_SUBMIT = 1,
	V_MULTIWINDOW_WIN32_READBACK_GPU_PENDING = 2,
	V_MULTIWINDOW_WIN32_READBACK_READY = 3,
	V_MULTIWINDOW_WIN32_READBACK_FAILED = 4,
} VMultiwindowWin32ReadbackPhase;

typedef struct VMultiwindowWin32ReadbackRecord {
	int used;
	int taken;
	int copied;
	VMultiwindowWin32ReadbackPhase phase;
	uint64_t request;
	uint64_t state_identity;
	uint64_t renderer_generation;
	uint64_t window_slot;
	uint32_t window_generation;
	uint64_t producing_frame;
	uint64_t submitted_frame;
	uint32_t width;
	uint32_t height;
	uint32_t format;
	size_t public_stride;
	size_t byte_length;
	void *staging;
	void *query;
	uint8_t *pixels;
} VMultiwindowWin32ReadbackRecord;

typedef struct VMultiwindowWin32ReadbackMachine {
	VMultiwindowWin32ReadbackOps ops;
	void *world;
	uint64_t state_identity;
	uint64_t renderer_generation;
	uint64_t owner_thread;
	int bound;
	int stopped;
	int device_lost;
	VMultiwindowWin32ReadbackRecord
		records[V_MULTIWINDOW_WIN32_READBACK_MAX_RECORDS];
} VMultiwindowWin32ReadbackMachine;

static uint64_t v_multiwindow_win32_readback_current_thread(void) {
#if defined(_WIN32)
	return (uint64_t)GetCurrentThreadId();
#else
	// The portable fake-COM harness is intentionally single-threaded.
	return UINT64_C(1);
#endif
}

static int v_multiwindow_win32_readback_is_owner(
		const VMultiwindowWin32ReadbackMachine *machine) {
	return machine != NULL && machine->owner_thread != 0
		&& machine->owner_thread == v_multiwindow_win32_readback_current_thread();
}

static int v_multiwindow_win32_readback_ops_valid(
		const VMultiwindowWin32ReadbackOps *ops) {
	return ops != NULL
		&& ops->retain_source != NULL
		&& ops->release_source != NULL
		&& ops->describe_source != NULL
		&& ops->copy_region != NULL
		&& ops->end_event_query != NULL
		&& ops->get_event_data != NULL
		&& ops->map_staging != NULL
		&& ops->unmap_staging != NULL
		&& ops->release_staging != NULL
		&& ops->release_query != NULL
		&& ops->device_removed_reason != NULL;
}

static void v_multiwindow_win32_readback_release_native(
		VMultiwindowWin32ReadbackMachine *machine,
		VMultiwindowWin32ReadbackRecord *record) {
	if (machine == NULL || record == NULL) {
		return;
	}
	if (record->staging != NULL) {
		machine->ops.release_staging(machine->world, record->staging);
		record->staging = NULL;
	}
	if (record->query != NULL) {
		machine->ops.release_query(machine->world, record->query);
		record->query = NULL;
	}
}

static void v_multiwindow_win32_readback_clear_record(
		VMultiwindowWin32ReadbackMachine *machine,
		VMultiwindowWin32ReadbackRecord *record) {
	if (record == NULL) {
		return;
	}
	v_multiwindow_win32_readback_release_native(machine, record);
	free(record->pixels);
	memset(record, 0, sizeof(*record));
}

static void v_multiwindow_win32_readback_clear_all(
		VMultiwindowWin32ReadbackMachine *machine) {
	if (machine == NULL) {
		return;
	}
	for (int index = 0; index < V_MULTIWINDOW_WIN32_READBACK_MAX_RECORDS;
			index++) {
		if (machine->records[index].used) {
			v_multiwindow_win32_readback_clear_record(machine,
				&machine->records[index]);
		}
	}
}

static VMultiwindowWin32ReadbackRecord *
v_multiwindow_win32_readback_find_request(
		VMultiwindowWin32ReadbackMachine *machine, uint64_t request) {
	if (machine == NULL || request == 0) {
		return NULL;
	}
	for (int index = 0; index < V_MULTIWINDOW_WIN32_READBACK_MAX_RECORDS;
			index++) {
		VMultiwindowWin32ReadbackRecord *record = &machine->records[index];
		if (record->used && record->request == request) {
			return record;
		}
	}
	return NULL;
}

static VMultiwindowWin32ReadbackRecord *
v_multiwindow_win32_readback_free_record(
		VMultiwindowWin32ReadbackMachine *machine) {
	if (machine == NULL) {
		return NULL;
	}
	for (int index = 0; index < V_MULTIWINDOW_WIN32_READBACK_MAX_RECORDS;
			index++) {
		if (!machine->records[index].used) {
			return &machine->records[index];
		}
	}
	return NULL;
}

static void v_multiwindow_win32_readback_mark_failed(
		VMultiwindowWin32ReadbackMachine *machine,
		VMultiwindowWin32ReadbackRecord *record) {
	if (machine == NULL || record == NULL || !record->used
			|| record->phase == V_MULTIWINDOW_WIN32_READBACK_READY
			|| record->phase == V_MULTIWINDOW_WIN32_READBACK_FAILED) {
		return;
	}
	v_multiwindow_win32_readback_release_native(machine, record);
	free(record->pixels);
	record->pixels = NULL;
	record->byte_length = 0;
	record->submitted_frame = 0;
	record->phase = V_MULTIWINDOW_WIN32_READBACK_FAILED;
}

static void v_multiwindow_win32_readback_latch_device_loss(
		VMultiwindowWin32ReadbackMachine *machine) {
	if (machine == NULL || machine->device_lost) {
		return;
	}
	machine->device_lost = 1;
	for (int index = 0; index < V_MULTIWINDOW_WIN32_READBACK_MAX_RECORDS;
			index++) {
		v_multiwindow_win32_readback_mark_failed(machine,
			&machine->records[index]);
	}
}

static VMultiwindowWin32ReadbackMachine *
v_multiwindow_win32_readback_create(
		const VMultiwindowWin32ReadbackOps *ops, void *world) {
	if (!v_multiwindow_win32_readback_ops_valid(ops)) {
		return NULL;
	}
	VMultiwindowWin32ReadbackMachine *machine =
		(VMultiwindowWin32ReadbackMachine *)calloc(1, sizeof(*machine));
	if (machine == NULL) {
		return NULL;
	}
	machine->ops = *ops;
	machine->world = world;
	machine->owner_thread = v_multiwindow_win32_readback_current_thread();
	return machine;
}

static int v_multiwindow_win32_readback_bind(
		VMultiwindowWin32ReadbackMachine *machine, uint64_t state_identity,
		uint64_t renderer_generation) {
	if (!v_multiwindow_win32_readback_is_owner(machine)
			|| machine->bound || machine->stopped
			|| state_identity == 0 || renderer_generation == 0) {
		return 0;
	}
	machine->state_identity = state_identity;
	machine->renderer_generation = renderer_generation;
	machine->bound = 1;
	return 1;
}

static int v_multiwindow_win32_readback_stage(
		VMultiwindowWin32ReadbackMachine *machine, uint64_t request,
		uint64_t state_identity, uint64_t renderer_generation,
		uint64_t window_slot, uint32_t window_generation, void *source,
		uint32_t x, uint32_t y, uint32_t width, uint32_t height,
		uint64_t producing_frame, int resize_pending) {
	if (!v_multiwindow_win32_readback_is_owner(machine)
			|| !machine->bound || machine->stopped
			|| machine->device_lost || request == 0 || source == NULL
			|| state_identity != machine->state_identity
			|| renderer_generation != machine->renderer_generation
			|| window_slot == 0 || window_generation == 0
			|| width == 0 || height == 0 || producing_frame == 0
			|| resize_pending != 0
			|| v_multiwindow_win32_readback_find_request(machine, request) != NULL) {
		return 0;
	}
	VMultiwindowWin32ReadbackRecord *record =
		v_multiwindow_win32_readback_free_record(machine);
	if (record == NULL || !machine->ops.retain_source(machine->world, source)) {
		return 0;
	}
	int source_retained = 1;
	uint32_t source_width = 0;
	uint32_t source_height = 0;
	uint32_t format = 0;
	void *staging = NULL;
	void *query = NULL;
	size_t public_stride = (size_t)width * 4;
	size_t byte_length = 0;
	int size_valid = width == 0
		|| public_stride / 4 == (size_t)width;
	if (size_valid && height != 0
			&& public_stride <= SIZE_MAX / (size_t)height) {
		byte_length = public_stride * (size_t)height;
	} else {
		size_valid = 0;
	}
	int valid = machine->ops.describe_source(machine->world, source,
			&source_width, &source_height, &format) == V_MW_D3D11_TEST_OK
		&& (format == V_MW_D3D11_TEST_FORMAT_BGRA8
			|| format == V_MW_D3D11_TEST_FORMAT_RGBA8)
		&& x <= source_width && y <= source_height
		&& width <= source_width - x && height <= source_height - y
		&& size_valid;
	if (valid) {
		valid = machine->ops.copy_region(machine->world, source, x, y,
			width, height, &staging) == V_MW_D3D11_TEST_OK
			&& staging != NULL;
	}
	if (valid) {
		valid = machine->ops.end_event_query(machine->world, &query)
			== V_MW_D3D11_TEST_OK && query != NULL;
	}
	if (source_retained) {
		machine->ops.release_source(machine->world, source);
		source_retained = 0;
	}
	if (!valid) {
		if (staging != NULL) {
			machine->ops.release_staging(machine->world, staging);
		}
		if (query != NULL) {
			machine->ops.release_query(machine->world, query);
		}
		return 0;
	}
	memset(record, 0, sizeof(*record));
	record->used = 1;
	record->phase = V_MULTIWINDOW_WIN32_READBACK_WAITING_SUBMIT;
	record->request = request;
	record->state_identity = state_identity;
	record->renderer_generation = renderer_generation;
	record->window_slot = window_slot;
	record->window_generation = window_generation;
	record->producing_frame = producing_frame;
	record->width = width;
	record->height = height;
	record->format = format;
	record->public_stride = public_stride;
	record->byte_length = byte_length;
	record->staging = staging;
	record->query = query;
	return 1;
}

static int v_multiwindow_win32_readback_resolve(
		VMultiwindowWin32ReadbackMachine *machine, uint64_t window_slot,
		uint32_t window_generation, uint64_t producing_frame, int succeeded,
		int failure_origin) {
	if (!v_multiwindow_win32_readback_is_owner(machine)
			|| !machine->bound || machine->stopped
			|| machine->device_lost || window_slot == 0
			|| window_generation == 0 || producing_frame == 0
			|| (succeeded != 0 && succeeded != 1)) {
		return 0;
	}
	int matched = 0;
	for (int index = 0; index < V_MULTIWINDOW_WIN32_READBACK_MAX_RECORDS;
			index++) {
		VMultiwindowWin32ReadbackRecord *record = &machine->records[index];
		if (record->used
				&& record->phase == V_MULTIWINDOW_WIN32_READBACK_WAITING_SUBMIT
				&& record->window_slot == window_slot
				&& record->window_generation == window_generation
				&& record->producing_frame == producing_frame) {
			matched++;
		}
	}
	if (matched == 0) {
		return 0;
	}
	if (!succeeded && failure_origin != 0
			&& machine->ops.device_removed_reason(machine->world) < 0) {
		v_multiwindow_win32_readback_latch_device_loss(machine);
		return 1;
	}
	for (int index = 0; index < V_MULTIWINDOW_WIN32_READBACK_MAX_RECORDS;
			index++) {
		VMultiwindowWin32ReadbackRecord *record = &machine->records[index];
		if (!record->used
				|| record->phase != V_MULTIWINDOW_WIN32_READBACK_WAITING_SUBMIT
				|| record->window_slot != window_slot
				|| record->window_generation != window_generation
				|| record->producing_frame != producing_frame) {
			continue;
		}
		if (succeeded) {
			record->submitted_frame = producing_frame;
			record->phase = V_MULTIWINDOW_WIN32_READBACK_GPU_PENDING;
		} else {
			v_multiwindow_win32_readback_mark_failed(machine, record);
		}
	}
	return 1;
}

static int v_multiwindow_win32_readback_resize_submitted(
		VMultiwindowWin32ReadbackMachine *machine, uint64_t window_slot,
		uint32_t window_generation) {
	if (!v_multiwindow_win32_readback_is_owner(machine)
			|| !machine->bound || machine->stopped
			|| window_slot == 0 || window_generation == 0) {
		return 0;
	}
	for (int index = 0; index < V_MULTIWINDOW_WIN32_READBACK_MAX_RECORDS;
			index++) {
		VMultiwindowWin32ReadbackRecord *record = &machine->records[index];
		if (record->used
				&& record->phase == V_MULTIWINDOW_WIN32_READBACK_GPU_PENDING
				&& record->window_slot == window_slot
				&& record->window_generation == window_generation) {
			return 1;
		}
	}
	return 0;
}

static int v_multiwindow_win32_readback_notify_failure(
		VMultiwindowWin32ReadbackMachine *machine, uint64_t state_identity,
		uint64_t renderer_generation, int failure_origin) {
	if (!v_multiwindow_win32_readback_is_owner(machine)
			|| !machine->bound || machine->stopped
			|| machine->device_lost || failure_origin == 0
			|| state_identity != machine->state_identity
			|| renderer_generation != machine->renderer_generation) {
		return 0;
	}
	if (machine->ops.device_removed_reason(machine->world) >= 0) {
		return 0;
	}
	v_multiwindow_win32_readback_latch_device_loss(machine);
	return 1;
}

static int v_multiwindow_win32_readback_normalize(
		VMultiwindowWin32ReadbackRecord *record, const uint8_t *mapped,
		size_t row_pitch) {
	if (record == NULL || mapped == NULL
			|| record->public_stride == 0 || record->byte_length == 0
			|| row_pitch < record->public_stride) {
		return 0;
	}
	uint8_t *pixels = (uint8_t *)malloc(record->byte_length);
	if (pixels == NULL) {
		return 0;
	}
	for (uint32_t row = 0; row < record->height; row++) {
		const uint8_t *source = mapped + (size_t)row * row_pitch;
		uint8_t *destination = pixels + (size_t)row * record->public_stride;
		if (record->format == V_MW_D3D11_TEST_FORMAT_RGBA8) {
			memcpy(destination, source, record->public_stride);
			continue;
		}
		for (uint32_t column = 0; column < record->width; column++) {
			size_t offset = (size_t)column * 4;
			destination[offset + 0] = source[offset + 2];
			destination[offset + 1] = source[offset + 1];
			destination[offset + 2] = source[offset + 0];
			destination[offset + 3] = source[offset + 3];
		}
	}
	record->pixels = pixels;
	return 1;
}

static int v_multiwindow_win32_readback_poll(
		VMultiwindowWin32ReadbackMachine *machine) {
	if (!v_multiwindow_win32_readback_is_owner(machine)
			|| !machine->bound || machine->stopped
			|| machine->device_lost) {
		return 0;
	}
	int published = 0;
	for (int index = 0; index < V_MULTIWINDOW_WIN32_READBACK_MAX_RECORDS;
			index++) {
		VMultiwindowWin32ReadbackRecord *record = &machine->records[index];
		if (!record->used
				|| record->phase != V_MULTIWINDOW_WIN32_READBACK_GPU_PENDING) {
			continue;
		}
		int query_result = machine->ops.get_event_data(machine->world,
			record->query);
		if (query_result == V_MW_D3D11_TEST_S_FALSE) {
			continue;
		}
		if (query_result != V_MW_D3D11_TEST_OK) {
			if (machine->ops.device_removed_reason(machine->world) < 0) {
				v_multiwindow_win32_readback_latch_device_loss(machine);
				return 1;
			}
			v_multiwindow_win32_readback_mark_failed(machine, record);
			published = 1;
			continue;
		}
		const uint8_t *mapped = NULL;
		size_t row_pitch = 0;
		int map_result = machine->ops.map_staging(machine->world,
			record->staging, &mapped, &row_pitch);
		if (map_result == V_MW_D3D11_TEST_WAS_STILL_DRAWING) {
			continue;
		}
		if (map_result != V_MW_D3D11_TEST_OK) {
			if (machine->ops.device_removed_reason(machine->world) < 0) {
				v_multiwindow_win32_readback_latch_device_loss(machine);
				return 1;
			}
			v_multiwindow_win32_readback_mark_failed(machine, record);
			published = 1;
			continue;
		}
		int normalized =
			v_multiwindow_win32_readback_normalize(record, mapped, row_pitch);
		machine->ops.unmap_staging(machine->world, record->staging);
		v_multiwindow_win32_readback_release_native(machine, record);
		if (!normalized) {
			v_multiwindow_win32_readback_mark_failed(machine, record);
		} else {
			record->phase = V_MULTIWINDOW_WIN32_READBACK_READY;
		}
		published = 1;
	}
	return published;
}

static int v_multiwindow_win32_readback_take(
		VMultiwindowWin32ReadbackMachine *machine,
		VMultiwindowWin32ReadbackResult *result) {
	if (!v_multiwindow_win32_readback_is_owner(machine)
			|| result == NULL || machine->stopped) {
		return 0;
	}
	for (int index = 0; index < V_MULTIWINDOW_WIN32_READBACK_MAX_RECORDS;
			index++) {
		VMultiwindowWin32ReadbackRecord *record = &machine->records[index];
		if (!record->used || record->taken
				|| (record->phase != V_MULTIWINDOW_WIN32_READBACK_READY
					&& record->phase != V_MULTIWINDOW_WIN32_READBACK_FAILED)) {
			continue;
		}
		record->taken = 1;
		memset(result, 0, sizeof(*result));
		result->request = record->request;
		result->state_identity = record->state_identity;
		result->renderer_generation = record->renderer_generation;
		result->window_slot = record->window_slot;
		result->window_generation = record->window_generation;
		result->submitted_frame = record->phase == V_MULTIWINDOW_WIN32_READBACK_READY
			? record->submitted_frame : 0;
		result->status = record->phase == V_MULTIWINDOW_WIN32_READBACK_READY
			? V_MW_D3D11_TEST_STATUS_READY : V_MW_D3D11_TEST_STATUS_FAILED;
		result->width = record->width;
		result->height = record->height;
		result->stride = record->phase == V_MULTIWINDOW_WIN32_READBACK_READY
			? record->public_stride : 0;
		result->byte_length = record->phase == V_MULTIWINDOW_WIN32_READBACK_READY
			? record->byte_length : 0;
		return 1;
	}
	return 0;
}

static int v_multiwindow_win32_readback_copy(
		VMultiwindowWin32ReadbackMachine *machine, uint64_t request,
		uint8_t *pixels, size_t capacity) {
	VMultiwindowWin32ReadbackRecord *record =
		v_multiwindow_win32_readback_find_request(machine, request);
	if (!v_multiwindow_win32_readback_is_owner(machine)
			|| record == NULL || !record->taken || record->copied
			|| record->phase != V_MULTIWINDOW_WIN32_READBACK_READY
			|| record->pixels == NULL || pixels == NULL
			|| capacity < record->byte_length) {
		return 0;
	}
	memcpy(pixels, record->pixels, record->byte_length);
	record->copied = 1;
	return 1;
}

static int v_multiwindow_win32_readback_release(
		VMultiwindowWin32ReadbackMachine *machine, uint64_t request) {
	VMultiwindowWin32ReadbackRecord *record =
		v_multiwindow_win32_readback_find_request(machine, request);
	if (!v_multiwindow_win32_readback_is_owner(machine)
			|| record == NULL || !record->taken) {
		return 0;
	}
	v_multiwindow_win32_readback_clear_record(machine, record);
	return 1;
}

static int v_multiwindow_win32_readback_cancel(
		VMultiwindowWin32ReadbackMachine *machine, uint64_t request) {
	VMultiwindowWin32ReadbackRecord *record =
		v_multiwindow_win32_readback_find_request(machine, request);
	if (!v_multiwindow_win32_readback_is_owner(machine) || record == NULL) {
		return 0;
	}
	v_multiwindow_win32_readback_clear_record(machine, record);
	return 1;
}

static int v_multiwindow_win32_readback_quiesce_window(
		VMultiwindowWin32ReadbackMachine *machine, uint64_t window_slot,
		uint32_t window_generation) {
	if (!v_multiwindow_win32_readback_is_owner(machine)
			|| window_slot == 0 || window_generation == 0) {
		return 0;
	}
	int quiesced = 0;
	for (int index = 0; index < V_MULTIWINDOW_WIN32_READBACK_MAX_RECORDS;
			index++) {
		VMultiwindowWin32ReadbackRecord *record = &machine->records[index];
		if (record->used && record->window_slot == window_slot
				&& record->window_generation == window_generation) {
			v_multiwindow_win32_readback_clear_record(machine, record);
			quiesced++;
		}
	}
	return quiesced;
}

static int v_multiwindow_win32_readback_restart(
		VMultiwindowWin32ReadbackMachine *machine, uint64_t state_identity,
		uint64_t renderer_generation) {
	if (!v_multiwindow_win32_readback_is_owner(machine)
			|| state_identity == 0 || renderer_generation == 0) {
		return 0;
	}
	v_multiwindow_win32_readback_clear_all(machine);
	machine->state_identity = state_identity;
	machine->renderer_generation = renderer_generation;
	machine->device_lost = 0;
	machine->stopped = 0;
	machine->bound = 1;
	return 1;
}

static int v_multiwindow_win32_readback_stop(
		VMultiwindowWin32ReadbackMachine *machine) {
	if (!v_multiwindow_win32_readback_is_owner(machine) || machine->stopped) {
		return 0;
	}
	v_multiwindow_win32_readback_clear_all(machine);
	machine->bound = 0;
	machine->stopped = 1;
	return 1;
}

static int v_multiwindow_win32_readback_active(
		const VMultiwindowWin32ReadbackMachine *machine) {
	if (!v_multiwindow_win32_readback_is_owner(machine)) {
		return 0;
	}
	int active = 0;
	for (int index = 0; index < V_MULTIWINDOW_WIN32_READBACK_MAX_RECORDS;
			index++) {
		if (machine->records[index].used) {
			active++;
		}
	}
	return active;
}

static int v_multiwindow_win32_readback_destroy(
		VMultiwindowWin32ReadbackMachine *machine) {
	if (!v_multiwindow_win32_readback_is_owner(machine)) {
		return 0;
	}
	v_multiwindow_win32_readback_clear_all(machine);
	free(machine);
	return 1;
}

#if defined(V_MULTIWINDOW_WIN32_D3D11_READBACK_TEST)
static void *v_multiwindow_win32_d3d11_readback_test_create(
		const VMultiwindowD3D11TestOps *ops, void *world) {
	return v_multiwindow_win32_readback_create(ops, world);
}

static int v_multiwindow_win32_d3d11_readback_test_bind(void *state,
		uint64_t state_identity, uint64_t renderer_generation) {
	return v_multiwindow_win32_readback_bind(
		(VMultiwindowWin32ReadbackMachine *)state, state_identity,
		renderer_generation);
}

static int v_multiwindow_win32_d3d11_readback_test_stage(void *state,
		uint64_t request, uint64_t state_identity, uint64_t renderer_generation,
		uint64_t window_slot, uint32_t window_generation, void *source,
		uint32_t x, uint32_t y, uint32_t width, uint32_t height,
		uint64_t producing_frame, int resize_pending) {
	return v_multiwindow_win32_readback_stage(
		(VMultiwindowWin32ReadbackMachine *)state, request, state_identity,
		renderer_generation, window_slot, window_generation, source, x, y,
		width, height, producing_frame, resize_pending);
}

static int v_multiwindow_win32_d3d11_readback_test_resolve(void *state,
		uint64_t window_slot, uint32_t window_generation,
		uint64_t producing_frame, int succeeded, int failure_origin) {
	return v_multiwindow_win32_readback_resolve(
		(VMultiwindowWin32ReadbackMachine *)state, window_slot,
		window_generation, producing_frame, succeeded, failure_origin);
}

static int v_multiwindow_win32_d3d11_readback_test_resize_submitted(void *state,
		uint64_t window_slot, uint32_t window_generation) {
	return v_multiwindow_win32_readback_resize_submitted(
		(VMultiwindowWin32ReadbackMachine *)state, window_slot,
		window_generation);
}

static int v_multiwindow_win32_d3d11_readback_test_notify_failure(void *state,
		uint64_t state_identity, uint64_t renderer_generation,
		int failure_origin) {
	return v_multiwindow_win32_readback_notify_failure(
		(VMultiwindowWin32ReadbackMachine *)state, state_identity,
		renderer_generation, failure_origin);
}

static int v_multiwindow_win32_d3d11_readback_test_poll(void *state) {
	return v_multiwindow_win32_readback_poll(
		(VMultiwindowWin32ReadbackMachine *)state);
}

static int v_multiwindow_win32_d3d11_readback_test_take(void *state,
		VMultiwindowD3D11TestResult *result) {
	if (result == NULL) {
		return 0;
	}
	VMultiwindowWin32ReadbackResult native_result;
	if (!v_multiwindow_win32_readback_take(
			(VMultiwindowWin32ReadbackMachine *)state, &native_result)) {
		return 0;
	}
	memset(result, 0, sizeof(*result));
	result->request = native_result.request;
	result->state_identity = native_result.state_identity;
	result->renderer_generation = native_result.renderer_generation;
	result->window_slot = native_result.window_slot;
	result->window_generation = native_result.window_generation;
	result->submitted_frame = native_result.submitted_frame;
	result->status = native_result.status;
	result->width = native_result.width;
	result->height = native_result.height;
	result->stride = native_result.stride;
	result->byte_length = native_result.byte_length;
	return 1;
}

static int v_multiwindow_win32_d3d11_readback_test_copy(void *state,
		uint64_t request, uint8_t *pixels, size_t capacity) {
	return v_multiwindow_win32_readback_copy(
		(VMultiwindowWin32ReadbackMachine *)state, request, pixels, capacity);
}

static int v_multiwindow_win32_d3d11_readback_test_release(void *state,
		uint64_t request) {
	return v_multiwindow_win32_readback_release(
		(VMultiwindowWin32ReadbackMachine *)state, request);
}

static int v_multiwindow_win32_d3d11_readback_test_cancel(void *state,
		uint64_t request) {
	return v_multiwindow_win32_readback_cancel(
		(VMultiwindowWin32ReadbackMachine *)state, request);
}

static int v_multiwindow_win32_d3d11_readback_test_quiesce_window(void *state,
		uint64_t window_slot, uint32_t window_generation) {
	return v_multiwindow_win32_readback_quiesce_window(
		(VMultiwindowWin32ReadbackMachine *)state, window_slot,
		window_generation);
}

static int v_multiwindow_win32_d3d11_readback_test_restart(void *state,
		uint64_t state_identity, uint64_t renderer_generation) {
	return v_multiwindow_win32_readback_restart(
		(VMultiwindowWin32ReadbackMachine *)state, state_identity,
		renderer_generation);
}

static int v_multiwindow_win32_d3d11_readback_test_stop(void *state) {
	return v_multiwindow_win32_readback_stop(
		(VMultiwindowWin32ReadbackMachine *)state);
}

static int v_multiwindow_win32_d3d11_readback_test_active(void *state) {
	return v_multiwindow_win32_readback_active(
		(VMultiwindowWin32ReadbackMachine *)state);
}

static uintptr_t v_multiwindow_win32_d3d11_readback_test_machine_identity(
		void *state) {
	return (uintptr_t)state;
}

static uintptr_t v_multiwindow_win32_d3d11_readback_native_machine_identity(
		void *state) {
	return (uintptr_t)(VMultiwindowWin32ReadbackMachine *)state;
}

static void v_multiwindow_win32_d3d11_readback_test_destroy(void *state) {
	(void)v_multiwindow_win32_readback_destroy(
		(VMultiwindowWin32ReadbackMachine *)state);
}
#endif

#endif
