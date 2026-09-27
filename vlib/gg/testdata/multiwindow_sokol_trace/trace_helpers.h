#ifndef V_MULTIWINDOW_SOKOL_TRACE_HELPERS_H
#define V_MULTIWINDOW_SOKOL_TRACE_HELPERS_H

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>

#define V_MULTIWINDOW_TRACE_CAPACITY 16384
#define V_MULTIWINDOW_TYPED_TRACE_CAPACITY 256

enum {
    V_MULTIWINDOW_TRACE_OPERATION_INVALID = 0,
    V_MULTIWINDOW_TRACE_OPERATION_MAKE_BUFFER,
    V_MULTIWINDOW_TRACE_OPERATION_MAKE_IMAGE,
    V_MULTIWINDOW_TRACE_OPERATION_MAKE_SAMPLER,
    V_MULTIWINDOW_TRACE_OPERATION_MAKE_SHADER,
    V_MULTIWINDOW_TRACE_OPERATION_MAKE_PIPELINE,
    V_MULTIWINDOW_TRACE_OPERATION_MAKE_ATTACHMENTS,
    V_MULTIWINDOW_TRACE_OPERATION_DESTROY_BUFFER,
    V_MULTIWINDOW_TRACE_OPERATION_DESTROY_IMAGE,
    V_MULTIWINDOW_TRACE_OPERATION_DESTROY_SAMPLER,
    V_MULTIWINDOW_TRACE_OPERATION_DESTROY_SHADER,
    V_MULTIWINDOW_TRACE_OPERATION_DESTROY_PIPELINE,
    V_MULTIWINDOW_TRACE_OPERATION_DESTROY_ATTACHMENTS,
    V_MULTIWINDOW_TRACE_OPERATION_UPDATE_BUFFER,
    V_MULTIWINDOW_TRACE_OPERATION_APPEND_BUFFER,
    V_MULTIWINDOW_TRACE_OPERATION_UPDATE_IMAGE,
    V_MULTIWINDOW_TRACE_OPERATION_BEGIN_SWAPCHAIN_PASS,
    V_MULTIWINDOW_TRACE_OPERATION_BEGIN_OFFSCREEN_PASS,
    V_MULTIWINDOW_TRACE_OPERATION_END_PASS,
    V_MULTIWINDOW_TRACE_OPERATION_COMMIT
};

typedef struct {
    int operation;
    uint32_t identity;
    int64_t value;
    uint64_t sequence;
    uint64_t swapchain_identity;
    int width;
    int height;
    int sample_count;
    int color_format;
    int depth_format;
} v_multiwindow_typed_trace_record;

static char v_multiwindow_trace_buffer[V_MULTIWINDOW_TRACE_CAPACITY];
static size_t v_multiwindow_trace_length;
static bool v_multiwindow_trace_installed;
static uint64_t v_multiwindow_trace_install_generation;
static uint64_t v_multiwindow_trace_active_generation;
static sg_trace_hooks v_multiwindow_trace_previous;
static v_multiwindow_typed_trace_record
    v_multiwindow_typed_trace[V_MULTIWINDOW_TYPED_TRACE_CAPACITY];
static size_t v_multiwindow_typed_trace_count;
static bool v_multiwindow_typed_trace_overflow;

static void v_multiwindow_trace_append_typed(int operation, uint32_t identity,
    int64_t value) {
    if (v_multiwindow_typed_trace_count >= V_MULTIWINDOW_TYPED_TRACE_CAPACITY) {
        v_multiwindow_typed_trace_overflow = true;
        return;
    }
    size_t index = v_multiwindow_typed_trace_count++;
    v_multiwindow_typed_trace[index].operation = operation;
    v_multiwindow_typed_trace[index].identity = identity;
    v_multiwindow_typed_trace[index].value = value;
    v_multiwindow_typed_trace[index].sequence = (uint64_t)index + 1;
}

static uint64_t v_multiwindow_trace_swapchain_identity(
    const sg_swapchain* swapchain) {
    if (swapchain->metal.current_drawable != NULL) {
        return (uint64_t)(uintptr_t)swapchain->metal.current_drawable;
    }
    if (swapchain->d3d11.render_view != NULL) {
        return (uint64_t)(uintptr_t)swapchain->d3d11.render_view;
    }
    if (swapchain->wgpu.render_view != NULL) {
        return (uint64_t)(uintptr_t)swapchain->wgpu.render_view;
    }
    return (uint64_t)swapchain->gl.framebuffer;
}

static void v_multiwindow_trace_append_swapchain(
    const sg_swapchain* swapchain) {
    if (v_multiwindow_typed_trace_count >= V_MULTIWINDOW_TYPED_TRACE_CAPACITY) {
        v_multiwindow_typed_trace_overflow = true;
        return;
    }
    size_t index = v_multiwindow_typed_trace_count++;
    v_multiwindow_typed_trace_record* record =
        &v_multiwindow_typed_trace[index];
    record->operation = V_MULTIWINDOW_TRACE_OPERATION_BEGIN_SWAPCHAIN_PASS;
    record->sequence = (uint64_t)index + 1;
    record->swapchain_identity =
        v_multiwindow_trace_swapchain_identity(swapchain);
    record->width = swapchain->width;
    record->height = swapchain->height;
    record->sample_count = swapchain->sample_count;
    record->color_format = (int)swapchain->color_format;
    record->depth_format = (int)swapchain->depth_format;
}

static void v_multiwindow_trace_append(const char* operation) {
    size_t operation_length = strlen(operation);
    if ((v_multiwindow_trace_length + operation_length + 2) >= V_MULTIWINDOW_TRACE_CAPACITY) {
        return;
    }
    memcpy(v_multiwindow_trace_buffer + v_multiwindow_trace_length,
        operation, operation_length);
    v_multiwindow_trace_length += operation_length;
    v_multiwindow_trace_buffer[v_multiwindow_trace_length++] = '\n';
    v_multiwindow_trace_buffer[v_multiwindow_trace_length] = '\0';
}

static void v_multiwindow_trace_make_buffer(const sg_buffer_desc* desc,
    sg_buffer result, void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_MAKE_BUFFER,
        result.id, 0);
    v_multiwindow_trace_append("make_buffer");
    if (v_multiwindow_trace_previous.make_buffer != NULL) {
        v_multiwindow_trace_previous.make_buffer(desc, result,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_make_image(const sg_image_desc* desc,
    sg_image result, void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_MAKE_IMAGE,
        result.id, 0);
    v_multiwindow_trace_append("make_image");
    if (v_multiwindow_trace_previous.make_image != NULL) {
        v_multiwindow_trace_previous.make_image(desc, result,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_make_sampler(const sg_sampler_desc* desc,
    sg_sampler result, void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_MAKE_SAMPLER,
        result.id, 0);
    v_multiwindow_trace_append("make_sampler");
    if (v_multiwindow_trace_previous.make_sampler != NULL) {
        v_multiwindow_trace_previous.make_sampler(desc, result,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_make_shader(const sg_shader_desc* desc,
    sg_shader result, void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_MAKE_SHADER,
        result.id, 0);
    v_multiwindow_trace_append("make_shader");
    if (v_multiwindow_trace_previous.make_shader != NULL) {
        v_multiwindow_trace_previous.make_shader(desc, result,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_make_pipeline(const sg_pipeline_desc* desc,
    sg_pipeline result, void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_MAKE_PIPELINE,
        result.id, 0);
    v_multiwindow_trace_append("make_pipeline");
    if (v_multiwindow_trace_previous.make_pipeline != NULL) {
        v_multiwindow_trace_previous.make_pipeline(desc, result,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_make_attachments(const sg_attachments_desc* desc,
    sg_attachments result, void* user_data) {
    v_multiwindow_trace_append_typed(
        V_MULTIWINDOW_TRACE_OPERATION_MAKE_ATTACHMENTS, result.id, 0);
    v_multiwindow_trace_append("make_attachments");
    if (v_multiwindow_trace_previous.make_attachments != NULL) {
        v_multiwindow_trace_previous.make_attachments(desc, result,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_destroy_image(sg_image image, void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_DESTROY_IMAGE,
        image.id, 0);
    v_multiwindow_trace_append("destroy_image");
    if (v_multiwindow_trace_previous.destroy_image != NULL) {
        v_multiwindow_trace_previous.destroy_image(image,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_destroy_buffer(sg_buffer buffer,
    void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_DESTROY_BUFFER,
        buffer.id, 0);
    v_multiwindow_trace_append("destroy_buffer");
    if (v_multiwindow_trace_previous.destroy_buffer != NULL) {
        v_multiwindow_trace_previous.destroy_buffer(buffer,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_destroy_sampler(sg_sampler sampler,
    void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_DESTROY_SAMPLER,
        sampler.id, 0);
    v_multiwindow_trace_append("destroy_sampler");
    if (v_multiwindow_trace_previous.destroy_sampler != NULL) {
        v_multiwindow_trace_previous.destroy_sampler(sampler,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_destroy_shader(sg_shader shader,
    void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_DESTROY_SHADER,
        shader.id, 0);
    v_multiwindow_trace_append("destroy_shader");
    if (v_multiwindow_trace_previous.destroy_shader != NULL) {
        v_multiwindow_trace_previous.destroy_shader(shader,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_destroy_pipeline(sg_pipeline pipeline,
    void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_DESTROY_PIPELINE,
        pipeline.id, 0);
    v_multiwindow_trace_append("destroy_pipeline");
    if (v_multiwindow_trace_previous.destroy_pipeline != NULL) {
        v_multiwindow_trace_previous.destroy_pipeline(pipeline,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_destroy_attachments(sg_attachments attachments,
    void* user_data) {
    v_multiwindow_trace_append_typed(
        V_MULTIWINDOW_TRACE_OPERATION_DESTROY_ATTACHMENTS, attachments.id, 0);
    v_multiwindow_trace_append("destroy_attachments");
    if (v_multiwindow_trace_previous.destroy_attachments != NULL) {
        v_multiwindow_trace_previous.destroy_attachments(attachments,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_update_image(sg_image image,
    const sg_image_data* data, void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_UPDATE_IMAGE,
        image.id, 0);
    v_multiwindow_trace_append("update_image");
    if (v_multiwindow_trace_previous.update_image != NULL) {
        v_multiwindow_trace_previous.update_image(image, data,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_update_buffer(sg_buffer buffer,
    const sg_range* data, void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_UPDATE_BUFFER,
        buffer.id, data != NULL ? (int64_t)data->size : 0);
    v_multiwindow_trace_append("update_buffer");
    if (v_multiwindow_trace_previous.update_buffer != NULL) {
        v_multiwindow_trace_previous.update_buffer(buffer, data,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_append_buffer(sg_buffer buffer,
    const sg_range* data, int result, void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_APPEND_BUFFER,
        buffer.id, (int64_t)result);
    v_multiwindow_trace_append("append_buffer");
    if (v_multiwindow_trace_previous.append_buffer != NULL) {
        v_multiwindow_trace_previous.append_buffer(buffer, data, result,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_begin_pass(const sg_pass* pass, void* user_data) {
    if (pass->attachments.id != 0) {
        v_multiwindow_trace_append_typed(
            V_MULTIWINDOW_TRACE_OPERATION_BEGIN_OFFSCREEN_PASS,
            pass->attachments.id, 0);
        v_multiwindow_trace_append("begin_offscreen_pass");
    } else {
        v_multiwindow_trace_append_swapchain(&pass->swapchain);
        v_multiwindow_trace_append("begin_swapchain_pass");
    }
    if (v_multiwindow_trace_previous.begin_pass != NULL) {
        v_multiwindow_trace_previous.begin_pass(pass,
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_end_pass(void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_END_PASS, 0, 0);
    v_multiwindow_trace_append("end_pass");
    if (v_multiwindow_trace_previous.end_pass != NULL) {
        v_multiwindow_trace_previous.end_pass(
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static void v_multiwindow_trace_commit(void* user_data) {
    v_multiwindow_trace_append_typed(V_MULTIWINDOW_TRACE_OPERATION_COMMIT, 0, 0);
    v_multiwindow_trace_append("commit");
    if (v_multiwindow_trace_previous.commit != NULL) {
        v_multiwindow_trace_previous.commit(
            v_multiwindow_trace_previous.user_data);
    }
    (void)user_data;
}

static uint64_t v_multiwindow_trace_install_owned(void) {
#if defined(SOKOL_TRACE_HOOKS)
    if (v_multiwindow_trace_installed) {
        return UINT64_C(0);
    }
    if (v_multiwindow_trace_install_generation == UINT64_MAX) {
        return UINT64_C(0);
    }
    sg_trace_hooks hooks;
    memset(&hooks, 0, sizeof(hooks));
    hooks.make_buffer = v_multiwindow_trace_make_buffer;
    hooks.make_image = v_multiwindow_trace_make_image;
    hooks.make_sampler = v_multiwindow_trace_make_sampler;
    hooks.make_shader = v_multiwindow_trace_make_shader;
    hooks.make_pipeline = v_multiwindow_trace_make_pipeline;
    hooks.make_attachments = v_multiwindow_trace_make_attachments;
    hooks.destroy_buffer = v_multiwindow_trace_destroy_buffer;
    hooks.destroy_image = v_multiwindow_trace_destroy_image;
    hooks.destroy_sampler = v_multiwindow_trace_destroy_sampler;
    hooks.destroy_shader = v_multiwindow_trace_destroy_shader;
    hooks.destroy_pipeline = v_multiwindow_trace_destroy_pipeline;
    hooks.destroy_attachments = v_multiwindow_trace_destroy_attachments;
    hooks.update_buffer = v_multiwindow_trace_update_buffer;
    hooks.append_buffer = v_multiwindow_trace_append_buffer;
    hooks.update_image = v_multiwindow_trace_update_image;
    hooks.begin_pass = v_multiwindow_trace_begin_pass;
    hooks.end_pass = v_multiwindow_trace_end_pass;
    hooks.commit = v_multiwindow_trace_commit;
    v_multiwindow_trace_previous = sg_install_trace_hooks(&hooks);
    v_multiwindow_trace_installed = true;
    v_multiwindow_trace_install_generation++;
    v_multiwindow_trace_active_generation =
        v_multiwindow_trace_install_generation;
    v_multiwindow_trace_length = 0;
    v_multiwindow_trace_buffer[0] = '\0';
    v_multiwindow_typed_trace_count = 0;
    v_multiwindow_typed_trace_overflow = false;
    memset(v_multiwindow_typed_trace, 0, sizeof(v_multiwindow_typed_trace));
    return v_multiwindow_trace_active_generation;
#else
    return UINT64_C(0);
#endif
}

static int v_multiwindow_trace_install(void) {
    return v_multiwindow_trace_install_owned() != UINT64_C(0) ? 1 : 0;
}

static int v_multiwindow_trace_uninstall_owned(uint64_t generation) {
#if defined(SOKOL_TRACE_HOOKS)
    if (!v_multiwindow_trace_installed || generation == UINT64_C(0) ||
        generation != v_multiwindow_trace_active_generation) {
        return 0;
    }
    sg_install_trace_hooks(&v_multiwindow_trace_previous);
    v_multiwindow_trace_installed = false;
    v_multiwindow_trace_active_generation = UINT64_C(0);
    return 1;
#else
    (void)generation;
    return 0;
#endif
}

static int v_multiwindow_trace_logical_release_owned(uint64_t generation) {
    if (!v_multiwindow_trace_installed || generation == UINT64_C(0) ||
        generation != v_multiwindow_trace_active_generation) {
        return 0;
    }
    v_multiwindow_trace_installed = false;
    v_multiwindow_trace_active_generation = UINT64_C(0);
    return 1;
}

static void v_multiwindow_trace_uninstall(void) {
    if (v_multiwindow_trace_installed) {
        (void)v_multiwindow_trace_uninstall_owned(
            v_multiwindow_trace_active_generation);
    }
}

static void v_multiwindow_trace_reset(void) {
    v_multiwindow_trace_length = 0;
    v_multiwindow_trace_buffer[0] = '\0';
    v_multiwindow_typed_trace_count = 0;
    v_multiwindow_typed_trace_overflow = false;
    memset(v_multiwindow_typed_trace, 0, sizeof(v_multiwindow_typed_trace));
}

static const char* v_multiwindow_trace_snapshot(void) {
    return v_multiwindow_trace_buffer;
}

static size_t v_multiwindow_typed_trace_snapshot_count(void) {
    return v_multiwindow_typed_trace_count;
}

static int v_multiwindow_typed_trace_snapshot_overflow(void) {
    return v_multiwindow_typed_trace_overflow ? 1 : 0;
}

static uint64_t v_multiwindow_trace_active_install_generation(void) {
    return v_multiwindow_trace_active_generation;
}

static int v_multiwindow_typed_trace_snapshot_operation(size_t index) {
    if (index >= v_multiwindow_typed_trace_count) {
        return V_MULTIWINDOW_TRACE_OPERATION_INVALID;
    }
    return v_multiwindow_typed_trace[index].operation;
}

static uint32_t v_multiwindow_typed_trace_snapshot_identity(size_t index) {
    if (index >= v_multiwindow_typed_trace_count) {
        return 0;
    }
    return v_multiwindow_typed_trace[index].identity;
}

static int64_t v_multiwindow_typed_trace_snapshot_value(size_t index) {
    if (index >= v_multiwindow_typed_trace_count) {
        return 0;
    }
    return v_multiwindow_typed_trace[index].value;
}

static uint64_t v_multiwindow_typed_trace_snapshot_sequence(size_t index) {
    if (index >= v_multiwindow_typed_trace_count) {
        return 0;
    }
    return v_multiwindow_typed_trace[index].sequence;
}

static uint64_t v_multiwindow_typed_trace_snapshot_swapchain_identity(
    size_t index) {
    if (index >= v_multiwindow_typed_trace_count) {
        return UINT64_C(0);
    }
    return v_multiwindow_typed_trace[index].swapchain_identity;
}

static int v_multiwindow_typed_trace_snapshot_width(size_t index) {
    return index < v_multiwindow_typed_trace_count ?
        v_multiwindow_typed_trace[index].width : 0;
}

static int v_multiwindow_typed_trace_snapshot_height(size_t index) {
    return index < v_multiwindow_typed_trace_count ?
        v_multiwindow_typed_trace[index].height : 0;
}

static int v_multiwindow_typed_trace_snapshot_sample_count(size_t index) {
    return index < v_multiwindow_typed_trace_count ?
        v_multiwindow_typed_trace[index].sample_count : 0;
}

static int v_multiwindow_typed_trace_snapshot_color_format(size_t index) {
    return index < v_multiwindow_typed_trace_count ?
        v_multiwindow_typed_trace[index].color_format : 0;
}

static int v_multiwindow_typed_trace_snapshot_depth_format(size_t index) {
    return index < v_multiwindow_typed_trace_count ?
        v_multiwindow_typed_trace[index].depth_format : 0;
}

#endif
