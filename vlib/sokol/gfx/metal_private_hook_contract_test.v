// vtest build: !windows && !musl? && !self_ubuntu_musl_ci?
module gfx

import os

const metal_hook_define = 'V_SOKOL_MTL_END_PASS_HOOK'
const metal_hook_setter = 'v_sokol_mtl_set_end_pass_hook'

fn metal_hook_source(path string) string {
	return os.read_file(path) or { panic(err) }
}

fn metal_hook_header() string {
	return metal_hook_source(os.join_path(@VEXEROOT, 'thirdparty', 'sokol', 'sokol_gfx.h'))
}

fn metal_hook_marked(source string, begin string, end string) string {
	begin_token := '/* ${begin} */'
	end_token := '/* ${end} */'
	start := source.index(begin_token) or { panic('missing `${begin}`') }
	finish_relative := source[start..].index(end_token) or { panic('missing `${end}`') }
	return source[start..start + finish_relative + end_token.len]
}

fn metal_hook_body(source string, signature string) string {
	start := source.index(signature) or { panic('missing `${signature}`') }
	open_relative := source[start..].index('{') or { panic('missing body for `${signature}`') }
	open := start + open_relative
	mut depth := 0
	for index := open; index < source.len; index++ {
		if source[index] == `{` {
			depth++
		} else if source[index] == `}` {
			depth--
			if depth == 0 {
				return source[open + 1..index]
			}
		}
	}
	panic('unterminated body for `${signature}`')
}

fn metal_hook_run(command string) string {
	result := os.execute(command)
	assert result.exit_code == 0, '`${command}` failed (${result.exit_code}):\n${result.output}'
	return result.output.trim_space()
}

fn metal_hook_cc() string {
	return os.find_abs_path_of_executable('cc') or { panic('a C compiler is required') }
}

fn test_metal_private_hook_order_and_scope() {
	header := metal_hook_header()
	declaration := metal_hook_marked(header, 'V_SOKOL_MTL_END_PASS_DECL_BEGIN',
		'V_SOKOL_MTL_END_PASS_DECL_END')
	implementation := metal_hook_marked(header, 'V_SOKOL_MTL_END_PASS_IMPL_BEGIN',
		'V_SOKOL_MTL_END_PASS_IMPL_END')
	assert declaration.contains('#if defined(SOKOL_METAL) && defined(${metal_hook_define})')
	assert implementation.contains('#if defined(SOKOL_METAL) && defined(${metal_hook_define})')
	assert declaration.contains('${metal_hook_setter}(')
	assert implementation.contains('SOKOL_API_IMPL bool ${metal_hook_setter}(')
	assert declaration.contains('borrowed only for the duration of')
	assert declaration.contains('the callback must not retain or store either pointer')
	assert !implementation.contains(' retain]')
	assert !implementation.contains(' commit]')
	assert !implementation.contains('presentDrawable')
	assert !implementation.contains('waitUntil')

	end_pass := metal_hook_body(header, '_SOKOL_PRIVATE void _sg_mtl_end_pass(void)')
	end_encoding := end_pass.index('[_sg.mtl.cmd_encoder endEncoding]') or { -1 }
	encoder_nil := end_pass.index('_sg.mtl.cmd_encoder = nil') or { -1 }
	invoke := end_pass.index('_v_sokol_metal_invoke_pre_present(') or { -1 }
	present := end_pass.index('[_sg.mtl.cmd_buffer presentDrawable:_sg.mtl.cur_drawable]') or { -1 }
	assert end_encoding >= 0
	assert end_encoding < encoder_nil
	assert encoder_nil < invoke
	assert invoke < present
	assert end_pass.count('_v_sokol_metal_invoke_pre_present(') == 1
	assert end_pass.contains('v_sokol_pass_encoded = true')
	assert end_pass.contains('if (v_sokol_pass_encoded)')
	assert end_pass.contains('(__bridge const void*)_sg.mtl.cmd_buffer')
	assert end_pass.contains('(__bridge const void*)_sg.mtl.cur_drawable')
	discard_backend := metal_hook_body(header, '_SOKOL_PRIVATE void _sg_mtl_discard_backend(void)')
	assert discard_backend.count('_v_sokol_mtl_clear_end_pass_hook();') == 1
	assert discard_backend.index('_v_sokol_mtl_clear_end_pass_hook();') or { -1 } < discard_backend.index('dispatch_semaphore_wait') or {
		-1
	}

	declaration_source := metal_hook_source(os.join_path(@VEXEROOT, 'vlib', 'sokol', 'c',
		'declaration.c.v'))
	assert declaration_source.count('-D${metal_hook_define}') == 1
	assert declaration_source.contains('$if gg_multiwindow ? {\n\t\t\t#flag darwin -D${metal_hook_define}')
	assert !metal_hook_body(header, 'typedef struct sg_desc').contains('v_sokol_')
}

fn test_metal_private_hook_preprocessor_and_sg_desc_abi() {
	root := os.join_path(os.vtmp_dir(), 'metal_private_hook_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	header_dir := os.join_path(@VEXEROOT, 'thirdparty', 'sokol')
	include_probe := os.join_path(root, 'include_probe.c')
	os.write_file(include_probe, '#include "sokol_gfx.h"\n') or { panic(err) }
	cc := os.quoted_path(metal_hook_cc())
	mut outputs := map[string]string{}
	for name, flags in {
		'plain':          ''
		'define_only':    '-D${metal_hook_define}'
		'metal_only':     '-DSOKOL_METAL'
		'gl_with_define': '-DSOKOL_GLCORE -D${metal_hook_define}'
		'enabled':        '-DSOKOL_METAL -D${metal_hook_define}'
	} {
		output := os.join_path(root, '${name}.i')
		metal_hook_run('${cc} -E -P -x c ${flags} -I${os.quoted_path(header_dir)} ${os.quoted_path(include_probe)} -o ${os.quoted_path(output)}')
		outputs[name] = metal_hook_source(output)
	}
	for name in ['plain', 'define_only', 'metal_only', 'gl_with_define'] {
		assert !outputs[name].contains(metal_hook_setter), '${name} leaked the private Metal hook'
	}
	assert outputs['enabled'].contains(metal_hook_setter)

	abi_source := os.join_path(root, 'abi.c')
	os.write_file(abi_source,
		'#include <stddef.h>\n#include <stdio.h>\n#include "sokol_gfx.h"\nint main(void) {\n#define P(field) printf("%zu ", offsetof(sg_desc, field))\nprintf("%zu ", sizeof(sg_desc));\nP(_start_canary); P(buffer_pool_size); P(image_pool_size); P(sampler_pool_size); P(shader_pool_size); P(pipeline_pool_size); P(attachments_pool_size); P(uniform_buffer_size); P(max_commit_listeners); P(disable_validation); P(mtl_force_managed_storage_mode); P(mtl_use_command_buffer_with_retained_references); P(wgpu_disable_bindgroups_cache); P(wgpu_bindgroups_cache_size); P(allocator); P(logger); P(environment); P(_end_canary);\nreturn 0;\n}\n') or {
		panic(err)
	}
	mut abi_outputs := []string{}
	for index, flags in ['', '-DSOKOL_METAL -D${metal_hook_define}'] {
		executable := os.join_path(root, 'abi_${index}')
		metal_hook_run('${cc} -std=c99 ${flags} -I${os.quoted_path(header_dir)} ${os.quoted_path(abi_source)} -o ${os.quoted_path(executable)}')
		abi_outputs << metal_hook_run(os.quoted_path(executable))
	}
	assert abi_outputs[0] == abi_outputs[1], 'the private hook changed sg_desc size/offsets'
}

fn test_metal_private_hook_slot_ownership_and_exactly_once() {
	header := metal_hook_header()
	declaration := metal_hook_marked(header, 'V_SOKOL_MTL_END_PASS_DECL_BEGIN',
		'V_SOKOL_MTL_END_PASS_DECL_END')
	implementation := metal_hook_marked(header, 'V_SOKOL_MTL_END_PASS_IMPL_BEGIN',
		'V_SOKOL_MTL_END_PASS_IMPL_END')
	root := os.join_path(os.vtmp_dir(), 'metal_private_hook_runtime_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source_path := os.join_path(root, 'runtime.c')
	executable := os.join_path(root, 'runtime')
	os.write_file(source_path,
		'#include <stdbool.h>\n#include <stddef.h>\n#define SOKOL_METAL\n#define ${metal_hook_define}\n#define SOKOL_GFX_API_DECL extern\n#define SOKOL_API_IMPL\n#define _SOKOL_PRIVATE static\n${declaration}\n${implementation}\nstatic int calls;\nstatic const void* expected_command;\nstatic const void* expected_drawable;\nstatic void callback(const void* command, const void* drawable, void* user_data) {\n    if ((command != expected_command) || (drawable != expected_drawable) || (user_data == NULL)) { calls = -100; return; }\n    calls++;\n}\nint main(void) {\n    int owner_a = 1; int owner_b = 2; int command = 3; int drawable = 4;\n    if (!${metal_hook_setter}(NULL, NULL)) return 1;\n    if (${metal_hook_setter}(callback, NULL)) return 2;\n    if (!${metal_hook_setter}(callback, &owner_a)) return 3;\n    if (${metal_hook_setter}(callback, &owner_a)) return 4;\n    if (${metal_hook_setter}(callback, &owner_b)) return 5;\n    for (int i = 0; i < 7; i++) {\n        expected_command = &command; expected_drawable = (i == 0) ? NULL : &drawable;\n        _v_sokol_metal_invoke_pre_present(expected_command, expected_drawable);\n    }\n    if (calls != 7) return 6;\n    if (${metal_hook_setter}(NULL, &owner_b)) return 7;\n    if (!${metal_hook_setter}(NULL, &owner_a)) return 8;\n    _v_sokol_metal_invoke_pre_present(&command, &drawable);\n    if (calls != 7) return 9;\n    if (!${metal_hook_setter}(callback, &owner_b)) return 10;\n    if (${metal_hook_setter}(NULL, &owner_a)) return 11;\n    _v_sokol_mtl_clear_end_pass_hook();\n    _v_sokol_metal_invoke_pre_present(&command, &drawable);\n    if (calls != 7) return 12;\n    if (!${metal_hook_setter}(callback, &owner_a)) return 13;\n    if (!${metal_hook_setter}(NULL, &owner_a)) return 14;\n    if (!${metal_hook_setter}(NULL, NULL)) return 15;\n    return 0;\n}\n') or {
		panic(err)
	}
	cc := os.quoted_path(metal_hook_cc())
	metal_hook_run('${cc} -std=c99 -Wall -Wextra ${os.quoted_path(source_path)} -o ${os.quoted_path(executable)}')
	if nm := os.find_abs_path_of_executable('nm') {
		symbols := metal_hook_run('${os.quoted_path(nm)} -g ${os.quoted_path(executable)}')
		assert symbols.contains(metal_hook_setter), 'the private C hook symbol is not link-visible'
	}
	metal_hook_run(os.quoted_path(executable))
}

fn test_metal_private_hook_missing_drawable_probe_reaches_backend_contract() {
	probe := metal_hook_marked(metal_hook_source(@FILE), 'V_SOKOL_MTL_NATIVE_PROBE_BEGIN',
		'V_SOKOL_MTL_NATIVE_PROBE_END')
	assert probe.contains('.disable_validation = disable_validation')
	validated_setup := probe.index('setup_metal(device, false);') or { -1 }
	unvalidated_setup := probe.index('setup_metal(device, true);') or { -1 }
	missing_drawable := probe.index('run_missing_drawable_pass();') or { -1 }
	assert validated_setup >= 0
	assert unvalidated_setup > validated_setup
	assert missing_drawable > unvalidated_setup
}

fn test_metal_private_hook_real_macos_pass_lifecycle() {
	$if macos {
		root := os.join_path(os.vtmp_dir(), 'metal_private_hook_macos_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source_path := os.join_path(root, 'runtime.m')
		executable := os.join_path(root, 'runtime')
		os.write_file(source_path,
			'/* V_SOKOL_MTL_NATIVE_PROBE_BEGIN */\n#define SOKOL_IMPL\n#define SOKOL_METAL\n#define ${metal_hook_define}\n#include "sokol_gfx.h"\n\nstatic int calls;\nstatic int callback_failed;\nstatic void* expected_owner;\n\nstatic void hook_callback(const void* command_buffer, const void* drawable_or_null, void* user_data) {\n    if ((command_buffer == NULL) || (drawable_or_null != NULL) || (user_data != expected_owner)) {\n        callback_failed = 1;\n        return;\n    }\n    calls++;\n}\n\nstatic sg_attachments make_offscreen_target(sg_image* image) {\n    *image = sg_make_image(&(sg_image_desc){\n        .render_target = true,\n        .width = 4,\n        .height = 4,\n        .pixel_format = SG_PIXELFORMAT_RGBA8,\n        .sample_count = 1,\n    });\n    return sg_make_attachments(&(sg_attachments_desc){\n        .colors[0].image = *image,\n    });\n}\n\nstatic void run_valid_pass(sg_attachments attachments) {\n    sg_begin_pass(&(sg_pass){ .attachments = attachments });\n    sg_end_pass();\n    sg_commit();\n}\n\nstatic void run_missing_drawable_pass(void) {\n    sg_begin_pass(&(sg_pass){\n        .swapchain = {\n            .width = 4,\n            .height = 4,\n            .sample_count = 1,\n            .color_format = SG_PIXELFORMAT_BGRA8,\n            .depth_format = SG_PIXELFORMAT_NONE,\n            .metal.current_drawable = NULL,\n        },\n    });\n    sg_end_pass();\n    sg_commit();\n}\n\nstatic void setup_metal(id<MTLDevice> device, bool disable_validation) {\n    sg_setup(&(sg_desc){\n        .disable_validation = disable_validation,\n        .environment = {\n            .defaults = {\n                .color_format = SG_PIXELFORMAT_BGRA8,\n                .depth_format = SG_PIXELFORMAT_NONE,\n                .sample_count = 1,\n            },\n            .metal.device = (__bridge const void*)device,\n        },\n    });\n}\n\nint main(void) {\n    @autoreleasepool {\n        id<MTLDevice> device = MTLCreateSystemDefaultDevice();\n        if (device == nil) return 1;\n        int owner_a = 1;\n        int owner_b = 2;\n        expected_owner = &owner_a;\n        setup_metal(device, false);\n        if (!sg_isvalid()) return 2;\n        if (!${metal_hook_setter}(hook_callback, &owner_a)) return 3;\n\n        sg_image image = {0};\n        sg_attachments attachments = make_offscreen_target(&image);\n        if ((sg_query_image_state(image) != SG_RESOURCESTATE_VALID) ||\n            (sg_query_attachments_state(attachments) != SG_RESOURCESTATE_VALID)) return 4;\n\n        run_valid_pass(attachments);\n        if (callback_failed || (calls != 1)) return 5;\n        if (!${metal_hook_setter}(NULL, &owner_a)) return 6;\n        run_valid_pass(attachments);\n        if (callback_failed || (calls != 1)) return 7;\n\n        if (!${metal_hook_setter}(hook_callback, &owner_a)) return 8;\n        sg_destroy_attachments(attachments);\n        sg_destroy_image(image);\n        sg_shutdown();\n\n        expected_owner = &owner_b;\n        setup_metal(device, true);\n        if (!sg_isvalid()) return 9;\n        if (!${metal_hook_setter}(hook_callback, &owner_b)) return 10;\n        run_missing_drawable_pass();\n        if (callback_failed || (calls != 1)) return 11;\n        if (!${metal_hook_setter}(NULL, &owner_b)) return 12;\n        sg_shutdown();\n    }\n    return 0;\n}\n/* V_SOKOL_MTL_NATIVE_PROBE_END */\n') or {
			panic(err)
		}
		assert !metal_hook_source(source_path).contains('_v_sokol_metal_invoke_pre_present')
		cc := os.quoted_path(metal_hook_cc())
		header_dir := os.join_path(@VEXEROOT, 'thirdparty', 'sokol')
		metal_hook_run('${cc} -std=c11 -Wall -Wextra -fobjc-arc -x objective-c -I${os.quoted_path(header_dir)} ${os.quoted_path(source_path)} -framework Metal -framework Foundation -framework QuartzCore -o ${os.quoted_path(executable)}')
		metal_hook_run(os.quoted_path(executable))
	}
}
