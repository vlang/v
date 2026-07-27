module amd64

import os

fn publication_test_present(path string) bool {
	return os.exists(path) || os.is_link(path)
}

fn publication_test_root(name string) string {
	root := os.join_path(os.temp_dir(), 'v3_amd64_publish_${name}_${os.getpid()}')
	assert !publication_test_present(root), 'test root `${root}` was stale'
	os.mkdir(root) or { assert false, 'create `${root}`: ${err.msg()}' }
	return root
}

fn publication_test_cleanup(root string) {
	if !publication_test_present(root) {
		return
	}
	assert os.is_dir(root) && !os.is_link(root), 'test root `${root}` changed type'
	for entry in os.ls(root) or { panic(err) } {
		assert !entry.ends_with('.amd64-stage'), 'publication stage leaked: `${entry}`'
	}
	os.rmdir_all(root) or { assert false, 'remove `${root}`: ${err.msg()}' }
	assert !publication_test_present(root), 'test root `${root}` survived cleanup'
}

fn publication_test_error(output string, bytes []u8) string {
	mut message := ''
	publish_object(output, bytes) or { message = err.msg() }
	assert message.len > 0, 'publication unexpectedly succeeded for `${output}`'
	return message
}

fn publication_test_executable_error(output string, bytes []u8) string {
	mut message := ''
	publish_executable(output, bytes) or { message = err.msg() }
	assert message.len > 0, 'executable publication unexpectedly succeeded for `${output}`'
	return message
}

fn publication_test_remove(path string) {
	assert publication_test_present(path), 'cleanup path `${path}` is absent'
	os.rm(path) or { assert false, 'remove `${path}`: ${err.msg()}' }
	assert !publication_test_present(path), 'cleanup path `${path}` survived'
}

fn publication_test_symlink(target string, link_name string) bool {
	os.symlink(target, link_name) or {
		$if windows {
			return false
		} $else {
			assert false, 'create symlink `${link_name}`: ${err.msg()}'
			return false
		}
	}
	return true
}

fn test_publish_object_success_and_stage_cleanup() {
	root := publication_test_root('success')
	defer {
		publication_test_cleanup(root)
	}
	output := os.join_path(root, 'canary.o')
	stage := publication_stage_path(output)
	bytes := [u8(0x7f), 0x45, 0x4c, 0x46, 0x31, 0xc0, 0xc3]
	publish_object(output, bytes) or { assert false, err.msg() }
	assert os.is_file(output) && !os.is_link(output)
	output_bytes := os.read_bytes(output) or { panic(err) }
	assert output_bytes == bytes
	assert !publication_test_present(stage)
}

fn test_publish_object_preserves_preexisting_final_and_stage() {
	root := publication_test_root('preexisting')
	defer {
		publication_test_cleanup(root)
	}
	bytes := [u8(0x31), 0xc0, 0xc3]
	final_output := os.join_path(root, 'final.o')
	final_sentinel := 'final-sentinel\n'
	os.write_file(final_output, final_sentinel) or { panic(err) }
	final_error := publication_test_error(final_output, bytes)
	assert final_error == 'final output `${final_output}` already exists'
	final_contents := os.read_file(final_output) or { panic(err) }
	assert final_contents == final_sentinel
	assert !publication_test_present(publication_stage_path(final_output))

	stage_output := os.join_path(root, 'stage.o')
	stage := publication_stage_path(stage_output)
	stage_sentinel := 'stage-sentinel\n'
	os.write_file(stage, stage_sentinel) or { panic(err) }
	stage_error := publication_test_error(stage_output, bytes)
	assert stage_error == 'stage `${stage}` already exists'
	stage_contents := os.read_file(stage) or { panic(err) }
	assert stage_contents == stage_sentinel
	assert !publication_test_present(stage_output)
	publication_test_remove(stage)

	stage_dir_output := os.join_path(root, 'stage-dir.o')
	stage_dir := publication_stage_path(stage_dir_output)
	os.mkdir(stage_dir) or { panic(err) }
	stage_dir_error := publication_test_error(stage_dir_output, bytes)
	assert stage_dir_error == 'stage `${stage_dir}` already exists'
	assert os.is_dir(stage_dir) && !os.is_link(stage_dir)
	os.rmdir(stage_dir) or { assert false, err.msg() }
	assert !publication_test_present(stage_dir)
}

fn test_publish_object_preserves_preexisting_symlinks_when_supported() {
	root := publication_test_root('symlink')
	defer {
		publication_test_cleanup(root)
	}
	bytes := [u8(0x31), 0xc0, 0xc3]
	target := os.join_path(root, 'target')
	target_sentinel := 'target-sentinel\n'
	os.write_file(target, target_sentinel) or { panic(err) }
	final_link := os.join_path(root, 'final-link.o')
	if !publication_test_symlink(target, final_link) {
		return
	}
	final_error := publication_test_error(final_link, bytes)
	assert final_error == 'final output `${final_link}` already exists'
	assert os.is_link(final_link)
	target_contents := os.read_file(target) or { panic(err) }
	assert target_contents == target_sentinel
	publication_test_remove(final_link)

	missing := os.join_path(root, 'missing')
	dangling_final := os.join_path(root, 'dangling-final.o')
	assert publication_test_symlink(missing, dangling_final)
	dangling_final_error := publication_test_error(dangling_final, bytes)
	assert dangling_final_error == 'final output `${dangling_final}` already exists'
	assert os.is_link(dangling_final) && !os.exists(dangling_final)
	publication_test_remove(dangling_final)

	stage_output := os.join_path(root, 'stage-link.o')
	stage_link := publication_stage_path(stage_output)
	assert publication_test_symlink(target, stage_link)
	stage_error := publication_test_error(stage_output, bytes)
	assert stage_error == 'stage `${stage_link}` already exists'
	assert os.is_link(stage_link)
	stage_target_contents := os.read_file(target) or { panic(err) }
	assert stage_target_contents == target_sentinel
	publication_test_remove(stage_link)

	dangling_stage_output := os.join_path(root, 'dangling-stage.o')
	dangling_stage := publication_stage_path(dangling_stage_output)
	assert publication_test_symlink(missing, dangling_stage)
	dangling_stage_error := publication_test_error(dangling_stage_output, bytes)
	assert dangling_stage_error == 'stage `${dangling_stage}` already exists'
	assert os.is_link(dangling_stage) && !os.exists(dangling_stage)
	publication_test_remove(dangling_stage)
}

fn test_publish_executable_enforces_platform_mode_contract_before_no_clobber_publication() {
	root := publication_test_root('executable-success')
	defer {
		publication_test_cleanup(root)
	}
	output := os.join_path(root, 'tiny-elf')
	stage := publication_stage_path(output)
	bytes := [u8(0x7f), 0x45, 0x4c, 0x46, 0x02, 0x01, 0x01, 0x00]
	$if windows {
		message := publication_test_executable_error(output, bytes)
		assert message == 'executable publication requires POSIX mode 0755 and is unsupported on Windows'
		assert !publication_test_present(output)
		assert !publication_test_present(stage)
	} $else {
		publish_executable(output, bytes) or { assert false, err.msg() }
		assert os.is_file(output) && !os.is_link(output)
		output_bytes := os.read_bytes(output) or { panic(err) }
		assert output_bytes == bytes
		assert !publication_test_present(stage)
		attributes := os.stat(output) or { panic(err) }
		assert (attributes.mode & u32(0o777)) == u32(0o755)
	}
}

fn test_publish_executable_preserves_preexisting_final_and_stage_on_posix() {
	$if !windows {
		root := publication_test_root('executable-preexisting')
		defer {
			publication_test_cleanup(root)
		}
		bytes := [u8(0x7f), 0x45, 0x4c, 0x46]
		final_output := os.join_path(root, 'final')
		final_sentinel := 'final-executable-sentinel\n'
		os.write_file(final_output, final_sentinel) or { panic(err) }
		final_error := publication_test_executable_error(final_output, bytes)
		assert final_error == 'final output `${final_output}` already exists'
		final_contents := os.read_file(final_output) or { panic(err) }
		assert final_contents == final_sentinel
		assert !publication_test_present(publication_stage_path(final_output))

		stage_output := os.join_path(root, 'stage')
		stage := publication_stage_path(stage_output)
		stage_sentinel := 'stage-executable-sentinel\n'
		os.write_file(stage, stage_sentinel) or { panic(err) }
		stage_error := publication_test_executable_error(stage_output, bytes)
		assert stage_error == 'stage `${stage}` already exists'
		stage_contents := os.read_file(stage) or { panic(err) }
		assert stage_contents == stage_sentinel
		assert !publication_test_present(stage_output)
		publication_test_remove(stage)
	}
}
