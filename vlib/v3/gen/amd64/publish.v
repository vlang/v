module amd64

import os

const publication_windows_executable_error = 'executable publication requires POSIX mode 0755 and is unsupported on Windows'

enum PublicationKind {
	relocatable_object
	executable
}

fn (kind PublicationKind) artifact_name() string {
	return match kind {
		.relocatable_object { 'object' }
		.executable { 'executable' }
	}
}

fn publication_stage_path(output_file string) string {
	return output_file + '.amd64-stage'
}

fn publication_path_present(path string) bool {
	return os.exists(path) || os.is_link(path)
}

fn cleanup_owned_publication_stage(stage string) string {
	if !publication_path_present(stage) {
		return 'owned stage `${stage}` disappeared before cleanup'
	}
	os.rm(stage) or { return 'remove owned stage `${stage}`: ${err.msg()}' }
	if publication_path_present(stage) {
		return 'owned stage `${stage}` remained after cleanup'
	}
	return ''
}

fn publication_failure(primary string, cleanup string) IError {
	if cleanup.len == 0 {
		return error(primary)
	}
	return error('${primary}; cleanup failed: ${cleanup}')
}

// publish_generated requires a controlled, non-hostile output parent.
// Concurrent adversarial mutation of the fixed sibling stage is outside this
// contract.
fn publish_generated(output_file string, generated_bytes []u8, kind PublicationKind) ! {
	if output_file.len == 0 {
		return error('output path is empty')
	}
	artifact_name := kind.artifact_name()
	if generated_bytes.len == 0 {
		return error('generated ${artifact_name} is empty')
	}
	$if windows {
		if kind == .executable {
			return error(publication_windows_executable_error)
		}
	}
	stage := publication_stage_path(output_file)
	if publication_path_present(output_file) {
		return error('final output `${output_file}` already exists')
	}
	if publication_path_present(stage) {
		return error('stage `${stage}` already exists')
	}
	os.write_bytes(stage, generated_bytes) or {
		cleanup := if publication_path_present(stage) {
			cleanup_owned_publication_stage(stage)
		} else {
			''
		}
		return publication_failure('write stage `${stage}`: ${err.msg()}', cleanup)
	}
	if os.is_link(stage) || !os.is_file(stage) {
		cleanup := cleanup_owned_publication_stage(stage)
		return publication_failure('stage `${stage}` is not a regular file', cleanup)
	}
	actual_size := os.file_size(stage)
	if actual_size != u64(generated_bytes.len) {
		cleanup := cleanup_owned_publication_stage(stage)
		return publication_failure('stage `${stage}` has size ${actual_size}, expected ${generated_bytes.len}',
			cleanup)
	}
	actual_bytes := os.read_bytes(stage) or {
		cleanup := cleanup_owned_publication_stage(stage)
		return publication_failure('read back stage `${stage}`: ${err.msg()}', cleanup)
	}
	if actual_bytes != generated_bytes {
		cleanup := cleanup_owned_publication_stage(stage)
		return publication_failure('stage `${stage}` bytes differ from generated ${artifact_name}',
			cleanup)
	}
	if kind == .executable {
		$if !windows {
			os.chmod(stage, 0o755) or {
				cleanup := cleanup_owned_publication_stage(stage)
				return publication_failure('set executable mode on stage `${stage}`: ${err.msg()}',
					cleanup)
			}
			attributes := os.stat(stage) or {
				cleanup := cleanup_owned_publication_stage(stage)
				return publication_failure('stat executable stage `${stage}`: ${err.msg()}', cleanup)
			}
			actual_mode := attributes.mode & u32(0o777)
			if actual_mode != u32(0o755) {
				cleanup := cleanup_owned_publication_stage(stage)
				return publication_failure('executable stage `${stage}` has mode 0o${actual_mode:o}, expected 0o755',
					cleanup)
			}
		}
	}
	if publication_path_present(output_file) {
		cleanup := cleanup_owned_publication_stage(stage)
		return publication_failure('final output `${output_file}` appeared before publication',
			cleanup)
	}
	os.link(stage, output_file) or {
		cleanup := cleanup_owned_publication_stage(stage)
		return publication_failure('publish `${output_file}` without replacement: ${err.msg()}',
			cleanup)
	}
	cleanup := cleanup_owned_publication_stage(stage)
	if cleanup.len > 0 {
		return error('published `${output_file}` but cleanup failed: ${cleanup}')
	}
}

// publish_object preserves the relocatable-object publication contract.
pub fn publish_object(output_file string, object_bytes []u8) ! {
	publish_generated(output_file, object_bytes, .relocatable_object)!
}

// publish_executable applies mode 0755 to the owned stage before publication
// on POSIX hosts and refuses publication on Windows.
pub fn publish_executable(output_file string, executable_bytes []u8) ! {
	publish_generated(output_file, executable_bytes, .executable)!
}
