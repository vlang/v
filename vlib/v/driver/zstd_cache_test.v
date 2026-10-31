module driver

import os
import crypto.sha256
import v.gen.c as cgen
import v.modulecache

fn test_cached_embedded_inputs_require_current_payload_and_metadata() ! {
	root := os.real_path(os.join_path(os.vtmp_dir(), 'cached_embed_inputs_${os.getpid()}'))
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	entry := os.join_path(root, 'main.v')
	resource := os.join_path(root, 'payload.txt')
	os.write_file(entry, 'fn main() {}\n')!
	os.write_file(resource, 'first')!
	mut state := V3ModuleCacheState{
		manager:        modulecache.new_manager(root, 'embedded-test', true, '', '')
		objects:        {
			'sample': os.join_path(root, 'sample.o')
		}
		module_sources: {
			'sample': [os.join_path(root, 'sample.v')]
		}
	}
	input := 'sample:${resource}'
	mut recorded := {
		'module:sample': 'stable module'
	}
	recorded['external:${input}'] = modulecache.file_signature(resource)
	recorded['external-meta:${input}'] = modulecache.file_metadata_signature(resource)
	recorded['external-sha256:${input}'] = sha256.sum(os.read_bytes(resource)!).hex()
	state.manager.write_cgen([entry], '', recorded, 'unused C', '')!
	mut current := {
		'module:sample': 'stable module'
	}
	v3_restore_cached_embedded_inputs(&state, [entry], '', mut current)
	assert current == recorded
	// Even a same-length edit must not restore historical signatures.
	os.write_file(resource, 'other')!
	current = {
		'module:sample': 'stable module'
	}
	v3_restore_cached_embedded_inputs(&state, [entry], '', mut current)
	assert current.len == 1
	os.rm(resource)!
	v3_restore_cached_embedded_inputs(&state, [entry], '', mut current)
	assert current.len == 1
}

fn test_bundled_native_inputs_require_c_compiler_processing() {
	source := os.real_path(os.join_path(@VEXEROOT, 'thirdparty', 'zstd', 'zstd.c'))
	inputs := cgen.CacheNativeInputs{
		module_inputs: {
			'zstd': [source]
		}
		native_paths:  {
			source: true
		}
	}
	closure := v3_native_input_closure(&inputs)
	assert closure.unassignable == source
	assert closure.inputs['zstd'] == [source]
	mut state := V3ModuleCacheState{}
	assert !prepare_v3_cache_external_inputs(mut state, &inputs, &closure)
}
