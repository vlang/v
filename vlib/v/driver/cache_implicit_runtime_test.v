module driver

import v.flat
import v.types

fn test_cached_module_objects_keep_implicit_runtimes_and_their_dependencies() {
	mut a := flat.FlatAst.new()
	tc := types.TypeChecker.new(&a)
	state := V3ModuleCacheState{
		module_sources:      {
			'builtin':         [@FILE]
			'builtin.closure': [@FILE]
			'sync':            [@FILE]
			'v.embed_file':    [@FILE]
			'os':              [@FILE]
			'support':         [@FILE]
			'unused':          [@FILE]
		}
		bundle_source_paths: {
			a.real_source_path(@FILE): true
		}
		module_dependencies: {
			'builtin.closure': ['support']
		}
	}
	objects := {
		'builtin':         'builtin.o'
		'builtin.closure': 'closure.o'
		'sync':            'sync.o'
		'v.embed_file':    'embed.o'
		'os':              'os.o'
		'support':         'support.o'
		'unused':          'unused.o'
	}
	// Only os is called by the program. Its cached object can also refer to runtime
	// helpers from functions that are absent from the program's used-function set.
	selected := cache_used_object_paths(objects, &map[string]bool{}, 'os__getwd();', tc,
		&state)
	assert selected == ['builtin.o', 'closure.o', 'os.o', 'support.o', 'sync.o', 'embed.o']
}
