// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ssa

import v2.ast

fn test_skip_modules_with_file_fragment_does_not_drop_user_modules() {
	mut mod := Module.new('skip_modules_test')
	mut b := Builder.new(mod)
	b.cur_module = 'c'
	b.skip_modules['c'] = true
	b.skip_module_file_fragments['c'] = '/vlib/v2_toberemoved/gen/c/'

	decl := ast.FnDecl{
		name: 'user_fn'
	}
	assert b.should_build_fn('/tmp/project/c/user.v', decl)
	assert !b.should_build_fn('/Users/me/code/v/vlib/v2_toberemoved/gen/c/c.v', decl)
	assert !b.should_build_fn('../../vlib/v2_toberemoved/gen/c/c.v', decl)
}

fn test_skip_modules_with_file_fragment_matches_eval_backend_path() {
	mut mod := Module.new('skip_modules_eval_test')
	mut b := Builder.new(mod)
	b.cur_module = 'eval'
	b.skip_modules['eval'] = true
	b.skip_module_file_fragments['eval'] = '/vlib/v2_toberemoved/eval/'

	decl := ast.FnDecl{
		name: 'user_fn'
	}
	assert b.should_build_fn('/tmp/project/eval/user.v', decl)
	assert !b.should_build_fn('/Users/me/code/v/vlib/v2_toberemoved/eval/eval.v', decl)
	assert !b.should_build_fn('../../vlib/v2_toberemoved/eval/eval.v', decl)
}
