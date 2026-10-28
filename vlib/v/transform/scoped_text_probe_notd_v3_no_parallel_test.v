module transform

import v.flat
import v.types

fn test_scoped_text_promotion_reuses_ast_text_and_deduplicates_unknown_text() {
	mut a := flat.FlatAst.new()
	_, canonical := a.intern_text('source identifier'.clone())
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.promote_text_cache = &PromoteTextCache{}
	t.begin_promote_text_window()
	source_copy := 'source identifier'.clone()
	assert t.promote_scoped_result_text(source_copy) == canonical
	assert t.scoped_promoted_texts.len == 0
	first := t.promote_scoped_result_text('new identifier'.clone())
	second := t.promote_scoped_result_text('new identifier'.clone())
	assert first == second
	assert first.str == second.str
	assert t.scoped_promoted_texts.len == 1
	t.end_promote_text_window()
	assert !t.promote_text_cache.active
	assert t.promote_scoped_result_text(source_copy) == canonical
}

fn test_scoped_text_local_clone_can_become_durable_ast_canonical_text() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	local := t.promote_scoped_result_text('later canonical identifier'.clone())
	assert t.scoped_promoted_texts.len == 1
	text_start := a.text_values.len
	scratch := transform_worker_scope_begin(true)
	a.intern_text('later canonical identifier'.clone())
	transform_worker_scope_leave(scratch)
	a.promote_transform_texts_from(text_start, scratch)
	id := a.text_ids['later canonical identifier'] or { panic('missing canonical text') }
	canonical := a.text(id)
	t.promote_text_cache = &PromoteTextCache{}
	t.begin_promote_text_window()
	published := t.promote_scoped_result_text('later canonical identifier'.clone())
	assert published == local
	assert published.str == canonical.str
	$if prealloc {
		assert !transform_scope_owns(scratch, published.str)
	}
	t.end_promote_text_window()
	transform_worker_scope_free(scratch)
	assert published == 'later canonical identifier'
	assert local == published
	assert t.promote_scoped_result_text('later canonical identifier'.clone()) == published
}
