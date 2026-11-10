module veb

import net.http

// Keep these ownership checks active with -prod, which removes assert statements.
fn test_response_writer_contract_in_production_builds() {
	payload := 'production-body\x00'.repeat(1024)
	mut source := Context{}
	source.text(payload)
	retained := source.res.body
	mut writer := Context{}
	writer.preserve_for_response_writer(mut source)
	if writer.res.body != payload {
		panic('response writer lost its body')
	}
	shares := writer.res.body.str == retained.str
	if shares != response_body_can_share_gc_buffer() {
		panic('response writer used the wrong ownership mode')
	}
	writer.release_response_body()
	gc_collect()
	if retained != payload || source.res.body != payload {
		panic('response writer invalidated a retained application body')
	}
}

fn test_response_writer_moves_owned_body_and_keeps_input_copy() {
	payload := 'a\x00b' + 'x'.repeat(8192)
	mut source := Context{}
	source.text(payload)
	owned := source.res.body.str
	assert owned != payload.str
	mut writer := Context{}
	writer.preserve_for_response_writer(mut source)
	if response_body_can_share_gc_buffer() {
		assert writer.res.body.str == owned
	}
	assert writer.res.body == payload
	assert source.res.body == payload
	assert source.owned_response_body == unsafe { nil }
	assert writer.owned_response_body == unsafe { nil }
	gc_collect()
	assert writer.res.bytestr().ends_with(payload)
	writer.release_response_body()
	assert source.res.body == payload
}

fn test_response_writer_keeps_independent_headers_and_file_path() {
	mut source := Context{}
	source.req.header = http.new_header_from_map({
		http.CommonHeader.connection: 'keep-alive'
	})
	source.res.header.add(.set_cookie, 'a=1')
	source.res.header.add(.set_cookie, 'b=2')
	source.res.set_status(.created)
	source.return_file = '/tmp/owned-response-file'.clone()
	source.text('owned')
	mut writer := Context{}
	writer.preserve_for_response_writer(mut source)
	source.res.header.set(.set_cookie, 'changed')
	source.req.header.set(.connection, 'close')
	source.return_file = ''
	assert writer.res.status_code == 201
	assert writer.res.header.values(.set_cookie) == ['a=1', 'b=2']
	assert writer.req.header.get(.connection) or { '' } == 'keep-alive'
	assert writer.return_file == '/tmp/owned-response-file'
	writer.release_response_body()
}

fn test_response_writer_empty_and_large_bodies() {
	for size in [0, 1, 1024 * 1024] {
		payload := 'z'.repeat(size)
		mut source := Context{}
		source.text(payload)
		owned := source.res.body.str
		mut writer := Context{}
		writer.preserve_for_response_writer(mut source)
		assert writer.res.body.len == size
		assert writer.res.body == payload
		if size > 0 {
			if response_body_can_share_gc_buffer() {
				assert writer.res.body.str == owned
			}
		}
		assert source.res.body == payload
		writer.release_response_body()
	}
}

fn test_response_writer_self_handoff_keeps_body() {
	mut ctx := Context{}
	ctx.text('same-owner')
	owned := ctx.res.body.str
	ctx.preserve_for_response_writer(mut ctx)
	assert ctx.res.body == 'same-owner'
	if response_body_can_share_gc_buffer() {
		assert ctx.res.body.str == owned
	} else {
		assert ctx.res.body.str != owned
	}
	ctx.release_response_body()
}

fn test_response_writer_clones_untracked_and_replaced_buffers() {
	for replaced in [false, true] {
		mut source := Context{}
		if replaced { source.text('previous owned response') }
		borrowed := 'borrowed'.repeat(32)
		source.res.body = borrowed
		mut writer := Context{}
		writer.preserve_for_response_writer(mut source)
		assert writer.res.body == borrowed
		assert writer.res.body.str != borrowed.str
		assert source.res.body == borrowed
		writer.release_response_body()
	}
}

fn test_response_writer_clones_a_slice_replacing_owned_body() {
	mut source := Context{}
	source.text('prefix:payload')
	source.res.body = source.res.body[7..]
	mut writer := Context{}
	writer.preserve_for_response_writer(mut source)
	assert writer.res.body == 'payload'
	assert writer.res.body.str != source.res.body.str
	writer.release_response_body()
	// The original allocation remains a managed source reference here.
}

fn test_response_writer_preserves_other_body_alias_after_write() {
	payload := 'retained:'.repeat(1024)
	mut source := Context{}
	source.text(payload)
	retained := source.res.body
	mut writer := Context{}
	writer.preserve_for_response_writer(mut source)
	assert writer.shared_response_body == response_body_can_share_gc_buffer()
	writer.release_response_body()
	mut churn := []string{}
	for _ in 0 .. 1000 { churn << 'overwrite'.repeat(1024) }
	gc_collect()
	assert retained == payload
	assert source.res.body == '' || source.res.body == payload
}

fn test_response_writer_releases_retained_owner_after_body_replacement() {
	mut source := Context{}
	source.text('old-owned-body'.repeat(1024))
	if response_body_can_share_gc_buffer() {
		assert source.owned_response_body == source.res.body.str
	}
	source.res.body = 'replacement'
	gc_collect()
	mut writer := Context{}
	writer.preserve_for_response_writer(mut source)
	assert writer.res.body == 'replacement'
	assert source.res.body == 'replacement'
	assert !writer.shared_response_body
	assert source.owned_response_body == unsafe { nil }
	assert writer.owned_response_body == unsafe { nil }
	writer.release_response_body()
}
