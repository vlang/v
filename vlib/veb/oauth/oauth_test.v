module oauth

// `get_token` is the only function in this module and it performs a network
// round trip to the provider's token endpoint (both `.form` and `.json`), so
// there is nothing to cover deterministically here. What is left is the public
// shape of the types and the one default value a caller depends on.

fn test_context_posts_a_form_by_default() {
	ctx := Context{
		token_url: 'https://example.com/token'
	}
	assert ctx.token_post_type == .form
}

fn test_context_can_be_asked_to_post_json() {
	ctx := Context{
		token_url:       'https://example.com/token'
		token_post_type: .json
	}
	assert ctx.token_post_type == .json
}

fn test_token_post_type_has_the_two_documented_members() {
	assert TokenPostType.form != TokenPostType.json
	all := [TokenPostType.form, TokenPostType.json]
	assert all.len == 2
}

fn test_context_keeps_the_provider_credentials_it_is_given() {
	ctx := Context{
		token_url:     'https://example.com/token'
		client_id:     'client-id'
		client_secret: 'client-secret'
		redirect_uri:  'https://example.com/callback'
	}
	assert ctx.token_url == 'https://example.com/token'
	assert ctx.client_id == 'client-id'
	assert ctx.client_secret == 'client-secret'
	assert ctx.redirect_uri == 'https://example.com/callback'
	assert Context{}.client_id == ''
	assert Context{}.redirect_uri == ''
	assert Context{}.token_url == ''
}

fn test_request_keeps_the_authorization_code_it_is_given() {
	req := Request{
		client_id:     'client-id'
		client_secret: 'client-secret'
		code:          'authorization-code'
		state:         'csrf-state'
	}
	assert req.client_id == 'client-id'
	assert req.client_secret == 'client-secret'
	assert req.code == 'authorization-code'
	assert req.state == 'csrf-state'
	assert Request{}.code == ''
	assert Request{}.state == ''
}
