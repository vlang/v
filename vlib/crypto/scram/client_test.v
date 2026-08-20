// Client behaviour: what it accepts, and everything it must refuse.
//
// The happy path is covered by `conformance_test.v`; this file is about the
// checks that stand between a client and a hostile or broken server.
module scram

// The RFC 7677 §3 exchange, reused as a starting point that is known good, so
// that each test can change exactly one thing and watch the client react.
const rfc_nonce = 'rOprNGfwEbeRWgbNEkqO'
const rfc_server_first = 'r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF\$k0,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096'
const rfc_server_final = 'v=6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4='

fn rfc_client() &Client {
	return new_client(username: 'user', password: 'pencil', nonce: rfc_nonce) or { panic(err) }
}

fn test_new_client_rejects_an_empty_username() {
	new_client(username: '', password: 'pencil') or {
		assert err.msg() == 'scram: the username must not be empty'
		return
	}
	assert false, 'accepted an empty username'
}

fn test_new_client_rejects_an_unusable_nonce() {
	// An empty `nonce` is the documented request for a generated one, so the
	// unusable values are the ones that would break the message grammar.
	for bad in ['a,b', 'with space', 'nul\0byte', 'trailing\n'] {
		new_client(username: 'user', password: 'pencil', nonce: bad) or { continue }
		assert false, 'accepted the nonce `${bad}`'
	}
}

fn test_new_client_rejects_channel_binding_without_a_name() {
	new_client(
		username:        'user'
		password:        'pencil'
		channel_binding: ChannelBinding{
			mode: .required
			data: [u8(1), 2, 3]
		}
	) or {
		assert err.msg().contains('channel binding name is required')
		return
	}
	assert false, 'accepted channel binding without a type name'
}

fn test_an_empty_password_is_allowed() {
	// SCRAM has no opinion on password length, and a server may well store
	// credentials for one. Refusing it here would be this module inventing a
	// policy that belongs to the application.
	mut client := new_client(username: 'user', password: '', nonce: rfc_nonce)!
	assert client.first()! == 'n,,n=user,r=${rfc_nonce}'
	assert client.final(rfc_server_first)!.contains(',p=')
}

fn test_mechanism_name_follows_the_channel_binding() {
	plain := new_client(username: 'user', password: 'pencil')!
	assert plain.mechanism_name() == 'SCRAM-SHA-256'
	sha1_client := new_client(username: 'user', password: 'pencil', mechanism: .sha1)!
	assert sha1_client.mechanism_name() == 'SCRAM-SHA-1'
	bound := new_client(
		username:        'user'
		password:        'pencil'
		channel_binding: ChannelBinding{
			mode: .required
			name: 'tls-server-end-point'
			data: [u8(1), 2, 3]
		}
	)!
	assert bound.mechanism_name() == 'SCRAM-SHA-256-PLUS'
}

fn test_the_gs2_flag_reflects_the_channel_binding_mode() {
	mut unsupported := new_client(username: 'user', password: 'pencil', nonce: rfc_nonce)!
	assert unsupported.first()!.starts_with('n,,')

	mut downgraded := new_client(
		username:        'user'
		password:        'pencil'
		nonce:           rfc_nonce
		channel_binding: ChannelBinding{
			mode: .unsupported_by_server
		}
	)!
	assert downgraded.first()!.starts_with('y,,')

	mut bound := new_client(
		username:        'user'
		password:        'pencil'
		nonce:           rfc_nonce
		channel_binding: ChannelBinding{
			mode: .required
			name: 'tls-server-end-point'
			data: [u8(1), 2, 3]
		}
	)!
	assert bound.first()!.starts_with('p=tls-server-end-point,,')
}

fn test_the_authzid_is_escaped_in_the_gs2_header() {
	mut client := new_client(
		username: 'user'
		password: 'pencil'
		authzid:  'a,b=c'
		nonce:    rfc_nonce
	)!
	assert client.first()!.starts_with('n,a=a=2Cb=3Dc,')
}

fn test_the_username_is_escaped_in_the_first_message() {
	mut client := new_client(username: 'a,b=c', password: 'pencil', nonce: rfc_nonce)!
	assert client.first()! == 'n,,n=a=2Cb=3Dc,r=${rfc_nonce}'
}

fn test_the_steps_must_be_called_in_order() {
	mut client := rfc_client()
	client.final(rfc_server_first) or {
		assert err.msg().contains('final() must be called once, after first()')
		client.verify(rfc_server_final) or {
			assert err.msg().contains('verify() must be called once, after final()')
			client.first()!
			client.first() or {
				assert err.msg().contains('first() must be called exactly once')
				return
			}
			assert false, 'first() ran twice'
		}
		assert false, 'verify() ran before final()'
	}
	assert false, 'final() ran before first()'
}

fn test_a_server_nonce_that_does_not_extend_the_client_nonce_is_refused() {
	cases := {
		'a different nonce entirely': 'r=somethingelse,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096'
		'the client nonce unchanged': 'r=${rfc_nonce},s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096'
		'a prefix of it':             'r=${rfc_nonce[..4]},s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096'
	}
	for what, server_first in cases {
		mut client := rfc_client()
		client.first()!
		client.final(server_first) or {
			assert err is AuthenticationFailed, what
			continue
		}
		assert false, 'accepted ${what}'
	}
}

fn test_a_low_iteration_count_is_refused() {
	for count in [1, 100, 4095] {
		mut client := rfc_client()
		client.first()!
		client.final('r=${rfc_nonce}srv,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=${count}') or {
			assert err.msg().contains('below the configured minimum of 4096')
			assert err is AuthenticationFailed
			continue
		}
		assert false, 'accepted ${count} iterations'
	}
}

fn test_the_iteration_floor_can_be_lowered_deliberately() {
	// Some deployments predate RFC 7677 §4 and still use 1000. Talking to them
	// has to be possible, but only by saying so explicitly.
	mut client := new_client(
		username:       'user'
		password:       'pencil'
		nonce:          rfc_nonce
		min_iterations: 1000
	)!
	client.first()!
	assert client.final('r=${rfc_nonce}srv,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=1000')!.contains(',p=')
}

fn test_a_malformed_server_first_message_is_refused() {
	cases := {
		'empty':                          ''
		'no attributes':                  'garbage'
		'attribute with no value':        'r=,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096'
		'wrong attribute order':          's=W22ZaJ0SNY7soEsUEjb6gQ==,r=${rfc_nonce}srv,i=4096'
		'missing iteration count':        'r=${rfc_nonce}srv,s=W22ZaJ0SNY7soEsUEjb6gQ=='
		'salt is not base64':             'r=${rfc_nonce}srv,s=not!base64,i=4096'
		'salt is empty':                  'r=${rfc_nonce}srv,s=,i=4096'
		'iterations are not a number':    'r=${rfc_nonce}srv,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=lots'
		'iterations are negative':        'r=${rfc_nonce}srv,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=-1'
		'iterations have a leading zero': 'r=${rfc_nonce}srv,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=04096'
		'a numeric attribute name':       'r=${rfc_nonce}srv,s=W22ZaJ0SNY7soEsUEjb6gQ==,1=4096'
	}
	for what, server_first in cases {
		mut client := rfc_client()
		client.first()!
		client.final(server_first) or {
			assert err is MalformedMessage, '${what}: ${err}'
			continue
		}
		assert false, 'accepted a server-first-message with ${what}'
	}
}

fn test_a_mandatory_extension_from_the_server_ends_the_exchange() {
	mut client := rfc_client()
	client.first()!
	client.final('m=somefeature,r=${rfc_nonce}srv,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096') or {
		assert err.msg().contains('unsupported mandatory extension `somefeature`')
		assert err is MalformedMessage
		return
	}
	assert false, 'ignored a mandatory extension'
}

fn test_a_wrong_server_signature_is_refused() {
	mut client := rfc_client()
	client.first()!
	client.final(rfc_server_first)!
	client.verify('v=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=') or {
		assert err.msg().contains('server signature does not match')
		assert err is AuthenticationFailed
		assert !client.done()
		return
	}
	assert false, 'accepted a forged server signature'
}

fn test_a_server_signature_of_the_wrong_length_is_refused() {
	mut client := rfc_client()
	client.first()!
	client.final(rfc_server_first)!
	client.verify('v=AAAA') or {
		assert err is AuthenticationFailed
		return
	}
	assert false, 'accepted a truncated server signature'
}

fn test_a_wrong_password_is_only_caught_by_the_server_signature() {
	// The client cannot know the password is wrong before the server answers:
	// this is what makes the last step mandatory rather than decorative.
	mut client := new_client(username: 'user', password: 'wrong', nonce: rfc_nonce)!
	client.first()!
	proof := client.final(rfc_server_first)!
	assert proof.contains(',p=')
	client.verify(rfc_server_final) or {
		assert err is AuthenticationFailed
		return
	}
	assert false, 'a wrong password produced a valid exchange'
}

fn test_a_server_refusal_surfaces_as_a_server_error() {
	mut client := rfc_client()
	client.first()!
	client.final(rfc_server_first)!
	client.verify('e=invalid-proof') or {
		if err is ServerError {
			assert err.code == 'invalid-proof'
		}
		assert err.msg() == 'scram: server refused the exchange: invalid-proof'
		assert err is ServerError
		return
	}
	assert false, 'a server refusal was treated as a success'
}

fn test_a_malformed_server_final_message_is_refused() {
	for server_final in ['', 'garbage', 'x=something', 'v=not!base64', 'v='] {
		mut client := rfc_client()
		client.first()!
		client.final(rfc_server_first)!
		client.verify(server_final) or {
			assert err is MalformedMessage, server_final
			continue
		}
		assert false, 'accepted the server-final-message `${server_final}`'
	}
}

fn test_a_refused_exchange_is_not_reported_as_done() {
	mut client := rfc_client()
	client.first()!
	client.final(rfc_server_first)!
	client.verify('e=invalid-proof') or {
		// The exchange ended, but it ended badly: a caller that only looks at
		// `done()` must not conclude it authenticated.
		assert !client.done()
		// And the step cannot be retried against the same nonce.
		client.verify(rfc_server_final) or {
			assert err.msg().contains('verify() must be called once')
			return
		}
		assert false, 'verify() ran a second time after a refusal'
	}
	assert false, 'a server refusal was treated as a success'
}

fn test_a_failed_step_cannot_be_retried() {
	mut client := rfc_client()
	client.first()!
	client.final('r=nope,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096') or {
		client.final(rfc_server_first) or {
			assert err.msg().contains('final() must be called once')
			assert !client.done()
			return
		}
		assert false, 'final() ran again after a failure'
	}
	assert false, 'accepted a bad server nonce'
}
