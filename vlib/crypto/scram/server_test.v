// Server behaviour: the checks that stand between a server and a client that
// guesses, replays or rewrites parts of the exchange.
module scram

import encoding.base64

// pencil_credentials is the record a server would have stored for `user`.
fn pencil_credentials(mechanism Mechanism) Credentials {
	return derive_credentials(mechanism, 'pencil', 'saltsaltsaltsalt'.bytes(), 4096) or {
		panic(err)
	}
}

// server_for builds a server that knows exactly one user.
fn server_for(mechanism Mechanism, binding ChannelBinding) &Server {
	creds := pencil_credentials(mechanism)
	return new_server(
		mechanism:       mechanism
		channel_binding: binding
		lookup:          fn [creds] (username string) !Credentials {
			return creds
		}
	) or { panic(err) }
}

// run_exchange drives both halves and returns the two clients' view of it.
fn run_exchange(mut client Client, mut server Server) ! {
	server_first := server.first(client.first()!)!
	server_final := server.final(client.final(server_first)!)!
	client.verify(server_final)!
}

fn test_a_full_exchange_succeeds_for_every_mechanism() {
	for mechanism in [Mechanism.sha1, .sha256, .sha512] {
		mut server := server_for(mechanism, ChannelBinding{})
		mut client := new_client(username: 'user', password: 'pencil', mechanism: mechanism)!
		run_exchange(mut client, mut server) or { assert false, '${mechanism.name()}: ${err}' }
		assert client.done(), mechanism.name()
		assert server.done(), mechanism.name()
		assert server.username() == 'user', mechanism.name()
	}
}

fn test_a_full_exchange_succeeds_with_a_randomly_salted_record() {
	creds := new_credentials(.sha256, 'pencil')!
	mut server := new_server(
		lookup: fn [creds] (username string) !Credentials {
			return creds
		}
	)!
	mut client := new_client(username: 'user', password: 'pencil')!
	run_exchange(mut client, mut server)!
	assert client.done() && server.done()
}

fn test_a_full_exchange_succeeds_with_channel_binding() {
	binding := ChannelBinding{
		mode: .required
		name: 'tls-server-end-point'
		data: 'a hash of the server certificate'.bytes()
	}
	mut server := server_for(.sha256, binding)
	mut client := new_client(
		username:        'user'
		password:        'pencil'
		channel_binding: binding
	)!
	run_exchange(mut client, mut server)!
	assert client.mechanism_name() == 'SCRAM-SHA-256-PLUS'
	assert server.mechanism_name() == 'SCRAM-SHA-256-PLUS'
}

fn test_a_full_exchange_carries_the_authorization_identity() {
	mut server := server_for(.sha256, ChannelBinding{})
	mut client := new_client(username: 'user', password: 'pencil', authzid: 'admin,role=x')!
	run_exchange(mut client, mut server)!
	assert server.authzid() == 'admin,role=x'
	assert server.username() == 'user'
}

fn test_a_full_exchange_carries_an_escaped_user_name() {
	creds := pencil_credentials(.sha256)
	mut server := new_server(
		lookup: fn [creds] (username string) !Credentials {
			assert username == 'a,b=c'
			return creds
		}
	)!
	mut client := new_client(username: 'a,b=c', password: 'pencil')!
	run_exchange(mut client, mut server)!
	assert server.username() == 'a,b=c'
}

fn test_a_wrong_password_is_refused() {
	mut server := server_for(.sha256, ChannelBinding{})
	mut client := new_client(username: 'user', password: 'crayon')!
	run_exchange(mut client, mut server) or {
		assert err.msg().contains('client proof does not match')
		assert err is AuthenticationFailed
		assert !server.done()
		return
	}
	assert false, 'a wrong password was accepted'
}

fn test_an_unknown_user_aborts_the_exchange() {
	mut server := new_server(
		lookup: fn (username string) !Credentials {
			return error('no such user: ${username}')
		}
	)!
	mut client := new_client(username: 'ghost', password: 'pencil')!
	server.first(client.first()!) or {
		assert err.msg() == 'no such user: ghost'
		return
	}
	assert false, 'an unknown user was let through'
}

fn test_credentials_for_another_mechanism_are_refused() {
	creds := pencil_credentials(.sha1)
	mut server := new_server(
		mechanism: .sha256
		lookup:    fn [creds] (username string) !Credentials {
			return creds
		}
	)!
	mut client := new_client(username: 'user', password: 'pencil')!
	server.first(client.first()!) or {
		assert err.msg().contains('are for SCRAM-SHA-1, but this server speaks SCRAM-SHA-256')
		return
	}
	assert false, 'mismatched credentials were used'
}

fn test_incomplete_credentials_are_refused() {
	for creds in [
		Credentials{
			mechanism:  .sha256
			salt:       []u8{}
			iterations: 4096
		},
		Credentials{
			mechanism:  .sha256
			salt:       'salt'.bytes()
			iterations: 0
		},
	] {
		mut server := new_server(
			lookup: fn [creds] (username string) !Credentials {
				return creds
			}
		)!
		mut client := new_client(username: 'user', password: 'pencil')!
		server.first(client.first()!) or {
			assert err.msg().contains('incomplete')
			continue
		}
		assert false, 'incomplete credentials were used'
	}
}

fn test_the_steps_must_be_called_in_order() {
	mut server := server_for(.sha256, ChannelBinding{})
	server.final('c=biws,r=x,p=y') or {
		assert err.msg().contains('final() must be called once, after first()')
		mut client := new_client(username: 'user', password: 'pencil')!
		first := client.first()!
		server.first(first)!
		server.first(first) or {
			assert err.msg().contains('first() must be called exactly once')
			return
		}
		assert false, 'first() ran twice'
	}
	assert false, 'final() ran before first()'
}

fn test_a_malformed_client_first_message_is_refused() {
	cases := {
		'empty':                    ''
		'no GS2 header':            'n=user'
		'an incomplete GS2 header': 'n,n=user,r=abc'
		'an unknown GS2 flag':      'z,,n=user,r=abc'
		'a bad authzid marker':     'n,zz,n=user,r=abc'
		'a missing user name':      'n,,r=abc'
		'an empty user name':       'n,,n=,r=abc'
		'a missing nonce':          'n,,n=user'
		'a bad escape in the name': 'n,,n=a=FFb,r=abc'
		'a mandatory extension':    'n,,m=feature,n=user,r=abc'
		'a nonce with a space':     'n,,n=user,r=a b'
	}
	for what, client_first in cases {
		mut server := server_for(.sha256, ChannelBinding{})
		server.first(client_first) or {
			assert err is MalformedMessage, '${what}: ${err}'
			continue
		}
		assert false, 'accepted a client-first-message with ${what}'
	}
}

fn test_a_malformed_client_final_message_is_refused() {
	cases := {
		'empty':                      ''
		'a missing channel binding':  'r=NONCE,p=AAAA'
		'a missing proof':            'c=biws,r=NONCE'
		'the proof not last':         'c=biws,p=AAAA,r=NONCE'
		'a proof that is not base64': 'c=biws,r=NONCE,p=not!base64'
	}
	for what, template in cases {
		mut server := server_for(.sha256, ChannelBinding{})
		mut client := new_client(username: 'user', password: 'pencil')!
		server_first := server.first(client.first()!)!
		nonce := server_first.all_after('r=').all_before(',')
		server.final(template.replace('NONCE', nonce)) or {
			assert err is MalformedMessage, '${what}: ${err}'
			continue
		}
		assert false, 'accepted a client-final-message with ${what}'
	}
}

fn test_a_proof_of_the_wrong_length_is_refused() {
	mut server := server_for(.sha256, ChannelBinding{})
	mut client := new_client(username: 'user', password: 'pencil')!
	server_first := server.first(client.first()!)!
	nonce := server_first.all_after('r=').all_before(',')
	server.final('c=biws,r=${nonce},p=${base64.encode([u8(1), 2, 3])}') or {
		assert err.msg().contains('is 3 bytes, expected 32')
		assert err is AuthenticationFailed
		return
	}
	assert false, 'accepted a short proof'
}

fn test_a_replayed_nonce_from_another_exchange_is_refused() {
	mut server := server_for(.sha256, ChannelBinding{})
	mut client := new_client(username: 'user', password: 'pencil')!
	server.first(client.first()!)!
	// A client-final-message that is internally consistent, but built on the
	// nonce of a different exchange.
	mut other_server := server_for(.sha256, ChannelBinding{})
	mut other_client := new_client(username: 'user', password: 'pencil')!
	other_first := other_server.first(other_client.first()!)!
	stolen := other_client.final(other_first)!
	server.final(stolen) or {
		assert err.msg().contains('nonce does not match')
		assert err is AuthenticationFailed
		return
	}
	assert false, 'a message from another exchange was accepted'
}

fn test_a_rewritten_channel_binding_is_refused() {
	mut server := server_for(.sha256, ChannelBinding{})
	mut client := new_client(username: 'user', password: 'pencil')!
	server_first := server.first(client.first()!)!
	final := client.final(server_first)!
	// `biws` is base64 of `n,,`; swap it for the base64 of a header claiming an
	// authorization identity the client never asked for.
	forged := final.replace('c=biws,', 'c=${base64.encode('n,a=root,'.bytes())},')
	server.final(forged) or {
		assert err.msg().contains('channel binding data does not match')
		assert err is AuthenticationFailed
		return
	}
	assert false, 'a rewritten GS2 header was accepted'
}

fn test_a_stripped_channel_binding_is_detected_as_a_downgrade() {
	// The server offers -PLUS, so a client that claims it did not is either
	// broken or being downgraded by something in the middle.
	binding := ChannelBinding{
		mode: .required
		name: 'tls-server-end-point'
		data: 'binding data'.bytes()
	}
	mut server := server_for(.sha256, binding)
	mut client := new_client(
		username:        'user'
		password:        'pencil'
		channel_binding: ChannelBinding{
			mode: .unsupported_by_server
		}
	)!
	server.first(client.first()!) or {
		assert err.msg().contains('requires the `tls-server-end-point` channel binding')
		assert err is AuthenticationFailed
		return
	}
	assert false, 'a downgraded exchange was accepted'
}

fn test_a_channel_binding_the_server_does_not_offer_is_refused() {
	mut server := server_for(.sha256, ChannelBinding{})
	mut client := new_client(
		username:        'user'
		password:        'pencil'
		channel_binding: ChannelBinding{
			mode: .required
			name: 'tls-exporter'
			data: 'binding data'.bytes()
		}
	)!
	server.first(client.first()!) or {
		assert err.msg().contains('`tls-exporter` channel binding, which this server does not offer')
		assert err is MalformedMessage
		return
	}
	assert false, 'accepted a channel binding the server does not offer'
}

fn test_a_different_binding_type_is_refused() {
	mut server := server_for(.sha256, ChannelBinding{
		mode: .required
		name: 'tls-server-end-point'
		data: 'binding data'.bytes()
	})
	mut client := new_client(
		username:        'user'
		password:        'pencil'
		channel_binding: ChannelBinding{
			mode: .required
			name: 'tls-exporter'
			data: 'binding data'.bytes()
		}
	)!
	server.first(client.first()!) or {
		assert err is AuthenticationFailed
		return
	}
	assert false, 'accepted a mismatched channel binding type'
}

fn test_different_binding_data_is_refused() {
	// Same type on both sides, different data: this is the case channel
	// binding exists for, a proxy terminating TLS in the middle.
	mut server := server_for(.sha256, ChannelBinding{
		mode: .required
		name: 'tls-server-end-point'
		data: 'the real certificate'.bytes()
	})
	mut client := new_client(
		username:        'user'
		password:        'pencil'
		channel_binding: ChannelBinding{
			mode: .required
			name: 'tls-server-end-point'
			data: 'the proxy certificate'.bytes()
		}
	)!
	server_first := server.first(client.first()!)!
	server.final(client.final(server_first)!) or {
		assert err.msg().contains('channel binding data does not match')
		assert err is AuthenticationFailed
		return
	}
	assert false, 'accepted an exchange bound to a different channel'
}

fn test_server_error_message_renders_a_refusal() {
	assert server_error_message('invalid-proof') == 'e=invalid-proof'
	assert server_error_message('unknown-user') == 'e=unknown-user'
	// A code that would break the grammar is replaced rather than emitted.
	assert server_error_message('') == 'e=other-error'
	assert server_error_message('has,comma') == 'e=other-error'
}

fn test_a_refusal_travels_to_the_client() {
	mut server := server_for(.sha256, ChannelBinding{})
	mut client := new_client(username: 'user', password: 'crayon')!
	server_first := server.first(client.first()!)!
	final := client.final(server_first)!
	refusal := server.final(final) or { server_error_message('invalid-proof') }
	client.verify(refusal) or {
		assert err is ServerError
		return
	}
	assert false, 'the client did not see the refusal'
}

fn test_a_rejected_client_cannot_try_another_proof() {
	// One Server value is one exchange. Letting a rejected client send a second
	// proof would turn a fixed nonce into an offline guessing oracle.
	mut server := server_for(.sha256, ChannelBinding{})
	mut client := new_client(username: 'user', password: 'crayon')!
	server_first := server.first(client.first()!)!
	server.final(client.final(server_first)!) or {
		assert !server.done()
		mut honest := new_client(username: 'user', password: 'pencil', nonce: 'x')!
		honest.first()!
		second_try := honest.final(server_first) or { '' }
		server.final(second_try) or {
			assert err.msg().contains('final() must be called once')
			return
		}
		assert false, 'the server accepted a second proof'
	}
	assert false, 'a wrong password was accepted'
}
