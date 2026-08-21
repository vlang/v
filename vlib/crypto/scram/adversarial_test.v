// What a hostile or misconfigured peer gets to choose. The conformance
// vectors check that two honest peers agree; these check that a peer which
// does not play along cannot obtain a protection it has not earned, and
// cannot make a configuration mistake look like a wrong password.
module scram

// bound_binding is a complete channel binding, as a caller with access to its
// TLS layer would build one.
fn bound_binding() ChannelBinding {
	return ChannelBinding{
		mode: .required
		name: 'tls-server-end-point'
		data: 'a hash of the server certificate'.bytes()
	}
}

// server_with builds a server that answers every user with `credentials`.
fn server_with(credentials Credentials) !&Server {
	return new_server(
		lookup: fn [credentials] (username string) !Credentials {
			return credentials
		}
	)
}

fn test_channel_binding_without_data_is_refused_rather_than_binding_nothing() {
	// `p=` announces a -PLUS mechanism, so an exchange that reaches the wire
	// without binding data leaves both peers believing the connection is tied
	// to their TLS channel when nothing ties it.
	starved := ChannelBinding{
		mode: .required
		name: 'tls-exporter'
	}
	new_client(username: 'user', password: 'pencil', channel_binding: starved) or {
		assert err.msg().contains('channel binding data is required')
		new_server(
			channel_binding: starved
			lookup:          fn (username string) !Credentials {
				return new_credentials(.sha256, 'pencil')!
			}
		) or {
			assert err.msg().contains('channel binding data is required')
			return
		}
		assert false, 'the server accepted a .required binding with no data'
	}
	assert false, 'the client accepted a .required binding with no data'
}

fn test_a_complete_channel_binding_still_works() {
	// The check above must not have made channel binding unusable.
	mut client :=
		new_client(username: 'user', password: 'pencil', channel_binding: bound_binding())!
	assert client.mechanism_name() == 'SCRAM-SHA-256-PLUS'
	assert client.first()!.starts_with('p=tls-server-end-point,,')
}

fn test_credentials_of_the_wrong_length_are_a_configuration_error() {
	// Truncated keys fail the proof check on their own, but as an
	// `AuthenticationFailed`, which reads as a user typing the wrong password.
	truncated := Credentials{
		mechanism:  .sha256
		salt:       'saltsaltsaltsalt'.bytes()
		iterations: 4096
		stored_key: [u8(1), 2, 3]
		server_key: [u8(4), 5, 6]
	}
	mut server := server_with(truncated)!
	mut client := new_client(username: 'user', password: 'pencil')!
	server.first(client.first()!) or {
		assert err !is AuthenticationFailed
		assert err.msg().contains('3 and 3 byte keys')
		assert err.msg().contains('needs 32')
		return
	}
	assert false, 'the server accepted credentials with 3 byte keys'
}

fn test_an_empty_authzid_in_the_gs2_header_is_malformed() {
	// `a=` with nothing after it is not an absent authzid: RFC 5802 §7 spells
	// authzid as `a=` saslname, and saslname is `1*`.
	mut server := server_with(new_credentials(.sha256, 'pencil')!)!
	server.first('n,a=,n=user,r=clientnonce') or {
		assert err is MalformedMessage
		assert err.msg().contains('authorization identity')
		return
	}
	assert false, 'the server accepted an empty `a=` authorization identity'
}

fn test_an_attribute_injected_into_the_client_first_message_breaks_the_proof() {
	// Nothing rejects a duplicate `r=` outright, because the whole message
	// goes into the auth message on both sides. That is what has to hold: an
	// injected attribute must make the exchange fail, not pass unnoticed.
	credentials := derive_credentials(.sha256, 'pencil', 'saltsaltsaltsalt'.bytes(), 4096)!
	mut server := server_with(credentials)!
	mut client := new_client(username: 'user', password: 'pencil')!
	client_first := client.first()!
	server_first := server.first('${client_first},r=injected')!
	server.final(client.final(server_first)!) or {
		assert err is AuthenticationFailed
		return
	}
	assert false, 'an attribute injected into the client-first-message went unnoticed'
}

fn test_an_extension_injected_into_the_server_first_message_breaks_the_proof() {
	// Unknown extensions are ignored, as RFC 5802 §7 requires, but they are
	// still covered by the auth message, so rewriting one is detected.
	credentials := derive_credentials(.sha256, 'pencil', 'saltsaltsaltsalt'.bytes(), 4096)!
	mut server := server_with(credentials)!
	mut client := new_client(username: 'user', password: 'pencil')!
	server_first := server.first(client.first()!)!
	server.final(client.final('${server_first},x=injected')!) or {
		assert err is AuthenticationFailed
		return
	}
	assert false, 'an extension injected into the server-first-message went unnoticed'
}

fn test_a_proof_of_the_wrong_length_is_refused_before_it_is_used() {
	credentials := derive_credentials(.sha256, 'pencil', 'saltsaltsaltsalt'.bytes(), 4096)!
	mut server := server_with(credentials)!
	mut client := new_client(username: 'user', password: 'pencil')!
	server_first := server.first(client.first()!)!
	client_final := client.final(server_first)!
	without_proof := client_final.all_before(',p=')
	server.final('${without_proof},p=AAAA') or {
		assert err is AuthenticationFailed
		assert err.msg().contains('3 bytes, expected 32')
		return
	}
	assert false, 'the server accepted a 3 byte proof'
}

fn test_an_absurd_iteration_count_is_refused_without_doing_the_work() {
	// The grammar allows nine digits, and `hi()` would run every one of them
	// before the server has proved anything at all.
	mut client := new_client(username: 'user', password: 'pencil', nonce: 'clientnonce')!
	_ := client.first()!
	client.final('r=clientnonceserver,s=c2FsdHNhbHRzYWx0c2FsdA==,i=999999999') or {
		assert err is AuthenticationFailed
		assert err.msg().contains('above the configured maximum of ${default_max_iterations}')
		return
	}
	assert false, 'the client accepted 999999999 iterations'
}

fn test_the_iteration_count_ceiling_is_configurable() {
	mut strict := new_client(
		username:       'user'
		password:       'pencil'
		nonce:          'clientnonce'
		max_iterations: 8192
	)!
	_ := strict.first()!
	strict.final('r=clientnonceserver,s=c2FsdHNhbHRzYWx0c2FsdA==,i=32768') or {
		assert err is AuthenticationFailed
		assert err.msg().contains('above the configured maximum of 8192')
		return
	}
	assert false, 'the client accepted 32768 iterations against a ceiling of 8192'
}

fn test_a_ceiling_below_the_floor_is_refused_at_construction() {
	// 1024 is below the 4096 floor, so no count could ever be accepted: a
	// client configured that way would fail every exchange for a reason that
	// looks like the server's fault.
	new_client(username: 'user', password: 'pencil', max_iterations: 1024) or {
		assert err.msg().contains('must not be below min_iterations')
		return
	}
	assert false, 'accepted a ceiling below the floor'
}

fn test_a_count_between_the_floor_and_the_ceiling_is_still_accepted() {
	credentials := derive_credentials(.sha256, 'pencil', 'saltsaltsaltsalt'.bytes(),
		default_iterations)!
	mut server := server_with(credentials)!
	mut client := new_client(username: 'user', password: 'pencil')!
	server_final := server.final(client.final(server.first(client.first()!)!)!)!
	client.verify(server_final)!
	assert client.done() && server.done()
}

// plus_advertising_server offers both the base and the `-PLUS` mechanism, and
// this exchange runs the base one — the case where a stripped advertisement
// is invisible to the mode alone.
fn plus_advertising_server() !&Server {
	credentials := derive_credentials(.sha256, 'pencil', 'saltsaltsaltsalt'.bytes(), 4096)!
	return new_server(
		advertises_plus: true
		lookup:          fn [credentials] (username string) !Credentials {
			return credentials
		}
	)
}

fn test_a_y_flag_is_a_downgrade_when_the_server_advertises_plus() {
	// The client says it supports channel binding and saw no `-PLUS` in the
	// list. This server put one there, so something rewrote the list.
	mut server := plus_advertising_server()!
	mut client := new_client(
		username:        'user'
		password:        'pencil'
		channel_binding: ChannelBinding{
			mode: .unsupported_by_server
		}
	)!
	server.first(client.first()!) or {
		assert err is AuthenticationFailed
		assert err.msg().contains('altered in transit')
		return
	}
	assert false, 'the server accepted a `y` flag while advertising a -PLUS mechanism'
}

fn test_an_n_flag_stays_valid_when_the_server_advertises_plus() {
	// A client that cannot do channel binding at all is not being downgraded,
	// so the same server must still authenticate it.
	mut server := plus_advertising_server()!
	mut client := new_client(username: 'user', password: 'pencil')!
	server_final := server.final(client.final(server.first(client.first()!)!)!)!
	client.verify(server_final)!
	assert client.done() && server.done()
}

fn test_a_y_flag_is_legitimate_when_the_server_advertises_no_plus() {
	// Unchanged behaviour for a server that really offers nothing better.
	credentials := derive_credentials(.sha256, 'pencil', 'saltsaltsaltsalt'.bytes(), 4096)!
	mut server := server_with(credentials)!
	mut client := new_client(
		username:        'user'
		password:        'pencil'
		channel_binding: ChannelBinding{
			mode: .unsupported_by_server
		}
	)!
	server_final := server.final(client.final(server.first(client.first()!)!)!)!
	client.verify(server_final)!
	assert client.done() && server.done()
}
