// SCRAM example program: registers a user, authenticates it, then shows
// what happens when the password is wrong and when the server cannot
// prove itself.
//
// Run with:  v run examples/scram.v
import crypto.scram

// A stand-in for the table a real server would keep. SCRAM never stores
// the password, only the salt, the iteration count and two derived keys.
struct UserStore {
mut:
	records map[string]scram.Credentials
}

fn (mut s UserStore) register(username string, password string) ! {
	s.records[username] = scram.new_credentials(.sha256, password)!
}

fn main() {
	mut store := UserStore{}
	store.register('alice', 'correct horse battery staple')!
	println('registered alice: stored ${store.records['alice'].stored_key.len} byte keys, no password')

	demo_successful_login(store)!
	demo_wrong_password(store)!
	demo_impostor_server(store)!
}

// The nominal exchange. In a real program the two halves sit on either
// side of a socket and only the four strings travel.
fn demo_successful_login(store UserStore) ! {
	println('\n--- a successful login ---')
	mut server := new_server_for(store)!
	mut client := scram.new_client(username: 'alice', password: 'correct horse battery staple')!

	client_first := client.first()!
	println('C: ${client_first}')
	server_first := server.first(client_first)!
	println('S: ${server_first}')
	client_final := client.final(server_first)!
	println('C: ${client_final}')
	server_final := server.final(client_final)!
	println('S: ${server_final}')

	// Only now is the connection authenticated, in both directions.
	client.verify(server_final)!
	println('=> ${server.username()} is authenticated, and the server proved itself too')
}

// A wrong password produces a well formed exchange that fails on the
// proof. The server answers with an `e=` code rather than hanging up, so
// the client can tell a refusal from a broken connection.
fn demo_wrong_password(store UserStore) ! {
	println('\n--- a wrong password ---')
	mut server := new_server_for(store)!
	mut client := scram.new_client(username: 'alice', password: 'hunter2')!

	server_first := server.first(client.first()!)!
	refusal := server.final(client.final(server_first)!) or {
		println('server: ${err}')
		scram.server_error_message('invalid-proof')
	}
	client.verify(refusal) or {
		println('client: ${err}')
		if err is scram.ServerError {
			println('=> the server refused with the code `${err.code}`')
		}
		return
	}
	println('=> unreachable: a wrong password was accepted')
}

// The step everyone is tempted to skip. A server that does not hold the
// credentials can still answer the first three messages; it cannot sign
// the fourth. Dropping `verify` turns SCRAM into a one-way protocol and
// throws away its protection against a relay.
fn demo_impostor_server(store UserStore) ! {
	println('\n--- a server that cannot prove itself ---')
	mut server := new_server_for(store)!
	mut client := scram.new_client(username: 'alice', password: 'correct horse battery staple')!

	server_first := server.first(client.first()!)!
	server.final(client.final(server_first)!)!
	// Everything above succeeded; the impostor now has to sign the
	// transcript, and forges the signature instead.
	forged := 'v=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA='
	client.verify(forged) or {
		println('client: ${err}')
		println('=> the client refused the server, and never revealed the password')
		return
	}
	println('=> unreachable: a forged server signature was accepted')
}

// new_server_for wires a server to the user store. Returning an error
// for an unknown user is the simple choice made here; a public facing
// server should answer with credentials derived from a server-side
// secret instead, so that probing for user names tells an attacker
// nothing. See RFC 5802 section 7.
fn new_server_for(store UserStore) !&scram.Server {
	return scram.new_server(
		mechanism: .sha256
		lookup:    fn [store] (username string) !scram.Credentials {
			return store.records[username] or { error('unknown user: ${username}') }
		}
	)
}
