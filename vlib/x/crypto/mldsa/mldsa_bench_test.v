module mldsa

import term
import x.benchmark

fn bench_header(name string) {
	println('\n' + term.bold(name))
}

fn test_benchmark_keygen() {
	bench_header('KeyGen')
	seed := []u8{len: 32, init: u8(index)}
	for kind in [Kind.ml_dsa_44, .ml_dsa_65, .ml_dsa_87] {
		print(term.bold(term.green('  ${kind}  ')))
		mut bench := benchmark.setup(fn [seed, kind] () ! {
			_ = PrivateKey.from_seed(seed, kind)!
		})!
		bench.run()
	}
}

fn test_benchmark_sign() {
	bench_header('Sign')
	seed := []u8{len: 32, init: u8(index)}
	msg := []u8{len: 128}
	for kind in [Kind.ml_dsa_44, .ml_dsa_65, .ml_dsa_87] {
		sk := PrivateKey.from_seed(seed, kind) or { panic(err) }
		print(term.bold(term.yellow('  ${kind}  ')))
		mut bench := benchmark.setup(fn [sk, msg] () ! {
			_ = sk.sign(msg)!
		})!
		bench.run()
	}
}

fn test_benchmark_sign_deterministic() {
	bench_header('Sign (deterministic)')
	seed := []u8{len: 32, init: u8(index)}
	msg := []u8{len: 128}
	opts := SignerOpts{
		context:       'context'
		deterministic: true
	}
	for kind in [Kind.ml_dsa_44, .ml_dsa_65, .ml_dsa_87] {
		sk := PrivateKey.from_seed(seed, kind) or { panic(err) }
		print(term.bold(term.yellow('  ${kind}  ')))
		mut bench := benchmark.setup(fn [sk, msg, opts] () ! {
			_ = sk.sign(msg, opts)!
		})!
		bench.run()
	}
}

fn test_benchmark_verify() {
	bench_header('Verify')
	seed := []u8{len: 32, init: u8(index)}
	msg := []u8{len: 128}
	for kind in [Kind.ml_dsa_44, .ml_dsa_65, .ml_dsa_87] {
		sk := PrivateKey.from_seed(seed, kind) or { panic(err) }
		pk := sk.public_key()
		sig := sk.sign(msg) or { panic(err) }
		print(term.bold(term.magenta('  ${kind}  ')))
		mut bench := benchmark.setup(fn [pk, msg, sig] () ! {
			_ = pk.verify(msg, sig)!
		})!
		bench.run()
	}
}

fn test_benchmark_verify_with_pk_parsing() {
	bench_header('Verify (with public key parsing)')
	seed := []u8{len: 32, init: u8(index)}
	msg := []u8{len: 128}
	opts := SignerOpts{ context: 'context' }
	for kind in [Kind.ml_dsa_44, .ml_dsa_65, .ml_dsa_87] {
		sk := PrivateKey.from_seed(seed, kind) or { panic(err) }
		pk_bytes := sk.public_key().bytes()
		sig := sk.sign(msg, opts) or { panic(err) }
		print(term.bold(term.magenta('  ${kind}  ')))
		mut bench := benchmark.setup(fn [pk_bytes, msg, sig, opts, kind] () ! {
			pk := PublicKey.from_bytes(pk_bytes, kind)!
			_ = pk.verify(msg, sig, opts)!
		})!
		bench.run()
	}
}
