module mldsa

// NIST ACVP siggen test vectors (FIPS 204).
// groups: 1,3,5 deterministic external pure; 2,4,6 deterministic preHash;
// 7,9,11 deterministic internal mu; 8,10,12 deterministic internal msg;
// 13,15,17 non-deterministic external pure; 14,16,18 non-deterministic preHash;
// 19,21,23 non-deterministic internal mu; 20,22,24 non-deterministic internal msg.
import encoding.hex
import json
import os
import crypto.sha3

struct SigGenTest {
	tc_id    int    @[json: 'tcId']
	msg      string @[json: 'message']
	mu       string
	sk       string
	rnd      string
	context  string
	hash_alg string @[json: 'hashAlg']
}

struct SigGenGroup {
	tg_id               int    @[json: 'tgId']
	parameter_set       string @[json: 'parameterSet']
	deterministic       bool
	signature_interface string @[json: 'signatureInterface']
	pre_hash            string @[json: 'preHash']
	tests               []SigGenTest
}

struct SigGenPrompt {
	test_groups []SigGenGroup @[json: 'testGroups']
}

struct SigGenResult {
	tc_id     int @[json: 'tcId']
	signature string
}

struct SigGenResultGroup {
	tg_id int @[json: 'tgId']
	tests []SigGenResult
}

struct SigGenExpected {
	test_groups []SigGenResultGroup @[json: 'testGroups']
}

fn load_siggen_vectors() !(SigGenPrompt, map[int]string) {
	dir := os.dir(@FILE)
	prompt_raw := os.read_file(os.join_path(dir, 'testdata', 'siggen_prompt.json'))!
	expected_raw := os.read_file(os.join_path(dir, 'testdata', 'siggen_expected.json'))!
	prompt := json.decode(SigGenPrompt, prompt_raw)!
	expected := json.decode(SigGenExpected, expected_raw)!

	mut results := map[int]string{}
	for g in expected.test_groups {
		for t in g.tests {
			results[t.tc_id] = t.signature.to_lower()
		}
	}
	return prompt, results
}

fn nist_kind(param_set string) !Kind {
	return match param_set {
		'ML-DSA-44' { .ml_dsa_44 }
		'ML-DSA-65' { .ml_dsa_65 }
		'ML-DSA-87' { .ml_dsa_87 }
		else { error('unknown parameter set: ${param_set}') }
	}
}

fn nist_prehash(hash_alg string) PreHash {
	return match hash_alg {
		'SHA2-224' { .sha2_224 }
		'SHA2-256' { .sha2_256 }
		'SHA2-384' { .sha2_384 }
		'SHA2-512' { .sha2_512 }
		'SHA2-512/224' { .sha2_512_224 }
		'SHA2-512/256' { .sha2_512_256 }
		'SHA3-224' { .sha3_224 }
		'SHA3-256' { .sha3_256 }
		'SHA3-384' { .sha3_384 }
		'SHA3-512' { .sha3_512 }
		'SHAKE-128' { .shake_128 }
		'SHAKE-256' { .shake_256 }
		else { panic('unknown hash algorithm: ${hash_alg}') }
	}
}

fn run_siggen_groups(prompt SigGenPrompt, results map[int]string, filter fn (SigGenGroup) bool, sign_fn fn (&PrivateKey, SigGenTest, SigGenGroup) ![]u8, label string) {
	mut total := 0

	for g in prompt.test_groups {
		if !filter(g) {
			continue
		}
		kind := nist_kind(g.parameter_set) or { panic('unknown parameter set: ${g.parameter_set}') }
		for t in g.tests {
			sk_bytes := hex.decode(t.sk) or { panic('tcId ${t.tc_id}: bad sk hex: ${err}') }
			sk := PrivateKey.from_bytes(sk_bytes, kind) or {
				panic('tcId ${t.tc_id}: PrivateKey.from_bytes failed: ${err}')
			}

			sig := sign_fn(&sk, t, g) or { panic('tcId ${t.tc_id}: sign failed: ${err}') }
			got := hex.encode(sig)
			want := results[t.tc_id]
			total++
			assert got == want, 'NIST ACVP ${g.parameter_set} sigGen tcId ${t.tc_id}: signature mismatch'
		}
	}

	assert total > 0, 'no ${label} siggen tests were run'
}

fn test_nist_acvp_siggen_deterministic_external_pure() {
	prompt, results := load_siggen_vectors() or { panic(err) }

	run_siggen_groups(prompt, results, fn (g SigGenGroup) bool {
		return g.deterministic && g.signature_interface == 'external' && g.pre_hash == 'pure'
	}, fn (sk &PrivateKey, t SigGenTest, g SigGenGroup) ![]u8 {
		msg_bytes := hex.decode(t.msg)!
		ctx_bytes := hex.decode(t.context)!
		mu := compute_mu(sk.pk.tr[..], msg_bytes, ctx_bytes.bytestr())
		return sign_internal(sk, mu, [32]u8{})
	}, 'deterministic-external-pure')
}

fn test_nist_acvp_siggen_deterministic_internal_mu() {
	prompt, results := load_siggen_vectors() or { panic(err) }

	run_siggen_groups(prompt, results, fn (g SigGenGroup) bool {
		return g.deterministic && g.signature_interface == 'internal' && g.tests.len > 0
			&& g.tests[0].mu != ''
	}, fn (sk &PrivateKey, t SigGenTest, g SigGenGroup) ![]u8 {
		mu := slice_to_64(hex.decode(t.mu)!)
		return sign_internal(sk, mu, [32]u8{})
	}, 'deterministic-internal-mu')
}

fn test_nist_acvp_siggen_deterministic_internal_msg() {
	prompt, results := load_siggen_vectors() or { panic(err) }

	run_siggen_groups(prompt, results, fn (g SigGenGroup) bool {
		return g.deterministic && g.signature_interface == 'internal' && g.tests.len > 0
			&& g.tests[0].mu == ''
	}, fn (sk &PrivateKey, t SigGenTest, g SigGenGroup) ![]u8 {
		msg_bytes := hex.decode(t.msg)!
		mut h_mu := sha3.new_shake256()
		h_mu.write(sk.pk.tr[..])
		h_mu.write(msg_bytes)
		mu := slice_to_64(h_mu.read(64))
		return sign_internal(sk, mu, [32]u8{})
	}, 'deterministic-internal-msg')
}

fn test_nist_acvp_siggen_nondeterministic_external_pure() {
	prompt, results := load_siggen_vectors() or { panic(err) }

	run_siggen_groups(prompt, results, fn (g SigGenGroup) bool {
		return !g.deterministic && g.signature_interface == 'external' && g.pre_hash == 'pure'
	}, fn (sk &PrivateKey, t SigGenTest, g SigGenGroup) ![]u8 {
		msg_bytes := hex.decode(t.msg)!
		ctx_bytes := hex.decode(t.context)!
		rnd := slice_to_32(hex.decode(t.rnd)!)
		mu := compute_mu(sk.pk.tr[..], msg_bytes, ctx_bytes.bytestr())
		return sign_internal(sk, mu, rnd)
	}, 'nondeterministic-external-pure')
}

fn test_nist_acvp_siggen_nondeterministic_internal_mu() {
	prompt, results := load_siggen_vectors() or { panic(err) }

	run_siggen_groups(prompt, results, fn (g SigGenGroup) bool {
		return !g.deterministic && g.signature_interface == 'internal' && g.tests.len > 0
			&& g.tests[0].mu != ''
	}, fn (sk &PrivateKey, t SigGenTest, g SigGenGroup) ![]u8 {
		mu := slice_to_64(hex.decode(t.mu)!)
		rnd := slice_to_32(hex.decode(t.rnd)!)
		return sign_internal(sk, mu, rnd)
	}, 'nondeterministic-internal-mu')
}

fn test_nist_acvp_siggen_nondeterministic_internal_msg() {
	prompt, results := load_siggen_vectors() or { panic(err) }

	run_siggen_groups(prompt, results, fn (g SigGenGroup) bool {
		return !g.deterministic && g.signature_interface == 'internal' && g.tests.len > 0
			&& g.tests[0].mu == ''
	}, fn (sk &PrivateKey, t SigGenTest, g SigGenGroup) ![]u8 {
		msg_bytes := hex.decode(t.msg)!
		rnd := slice_to_32(hex.decode(t.rnd)!)
		mut h_mu := sha3.new_shake256()
		h_mu.write(sk.pk.tr[..])
		h_mu.write(msg_bytes)
		mu := slice_to_64(h_mu.read(64))
		return sign_internal(sk, mu, rnd)
	}, 'nondeterministic-internal-msg')
}

fn test_nist_acvp_siggen_deterministic_prehash() {
	prompt, results := load_siggen_vectors() or { panic(err) }

	run_siggen_groups(prompt, results, fn (g SigGenGroup) bool {
		return g.deterministic && g.pre_hash == 'preHash'
	}, fn (sk &PrivateKey, t SigGenTest, g SigGenGroup) ![]u8 {
		msg_bytes := hex.decode(t.msg)!
		ctx_bytes := hex.decode(t.context)!
		ph := nist_prehash(t.hash_alg)
		mu := compute_mu_prehash(sk.pk.tr[..], msg_bytes, ctx_bytes.bytestr(), ph)
		return sign_internal(sk, mu, [32]u8{})
	}, 'deterministic-prehash')
}

fn test_nist_acvp_siggen_nondeterministic_prehash() {
	prompt, results := load_siggen_vectors() or { panic(err) }

	run_siggen_groups(prompt, results, fn (g SigGenGroup) bool {
		return !g.deterministic && g.pre_hash == 'preHash'
	}, fn (sk &PrivateKey, t SigGenTest, g SigGenGroup) ![]u8 {
		msg_bytes := hex.decode(t.msg)!
		ctx_bytes := hex.decode(t.context)!
		rnd := slice_to_32(hex.decode(t.rnd)!)
		ph := nist_prehash(t.hash_alg)
		mu := compute_mu_prehash(sk.pk.tr[..], msg_bytes, ctx_bytes.bytestr(), ph)
		return sign_internal(sk, mu, rnd)
	}, 'nondeterministic-prehash')
}
