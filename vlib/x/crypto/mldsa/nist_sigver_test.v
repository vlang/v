module mldsa

// NIST ACVP sigver test vectors (FIPS 204).
// groups: 1,3,5 external pure; 2,4,6 preHash; 7,9,11 internal mu; 8,10,12 internal msg.
import encoding.hex
import json
import os
import crypto.sha3

struct SigVerTest {
	tc_id    int    @[json: 'tcId']
	pk       string
	msg      string @[json: 'message']
	mu       string
	context  string
	hash_alg string @[json: 'hashAlg']
	signature string
}

struct SigVerGroup {
	tg_id               int    @[json: 'tgId']
	parameter_set       string @[json: 'parameterSet']
	signature_interface string @[json: 'signatureInterface']
	pre_hash            string @[json: 'preHash']
	tests               []SigVerTest
}

struct SigVerPrompt {
	test_groups []SigVerGroup @[json: 'testGroups']
}

struct SigVerResult {
	tc_id       int  @[json: 'tcId']
	test_passed bool @[json: 'testPassed']
}

struct SigVerResultGroup {
	tg_id int @[json: 'tgId']
	tests []SigVerResult
}

struct SigVerExpected {
	test_groups []SigVerResultGroup @[json: 'testGroups']
}


fn load_sigver_vectors() !(SigVerPrompt, map[int]bool) {
	dir := os.dir(@FILE)
	prompt_raw := os.read_file(os.join_path(dir, 'testdata', 'sigver_prompt.json'))!
	expected_raw := os.read_file(os.join_path(dir, 'testdata', 'sigver_expected.json'))!
	prompt := json.decode(SigVerPrompt, prompt_raw)!
	expected := json.decode(SigVerExpected, expected_raw)!

	mut results := map[int]bool{}
	for g in expected.test_groups {
		for t in g.tests {
			results[t.tc_id] = t.test_passed
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

// run_sigver_groups runs signature verification tests for groups matching the filter.
// The verify_fn callback receives (pub_key, test, group) and returns the verification result.
fn run_sigver_groups(prompt SigVerPrompt, results map[int]bool, filter fn (SigVerGroup) bool, verify_fn fn (&PublicKey, SigVerTest, SigVerGroup) !bool, label string) {
	mut total := 0

	for g in prompt.test_groups {
		if !filter(g) {
			continue
		}
		kind := nist_kind(g.parameter_set) or {
			panic('unknown parameter set: ${g.parameter_set}')
		}
		for t in g.tests {
			pk_bytes := hex.decode(t.pk) or { panic('tcId ${t.tc_id}: bad pk hex: ${err}') }
			sig_bytes := hex.decode(t.signature) or {
				panic('tcId ${t.tc_id}: bad signature hex: ${err}')
			}
			pub_key := PublicKey.from_bytes(pk_bytes, kind) or {
				panic('tcId ${t.tc_id}: PublicKey.from_bytes failed: ${err}')
			}

			got := verify_fn(&pub_key, t, g) or { false }
			want := results[t.tc_id]
			total++
			assert got == want, 'NIST ACVP ${g.parameter_set} sigVer tcId ${t.tc_id}: got ${got}, want ${want}'
		}
	}

	assert total > 0, 'no ${label} sigver tests were run'
}

fn test_nist_acvp_sigver_external_pure() {
	prompt, results := load_sigver_vectors() or { panic(err) }

	run_sigver_groups(prompt, results, fn (g SigVerGroup) bool {
		return g.signature_interface == 'external' && g.pre_hash == 'pure'
	}, fn (pub_key &PublicKey, t SigVerTest, g SigVerGroup) !bool {
		msg_bytes := hex.decode(t.msg)!
		ctx_bytes := hex.decode(t.context)!
		sig_bytes := hex.decode(t.signature)!
		return pub_key.verify(msg_bytes, sig_bytes, context: ctx_bytes.bytestr())
	}, 'external-pure')
}

fn test_nist_acvp_sigver_internal_mu() {
	prompt, results := load_sigver_vectors() or { panic(err) }

	run_sigver_groups(prompt, results, fn (g SigVerGroup) bool {
		return g.signature_interface == 'internal' && g.tests.len > 0 && g.tests[0].mu != ''
	}, fn (pub_key &PublicKey, t SigVerTest, g SigVerGroup) !bool {
		mu_bytes := hex.decode(t.mu)!
		sig_bytes := hex.decode(t.signature)!
		return pub_key.verify_mu(mu_bytes, sig_bytes)
	}, 'internal-mu')
}

fn test_nist_acvp_sigver_internal_msg() {
	prompt, results := load_sigver_vectors() or { panic(err) }

	run_sigver_groups(prompt, results, fn (g SigVerGroup) bool {
		return g.signature_interface == 'internal' && g.tests.len > 0 && g.tests[0].mu == ''
	}, fn (pub_key &PublicKey, t SigVerTest, g SigVerGroup) !bool {
		msg_bytes := hex.decode(t.msg)!
		sig_bytes := hex.decode(t.signature)!
		mut h_mu := sha3.new_shake256()
		h_mu.write(pub_key.tr[..])
		h_mu.write(msg_bytes)
		mu_bytes := h_mu.read(64)
		return pub_key.verify_mu(mu_bytes, sig_bytes)
	}, 'internal-msg')
}

fn test_nist_acvp_sigver_prehash() {
	prompt, results := load_sigver_vectors() or { panic(err) }

	run_sigver_groups(prompt, results, fn (g SigVerGroup) bool {
		return g.pre_hash == 'preHash'
	}, fn (pub_key &PublicKey, t SigVerTest, g SigVerGroup) !bool {
		msg_bytes := hex.decode(t.msg)!
		ctx_bytes := hex.decode(t.context)!
		sig_bytes := hex.decode(t.signature)!
		ph := nist_prehash(t.hash_alg)
		return pub_key.verify(msg_bytes, sig_bytes, context: ctx_bytes.bytestr(), prehash: ph)
	}, 'prehash')
}
