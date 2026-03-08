module mldsa

// NIST ACVP keygen test vectors (FIPS 204).
import encoding.hex
import json
import os

struct KeyGenTest {
	tc_id int @[json: 'tcId']
	seed  string
}

struct KeyGenGroup {
	tg_id         int    @[json: 'tgId']
	parameter_set string @[json: 'parameterSet']
	tests         []KeyGenTest
}

struct KeyGenPrompt {
	test_groups []KeyGenGroup @[json: 'testGroups']
}

struct KeyGenResult {
	tc_id int @[json: 'tcId']
	pk    string
	sk    string
}

struct KeyGenResultGroup {
	tg_id int @[json: 'tgId']
	tests []KeyGenResult
}

struct KeyGenExpected {
	test_groups []KeyGenResultGroup @[json: 'testGroups']
}

fn load_keygen_vectors() !(KeyGenPrompt, KeyGenExpected) {
	dir := os.dir(@FILE)
	prompt_raw := os.read_file(os.join_path(dir, 'testdata', 'keygen_prompt.json'))!
	expected_raw := os.read_file(os.join_path(dir, 'testdata', 'keygen_expected.json'))!
	prompt := json.decode(KeyGenPrompt, prompt_raw)!
	expected := json.decode(KeyGenExpected, expected_raw)!
	return prompt, expected
}

fn nist_kind(param_set string) !Kind {
	return match param_set {
		'ML-DSA-44' { .ml_dsa_44 }
		'ML-DSA-65' { .ml_dsa_65 }
		'ML-DSA-87' { .ml_dsa_87 }
		else { error('unknown parameter set: ${param_set}') }
	}
}

fn test_nist_acvp_keygen() {
	prompt, expected := load_keygen_vectors() or { panic(err) }

	// tcId -> KeyGenResult
	mut results := map[int]KeyGenResult{}
	for g in expected.test_groups {
		for t in g.tests {
			results[t.tc_id] = t
		}
	}

	mut total := 0

	for g_prompt in prompt.test_groups {
		kind := nist_kind(g_prompt.parameter_set) or {
			panic('unknown parameter set: ${g_prompt.parameter_set}')
		}
		for t in g_prompt.tests {
			seed_bytes := hex.decode(t.seed) or { panic('tcId ${t.tc_id}: bad seed hex: ${err}') }

			priv_key := PrivateKey.from_seed(seed_bytes, kind) or {
				panic('tcId ${t.tc_id}: key generation failed: ${err}')
			}

			got_pk := priv_key.public_key().bytes()
			got_sk := priv_key.bytes()

			want_result := results[t.tc_id]
			want_pk := hex.decode(want_result.pk) or {
				panic('tcId ${t.tc_id}: bad expected pk hex: ${err}')
			}
			want_sk := hex.decode(want_result.sk) or {
				panic('tcId ${t.tc_id}: bad expected sk hex: ${err}')
			}

			assert got_pk == want_pk, 'NIST ACVP ${g_prompt.parameter_set} keyGen tcId ${t.tc_id}: public key mismatch'
			assert got_sk == want_sk, 'NIST ACVP ${g_prompt.parameter_set} keyGen tcId ${t.tc_id}: secret key mismatch'

			total++
		}
	}

	assert total > 0, 'no keygen tests were run'
}
