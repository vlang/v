// Tests for ES384 and ES512 (P-384 and P-521 curves), validating that
// the DER ↔ R||S conversion handles the wider integer widths correctly.
// Sign + verify roundtrip is the right invariant since ECDSA signatures
// are randomised.
module cose

import encoding.base64
import encoding.hex

// ecdsa-sig-02 (P-384) reference message and key.
const p384_x_b64u = 'kTJyP2KSsBBhnb4kjWmMF7WHVsY55xUPgb7k64rDcjatChoZ1nvjKmYmPh5STRKc'
const p384_y_b64u = 'mM0weMVU2DKsYDxDJkEP9hZiRZtB8fPfXbzINZj_fF7YQRynNWedHEyzAJOX2e8s'
const p384_d_b64u = 'ok3Nq97AXlpEusO7jIy1FZATlBP9PNReMU7DWbkLQ5dU90snHuuHVDjEPmtV0fTo'
const p384_sig02_message = 'D28444A1013822A104445033383454546869732069732074686520636F6E74656E742E58605F150ABD1C7D25B32065A14E05D6CB1F665D10769FF455EA9A2E0ADAB5DE63838DB257F0949C41E13330E110EBA7B912F34E1546FB1366A2568FAA91EC3E6C8D42F4A67A0EDF731D88C9AEAD52258B2E2C4740EF614F02E9D91E9B7B59622A3C'

// ecdsa-sig-03 (P-521) — note: the file title says "P-512" but the
// IANA name is P-521 (521-bit curve, hence 66-byte coordinates).
const p521_x_b64u = 'AHKZLLOsCOzz5cY97ewNUajB957y-C-U88c3v13nmGZx6sYl_oJXu9A5RkTKqjqvjyekWF-7ytDyRXYgCF5cj0Kt'
const p521_y_b64u = 'AdymlHvOiLxXkEhayXQnNCvDX4h9htZaCJN34kfmC6pV5OhQHiraVySsUdaQkAgDPrwQrJmbnX9cwlGfP-HqHZR1'
const p521_d_b64u = 'AAhRON2r9cqXX1hg-RoI6R1tX5p2rUAYdmpHZoC1XNM56KtscrX6zbKipQrCW9CGZH3T4ubpnoTKLDYJ_fF3_rJt'
const p521_sig03_message = 'D28444A1013823A104581E62696C626F2E62616767696E7340686F626269746F6E2E6578616D706C6554546869732069732074686520636F6E74656E742E588401664DD6962091B5100D6E1833D503539330EC2BC8FD3E8996950CE9F70259D9A30F73794F603B0D3E7C5E9C4C2A57E10211F76E79DF8FFD1B79D7EF5B9FA7DA109001965FA2D37E093BB13C040399C467B3B9908C09DB2B0F1F4996FE07BB02AAA121A8E1C671F3F997ADE7D651081017057BD3A8A5FBF394972EA71CFDC15E6F8FE2E1'

const sample_text = 'This is the content.'

fn test_verify1_accepts_p384_reference_vector() {
	x := base64.url_decode(p384_x_b64u)
	y := base64.url_decode(p384_y_b64u)
	pub_key := Key.ec2_public(.p_384, x, y)
	msg := hex.decode(p384_sig02_message)!
	payload := verify1(msg, pub_key)!
	assert payload == sample_text.bytes()
}

fn test_sign1_p384_roundtrip() {
	x := base64.url_decode(p384_x_b64u)
	y := base64.url_decode(p384_y_b64u)
	d := base64.url_decode(p384_d_b64u)
	priv_key := Key.ec2_private(.p_384, x, y, d)
	pub_key := Key.ec2_public(.p_384, x, y)
	mut hp := Headers{}
	hp.algorithm = .es384
	signed := sign1('p384'.bytes(), priv_key, protected: hp)!
	got := verify1(signed, pub_key)!
	assert got == 'p384'.bytes()
}

fn test_verify1_accepts_p521_reference_vector() {
	x := base64.url_decode(p521_x_b64u)
	y := base64.url_decode(p521_y_b64u)
	pub_key := Key.ec2_public(.p_521, x, y)
	msg := hex.decode(p521_sig03_message)!
	payload := verify1(msg, pub_key)!
	assert payload == sample_text.bytes()
}

fn test_sign1_p521_roundtrip() {
	x := base64.url_decode(p521_x_b64u)
	y := base64.url_decode(p521_y_b64u)
	d := base64.url_decode(p521_d_b64u)
	priv_key := Key.ec2_private(.p_521, x, y, d)
	pub_key := Key.ec2_public(.p_521, x, y)
	mut hp := Headers{}
	hp.algorithm = .es512
	signed := sign1('p521'.bytes(), priv_key, protected: hp)!
	got := verify1(signed, pub_key)!
	assert got == 'p521'.bytes()
}
