import encoding.hex
import x.crypto.chacha20poly1305

// test public function based encrypt and decrypt of chacha20poly1305
fn test_aead_chacha20poly1305_fn_encrypt_and_decrypt_operation() ! {
	// the data mostly based on rfc
	// gives yours secure key
	key := hex.decode('1c9240a5eb55d38af333888604f6b5f0473917c1402b80099dca5cbc207075c0')!
	// gives your additional data
	aad := hex.decode('f33388860000000000004e91')!
	// gives your nonce
	nonce := hex.decode('000000000102030405060708')!

	// your message to be encrypted and authenticated
	plaintext := 'Internet-Drafts are draft documents valid for a maximum of six months and may be updated, replaced, or obsoleted by other documents at any time. It is inappropriate to use Internet-Drafts as reference material or to cite them other than as /“work in progress./”'.bytes()

	ciphertext_out := chacha20poly1305.encrypt(plaintext, key, nonce, aad)!
	ciphertext := hex.decode('64a0861575861af460f062c79be643bd5e805cfd345cf389f108670ac76c8cb24c6cfc18755d43eea09ee94e382d26b0bdb7b73c321b0100d4f03b7f355894cf332f830e710b97ce98c8a84abd0b948114ad176e008d33bd60f982b1ff37c8559797a06ef4f0ef61c186324e2b3506383606907b6a7c02b0f9f6157b53c867e4b9166c767b804d46a59b5216cde7a4e99040c5a40433225ee282a1b0a06c523eaf4534d7f83fa1155b0047718cbc546a0d072b04b3564eea1b422273f548271a0bb2316053fa76991955ebd63159434ecebb4e466dae5a1073a6727627097a1049e617d91d361094fa68f0ff77987130305beaba2eda04df997b714d6c6f2c29a6ad5cb4022b02709b')!
	// Received Tag
	expected_tag := hex.decode('eead9d67890cbb22392336fea1851f38')!
	// aead output is concatenation of ciphertext and the tag
	mut encrypted_output := []u8{}
	encrypted_output << ciphertext
	encrypted_output << expected_tag

	assert ciphertext_out == encrypted_output
	assert ciphertext_out[ciphertext_out.len - 16..] == expected_tag
	// Lets decrypt back
	message := chacha20poly1305.decrypt(ciphertext_out, key, nonce, aad)!

	assert message == plaintext
}

// we going to use same data with the first test, but testing for instance based encrypt
fn test_aead_chacha20poly1305_instance_encrypt_and_decrypt_operation() ! {
	// gives yours secure key
	key := hex.decode('1c9240a5eb55d38af333888604f6b5f0473917c1402b80099dca5cbc207075c0')!
	// gives your additional data
	aad := hex.decode('f33388860000000000004e91')!
	// gives your nonce
	nonce := hex.decode('000000000102030405060708')!
	// your message to be encrypted and authenticated
	plaintext := hex.decode('496e7465726e65742d4472616674732061726520647261667420646f63756d656e74732076616c696420666f722061206d6178696d756d206f6620736978206d6f6e74687320616e64206d617920626520757064617465642c207265706c616365642c206f72206f62736f6c65746564206279206f7468657220646f63756d656e747320617420616e792074696d652e20497420697320696e617070726f70726961746520746f2075736520496e7465726e65742d447261667473206173207265666572656e6365206d6174657269616c206f7220746f2063697465207468656d206f74686572207468616e206173202fe2809c776f726b20696e2070726f67726573732e2fe2809d')!

	// lets create chacha20poly1305 AEAD
	mut co := chacha20poly1305.new(key, nonce.len)!
	ciphertext_out := co.encrypt(plaintext, nonce, aad)!

	// we have expected ciphertext
	ciphertext := hex.decode('64a0861575861af460f062c79be643bd5e805cfd345cf389f108670ac76c8cb24c6cfc18755d43eea09ee94e382d26b0bdb7b73c321b0100d4f03b7f355894cf332f830e710b97ce98c8a84abd0b948114ad176e008d33bd60f982b1ff37c8559797a06ef4f0ef61c186324e2b3506383606907b6a7c02b0f9f6157b53c867e4b9166c767b804d46a59b5216cde7a4e99040c5a40433225ee282a1b0a06c523eaf4534d7f83fa1155b0047718cbc546a0d072b04b3564eea1b422273f548271a0bb2316053fa76991955ebd63159434ecebb4e466dae5a1073a6727627097a1049e617d91d361094fa68f0ff77987130305beaba2eda04df997b714d6c6f2c29a6ad5cb4022b02709b')!
	// Received Tag
	expected_tag := hex.decode('eead9d67890cbb22392336fea1851f38')!
	// aead output is concatenation of ciphertext and the tag
	mut encrypted_output := []u8{}
	encrypted_output << ciphertext
	encrypted_output << expected_tag

	assert ciphertext_out == encrypted_output
	assert ciphertext_out[ciphertext_out.len - 16..] == expected_tag

	// Lets decrypt back
	message := co.decrypt(ciphertext_out, nonce, aad)!
	assert message == plaintext
}
