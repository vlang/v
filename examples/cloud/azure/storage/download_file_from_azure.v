// hardcoded-credentials Embedding credentials in source code risks unauthorized access
import os
import net.http
import time
import encoding.base64
import crypto.hmac
import crypto.sha256
import strings

// Generates a Base64-encoded signature for the given string to sign using the provided account key.
// Signature is a Hash-based Message Authentication Code (HMAC) constructed from the request and computed by using the SHA256 algorithm, and then encoded by using Base64 encoding.
// To encode the signature, call the HMAC-SHA256 algorithm on the UTF-8-encoded signature string and encode the result as Base64. Note that you also need to Base64-decode your storage account key. Use the following format (shown as pseudocode):
// Signature=Base64(HMAC-SHA256(UTF8(StringToSign), Base64.decode(<your_azure_storage_account_shared_key>)))
fn generate_signature_b64(account_key string, sb_string_to_sign string) ?string {
	decoded_key := base64.decode(account_key)

	signature := hmac.new(decoded_key, sb_string_to_sign.bytes(), sha256.sum, sha256.block_size).bytestr().bytes()

	signature_b64 := base64.encode(signature)

	return signature_b64
}

fn main() {
	// TODO
}
