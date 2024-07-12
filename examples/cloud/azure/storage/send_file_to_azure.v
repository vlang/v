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
	signature := hmac.new(base64.decode(account_key), sb_string_to_sign.bytes(), sha256.sum,
		sha256.block_size).bytestr().bytes()

	signature_b64 := base64.encode(signature)

	return signature_b64
}

fn main() {
	args := os.args

	if args.len < 6 || args.len > 6 || args[1] == '-h' || args[1] == '--help' {
		eprintln('Usage: v run send_file_to_azure.v "<account_name>" "<account_key>" "<container_name>" "<blob_name>" "<file_path>"')
		return
	}

	account_name := args[1]
	account_key := args[2]
	container_name := args[3]
	blob_name := args[4]
	file_path := args[5]

	api_version := '2019-12-12'

	file_data := os.read_file(file_path) or {
		eprintln('Failed to read file: ${err}')
		return
	}

	if file_data.len == 0 {
		eprintln("File '${file_path}' is empty")
		return
	}

	// url := 'https://${account_name}.blob.core.windows.net/${container_name}/${blob_name}'
	mut url_builder := strings.new_builder(32)
	url_builder.write_string('https://')
	url_builder.write_string(account_name)
	url_builder.write_string('.blob.core.windows.net/')
	url_builder.write_string(container_name)
	url_builder << `/`
	url_builder.write_string(blob_name)
	url := url_builder.str()

	// "x-ms-date": "Fri, 12 Jul 2024 01:50:53 GMT"
	request_time_str := time.utc().http_header_string()

	mut sb_string_to_sign := strings.new_builder(56)
	sb_string_to_sign.writeln('PUT') // VERB + "\n"
	sb_string_to_sign.writeln('') // Content-Encoding + "\n"
	sb_string_to_sign.writeln('') // Content-Language + "\n"
	sb_string_to_sign.writeln(file_data.len.str()) // Content-Length + "\n"
	sb_string_to_sign.writeln('') // Content-MD5 + "\n" +
	sb_string_to_sign.writeln('application/octet-stream') // Content-Type + "\n"
	sb_string_to_sign.writeln('') // Date + "\n"
	sb_string_to_sign.writeln('') // If-Modified-Since + "\n"
	sb_string_to_sign.writeln('') // If-Match + "\n"
	sb_string_to_sign.writeln('') // If-None-Match + "\n"
	sb_string_to_sign.writeln('') // If-Unmodified-Since + "\n"
	sb_string_to_sign.writeln('') // Range + "\n"

	// CanonicalizedHeaders
	sb_string_to_sign.writeln('x-ms-blob-type:BlockBlob')
	sb_string_to_sign.writeln('x-ms-date:' + request_time_str)
	sb_string_to_sign.writeln('x-ms-version:' + api_version)

	// CanonicalizedResource
	sb_string_to_sign << `/`
	sb_string_to_sign.write_string(account_name)
	sb_string_to_sign << `/`
	sb_string_to_sign.write_string(container_name)
	sb_string_to_sign << `/`
	sb_string_to_sign.write_string(blob_name)
	signature_b64 := generate_signature_b64(account_key, sb_string_to_sign.str()) or {
		eprintln('Failed to generate signature: ${err}')
		return
	}

	authorization_header := 'SharedKey ${account_name}:${signature_b64}'

	mut headers := http.new_header()
	// header.add_custom('Test-Header', 'hello world')!
	headers.add_custom('x-ms-date', request_time_str)!
	headers.add_custom('x-ms-version', api_version)!
	headers.add_custom('x-ms-blob-type', 'BlockBlob')! // FIXME: Add support for other blob types
	headers.add_custom('Authorization', authorization_header)!
	headers.add_custom('Content-Length', file_data.len.str())!
	headers.add_custom('Content-Type', 'application/octet-stream')!

	response := http.fetch(
		method: .put
		url: url
		data: file_data
		header: headers
	) or {
		eprintln('Failed to upload file: ${err}')
		return
	}

	if response.status_code == 200 || response.status_code == 201 {
		println('File uploaded successfully.')
	} else {
		println('Failed to upload file.')
		println('Status code: ${response.status_code}')
		println('Response: ${response.body}')
	}
}
