`net.s3` is an S3-compatible client written in pure V.

It speaks the AWS Signature Version 4 protocol on top of `crypto.hmac` and
`crypto.sha256`, with no third-party dependencies. The same client targets any
S3-compatible endpoint (hosted or self-hosted) by configuring the right
`endpoint`, `region` and credentials.

## Quick start

```v ignore
import net.s3

c := s3.new_client(s3.Credentials{
	endpoint:          'https://s3.example.com'
	access_key_id:     '...'
	secret_access_key: '...'
	bucket:            'my-bucket'
})

c.put('hello.txt', 'Hi from V!'.bytes())!
text := c.get_string('hello.txt')!
url := c.presign('hello.txt', expires_in: 3600)!
```

## Credentials from the environment

`Credentials.from_env()` resolves each field from the first non-empty
provider-specific environment variable, so the same code works against AWS,
self-hosted and managed S3 services without reconfiguration:

```v ignore
import net.s3

c := s3.new_client(s3.Credentials.from_env())
```

Supported variables (first non-empty wins per field):

* key id: `S3_ACCESS_KEY_ID`, `AWS_ACCESS_KEY_ID`, `CELLAR_ADDON_KEY_ID`,
  `SCW_ACCESS_KEY`, `B2_APPLICATION_KEY_ID`, `R2_ACCESS_KEY_ID`, `SPACES_KEY`
* secret: `S3_SECRET_ACCESS_KEY`, `AWS_SECRET_ACCESS_KEY`,
  `CELLAR_ADDON_KEY_SECRET`, `SCW_SECRET_KEY`, `B2_APPLICATION_KEY`,
  `R2_SECRET_ACCESS_KEY`, `SPACES_SECRET`
* session token: `S3_SESSION_TOKEN`, `AWS_SESSION_TOKEN`
* region: `S3_REGION`, `AWS_REGION`, `AWS_DEFAULT_REGION`,
  `SCW_DEFAULT_REGION`
* bucket: `S3_BUCKET`
* endpoint: `S3_ENDPOINT`, `AWS_ENDPOINT`, `AWS_ENDPOINT_URL`,
  `CELLAR_ADDON_HOST`, `B2_ENDPOINT`, `R2_ENDPOINT`, `SPACES_ENDPOINT`

## Multipart upload

`upload_file` automatically picks single-shot or multipart based on file size.
For finer control, `start_multipart` returns a stateful `MultipartUploader`
that streams chunks generated on the fly:

```v ignore
import net.s3

c := s3.new_client(s3.Credentials.from_env())
c.upload_file('big.bin', '/path/to/big.bin', s3.PutOptions{
	content_type: 'application/octet-stream'
})!
```

## `s3://` URLs

Importing `net.s3` registers an `s3://` scheme handler with `net.http`, so the
generic `http.fetch(url: 's3://...')` route works out of the box. There is
also a direct `s3.fetch` helper:

```v ignore
import net.s3

resp := s3.fetch('s3://my-bucket/hello.txt')!
println(resp.body.bytestr())
```

## File handle

`Client.file(key)` returns a small `File` reference for ergonomic call sites:

```v ignore
import net.s3

c := s3.new_client(s3.Credentials.from_env())
f := c.file('hello.txt')
text := f.text()!
url := f.presign(expires_in: 3600)!
```

## Tests

The unit suite is offline and runs by default:

```
v test vlib/net/s3/
```

The integration suite is gated on `S3_INTEGRATION=1` and exercises the
client against a live endpoint:

```
S3_INTEGRATION=1 \
S3_HOST=https://s3.example.com \
S3_KEY_ID=... S3_KEY_SECRET=... \
S3_BUCKET=v-s3-tests \
v test vlib/net/s3/integration_test.v
```
