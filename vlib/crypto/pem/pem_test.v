module pem

// example PEM structures from the RFC
fn test_decode_rfc1421() {
	for i in 0 .. pem.test_data_rfc1421.len {
		decoded, rest := Block.decode_partial(pem.test_data_rfc1421[i]) or { Block{}, '' }
		assert decoded == pem.expected_results_rfc1421[i]
		assert rest == ''
	}
}

fn test_decode() {
	for i in 0 .. pem.test_data.len {
		decoded, rest := Block.decode_partial(pem.test_data[i]) or { Block{}, '' }
		assert decoded == pem.expected_results[i]
		assert decoded == Block.decode(pem.test_data[i]) or { Block{} }
		assert rest == pem.expected_rest[i]
	}
}

fn test_encode_rfc1421() {
	for i in 0 .. pem.test_data_rfc1421.len {
		encoded := pem.expected_results_rfc1421[i].encode() or { '' }
		decoded, rest := Block.decode_partial(encoded) or { Block{}, '' }
		assert rest == ''
		assert decoded == pem.expected_results_rfc1421[i]
		assert decoded == Block.decode(encoded) or { Block{} }
	}
}

fn test_encode() {
	for i in 0 .. pem.test_data.len {
		encoded := pem.expected_results[i].encode() or { '' }
		decoded, rest := Block.decode_partial(encoded) or { Block{}, '' }
		assert rest == ''
		assert decoded == pem.expected_results[i]
		assert decoded == Block.decode(pem.test_data[i]) or { Block{} }
	}
}

fn test_encode_config() {
	for i in 0 .. pem.test_data.len {
		encoded := pem.expected_results[i].encode(EncodeConfig{31, '\r\n'}) or { '' }
		decoded, rest := Block.decode_partial(encoded) or { Block{}, '' }
		assert rest == ''
		assert decoded == pem.expected_results[i]
		assert decoded == Block.decode(encoded) or { Block{} }
	}
}

fn test_decode_no_pem() {
	for test in pem.test_data_no_pem {
		if _, _ := Block.decode_partial(test) {
			assert false, 'Block.decode_partial should return `none` on input without PEM data'
		} else {
			assert true
		}

		if _ := Block.decode(test) {
			assert false, 'Block.decode should return `none` on input without PEM data'
		} else {
			assert true
		}
	}
}

const test_data_no_pem = [
	'',
	'-----BEGIN',
	'-----BEGIN -----',
	'-----END',
]

// https://datatracker.ietf.org/doc/html/rfc7468#section-4
const test_data_rfc1421 = [
	'-----BEGIN PRIVACY-ENHANCED MESSAGE-----
Proc-Type: 4,MIC-ONLY
Content-Domain: RFC822
Originator-Certificate:
MIIBlTCCAScCAWUwDQYJKoZIhvcNAQECBQAwUTELMAkGA1UEBhMCVVMxIDAeBgNV
BAoTF1JTQSBEYXRhIFNlY3VyaXR5LCBJbmMuMQ8wDQYDVQQLEwZCZXRhIDExDzAN
BgNVBAsTBk5PVEFSWTAeFw05MTA5MDQxODM4MTdaFw05MzA5MDMxODM4MTZaMEUx
CzAJBgNVBAYTAlVTMSAwHgYDVQQKExdSU0EgRGF0YSBTZWN1cml0eSwgSW5jLjEU
MBIGA1UEAxMLVGVzdCBVc2VyIDEwWTAKBgRVCAEBAgICAANLADBIAkEAwHZHl7i+
yJcqDtjJCowzTdBJrdAiLAnSC+CnnjOJELyuQiBgkGrgIh3j8/x0fM+YrsyF1u3F
LZPVtzlndhYFJQIDAQABMA0GCSqGSIb3DQEBAgUAA1kACKr0PqphJYw1j+YPtcIq
iWlFPuN5jJ79Khfg7ASFxskYkEMjRNZV/HZDZQEhtVaU7Jxfzs2wfX5byMp2X3U/
5XUXGx7qusDgHQGs7Jk9W8CW1fuSWUgN4w==
Issuer-Certificate:
MIIB3DCCAUgCAQowDQYJKoZIhvcNAQECBQAwTzELMAkGA1UEBhMCVVMxIDAeBgNV
BAoTF1JTQSBEYXRhIFNlY3VyaXR5LCBJbmMuMQ8wDQYDVQQLEwZCZXRhIDExDTAL
BgNVBAsTBFRMQ0EwHhcNOTEwOTAxMDgwMDAwWhcNOTIwOTAxMDc1OTU5WjBRMQsw
CQYDVQQGEwJVUzEgMB4GA1UEChMXUlNBIERhdGEgU2VjdXJpdHksIEluYy4xDzAN
BgNVBAsTBkJldGEgMTEPMA0GA1UECxMGTk9UQVJZMHAwCgYEVQgBAQICArwDYgAw
XwJYCsnp6lQCxYykNlODwutF/jMJ3kL+3PjYyHOwk+/9rLg6X65B/LD4bJHtO5XW
cqAz/7R7XhjYCm0PcqbdzoACZtIlETrKrcJiDYoP+DkZ8k1gCk7hQHpbIwIDAQAB
MA0GCSqGSIb3DQEBAgUAA38AAICPv4f9Gx/tY4+p+4DB7MV+tKZnvBoy8zgoMGOx
dD2jMZ/3HsyWKWgSF0eH/AJB3qr9zosG47pyMnTf3aSy2nBO7CMxpUWRBcXUpE+x
EREZd9++32ofGBIXaialnOgVUn0OzSYgugiQ077nJLDUj0hQehCizEs5wUJ35a5h
MIC-Info: RSA-MD5,RSA,
jV2OfH+nnXHU8bnL8kPAad/mSQlTDZlbVuxvZAOVRZ5q5+Ejl5bQvqNeqOUNQjr6
EtE7K2QDeVMCyXsdJlA8fA==

LSBBIG1lc3NhZ2UgZm9yIHVzZSBpbiB0ZXN0aW5nLg0KLSBGb2xsb3dpbmcgaXMg
YSBibGFuayBsaW5lOg0KDQpUaGlzIGlzIHRoZSBlbmQuDQo=
-----END PRIVACY-ENHANCED MESSAGE-----',
	'-----BEGIN PRIVACY-ENHANCED MESSAGE-----
Proc-Type: 4,ENCRYPTED
Content-Domain: RFC822
DEK-Info: DES-CBC,BFF968AA74691AC1
Originator-Certificate:
MIIBlTCCAScCAWUwDQYJKoZIhvcNAQECBQAwUTELMAkGA1UEBhMCVVMxIDAeBgNV
BAoTF1JTQSBEYXRhIFNlY3VyaXR5LCBJbmMuMQ8wDQYDVQQLEwZCZXRhIDExDzAN
BgNVBAsTBk5PVEFSWTAeFw05MTA5MDQxODM4MTdaFw05MzA5MDMxODM4MTZaMEUx
CzAJBgNVBAYTAlVTMSAwHgYDVQQKExdSU0EgRGF0YSBTZWN1cml0eSwgSW5jLjEU
MBIGA1UEAxMLVGVzdCBVc2VyIDEwWTAKBgRVCAEBAgICAANLADBIAkEAwHZHl7i+
yJcqDtjJCowzTdBJrdAiLAnSC+CnnjOJELyuQiBgkGrgIh3j8/x0fM+YrsyF1u3F
LZPVtzlndhYFJQIDAQABMA0GCSqGSIb3DQEBAgUAA1kACKr0PqphJYw1j+YPtcIq
iWlFPuN5jJ79Khfg7ASFxskYkEMjRNZV/HZDZQEhtVaU7Jxfzs2wfX5byMp2X3U/
5XUXGx7qusDgHQGs7Jk9W8CW1fuSWUgN4w==
Key-Info: RSA,
I3rRIGXUGWAF8js5wCzRTkdhO34PTHdRZY9Tuvm03M+NM7fx6qc5udixps2Lng0+
wGrtiUm/ovtKdinz6ZQ/aQ==
Issuer-Certificate:
MIIB3DCCAUgCAQowDQYJKoZIhvcNAQECBQAwTzELMAkGA1UEBhMCVVMxIDAeBgNV
BAoTF1JTQSBEYXRhIFNlY3VyaXR5LCBJbmMuMQ8wDQYDVQQLEwZCZXRhIDExDTAL
BgNVBAsTBFRMQ0EwHhcNOTEwOTAxMDgwMDAwWhcNOTIwOTAxMDc1OTU5WjBRMQsw
CQYDVQQGEwJVUzEgMB4GA1UEChMXUlNBIERhdGEgU2VjdXJpdHksIEluYy4xDzAN
BgNVBAsTBkJldGEgMTEPMA0GA1UECxMGTk9UQVJZMHAwCgYEVQgBAQICArwDYgAw
XwJYCsnp6lQCxYykNlODwutF/jMJ3kL+3PjYyHOwk+/9rLg6X65B/LD4bJHtO5XW
cqAz/7R7XhjYCm0PcqbdzoACZtIlETrKrcJiDYoP+DkZ8k1gCk7hQHpbIwIDAQAB
MA0GCSqGSIb3DQEBAgUAA38AAICPv4f9Gx/tY4+p+4DB7MV+tKZnvBoy8zgoMGOx
dD2jMZ/3HsyWKWgSF0eH/AJB3qr9zosG47pyMnTf3aSy2nBO7CMxpUWRBcXUpE+x
EREZd9++32ofGBIXaialnOgVUn0OzSYgugiQ077nJLDUj0hQehCizEs5wUJ35a5h
MIC-Info: RSA-MD5,RSA,
UdFJR8u/TIGhfH65ieewe2lOW4tooa3vZCvVNGBZirf/7nrgzWDABz8w9NsXSexv
AjRFbHoNPzBuxwmOAFeA0HJszL4yBvhG
Recipient-ID-Asymmetric:
MFExCzAJBgNVBAYTAlVTMSAwHgYDVQQKExdSU0EgRGF0YSBTZWN1cml0eSwgSW5j
LjEPMA0GA1UECxMGQmV0YSAxMQ8wDQYDVQQLEwZOT1RBUlk=,
66
Key-Info: RSA,
O6BS1ww9CTyHPtS3bMLD+L0hejdvX6Qv1HK2ds2sQPEaXhX8EhvVphHYTjwekdWv
7x0Z3Jx2vTAhOYHMcqqCjA==

qeWlj/YJ2Uf5ng9yznPbtD0mYloSwIuV9FRYx+gzY+8iXd/NQrXHfi6/MhPfPF3d
jIqCJAxvld2xgqQimUzoS1a4r7kQQ5c/Iua4LqKeq3ciFzEv/MbZhA==
-----END PRIVACY-ENHANCED MESSAGE-----',
	'-----BEGIN PRIVACY-ENHANCED MESSAGE-----
Proc-Type: 4,ENCRYPTED
Content-Domain: RFC822
DEK-Info: DES-CBC,F8143EDE5960C597
Originator-ID-Symmetric: linn@zendia.enet.dec.com,,
Recipient-ID-Symmetric: linn@zendia.enet.dec.com,ptf-kmc,3
Key-Info: DES-ECB,RSA-MD2,9FD3AAD2F2691B9A,
         B70665BB9BF7CBCDA60195DB94F727D3
Recipient-ID-Symmetric: pem-dev@tis.com,ptf-kmc,4
Key-Info: DES-ECB,RSA-MD2,161A3F75DC82EF26,
         E2EF532C65CBCFF79F83A2658132DB47

LLrHB0eJzyhP+/fSStdW8okeEnv47jxe7SJ/iN72ohNcUk2jHEUSoH1nvNSIWL9M
8tEjmF/zxB+bATMtPjCUWbz8Lr9wloXIkjHUlBLpvXR0UrUzYbkNpk0agV2IzUpk
J6UiRRGcDSvzrsoK+oNvqu6z7Xs5Xfz5rDqUcMlK1Z6720dcBWGGsDLpTpSCnpot
dXd/H5LMDWnonNvPCwQUHt==
-----END PRIVACY-ENHANCED MESSAGE-----',
]

const expected_results_rfc1421 = [
	Block{
		block_type: 'PRIVACY-ENHANCED MESSAGE'
		headers: {
			'Proc-Type':              ['4,MIC-ONLY']
			'Content-Domain':         ['RFC822']
			'Originator-Certificate': [
				'MIIBlTCCAScCAWUwDQYJKoZIhvcNAQECBQAwUTELMAkGA1UEBhMCVVMxIDAeBgNVBAoTF1JTQSBEYXRhIFNlY3VyaXR5LCBJbmMuMQ8wDQYDVQQLEwZCZXRhIDExDzANBgNVBAsTBk5PVEFSWTAeFw05MTA5MDQxODM4MTdaFw05MzA5MDMxODM4MTZaMEUxCzAJBgNVBAYTAlVTMSAwHgYDVQQKExdSU0EgRGF0YSBTZWN1cml0eSwgSW5jLjEUMBIGA1UEAxMLVGVzdCBVc2VyIDEwWTAKBgRVCAEBAgICAANLADBIAkEAwHZHl7i+yJcqDtjJCowzTdBJrdAiLAnSC+CnnjOJELyuQiBgkGrgIh3j8/x0fM+YrsyF1u3FLZPVtzlndhYFJQIDAQABMA0GCSqGSIb3DQEBAgUAA1kACKr0PqphJYw1j+YPtcIqiWlFPuN5jJ79Khfg7ASFxskYkEMjRNZV/HZDZQEhtVaU7Jxfzs2wfX5byMp2X3U/5XUXGx7qusDgHQGs7Jk9W8CW1fuSWUgN4w==',
			]
			'Issuer-Certificate':     [
				'MIIB3DCCAUgCAQowDQYJKoZIhvcNAQECBQAwTzELMAkGA1UEBhMCVVMxIDAeBgNVBAoTF1JTQSBEYXRhIFNlY3VyaXR5LCBJbmMuMQ8wDQYDVQQLEwZCZXRhIDExDTALBgNVBAsTBFRMQ0EwHhcNOTEwOTAxMDgwMDAwWhcNOTIwOTAxMDc1OTU5WjBRMQswCQYDVQQGEwJVUzEgMB4GA1UEChMXUlNBIERhdGEgU2VjdXJpdHksIEluYy4xDzANBgNVBAsTBkJldGEgMTEPMA0GA1UECxMGTk9UQVJZMHAwCgYEVQgBAQICArwDYgAwXwJYCsnp6lQCxYykNlODwutF/jMJ3kL+3PjYyHOwk+/9rLg6X65B/LD4bJHtO5XWcqAz/7R7XhjYCm0PcqbdzoACZtIlETrKrcJiDYoP+DkZ8k1gCk7hQHpbIwIDAQABMA0GCSqGSIb3DQEBAgUAA38AAICPv4f9Gx/tY4+p+4DB7MV+tKZnvBoy8zgoMGOxdD2jMZ/3HsyWKWgSF0eH/AJB3qr9zosG47pyMnTf3aSy2nBO7CMxpUWRBcXUpE+xEREZd9++32ofGBIXaialnOgVUn0OzSYgugiQ077nJLDUj0hQehCizEs5wUJ35a5h',
			]
			'MIC-Info':               [
				'RSA-MD5,RSA,jV2OfH+nnXHU8bnL8kPAad/mSQlTDZlbVuxvZAOVRZ5q5+Ejl5bQvqNeqOUNQjr6EtE7K2QDeVMCyXsdJlA8fA==',
			]
		}
		// vfmt off
		data: [
			u8(0x2d), 0x20, 0x41, 0x20, 0x6d, 0x65, 0x73, 0x73,
			0x61, 0x67, 0x65, 0x20, 0x66, 0x6f, 0x72, 0x20,
			0x75, 0x73, 0x65, 0x20, 0x69, 0x6e, 0x20, 0x74,
			0x65, 0x73, 0x74, 0x69, 0x6e, 0x67, 0x2e, 0x0d,
			0x0a, 0x2d, 0x20, 0x46, 0x6f, 0x6c, 0x6c, 0x6f,
			0x77, 0x69, 0x6e, 0x67, 0x20, 0x69, 0x73, 0x20,
			0x61, 0x20, 0x62, 0x6c, 0x61, 0x6e, 0x6b, 0x20,
			0x6c, 0x69, 0x6e, 0x65, 0x3a, 0x0d, 0x0a, 0x0d,
			0x0a, 0x54, 0x68, 0x69, 0x73, 0x20, 0x69, 0x73,
			0x20, 0x74, 0x68, 0x65, 0x20, 0x65, 0x6e, 0x64,
			0x2e, 0x0d, 0x0a,
		]
		// vfmt on
	},
	Block{
		block_type: 'PRIVACY-ENHANCED MESSAGE'
		headers: {
			'Proc-Type':               [
				'4,ENCRYPTED',
			]
			'Content-Domain':          [
				'RFC822',
			]
			'DEK-Info':                [
				'DES-CBC,BFF968AA74691AC1',
			]
			'Originator-Certificate':  [
				'MIIBlTCCAScCAWUwDQYJKoZIhvcNAQECBQAwUTELMAkGA1UEBhMCVVMxIDAeBgNVBAoTF1JTQSBEYXRhIFNlY3VyaXR5LCBJbmMuMQ8wDQYDVQQLEwZCZXRhIDExDzANBgNVBAsTBk5PVEFSWTAeFw05MTA5MDQxODM4MTdaFw05MzA5MDMxODM4MTZaMEUxCzAJBgNVBAYTAlVTMSAwHgYDVQQKExdSU0EgRGF0YSBTZWN1cml0eSwgSW5jLjEUMBIGA1UEAxMLVGVzdCBVc2VyIDEwWTAKBgRVCAEBAgICAANLADBIAkEAwHZHl7i+yJcqDtjJCowzTdBJrdAiLAnSC+CnnjOJELyuQiBgkGrgIh3j8/x0fM+YrsyF1u3FLZPVtzlndhYFJQIDAQABMA0GCSqGSIb3DQEBAgUAA1kACKr0PqphJYw1j+YPtcIqiWlFPuN5jJ79Khfg7ASFxskYkEMjRNZV/HZDZQEhtVaU7Jxfzs2wfX5byMp2X3U/5XUXGx7qusDgHQGs7Jk9W8CW1fuSWUgN4w==',
			]
			'Key-Info':                [
				'RSA,I3rRIGXUGWAF8js5wCzRTkdhO34PTHdRZY9Tuvm03M+NM7fx6qc5udixps2Lng0+wGrtiUm/ovtKdinz6ZQ/aQ==',
				'RSA,O6BS1ww9CTyHPtS3bMLD+L0hejdvX6Qv1HK2ds2sQPEaXhX8EhvVphHYTjwekdWv7x0Z3Jx2vTAhOYHMcqqCjA==',
			]
			'Issuer-Certificate':      [
				'MIIB3DCCAUgCAQowDQYJKoZIhvcNAQECBQAwTzELMAkGA1UEBhMCVVMxIDAeBgNVBAoTF1JTQSBEYXRhIFNlY3VyaXR5LCBJbmMuMQ8wDQYDVQQLEwZCZXRhIDExDTALBgNVBAsTBFRMQ0EwHhcNOTEwOTAxMDgwMDAwWhcNOTIwOTAxMDc1OTU5WjBRMQswCQYDVQQGEwJVUzEgMB4GA1UEChMXUlNBIERhdGEgU2VjdXJpdHksIEluYy4xDzANBgNVBAsTBkJldGEgMTEPMA0GA1UECxMGTk9UQVJZMHAwCgYEVQgBAQICArwDYgAwXwJYCsnp6lQCxYykNlODwutF/jMJ3kL+3PjYyHOwk+/9rLg6X65B/LD4bJHtO5XWcqAz/7R7XhjYCm0PcqbdzoACZtIlETrKrcJiDYoP+DkZ8k1gCk7hQHpbIwIDAQABMA0GCSqGSIb3DQEBAgUAA38AAICPv4f9Gx/tY4+p+4DB7MV+tKZnvBoy8zgoMGOxdD2jMZ/3HsyWKWgSF0eH/AJB3qr9zosG47pyMnTf3aSy2nBO7CMxpUWRBcXUpE+xEREZd9++32ofGBIXaialnOgVUn0OzSYgugiQ077nJLDUj0hQehCizEs5wUJ35a5h',
			]
			'MIC-Info':                [
				'RSA-MD5,RSA,UdFJR8u/TIGhfH65ieewe2lOW4tooa3vZCvVNGBZirf/7nrgzWDABz8w9NsXSexvAjRFbHoNPzBuxwmOAFeA0HJszL4yBvhG',
			]
			'Recipient-ID-Asymmetric': [
				'MFExCzAJBgNVBAYTAlVTMSAwHgYDVQQKExdSU0EgRGF0YSBTZWN1cml0eSwgSW5jLjEPMA0GA1UECxMGQmV0YSAxMQ8wDQYDVQQLEwZOT1RBUlk=,66',
			]
		}
		// vfmt off
		data: [
			u8(0xa9), 0xe5, 0xa5, 0x8f, 0xf6, 0x09, 0xd9, 0x47,
			0xf9, 0x9e, 0x0f, 0x72, 0xce, 0x73, 0xdb, 0xb4,
			0x3d, 0x26, 0x62, 0x5a, 0x12, 0xc0, 0x8b, 0x95,
			0xf4, 0x54, 0x58, 0xc7, 0xe8, 0x33, 0x63, 0xef,
			0x22, 0x5d, 0xdf, 0xcd, 0x42, 0xb5, 0xc7, 0x7e,
			0x2e, 0xbf, 0x32, 0x13, 0xdf, 0x3c, 0x5d, 0xdd,
			0x8c, 0x8a, 0x82, 0x24, 0x0c, 0x6f, 0x95, 0xdd,
			0xb1, 0x82, 0xa4, 0x22, 0x99, 0x4c, 0xe8, 0x4b,
			0x56, 0xb8, 0xaf, 0xb9, 0x10, 0x43, 0x97, 0x3f,
			0x22, 0xe6, 0xb8, 0x2e, 0xa2, 0x9e, 0xab, 0x77,
			0x22, 0x17, 0x31, 0x2f, 0xfc, 0xc6, 0xd9, 0x84,
		]
		// vfmt on
	},
	Block{
		block_type: 'PRIVACY-ENHANCED MESSAGE'
		headers: {
			'Proc-Type':               [
				'4,ENCRYPTED',
			]
			'Content-Domain':          [
				'RFC822',
			]
			'DEK-Info':                [
				'DES-CBC,F8143EDE5960C597',
			]
			'Originator-ID-Symmetric': [
				'linn@zendia.enet.dec.com,,',
			]
			'Recipient-ID-Symmetric':  [
				'linn@zendia.enet.dec.com,ptf-kmc,3',
				'pem-dev@tis.com,ptf-kmc,4',
			]
			'Key-Info':                [
				'DES-ECB,RSA-MD2,9FD3AAD2F2691B9A,B70665BB9BF7CBCDA60195DB94F727D3',
				'DES-ECB,RSA-MD2,161A3F75DC82EF26,E2EF532C65CBCFF79F83A2658132DB47',
			]
		}
		// vfmt off
		data: [
			u8(0x2c), 0xba, 0xc7, 0x07, 0x47, 0x89, 0xcf, 0x28,
			0x4f, 0xfb, 0xf7, 0xd2, 0x4a, 0xd7, 0x56, 0xf2,
			0x89, 0x1e, 0x12, 0x7b, 0xf8, 0xee, 0x3c, 0x5e,
			0xed, 0x22, 0x7f, 0x88, 0xde, 0xf6, 0xa2, 0x13,
			0x5c, 0x52, 0x4d, 0xa3, 0x1c, 0x45, 0x12, 0xa0,
			0x7d, 0x67, 0xbc, 0xd4, 0x88, 0x58, 0xbf, 0x4c,
			0xf2, 0xd1, 0x23, 0x98, 0x5f, 0xf3, 0xc4, 0x1f,
			0x9b, 0x01, 0x33, 0x2d, 0x3e, 0x30, 0x94, 0x59,
			0xbc, 0xfc, 0x2e, 0xbf, 0x70, 0x96, 0x85, 0xc8,
			0x92, 0x31, 0xd4, 0x94, 0x12, 0xe9, 0xbd, 0x74,
			0x74, 0x52, 0xb5, 0x33, 0x61, 0xb9, 0x0d, 0xa6,
			0x4d, 0x1a, 0x81, 0x5d, 0x88, 0xcd, 0x4a, 0x64,
			0x27, 0xa5, 0x22, 0x45, 0x11, 0x9c, 0x0d, 0x2b,
			0xf3, 0xae, 0xca, 0x0a, 0xfa, 0x83, 0x6f, 0xaa,
			0xee, 0xb3, 0xed, 0x7b, 0x39, 0x5d, 0xfc, 0xf9,
			0xac, 0x3a, 0x94, 0x70, 0xc9, 0x4a, 0xd5, 0x9e,
			0xbb, 0xdb, 0x47, 0x5c, 0x05, 0x61, 0x86, 0xb0,
			0x32, 0xe9, 0x4e, 0x94, 0x82, 0x9e, 0x9a, 0x2d,
			0x75, 0x77, 0x7f, 0x1f, 0x92, 0xcc, 0x0d, 0x69,
			0xe8, 0x9c, 0xdb, 0xcf, 0x0b, 0x04, 0x14, 0x1e,
		]
		// vfmt on
	},
]

const test_data = [
	// https://qsupport.quantum.com/kb/flare/Content/stornext/SN5_DocSite/Guide_Users/Topics/Example_of_a_server_pem_.htm
	"--BEG
An RSA PRIVATE KEY is on it's way!!
----BEGIN
I only want to parse the certificate below
-----BEGIN RSA PRIVATE KEY-----
izfrNTmQLnfsLzi2Wb9xPz2Qj9fQYGgeug3N2MkDuVHwpPcgkhHkJgCQuuvT+qZI
MbS2U6wTS24SZk5RunJIUkitRKeWWMS28SLGfkDs1bBYlSPa5smAd3/q1OePi4ae
dU6YgWuDxzBAKEKVSUu6pA2HOdyQ9N4F1dI+F8w9J990zE93EgyNqZFBBa2L70h4
M7DrB0gJBWMdUMoxGnun5glLiCMo2JrHZ9RkMiallS1sHMhELx2UAlP8I1+0Mav8
iMlHGyUW8EJy0paVf09MPpceEcVwDBeX0+G4UQlO551GTFtOSRjcD8U+GkCzka9W
/SFQrSGe3Gh3SDaOw/4JEMAjWPDLiCglwh0rLIO4VwU6AxzTCuCw3d1ZxQsU6VFQ
PqHA8haOUATZIrp3886PBThVqALBk9p1Nqn51bXLh13Zy9DZIVx4Z5Ioz/EGuzgR
d68VW5wybLjYE2r6Q9nHpitSZ4ZderwjIZRes67HdxYFw8unm4Wo6kuGnb5jSSag
vwBxKzAf3Omn+J6IthTJKuDd13rKZGMcRpQQ6VstwihYt1TahQ/qfJUWPjPcU5ML
9LkgVwA8Ndi1wp1/sEPe+UlL16L6vO9jUHcueWN7+zSUOE/cDSJyMd9x/ZL8QASA
ETd5dujVIqlINL2vJKr1o4T+i0RsnpfFiqFmBKlFqww/SKzJeChdyEtpa/dJMrt2
8S86b6zEmkser+SDYgGketS2DZ4hB+vh2ujSXmS8Gkwrn+BfHMzkbtio8lWbGw0l
eM1tfdFZ6wMTLkxRhBkBK4JiMiUMvpERyPib6a2L6iXTfH+3RUDS6A==
-----END RSA PRIVATE KEY-----Extra stuff
This should be sent back in the second variable
I will now continue to spam my keyboard to add junk
lfajsdkfsndckasdjflkasdjclsdfjljjasldfj
alsdfsdkfasdfasfsdafasdcjaosdicnsad


fas90dfsdsdj
----------------------------------
-----
asddfadsfasjfsadf
-----BEGIN lkfajsdklfjasldkfjalsdjf;kasdflkjsaf
flajsdfkjasdfajskfjslkdfjaskdjf
FJASKDJjklfjasdfklsdjf
BEGIN BEGIN BEGIN
----BEGIN
-----END RSA PRIVATE KEY
: fkalsdjflkasdjf
private key: fsaddf",
	'Mollitia magnam ullam ipsam voluptas ipsa
rerum debitis. Vel nulla ipsum enim perspiciatis adipisci quam. Nihil incidunt ipsum
--- --BEGIN


-- BEGIN ------
---
--BEGIN ------
amet rem rerum explicabo. Cum distinctio ipsum dolorum quae. Suscipit asperiores et aut eos numquam.
-----BEGIN CERTIFICATE-----
MIICMzCCAZygAwIBAgIJALiPnVsvq8dsMA0GCSqGSIb3DQEBBQUAMFMxCzAJBgNV
BAYTAlVTMQwwCgYDVQQIEwNmb28xDDAKBgNVBAcTA2ZvbzEMMAoGA1UEChMDZm9v
MQwwCgYDVQQLEwNmb28xDDAKBgNVBAMTA2ZvbzAeFw0xMzAzMTkxNTQwMTlaFw0x
ODAzMTgxNTQwMTlaMFMxCzAJBgNVBAYTAlVTMQwwCgYDVQQIEwNmb28xDDAKBgNV
BAcTA2ZvbzEMMAoGA1UEChMDZm9vMQwwCgYDVQQLEwNmb28xDDAKBgNVBAMTA2Zv
bzCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEAzdGfxi9CNbMf1UUcvDQh7MYB
OveIHyc0E0KIbhjK5FkCBU4CiZrbfHagaW7ZEcN0tt3EvpbOMxxc/ZQU2WN/s/wP
xph0pSfsfFsTKM4RhTWD2v4fgk+xZiKd1p0+L4hTtpwnEw0uXRVd0ki6muwV5y/P
+5FHUeldq+pgTcgzuK8CAwEAAaMPMA0wCwYDVR0PBAQDAgLkMA0GCSqGSIb3DQEB
BQUAA4GBAJiDAAtY0mQQeuxWdzLRzXmjvdSuL9GoyT3BF/jSnpxz5/58dba8pWen
v3pj4P3w5DoOso0rzkZy2jEsEitlVM2mLSbQpMM+MUVQCQoiG6W9xuCFuxSrwPIS
pAqEAuV4DNoxQKKWmhVv+J0ptMWD25Pnpxeq5sXzghfJnslJlQND
-----END CERTIFICATE-----
fdsjaf888888888888
-----

-----END
-----BEGIN',
	'Lorem ipsum dolor sit amet
, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore
et dolore magna aliqua. Massa id neque aliquam vestibulum
morbi blandit cursus risus. Elit at imperdiet dui accumsan sit amet nulla. Pulvinar pellentesque habitant


morbi tristique senectus. Vulputate
dignissim suspendisse in est ante in. Egestas dui id ornare arcu. Ultrices mi tempus imperdiet
nulla malesuada. Elementum nisi quis eleifend quam adipiscing.
Mi in nulla posuere sollicitudin aliquam ultrices. Elit at imperdiet dui accumsan sit amet nulla facilisi. In hac
habitasse platea dictumst quisque sagittis. Vestibulum
lectus mauris ultrices eros in cursus. Blandit volutpat maecenas volutpat blandit. Sed nisi
lacus sed viverra tellus in hac habitasse platea.
Nulla facilisi etiam dignissim diam. Donec et odio pellentesque diam volutpat
commodo sed egestas. Eleifend quam adipiscing
vitae proin sagittis nisl.

Pharetra et ultrices neque ornare aenean euismod elementum nisi. Sit amet consectetur sed id semper risus in.
Eget nullam non nisi est. A diam maecenas sed enim. Enim nec dui nunc mattis. Lectus quam id leo in vitae turpis massa sed
. In eu mi bibendum neque egestas congue. Dui faucibus in ornare quam viverra orci j
sagittis. Lectus sit amet est placerat in egestas erat imperdiet.

Suspendisse potenti nullam ac tortor. Iaculis nunc sed augue lacus viverra vitae congue eu consequat.
Lacus vestibulum sed arcu
non odio euismod. Massa sed elementum tempus egestas sed. Nulla facilisi etiam dignissim diam quis enim
. Ac ut consequat semper viverra. Eleifend quam adipiscing vitae proin sagittis nisl rhoncus mattis rhoncus. Nunc consequat interdum varius sit amet mattis vulputate enim.
Orci nulla pellentesque dignissim enim sit amet. Sed vulputate mi sit amet.
Sagittis vitae et leo duis ut diam. Orci a scelerisque purus semper eget duis
at tellus at. In hac habitasse platea dictumst vestibulum rhoncus est
. Fames
ac turpis egestas integer. Mattis enim ut
tellus elementum sagittis vitae. Pellentesque pulvinar pellentesque habitant morbi tristique senectus et netus
et. Id semper risus in hendrerit.
Et sollicitudin ac orci phasellus egestas. Sem integer vitae justo eget
magna. Et ligula ullamcorper malesuada proin libero nunc consequat.-----BEGIN CERTIFICATE-----
MIICMzCCAZygAwIBAgIJALiPnVsvq8dsMA0GCSqGSIb3DQEBBQUAMFMxCzAJBgNV
BAYTAlVTMQwwCgYDVQQIEwNmb28xDDAKBgNVBAcTA2ZvbzEMMAoGA1UEChMDZm9v
MQwwCgYDVQQLEwNmb28xDDAKBgNVBAMTA2ZvbzAeFw0xMzAzMTkxNTQwMTlaFw0x
ODAzMTgxNTQwMTlaMFMxCzAJBgNVBAYTAlVTMQwwCgYDVQQIEwNmb28xDDAKBgNV
BAcTA2ZvbzEMMAoGA1UEChMDZm9vMQwwCgYDVQQLEwNmb28xDDAKBgNVBAMTA2Zv
bzCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEAzdGfxi9CNbMf1UUcvDQh7MYB
OveIHyc0E0KIbhjK5FkCBU4CiZrbfHagaW7ZEcN0tt3EvpbOMxxc/ZQU2WN/s/wP
xph0pSfsfFsTKM4RhTWD2v4fgk+xZiKd1p0+L4hTtpwnEw0uXRVd0ki6muwV5y/P
+5FHUeldq+pgTcgzuK8CAwEAAaMPMA0wCwYDVR0PBAQDAgLkMA0GCSqGSIb3DQEB
BQUAA4GBAJiDAAtY0mQQeuxWdzLRzXmjvdSuL9GoyT3BF/jSnpxz5/58dba8pWen
v3pj4P3w5DoOso0rzkZy2jEsEitlVM2mLSbQpMM+MUVQCQoiG6W9xuCFuxSrwPIS
pAqEAuV4DNoxQKKWmhVv+J0ptMWD25Pnpxeq5sXzghfJnslJlQND
-----END CERTIFICATE-----YXMuIEVsZWlmZW5kIHF1YW0gYWRpcGlzY2luZyB2aXRh
ZSBwcm9pbiBzYWdpdHRpcyBuaXNsLgoKUGVsbGVudGVzcXVlIG
hh



Yml0YW50IG1vcmJpIHRyaXN0aXF1ZSBzZW5lY3R1cy4gRWdldCBudW5jIGxvYm9ydGlzIG1hdHRpcyBhb
GlxdWFtIGZhdWNpYnVzLiBOdWxsYW0g
dmVoaWN1bGEgaXBzdW0gYSBhcmN1IGN1cnN1cy4gUG9ydHRpdG9yIGVnZXQg
ZG9sb3IgbW9yYmkgbm9uIGFyY3Ugcml


zdXMgcXVpcyB2YXJpdXMgcXVhbS4gRGlnbmlzc2ltIGVuaW0gc2l0IGFtZXQgdmVuZW5hd
GlzLiBDb25zZWN0ZXR1ciBhIGVyYXQgbmFtIGF0IGxlY3R1cyB1cm5h
IGR1aXMuIEluIGZlcm1lbnR1bS
Bwb3N1ZXJlIHVybmEgbmVjIHRpbmNpZHVudCBwcmFlc2VudCBzZW1wZXIgZmV1Z2lhdCBuaWJoLiBOYW
0gYWxpcXVhbSBzZW0gZXQgdG9ydG9yIGNvbg==',
]

const expected_results = [
	Block{
		block_type: 'RSA PRIVATE KEY'
		// vfmt off
		data: [
			u8(0x8b), 0x37, 0xeb, 0x35, 0x39, 0x90, 0x2e, 0x77,
			0xec, 0x2f, 0x38, 0xb6, 0x59, 0xbf, 0x71, 0x3f,
			0x3d, 0x90, 0x8f, 0xd7, 0xd0, 0x60, 0x68, 0x1e,
			0xba, 0x0d, 0xcd, 0xd8, 0xc9, 0x03, 0xb9, 0x51,
			0xf0, 0xa4, 0xf7, 0x20, 0x92, 0x11, 0xe4, 0x26,
			0x00, 0x90, 0xba, 0xeb, 0xd3, 0xfa, 0xa6, 0x48,
			0x31, 0xb4, 0xb6, 0x53, 0xac, 0x13, 0x4b, 0x6e,
			0x12, 0x66, 0x4e, 0x51, 0xba, 0x72, 0x48, 0x52,
			0x48, 0xad, 0x44, 0xa7, 0x96, 0x58, 0xc4, 0xb6,
			0xf1, 0x22, 0xc6, 0x7e, 0x40, 0xec, 0xd5, 0xb0,
			0x58, 0x95, 0x23, 0xda, 0xe6, 0xc9, 0x80, 0x77,
			0x7f, 0xea, 0xd4, 0xe7, 0x8f, 0x8b, 0x86, 0x9e,
			0x75, 0x4e, 0x98, 0x81, 0x6b, 0x83, 0xc7, 0x30,
			0x40, 0x28, 0x42, 0x95, 0x49, 0x4b, 0xba, 0xa4,
			0x0d, 0x87, 0x39, 0xdc, 0x90, 0xf4, 0xde, 0x05,
			0xd5, 0xd2, 0x3e, 0x17, 0xcc, 0x3d, 0x27, 0xdf,
			0x74, 0xcc, 0x4f, 0x77, 0x12, 0x0c, 0x8d, 0xa9,
			0x91, 0x41, 0x05, 0xad, 0x8b, 0xef, 0x48, 0x78,
			0x33, 0xb0, 0xeb, 0x07, 0x48, 0x09, 0x05, 0x63,
			0x1d, 0x50, 0xca, 0x31, 0x1a, 0x7b, 0xa7, 0xe6,
			0x09, 0x4b, 0x88, 0x23, 0x28, 0xd8, 0x9a, 0xc7,
			0x67, 0xd4, 0x64, 0x32, 0x26, 0xa5, 0x95, 0x2d,
			0x6c, 0x1c, 0xc8, 0x44, 0x2f, 0x1d, 0x94, 0x02,
			0x53, 0xfc, 0x23, 0x5f, 0xb4, 0x31, 0xab, 0xfc,
			0x88, 0xc9, 0x47, 0x1b, 0x25, 0x16, 0xf0, 0x42,
			0x72, 0xd2, 0x96, 0x95, 0x7f, 0x4f, 0x4c, 0x3e,
			0x97, 0x1e, 0x11, 0xc5, 0x70, 0x0c, 0x17, 0x97,
			0xd3, 0xe1, 0xb8, 0x51, 0x09, 0x4e, 0xe7, 0x9d,
			0x46, 0x4c, 0x5b, 0x4e, 0x49, 0x18, 0xdc, 0x0f,
			0xc5, 0x3e, 0x1a, 0x40, 0xb3, 0x91, 0xaf, 0x56,
			0xfd, 0x21, 0x50, 0xad, 0x21, 0x9e, 0xdc, 0x68,
			0x77, 0x48, 0x36, 0x8e, 0xc3, 0xfe, 0x09, 0x10,
			0xc0, 0x23, 0x58, 0xf0, 0xcb, 0x88, 0x28, 0x25,
			0xc2, 0x1d, 0x2b, 0x2c, 0x83, 0xb8, 0x57, 0x05,
			0x3a, 0x03, 0x1c, 0xd3, 0x0a, 0xe0, 0xb0, 0xdd,
			0xdd, 0x59, 0xc5, 0x0b, 0x14, 0xe9, 0x51, 0x50,
			0x3e, 0xa1, 0xc0, 0xf2, 0x16, 0x8e, 0x50, 0x04,
			0xd9, 0x22, 0xba, 0x77, 0xf3, 0xce, 0x8f, 0x05,
			0x38, 0x55, 0xa8, 0x02, 0xc1, 0x93, 0xda, 0x75,
			0x36, 0xa9, 0xf9, 0xd5, 0xb5, 0xcb, 0x87, 0x5d,
			0xd9, 0xcb, 0xd0, 0xd9, 0x21, 0x5c, 0x78, 0x67,
			0x92, 0x28, 0xcf, 0xf1, 0x06, 0xbb, 0x38, 0x11,
			0x77, 0xaf, 0x15, 0x5b, 0x9c, 0x32, 0x6c, 0xb8,
			0xd8, 0x13, 0x6a, 0xfa, 0x43, 0xd9, 0xc7, 0xa6,
			0x2b, 0x52, 0x67, 0x86, 0x5d, 0x7a, 0xbc, 0x23,
			0x21, 0x94, 0x5e, 0xb3, 0xae, 0xc7, 0x77, 0x16,
			0x05, 0xc3, 0xcb, 0xa7, 0x9b, 0x85, 0xa8, 0xea,
			0x4b, 0x86, 0x9d, 0xbe, 0x63, 0x49, 0x26, 0xa0,
			0xbf, 0x00, 0x71, 0x2b, 0x30, 0x1f, 0xdc, 0xe9,
			0xa7, 0xf8, 0x9e, 0x88, 0xb6, 0x14, 0xc9, 0x2a,
			0xe0, 0xdd, 0xd7, 0x7a, 0xca, 0x64, 0x63, 0x1c,
			0x46, 0x94, 0x10, 0xe9, 0x5b, 0x2d, 0xc2, 0x28,
			0x58, 0xb7, 0x54, 0xda, 0x85, 0x0f, 0xea, 0x7c,
			0x95, 0x16, 0x3e, 0x33, 0xdc, 0x53, 0x93, 0x0b,
			0xf4, 0xb9, 0x20, 0x57, 0x00, 0x3c, 0x35, 0xd8,
			0xb5, 0xc2, 0x9d, 0x7f, 0xb0, 0x43, 0xde, 0xf9,
			0x49, 0x4b, 0xd7, 0xa2, 0xfa, 0xbc, 0xef, 0x63,
			0x50, 0x77, 0x2e, 0x79, 0x63, 0x7b, 0xfb, 0x34,
			0x94, 0x38, 0x4f, 0xdc, 0x0d, 0x22, 0x72, 0x31,
			0xdf, 0x71, 0xfd, 0x92, 0xfc, 0x40, 0x04, 0x80,
			0x11, 0x37, 0x79, 0x76, 0xe8, 0xd5, 0x22, 0xa9,
			0x48, 0x34, 0xbd, 0xaf, 0x24, 0xaa, 0xf5, 0xa3,
			0x84, 0xfe, 0x8b, 0x44, 0x6c, 0x9e, 0x97, 0xc5,
			0x8a, 0xa1, 0x66, 0x04, 0xa9, 0x45, 0xab, 0x0c,
			0x3f, 0x48, 0xac, 0xc9, 0x78, 0x28, 0x5d, 0xc8,
			0x4b, 0x69, 0x6b, 0xf7, 0x49, 0x32, 0xbb, 0x76,
			0xf1, 0x2f, 0x3a, 0x6f, 0xac, 0xc4, 0x9a, 0x4b,
			0x1e, 0xaf, 0xe4, 0x83, 0x62, 0x01, 0xa4, 0x7a,
			0xd4, 0xb6, 0x0d, 0x9e, 0x21, 0x07, 0xeb, 0xe1,
			0xda, 0xe8, 0xd2, 0x5e, 0x64, 0xbc, 0x1a, 0x4c,
			0x2b, 0x9f, 0xe0, 0x5f, 0x1c, 0xcc, 0xe4, 0x6e,
			0xd8, 0xa8, 0xf2, 0x55, 0x9b, 0x1b, 0x0d, 0x25,
			0x78, 0xcd, 0x6d, 0x7d, 0xd1, 0x59, 0xeb, 0x03,
			0x13, 0x2e, 0x4c, 0x51, 0x84, 0x19, 0x01, 0x2b,
			0x82, 0x62, 0x32, 0x25, 0x0c, 0xbe, 0x91, 0x11,
			0xc8, 0xf8, 0x9b, 0xe9, 0xad, 0x8b, 0xea, 0x25,
			0xd3, 0x7c, 0x7f, 0xb7, 0x45, 0x40, 0xd2, 0xe8,
		]
		// vfmt on
	},
	Block{
		block_type: 'CERTIFICATE'
		// vfmt off
		data: [
			u8(0x30), 0x82, 0x02, 0x33, 0x30, 0x82, 0x01, 0x9c,
			0xa0, 0x03, 0x02, 0x01, 0x02, 0x02, 0x09, 0x00,
			0xb8, 0x8f, 0x9d, 0x5b, 0x2f, 0xab, 0xc7, 0x6c,
			0x30, 0x0d, 0x06, 0x09, 0x2a, 0x86, 0x48, 0x86,
			0xf7, 0x0d, 0x01, 0x01, 0x05, 0x05, 0x00, 0x30,
			0x53, 0x31, 0x0b, 0x30, 0x09, 0x06, 0x03, 0x55,
			0x04, 0x06, 0x13, 0x02, 0x55, 0x53, 0x31, 0x0c,
			0x30, 0x0a, 0x06, 0x03, 0x55, 0x04, 0x08, 0x13,
			0x03, 0x66, 0x6f, 0x6f, 0x31, 0x0c, 0x30, 0x0a,
			0x06, 0x03, 0x55, 0x04, 0x07, 0x13, 0x03, 0x66,
			0x6f, 0x6f, 0x31, 0x0c, 0x30, 0x0a, 0x06, 0x03,
			0x55, 0x04, 0x0a, 0x13, 0x03, 0x66, 0x6f, 0x6f,
			0x31, 0x0c, 0x30, 0x0a, 0x06, 0x03, 0x55, 0x04,
			0x0b, 0x13, 0x03, 0x66, 0x6f, 0x6f, 0x31, 0x0c,
			0x30, 0x0a, 0x06, 0x03, 0x55, 0x04, 0x03, 0x13,
			0x03, 0x66, 0x6f, 0x6f, 0x30, 0x1e, 0x17, 0x0d,
			0x31, 0x33, 0x30, 0x33, 0x31, 0x39, 0x31, 0x35,
			0x34, 0x30, 0x31, 0x39, 0x5a, 0x17, 0x0d, 0x31,
			0x38, 0x30, 0x33, 0x31, 0x38, 0x31, 0x35, 0x34,
			0x30, 0x31, 0x39, 0x5a, 0x30, 0x53, 0x31, 0x0b,
			0x30, 0x09, 0x06, 0x03, 0x55, 0x04, 0x06, 0x13,
			0x02, 0x55, 0x53, 0x31, 0x0c, 0x30, 0x0a, 0x06,
			0x03, 0x55, 0x04, 0x08, 0x13, 0x03, 0x66, 0x6f,
			0x6f, 0x31, 0x0c, 0x30, 0x0a, 0x06, 0x03, 0x55,
			0x04, 0x07, 0x13, 0x03, 0x66, 0x6f, 0x6f, 0x31,
			0x0c, 0x30, 0x0a, 0x06, 0x03, 0x55, 0x04, 0x0a,
			0x13, 0x03, 0x66, 0x6f, 0x6f, 0x31, 0x0c, 0x30,
			0x0a, 0x06, 0x03, 0x55, 0x04, 0x0b, 0x13, 0x03,
			0x66, 0x6f, 0x6f, 0x31, 0x0c, 0x30, 0x0a, 0x06,
			0x03, 0x55, 0x04, 0x03, 0x13, 0x03, 0x66, 0x6f,
			0x6f, 0x30, 0x81, 0x9f, 0x30, 0x0d, 0x06, 0x09,
			0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01,
			0x01, 0x05, 0x00, 0x03, 0x81, 0x8d, 0x00, 0x30,
			0x81, 0x89, 0x02, 0x81, 0x81, 0x00, 0xcd, 0xd1,
			0x9f, 0xc6, 0x2f, 0x42, 0x35, 0xb3, 0x1f, 0xd5,
			0x45, 0x1c, 0xbc, 0x34, 0x21, 0xec, 0xc6, 0x01,
			0x3a, 0xf7, 0x88, 0x1f, 0x27, 0x34, 0x13, 0x42,
			0x88, 0x6e, 0x18, 0xca, 0xe4, 0x59, 0x02, 0x05,
			0x4e, 0x02, 0x89, 0x9a, 0xdb, 0x7c, 0x76, 0xa0,
			0x69, 0x6e, 0xd9, 0x11, 0xc3, 0x74, 0xb6, 0xdd,
			0xc4, 0xbe, 0x96, 0xce, 0x33, 0x1c, 0x5c, 0xfd,
			0x94, 0x14, 0xd9, 0x63, 0x7f, 0xb3, 0xfc, 0x0f,
			0xc6, 0x98, 0x74, 0xa5, 0x27, 0xec, 0x7c, 0x5b,
			0x13, 0x28, 0xce, 0x11, 0x85, 0x35, 0x83, 0xda,
			0xfe, 0x1f, 0x82, 0x4f, 0xb1, 0x66, 0x22, 0x9d,
			0xd6, 0x9d, 0x3e, 0x2f, 0x88, 0x53, 0xb6, 0x9c,
			0x27, 0x13, 0x0d, 0x2e, 0x5d, 0x15, 0x5d, 0xd2,
			0x48, 0xba, 0x9a, 0xec, 0x15, 0xe7, 0x2f, 0xcf,
			0xfb, 0x91, 0x47, 0x51, 0xe9, 0x5d, 0xab, 0xea,
			0x60, 0x4d, 0xc8, 0x33, 0xb8, 0xaf, 0x02, 0x03,
			0x01, 0x00, 0x01, 0xa3, 0x0f, 0x30, 0x0d, 0x30,
			0x0b, 0x06, 0x03, 0x55, 0x1d, 0x0f, 0x04, 0x04,
			0x03, 0x02, 0x02, 0xe4, 0x30, 0x0d, 0x06, 0x09,
			0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01,
			0x05, 0x05, 0x00, 0x03, 0x81, 0x81, 0x00, 0x98,
			0x83, 0x00, 0x0b, 0x58, 0xd2, 0x64, 0x10, 0x7a,
			0xec, 0x56, 0x77, 0x32, 0xd1, 0xcd, 0x79, 0xa3,
			0xbd, 0xd4, 0xae, 0x2f, 0xd1, 0xa8, 0xc9, 0x3d,
			0xc1, 0x17, 0xf8, 0xd2, 0x9e, 0x9c, 0x73, 0xe7,
			0xfe, 0x7c, 0x75, 0xb6, 0xbc, 0xa5, 0x67, 0xa7,
			0xbf, 0x7a, 0x63, 0xe0, 0xfd, 0xf0, 0xe4, 0x3a,
			0x0e, 0xb2, 0x8d, 0x2b, 0xce, 0x46, 0x72, 0xda,
			0x31, 0x2c, 0x12, 0x2b, 0x65, 0x54, 0xcd, 0xa6,
			0x2d, 0x26, 0xd0, 0xa4, 0xc3, 0x3e, 0x31, 0x45,
			0x50, 0x09, 0x0a, 0x22, 0x1b, 0xa5, 0xbd, 0xc6,
			0xe0, 0x85, 0xbb, 0x14, 0xab, 0xc0, 0xf2, 0x12,
			0xa4, 0x0a, 0x84, 0x02, 0xe5, 0x78, 0x0c, 0xda,
			0x31, 0x40, 0xa2, 0x96, 0x9a, 0x15, 0x6f, 0xf8,
			0x9d, 0x29, 0xb4, 0xc5, 0x83, 0xdb, 0x93, 0xe7,
			0xa7, 0x17, 0xaa, 0xe6, 0xc5, 0xf3, 0x82, 0x17,
			0xc9, 0x9e, 0xc9, 0x49, 0x95, 0x03, 0x43,
		]
		// vfmt on
	},
	Block{
		block_type: 'CERTIFICATE'
		// vfmt off
		data: [
			u8(0x30), 0x82, 0x02, 0x33, 0x30, 0x82, 0x01, 0x9c,
			0xa0, 0x03, 0x02, 0x01, 0x02, 0x02, 0x09, 0x00,
			0xb8, 0x8f, 0x9d, 0x5b, 0x2f, 0xab, 0xc7, 0x6c,
			0x30, 0x0d, 0x06, 0x09, 0x2a, 0x86, 0x48, 0x86,
			0xf7, 0x0d, 0x01, 0x01, 0x05, 0x05, 0x00, 0x30,
			0x53, 0x31, 0x0b, 0x30, 0x09, 0x06, 0x03, 0x55,
			0x04, 0x06, 0x13, 0x02, 0x55, 0x53, 0x31, 0x0c,
			0x30, 0x0a, 0x06, 0x03, 0x55, 0x04, 0x08, 0x13,
			0x03, 0x66, 0x6f, 0x6f, 0x31, 0x0c, 0x30, 0x0a,
			0x06, 0x03, 0x55, 0x04, 0x07, 0x13, 0x03, 0x66,
			0x6f, 0x6f, 0x31, 0x0c, 0x30, 0x0a, 0x06, 0x03,
			0x55, 0x04, 0x0a, 0x13, 0x03, 0x66, 0x6f, 0x6f,
			0x31, 0x0c, 0x30, 0x0a, 0x06, 0x03, 0x55, 0x04,
			0x0b, 0x13, 0x03, 0x66, 0x6f, 0x6f, 0x31, 0x0c,
			0x30, 0x0a, 0x06, 0x03, 0x55, 0x04, 0x03, 0x13,
			0x03, 0x66, 0x6f, 0x6f, 0x30, 0x1e, 0x17, 0x0d,
			0x31, 0x33, 0x30, 0x33, 0x31, 0x39, 0x31, 0x35,
			0x34, 0x30, 0x31, 0x39, 0x5a, 0x17, 0x0d, 0x31,
			0x38, 0x30, 0x33, 0x31, 0x38, 0x31, 0x35, 0x34,
			0x30, 0x31, 0x39, 0x5a, 0x30, 0x53, 0x31, 0x0b,
			0x30, 0x09, 0x06, 0x03, 0x55, 0x04, 0x06, 0x13,
			0x02, 0x55, 0x53, 0x31, 0x0c, 0x30, 0x0a, 0x06,
			0x03, 0x55, 0x04, 0x08, 0x13, 0x03, 0x66, 0x6f,
			0x6f, 0x31, 0x0c, 0x30, 0x0a, 0x06, 0x03, 0x55,
			0x04, 0x07, 0x13, 0x03, 0x66, 0x6f, 0x6f, 0x31,
			0x0c, 0x30, 0x0a, 0x06, 0x03, 0x55, 0x04, 0x0a,
			0x13, 0x03, 0x66, 0x6f, 0x6f, 0x31, 0x0c, 0x30,
			0x0a, 0x06, 0x03, 0x55, 0x04, 0x0b, 0x13, 0x03,
			0x66, 0x6f, 0x6f, 0x31, 0x0c, 0x30, 0x0a, 0x06,
			0x03, 0x55, 0x04, 0x03, 0x13, 0x03, 0x66, 0x6f,
			0x6f, 0x30, 0x81, 0x9f, 0x30, 0x0d, 0x06, 0x09,
			0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01,
			0x01, 0x05, 0x00, 0x03, 0x81, 0x8d, 0x00, 0x30,
			0x81, 0x89, 0x02, 0x81, 0x81, 0x00, 0xcd, 0xd1,
			0x9f, 0xc6, 0x2f, 0x42, 0x35, 0xb3, 0x1f, 0xd5,
			0x45, 0x1c, 0xbc, 0x34, 0x21, 0xec, 0xc6, 0x01,
			0x3a, 0xf7, 0x88, 0x1f, 0x27, 0x34, 0x13, 0x42,
			0x88, 0x6e, 0x18, 0xca, 0xe4, 0x59, 0x02, 0x05,
			0x4e, 0x02, 0x89, 0x9a, 0xdb, 0x7c, 0x76, 0xa0,
			0x69, 0x6e, 0xd9, 0x11, 0xc3, 0x74, 0xb6, 0xdd,
			0xc4, 0xbe, 0x96, 0xce, 0x33, 0x1c, 0x5c, 0xfd,
			0x94, 0x14, 0xd9, 0x63, 0x7f, 0xb3, 0xfc, 0x0f,
			0xc6, 0x98, 0x74, 0xa5, 0x27, 0xec, 0x7c, 0x5b,
			0x13, 0x28, 0xce, 0x11, 0x85, 0x35, 0x83, 0xda,
			0xfe, 0x1f, 0x82, 0x4f, 0xb1, 0x66, 0x22, 0x9d,
			0xd6, 0x9d, 0x3e, 0x2f, 0x88, 0x53, 0xb6, 0x9c,
			0x27, 0x13, 0x0d, 0x2e, 0x5d, 0x15, 0x5d, 0xd2,
			0x48, 0xba, 0x9a, 0xec, 0x15, 0xe7, 0x2f, 0xcf,
			0xfb, 0x91, 0x47, 0x51, 0xe9, 0x5d, 0xab, 0xea,
			0x60, 0x4d, 0xc8, 0x33, 0xb8, 0xaf, 0x02, 0x03,
			0x01, 0x00, 0x01, 0xa3, 0x0f, 0x30, 0x0d, 0x30,
			0x0b, 0x06, 0x03, 0x55, 0x1d, 0x0f, 0x04, 0x04,
			0x03, 0x02, 0x02, 0xe4, 0x30, 0x0d, 0x06, 0x09,
			0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01,
			0x05, 0x05, 0x00, 0x03, 0x81, 0x81, 0x00, 0x98,
			0x83, 0x00, 0x0b, 0x58, 0xd2, 0x64, 0x10, 0x7a,
			0xec, 0x56, 0x77, 0x32, 0xd1, 0xcd, 0x79, 0xa3,
			0xbd, 0xd4, 0xae, 0x2f, 0xd1, 0xa8, 0xc9, 0x3d,
			0xc1, 0x17, 0xf8, 0xd2, 0x9e, 0x9c, 0x73, 0xe7,
			0xfe, 0x7c, 0x75, 0xb6, 0xbc, 0xa5, 0x67, 0xa7,
			0xbf, 0x7a, 0x63, 0xe0, 0xfd, 0xf0, 0xe4, 0x3a,
			0x0e, 0xb2, 0x8d, 0x2b, 0xce, 0x46, 0x72, 0xda,
			0x31, 0x2c, 0x12, 0x2b, 0x65, 0x54, 0xcd, 0xa6,
			0x2d, 0x26, 0xd0, 0xa4, 0xc3, 0x3e, 0x31, 0x45,
			0x50, 0x09, 0x0a, 0x22, 0x1b, 0xa5, 0xbd, 0xc6,
			0xe0, 0x85, 0xbb, 0x14, 0xab, 0xc0, 0xf2, 0x12,
			0xa4, 0x0a, 0x84, 0x02, 0xe5, 0x78, 0x0c, 0xda,
			0x31, 0x40, 0xa2, 0x96, 0x9a, 0x15, 0x6f, 0xf8,
			0x9d, 0x29, 0xb4, 0xc5, 0x83, 0xdb, 0x93, 0xe7,
			0xa7, 0x17, 0xaa, 0xe6, 0xc5, 0xf3, 0x82, 0x17,
			0xc9, 0x9e, 0xc9, 0x49, 0x95, 0x03, 0x43,
		]
		// vfmt on
	},
]

const expected_rest = [
	'Extra stuff
This should be sent back in the second variable
I will now continue to spam my keyboard to add junk
lfajsdkfsndckasdjflkasdjclsdfjljjasldfj
alsdfsdkfasdfasfsdafasdcjaosdicnsad


fas90dfsdsdj
----------------------------------
-----
asddfadsfasjfsadf
-----BEGIN lkfajsdklfjasldkfjalsdjf;kasdflkjsaf
flajsdfkjasdfajskfjslkdfjaskdjf
FJASKDJjklfjasdfklsdjf
BEGIN BEGIN BEGIN
----BEGIN
-----END RSA PRIVATE KEY
: fkalsdjflkasdjf
private key: fsaddf',
	'
fdsjaf888888888888
-----

-----END
-----BEGIN',
	'YXMuIEVsZWlmZW5kIHF1YW0gYWRpcGlzY2luZyB2aXRh
ZSBwcm9pbiBzYWdpdHRpcyBuaXNsLgoKUGVsbGVudGVzcXVlIG
hh



Yml0YW50IG1vcmJpIHRyaXN0aXF1ZSBzZW5lY3R1cy4gRWdldCBudW5jIGxvYm9ydGlzIG1hdHRpcyBhb
GlxdWFtIGZhdWNpYnVzLiBOdWxsYW0g
dmVoaWN1bGEgaXBzdW0gYSBhcmN1IGN1cnN1cy4gUG9ydHRpdG9yIGVnZXQg
ZG9sb3IgbW9yYmkgbm9uIGFyY3Ugcml


zdXMgcXVpcyB2YXJpdXMgcXVhbS4gRGlnbmlzc2ltIGVuaW0gc2l0IGFtZXQgdmVuZW5hd
GlzLiBDb25zZWN0ZXR1ciBhIGVyYXQgbmFtIGF0IGxlY3R1cyB1cm5h
IGR1aXMuIEluIGZlcm1lbnR1bS
Bwb3N1ZXJlIHVybmEgbmVjIHRpbmNpZHVudCBwcmFlc2VudCBzZW1wZXIgZmV1Z2lhdCBuaWJoLiBOYW
0gYWxpcXVhbSBzZW0gZXQgdG9ydG9yIGNvbg==',
]
