module tls

import crypto.x509

// parse_certutil_output extracts every CERTIFICATE block from Windows
// certutil or PowerShell PEM output, skipping surrounding human-readable
// headers. It never fails hard: input without parsable certificates yields
// an empty set so callers can fall back to the embedded Mozilla bundle.
fn parse_certutil_output(text string) []x509.Certificate {
	return parse_bundle_pem(text)
}
