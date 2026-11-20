module tls

import crypto.x509
import os

// winroots_ps_script exports every Trusted Root certificate (machine and
// user stores) as PEM blocks. Base64 with line breaks keeps each block
// parseable by parse_bundle_pem. Forward slashes work in PowerShell
// provider paths (verified), avoiding backslash escapes; `\$` keeps V
// from interpolating PowerShell's `$_`.
const winroots_ps_script = "Get-ChildItem Cert:/LocalMachine/Root, Cert:/CurrentUser/Root -ErrorAction SilentlyContinue | ForEach-Object { '-----BEGIN CERTIFICATE-----'; [Convert]::ToBase64String(\$_.RawData, 'InsertLineBreaks'); '-----END CERTIFICATE-----' }"

// load_windows_roots enumerates the Windows Trusted Root stores in pure V
// (no C interop): PowerShell PEM export first, certutil output scrape as a
// best-effort fallback. It never fails hard: an empty set means the caller
// must fall back to the embedded bundle.
fn load_windows_roots() []x509.Certificate {
	mut roots := []x509.Certificate{}
	for exe in ['powershell', 'pwsh'] {
		res := os.exec([exe, '-NoProfile', '-NonInteractive', '-Command', winroots_ps_script])
		if res.exit_code == 0 && res.output.len > 0 {
			roots << parse_certutil_output(res.output)
			if roots.len > 0 {
				return roots
			}
		}
	}
	res := os.exec(['certutil', '-store', 'Root'])
	if res.exit_code == 0 && res.output.len > 0 {
		roots << parse_certutil_output(res.output)
	}
	return roots
}
