## Autobahn report checks

Run `./v run cmd/tools/check_autobahn_results/main.v <reports_dir>` to validate
`clients/index.json` and `servers/index.json`. Add `--clients-only` after the
report directory for the WSS client job. Missing or malformed reports and cases
with a failed behavior or close behavior return a nonzero status.
