The `has_modern_openssl` test build constraint requires OpenSSL 3.5.0 or newer.
The runner reads the version number from the second word of `openssl version` output.
Prerelease suffixes use the same numeric version threshold as direct compiler test runs.

Tool subprocess helpers accept literal argument arrays through `TestSession.exec`,
`TestSession.system_args`, and `build_v_args_failed`. Pass each path and option as
a separate element. The command string versions are deprecated with migration hints.

Each selected test gets its own temporary folder. If the runner cannot create that folder,
it reports a failed test with the path and filesystem error, and skips compilation for that file.

By default, `v test` keeps the V3 module cache under the incoming `VTMP` folder and exports
that location as `V3CACHE` before creating the session's nested temporary folder. Nested and
later sessions reuse that cache. Successful session cleanup removes the session folder while
leaving the shared cache available for subsequent runs.

An existing nonempty `V3CACHE` takes precedence, even with `V3_TEST_ISOLATE_CACHE=1`.
To isolate a session's cache, leave `V3CACHE` unset and set `V3_TEST_ISOLATE_CACHE=1`.
The compiler then uses an isolated cache inside the session's temporary folder.

Resume records replace the previous record only after the new contents are complete.
On Windows, saving retries temporary sharing errors for about half a second. A refused
replacement preserves the previous record and reports a failed test.
