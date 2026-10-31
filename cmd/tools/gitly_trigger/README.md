## Gitly workflow JSON

`./v run cmd/tools/gitly_trigger/main.v encode <commit> <branch> <repo_path> <config_file>`
prints the workflow request payload. The configuration file must be readable.
`extract-id` reads a response from standard input and prints its positive run ID;
invalid responses return a nonzero status.
