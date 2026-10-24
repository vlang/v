module main

import os
import json2

struct Payload {
	repo_id     int
	commit_hash string
	branch      string
	repo_path   string
	yaml_config string
	callback_url string
}

struct TriggerResult {
	result struct {
		id int
	}
}

fn main() {
	if os.args.len >= 2 && os.args[1] == 'encode' {
		// args: encode <commit> <branch> <repo_path> <config_file>
		if os.args.len < 6 {
			eprintln('usage: gitly_trigger encode <commit> <branch> <repo_path> <config_file>')
			exit(1)
		}
		cfg := os.read_file(os.args[5]) or { '' }
		p := Payload{
			repo_id: 0
			commit_hash: os.args[2]
			branch: os.args[3]
			repo_path: os.args[4]
			yaml_config: cfg
			callback_url: ''
		}
		println(json2.encode(p))
	} else if os.args.len >= 2 && os.args[1] == 'extract-id' {
		// read from stdin
		input := os.get_raw_lines().join('\n')
		r := json2.decode[TriggerResult](input) or {
			eprintln('decode failed: ${err}')
			exit(1)
		}
		println(r.result.id)
	} else {
		eprintln('usage: gitly_trigger <encode|extract-id> ...')
		exit(1)
	}
}
