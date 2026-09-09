module main

import os
import time
import term

fn iso_week_year(t time.Time) int {
	// ISO 8601 assigns a date to the year of the Thursday in its week.
	return t.add_days(4 - t.day_of_week()).year
}

fn default_release_tag_for(t time.Time) string {
	return 'weekly.${iso_week_year(t):04}.${t.week_of_year():02}'
}

fn release_time() time.Time {
	now_override := os.getenv('V_RELEASE_TAG_NOW')
	if now_override != '' {
		return time.parse(now_override) or {
			panic('invalid V_RELEASE_TAG_NOW `${now_override}`: ${err}')
		}
	}
	return time.now()
}

fn main() {
	now := release_time()
	mut remote_name := 'origin'
	mut release_tag := default_release_tag_for(now)

	if os.args.len > 1 {
		remote_name = os.args[1]
	}

	if os.args.len > 2 {
		release_tag = os.args[2]
	}

	println('## Usage: show_manual_release_cmd.vsh [REMOTE] [TAGNAME]')
	println('##         current remote_name: ${remote_name}')
	println('##         current release_tag: ${release_tag}')
	println('##  ▼▼▼ ${term.ecolorize(term.yellow,
		'run the following, to make a new github release')} ▼▼▼ ')

	git_cmd := 'git tag -s -m "releases: ${release_tag}" ${release_tag} && git push --atomic ${remote_name} ${release_tag}'
	println(git_cmd)
}
