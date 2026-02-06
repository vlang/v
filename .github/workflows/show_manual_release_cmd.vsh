import os
import time
import term

now := time.now()
year := now.year
week := now.strftime('%V')

mut remote_name := 'origin'
mut release_tag := 'weekly.${year:04}.${week:02}'

if os.args.len > 1 {
	remote_name = os.args[1]
}

if os.args.len > 2 {
	release_tag = os.args[2]
}

println('## Usage: show_manual_release_cmd.vsh [REMOTE] [TAGNAME]')
println('##         current remote_name: ${remote_name}')
println('##         current release_tag: ${release_tag}')
println('##  ▼▼▼ ${term.ecolorize(term.yellow, 'run the following, to make a new github release')} ▼▼▼ ')

git_cmd := 'git tag -s -m "releases: ${release_tag}" ${release_tag} && git push --atomic ${remote_name} ${release_tag}'
println(git_cmd)
