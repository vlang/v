module veb

import time

// Note: to use live reloading while developing, the suggested workflow is doing:
// `v -d veb_livereload watch --keep run your_veb_server_project.v`
// in one shell, then open the start page of your veb app in a browser.
//
// While developing, just open your files and edit them, then just save your
// changes. Once you save, the watch command from above, will restart your server,
// and your HTML pages will detect that shortly, then they will refresh themselves
// automatically.

// veb_livereload_server_start records, when the veb server process started.
// That is later used by the /script.js and /current endpoints, which are active,
// if you have compiled your veb project with `-d veb_livereload`, to detect
// whether the web server has been restarted.
const veb_livereload_server_start = time.ticks().str()

// handle_veb_livereload_current serves a small text file, containing the
// timestamp/ticks corresponding to when the veb server process was started
@[if veb_livereload ?]
fn (mut ctx Context) handle_veb_livereload_current() {
	ctx.send_response_to_client('text/plain', veb_livereload_server_start)
}

// handle_veb_livereload_script serves a small dynamically generated .js file,
// that contains code for polling the veb server, and reloading the page, if it
// detects that the veb server is newer than the veb server, that served the
// .js file originally.
@[if veb_livereload ?]
fn (mut ctx Context) handle_veb_livereload_script() {
	res := '"use strict";
function veb_livereload_checker_fn(started_at) {
	fetch("/veb_livereload/" + started_at + "/current", { cache: "no-cache" })
		.then((response) => response.text())
		.then(function (current_at) {
			// console.log(started_at); console.log(current_at);
			if (started_at !== current_at) {
				// the app was restarted on the server:
				window.location.reload();
			}
		});
}
const veb_livereload_checker = setInterval(veb_livereload_checker_fn, ${ctx.livereload_poll_interval_ms}, "${veb_livereload_server_start}");
'
	ctx.send_response_to_client('text/javascript', res)
}
