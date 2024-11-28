module vweb

import time

// Note: to use live reloading while developing, the suggested workflow is doing:
// `v -d vweb_livereload watch --keep run your_vweb_server_project.v`
// in one shell, then open the start page of your vweb app in a browser.
//
// While developing, just open your files and edit them, then just save your
// changes. Once you save, the watch command from above, will restart your server,
// and your HTML pages will detect that shortly, then they will refresh themselves
// automatically.

// vweb_livereload_server_start records, when the vweb server process started.
// That is later used by the /script.js and /current endpoints, which are active,
// if you have compiled your vweb project with `-d vweb_livereload`, to detect
// whether the web server has been restarted.
const vweb_livereload_server_start = time.ticks().str()

// handle_vweb_livereload_current serves a small text file, containing the
// timestamp/ticks corresponding to when the vweb server process was started
@[if vweb_livereload ?]
fn (mut ctx Context) handle_vweb_livereload_current() {
	ctx.send_response_to_client('text/plain', vweb_livereload_server_start)
}

// handle_vweb_livereload_script serves a small dynamically generated .js file,
// that contains code for polling the vweb server, and reloading the page, if it
// detects that the vweb server is newer than the vweb server, that served the
// .js file originally.
@[if vweb_livereload ?]
fn (mut ctx Context) handle_vweb_livereload_script() {
	res := '"use strict";
function vweb_livereload_checker_fn(started_at) {
    fetch("/vweb_livereload/" + started_at + "/current", { cache: "no-cache" })
        .then(response=>response.text())
        .then(function(current_at) {
            // console.log(started_at); console.log(current_at);
            if(started_at !== current_at){
                // the app was restarted on the server:
                window.location.reload();
            }
        });
}
const vweb_livereload_checker = setInterval(vweb_livereload_checker_fn, ${ctx.livereload_poll_interval_ms}, "${vweb_livereload_server_start}");
'
	ctx.send_response_to_client('text/javascript', res)
}
