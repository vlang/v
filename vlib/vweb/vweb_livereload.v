module vweb

import time

const vweb_livereload_server_start = time.ticks().str()

[if vweb_livereload ?]
pub fn (mut ctx Context) handle_vweb_livereload_current() {
	ctx.send_response_to_client('text/plain', vweb.vweb_livereload_server_start)
}

[if vweb_livereload ?]
pub fn (mut ctx Context) handle_vweb_livereload_script() {
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
const vweb_livereload_checker = setInterval(vweb_livereload_checker_fn, ${ctx.livereload_poll_interval_ms}, "${vweb.vweb_livereload_server_start}");
'
	ctx.send_response_to_client('text/javascript', res)
}
