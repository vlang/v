import log
import veb
import time
import net
import x.json2
import net.http

const port = 31228

pub struct Context {
	veb.Context
}

pub struct App {
pub mut:
	started chan bool
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

@['/data/:filename'; post]
fn (mut app App) data(mut ctx Context, filename string) veb.Result {
	content_type := ctx.get_header(http.CommonHeader.content_type) or { return ctx.no_content() }
	f := http.FileData{
		filename:     filename
		content_type: content_type
		data:         ctx.req.data
	}
	log.info('Received ${filename} with content_type ${content_type} and length ${f.data.len}')
	return ctx.json(f)
}

const svg_image_content = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="-8 -308 316 316" width="316" height="316"><g fill-opacity="0" stroke="#000" xmlns="http://www.w3.org/2000/svg"><path xmlns="http://www.w3.org/2000/svg" d="M0 0 l -1.8369701987210297e-14 -100 m -1.8369701987210297e-14 -100 l -1.8369701987210297e-14 -100 l 100 -2.4492935982947064e-14 m 100 -2.4492935982947064e-14 l 100 -2.4492935982947064e-14 l 3.061616997868383e-14 100 m 3.061616997868383e-14 100 l 3.061616997868383e-14 100 l -100 3.6739403974420595e-14 m -100 3.6739403974420595e-14 l -100 3.6739403974420595e-14" stroke="#000000" stroke-width="5"></path></g></svg>'

fn test_veb_app_start() {
	log.info('starting watchdog ...')
	spawn fn () {
		log.info('watchdog running')
		time.sleep(10 * time.second)
		log.info('exiting...')
		exit(0)
	}()
	mut app := &App{}
	spawn veb.run_at[App, Context](mut app, port: port)
	_ := <-app.started
	log.info('app started')
}

fn test_make_request() {
	mut client := net.dial_tcp('127.0.0.1:${port}')!
	defer { client.close() or {} }
	client.write_string('POST /data/Seeker.svg HTTP/1.1\r
Host: localhost:8090\r
User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:136.0) Gecko/20100101 Firefox/136.0\r
Accept: */*\r
Accept-Language: en-US,en;q=0.5\r
Accept-Encoding: gzip, deflate, br, zstd\r
Content-Type: image/svg+xml\r
Content-Length: 618\r
Origin: null\r
Connection: close\r
Sec-Fetch-Dest: empty\r
Sec-Fetch-Mode: cors\r
Sec-Fetch-Site: cross-site\r
Priority: u=4\r
\r
')! // "

	time.sleep(25 * time.millisecond)
	client.write_string(svg_image_content)!
	mut res := []u8{}
	mut buf := []u8{len: 512}
	for {
		read_len := client.read(mut buf)!
		if read_len == 0 {
			break
		}
		res << buf[0..read_len]
	}
	response := res.bytestr()
	assert response.starts_with('HTTP/1.1 200 OK')
	assert response.contains('Content-Length: 706')
	assert response.contains('Content-Type: application/json')
	payload := response.all_after('\r\n\r\n')
	r := json2.decode[http.FileData](payload)!
	dump(r.filename)
	dump(r.content_type)
	assert r.filename == 'Seeker.svg'
	assert r.content_type == 'image/svg+xml'
	assert r.data.starts_with('<svg xmlns=')
	assert r.data.ends_with('</svg>')
	assert r.data == svg_image_content
}
