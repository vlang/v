// Hello HTTP is an example of how you can use the net module to create a very basic http server. 
// This is how vweb works behind the scenes.
// It simply listens to http requests on port 8080 and responds with "Hello, HTTP" or "Hello, $url_route".
import (
	net
	http
)

const (
  port = 8080
	HTTP_500 = 'HTTP/1.1 500 Internal Server Error\r\nContent-Type: text/plain\r\n\r\n500 Internal Server Error'
)

fn main(){
	listener := net.listen(port) or {panic("Failed to listen to port $port")}
	for {
		conn := listener.accept() or {panic("conn accept() failed.")}
		s := conn.read_line()
		if s == '' {
			conn.write(HTTP_500)
			conn.close()
			return
		}
    
		first_line := s.all_before('\n')
		vals := first_line.split(' ')
		if vals.len < 2 {
			println('no vals for http')
			conn.write(HTTP_500)
			conn.close()
			return
		}
    
		req := to_request(s, vals)
		
		mut name := "HTTP"
		if req.url.contains("/") {
			split := req.url.split("/")
			if split.len > 0 {
			name = req.url.split("/")[0]
			}
		}
    
		write_html(conn, "<h1>Hello, $name </h1>") 
		conn.close()
	}
}
fn write_html(conn net.Socket, html string){
		conn.write("HTTP/1.1 200 OK\r")
		conn.write("Content-Type: text/html")	
		conn.write("")
		conn.write(html)
}
fn print_request(req http.Request) {
		if req.url != "/favicon.ico" {
			println("Received request $req.url ($req.method) with headers:")
			println(req.headers)
		}
}
fn to_request(s string, vals []string) http.Request {
	return http.Request{
				headers: http.parse_headers(s.split_into_lines())
				ws_func: 0
				user_ptr: 0
				method: vals[0]
				url: vals[1]
		}
}
