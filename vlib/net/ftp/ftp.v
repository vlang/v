module ftp

/*
basic ftp module
	RFC-959
	https://tools.ietf.org/html/rfc959

	Methods:
	ftp.connect(host)
	ftp.login(user, passw)
	pwd := ftp.pwd()
	ftp.cd(folder)
	dtp := ftp.pasv()
	ftp.dir()
	ftp.get(file)
	dtp.read()
	dtp.close()
	ftp.close()
*/
import net
import io

pub const (
	default_port = 21
)

const (
	opening_dtp           = 150
	connected             = 220
	connection_closing    = 221
	completed_dtp         = 226
	entering_passive_mode = 227
	logged_in             = 230
	ok_completed          = 250
	pathname_created      = 257
	specify_password      = 331
)

struct Response {
	code int
	msg  string
}

fn Response.parse(r string) !Response {
	code := r[..3]!.u16()
	msg := r#[4..]
	return Response{
		code: int(code)
		msg: msg
	}
}

struct DTP {
mut:
	conn   &net.TcpConn
	reader io.BufferedReader
}

fn DTP.new(ip string, port u16) !&DTP {
	conn := net.dial_tcp('${ip}:${port}')!
	mut dtp := &DTP{
		conn: conn
		reader: io.new_buffered_reader(reader: conn)
	}

	return dtp
}

fn (mut dtp DTP) read(buffer_size int) ![]u8 {
	if buffer_size <= 0 {
		return error('buffer_size must be > 0')
	}
	mut data := []u8{}
	mut buf := []u8{len: buffer_size}
	for {
		len := dtp.reader.read(mut buf) or { break }
		if len == 0 {
			break
		}
		data << buf[..len]
	}

	return data[..data.len - 2] // removes the '\r\n' delim
}

fn (mut dtp DTP) close() ! {
	dtp.conn.close()!
}

struct FTP {
mut:
	conn   &net.TcpConn
	reader io.BufferedReader
}

// new returns an `FTP` instance.
pub fn new(ip string, port u16) !&FTP {
	conn := net.dial_tcp('${ip}:${port}')!
	mut zftp := &FTP{
		conn: conn
		reader: io.new_buffered_reader(reader: conn)
	}

	res := zftp.read()!

	if res.code != ftp.connected {
		return error_with_code(res.msg, res.code)
	}
	return zftp
}

// login sends the "USER `user`" and "PASS `passwd`" commands to the remote host.
pub fn (mut zftp FTP) login(user string, password string) ! {
	zftp.write('USER ${user}')!
	res_u := zftp.read()!
	if res_u.code == ftp.logged_in {
		return
	}
	if res_u.code != ftp.specify_password {
		return error_with_code(res_u.msg, res_u.code)
	}

	zftp.write('PASS ${password}')!
	res_p := zftp.read()!

	if res_p.code != ftp.logged_in {
		return error_with_code(res_p.msg, res_p.code)
	}
}

// close closes the FTP connection.
pub fn (mut zftp FTP) close() ! {
	zftp.write('QUIT') or {}
	zftp.conn.close()!
	return
}

// pwd returns the current working directory on the remote host for the logged in user.
pub fn (mut zftp FTP) pwd() !string {
	zftp.write('PWD')!
	res := zftp.read()!
	if res.code != ftp.pathname_created {
		return error_with_code(res.msg, res.code)
	}

	if `"` !in res.msg.bytes() {
		return error('Unable to parse path')
	}
	after := res.msg.after_char(`"`)
	return after[..after.len - 1]!
}

fn (mut zftp FTP) read() !Response {
	data := zftp.reader.read_line()!
	$if debug { println('FTP.v <<< ${data}') }
	return Response.parse(data)!
}

fn (mut zftp FTP) write(data string) !int {
	$if debug { println('FTP.v >>> ${data}') }
	return zftp.conn.write_string('${data}\r\n')
}

fn get_host_ip_from_dtp_message(msg string) !(string, u16) {
	after_par := msg.after('(')
	data := after_par[..after_par.len - 2].split(',')
	if data.len < 6 {
		return error('Unable to parse message')
	}
	ip := data[..4].join('.')
	port := data[4].u16() * 256 + data[5].u16()
	return ip, port
}

fn (mut zftp FTP) pasv() !&DTP {
	zftp.write('PASV')!
	res := zftp.read()!
	if res.code != ftp.entering_passive_mode {
		return error_with_code(res.msg, res.code)
	}

	ip, port := get_host_ip_from_dtp_message(res.msg)!
	dtp := DTP.new(ip, port)!
	return dtp
}

// cd changes the current working directory to the specified remote directory `dir`.
pub fn (mut zftp FTP) cd(dir string) ! {
	zftp.write('CWD ${dir}') or { return }
	res := zftp.read()!

	if res.code != ftp.ok_completed {
		return error_with_code(res.msg, res.code)
	}
}

// dir returns a list of the files in the current working directory.
pub fn (mut zftp FTP) dir() ![]string {
	mut dtp := zftp.pasv()!
	zftp.write('NLST')!
	res := zftp.read()!

	if res.code != ftp.opening_dtp {
		return error_with_code(res.msg, res.code)
	}

	data := dtp.read(128)!.bytestr()

	end_res := zftp.read()!
	if end_res.code != ftp.completed_dtp {
		return error_with_code(end_res.msg, end_res.code)
	}
	dtp.close()!

	return data.split('\r\n')
}

// get retrieves `file` from the remote host.
pub fn (mut zftp FTP) get(file string) ![]u8 {
	mut dtp := zftp.pasv()!
	zftp.write('RETR ${file}')!
	res := zftp.read()!

	if res.code != ftp.opening_dtp {
		return error_with_code(res.msg, res.code)
	}

	blob := dtp.read(8192)!

	end_res := zftp.read()!
	if end_res.code != ftp.completed_dtp {
		return error_with_code(end_res.msg, end_res.code)
	}
	dtp.close()!
	return blob
}
