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

const connected = 220
const specify_password = 331
const logged_in = 230
const login_first = 503
const anonymous = 530
const open_data_connection = 150
const close_data_connection = 226
const command_ok = 200
const denied = 550
const passive_mode = 227
const complete = 226

struct DTP {
mut:
	conn   &net.TcpConn = unsafe { nil }
	reader io.BufferedReader
	ip     string
	port   int
}

fn (mut dtp DTP) read() ![]u8 {
	mut data := []u8{}
	mut buf := []u8{len: 1024}
	for {
		len := dtp.reader.read(mut buf) or { break }
		if len == 0 {
			break
		}
		data << buf[..len]
	}
	return data
}

fn (mut dtp DTP) close() {
	dtp.conn.close() or { panic(err) }
}

struct FTP {
mut:
	conn        &net.TcpConn = unsafe { nil }
	reader      io.BufferedReader
	buffer_size int
}

// new returns an `FTP` instance.
pub fn new() FTP {
	mut f := FTP{
		conn: 0
	}
	f.buffer_size = 1024
	return f
}

fn (mut zftp FTP) write(data string) !int {
	$if debug {
		println('FTP.v >>> ${data}')
	}
	return zftp.conn.write('${data}\r\n'.bytes())
}

fn (mut zftp FTP) read() !(int, string) {
	mut data := zftp.reader.read_line()!
	$if debug {
		println('FTP.v <<< ${data}')
	}
	if data.len < 5 {
		return 0, ''
	}
	code := data[..3].int()
	if data[3] == `-` {
		for {
			data = zftp.reader.read_line()!
			if data[..3].int() == code && data[3] != `-` {
				break
			}
		}
	}
	return code, data
}

// connect establishes an FTP connection to the host at `oaddress` (ip:port).
pub fn (mut zftp FTP) connect(oaddress string) !bool {
	zftp.conn = net.dial_tcp(oaddress)!
	zftp.reader = io.new_buffered_reader(reader: zftp.conn)
	code, _ := zftp.read()!
	if code == connected {
		return true
	}
	return false
}

// login sends the "USER `user`" and "PASS `passwd`" commands to the remote host.
pub fn (mut zftp FTP) login(user string, passwd string) !bool {
	zftp.write('USER ${user}') or {
		$if debug {
			println('ERROR sending user')
		}
		return false
	}
	mut code, _ := zftp.read()!
	if code == logged_in {
		return true
	}
	if code != specify_password {
		return false
	}
	zftp.write('PASS ${passwd}') or {
		$if debug {
			println('ERROR sending password')
		}
		return false
	}
	code, _ = zftp.read()!
	if code == logged_in {
		return true
	}
	return false
}

// close closes the FTP connection.
pub fn (mut zftp FTP) close() ! {
	zftp.write('QUIT')!
	zftp.conn.close()!
}

// pwd returns the current working directory on the remote host for the logged in user.
pub fn (mut zftp FTP) pwd() !string {
	zftp.write('PWD')!
	_, data := zftp.read()!
	spl := data.split('"') // "
	if spl.len >= 2 {
		return spl[1]
	}
	return data
}

// cd changes the current working directory to the specified remote directory `dir`.
pub fn (mut zftp FTP) cd(dir string) ! {
	zftp.write('CWD ${dir}') or { return }
	mut code, mut data := zftp.read()!
	match int(code) {
		denied {
			$if debug {
				println('CD ${dir} denied!')
			}
		}
		complete {
			code, data = zftp.read()!
		}
		else {}
	}
	$if debug {
		println('CD ${data}')
	}
}

fn new_dtp(msg string) !&DTP {
	if !is_dtp_message_valid(msg) {
		return error('Bad message')
	}
	ip, port := get_host_ip_from_dtp_message(msg)
	mut dtp := &DTP{
		ip:   ip
		port: port
		conn: 0
	}
	conn := net.dial_tcp('${ip}:${port}') or { return error('Cannot connect to the data channel') }
	dtp.conn = conn
	dtp.reader = io.new_buffered_reader(reader: dtp.conn)
	return dtp
}

fn (mut zftp FTP) pasv() !&DTP {
	zftp.write('PASV')!
	code, data := zftp.read()!
	$if debug {
		println('pass: ${data}')
	}
	if code != passive_mode {
		return error('passive mode not allowed')
	}
	dtp := new_dtp(data)!
	return dtp
}

// dir returns a list of the files in the current working directory.
pub fn (mut zftp FTP) dir() ![]string {
	mut dtp := zftp.pasv() or { return error('Cannot establish data connection') }
	zftp.write('LIST')!
	code, _ := zftp.read()!
	if code == denied {
		return error('`LIST` denied')
	}
	if code != open_data_connection {
		return error('Data channel empty')
	}
	list_dir := dtp.read()!
	result, _ := zftp.read()!
	if result != close_data_connection {
		println('`LIST` not ok')
	}
	dtp.close()
	mut dir := []string{}
	sdir := list_dir.bytestr()
	for lfile in sdir.split('\n') {
		if lfile.len > 56 {
			dir << lfile#[56..lfile.len - 1]
			continue
		}
		if lfile.len > 1 {
			trimmed := lfile.after(':')
			dir << trimmed#[3..trimmed.len - 1]
			continue
		}
	}
	return dir
}

// get retrieves `file` from the remote host.
pub fn (mut zftp FTP) get(file string) ![]u8 {
	mut dtp := zftp.pasv() or { return error('Cannot stablish data connection') }
	zftp.write('RETR ${file}')!
	code, _ := zftp.read()!
	if code == denied {
		return error('Permission denied')
	}
	if code != open_data_connection {
		return error('Data connection not ready')
	}
	blob := dtp.read()!
	dtp.close()
	return blob
}

fn is_dtp_message_valid(msg string) bool {
	// An example of message:
	// '227 Entering Passive Mode (209,132,183,61,48,218)'
	return msg.contains('(') && msg.contains(')') && msg.contains(',')
}

fn get_host_ip_from_dtp_message(msg string) (string, int) {
	mut par_start_idx := -1
	mut par_end_idx := -1
	for i, c in msg {
		if c == `(` {
			par_start_idx = i + 1
		} else if c == `)` {
			par_end_idx = i
		}
	}
	data := msg[par_start_idx..par_end_idx].split(',')
	ip := data[0..4].join('.')
	port := data[4].int() * 256 + data[5].int()
	return ip, port
}
