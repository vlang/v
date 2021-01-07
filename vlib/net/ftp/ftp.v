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

const (
	connected             = 220
	specify_password      = 331
	logged_in             = 230
	login_first           = 503
	anonymous             = 530
	open_data_connection  = 150
	close_data_connection = 226
	command_ok            = 200
	denied                = 550
	passive_mode          = 227
	complete              = 226
)

struct DTP {
mut:
	conn   net.TcpConn
	reader io.BufferedReader
	ip     string
	port   int
}

fn (mut dtp DTP) read() ?[]byte {
	mut data := []byte{}
	mut buf := []byte{len: 1024}
	for {
		len := dtp.reader.read(mut buf) or { break }
		if len == 0 {
			break
		}
		data << buf[..len]
	}
	return data
}

fn (dtp DTP) close() {
	dtp.conn.close()
}

struct FTP {
mut:
	conn        net.TcpConn
	reader      io.BufferedReader
	buffer_size int
}

pub fn new() FTP {
	mut f := FTP{}
	f.buffer_size = 1024
	return f
}

fn (mut ftp FTP) write(data string) ? {
	$if debug {
		println('FTP.v >>> $data')
	}
	ftp.conn.write('$data\r\n'.bytes()) ?
}

fn (mut ftp FTP) read() ?(int, string) {
	mut data := ftp.reader.read_line() ?
	$if debug {
		println('FTP.v <<< $data')
	}
	if data.len < 5 {
		return 0, ''
	}
	code := data[..3].int()
	if data[3] == `-` {
		for {
			data = ftp.reader.read_line() ?
			if data[..3].int() == code && data[3] != `-` {
				break
			}
		}
	}
	return code, data
}

pub fn (mut ftp FTP) connect(ip string) ?bool {
	ftp.conn = net.dial_tcp('$ip:21') ?
	ftp.reader = io.new_buffered_reader(reader: io.make_reader(ftp.conn))
	code, _ := ftp.read() ?
	if code == connected {
		return true
	}
	return false
}

pub fn (mut ftp FTP) login(user string, passwd string) ?bool {
	ftp.write('USER $user') or {
		$if debug {
			println('ERROR sending user')
		}
		return false
	}
	mut code, _ := ftp.read() ?
	if code == logged_in {
		return true
	}
	if code != specify_password {
		return false
	}
	ftp.write('PASS $passwd') or {
		$if debug {
			println('ERROR sending password')
		}
		return false
	}
	code, _ = ftp.read() ?
	if code == logged_in {
		return true
	}
	return false
}

pub fn (mut ftp FTP) close() ? {
	ftp.write('QUIT') ?
	ftp.conn.close()
}

pub fn (mut ftp FTP) pwd() ?string {
	ftp.write('PWD') ?
	_, data := ftp.read() ?
	spl := data.split('"') // "
	if spl.len >= 2 {
		return spl[1]
	}
	return data
}

pub fn (mut ftp FTP) cd(dir string) ? {
	ftp.write('CWD $dir') or { return }
	mut code, mut data := ftp.read() ?
	match int(code) {
		denied {
			$if debug {
				println('CD $dir denied!')
			}
		}
		complete {
			code, data = ftp.read() ?
		}
		else {}
	}
	$if debug {
		println('CD $data')
	}
}

fn new_dtp(msg string) ?&DTP {
	if !is_dtp_message_valid(msg) {
		return error('Bad message')
	}
	ip, port := get_host_ip_from_dtp_message(msg)
	mut dtp := &DTP{
		ip: ip
		port: port
	}
	conn := net.dial_tcp('$ip:$port') or { return error('Cannot connect to the data channel') }
	dtp.conn = conn
	dtp.reader = io.new_buffered_reader(reader: io.make_reader(dtp.conn))
	return dtp
}

fn (mut ftp FTP) pasv() ?&DTP {
	ftp.write('PASV') ?
	code, data := ftp.read() ?
	$if debug {
		println('pass: $data')
	}
	if code != passive_mode {
		return error('pasive mode not allowed')
	}
	dtp := new_dtp(data) ?
	return dtp
}

pub fn (mut ftp FTP) dir() ?[]string {
	mut dtp := ftp.pasv() or { return error('Cannot establish data connection') }
	ftp.write('LIST') ?
	code, _ := ftp.read() ?
	if code == denied {
		return error('`LIST` denied')
	}
	if code != open_data_connection {
		return error('Data channel empty')
	}
	list_dir := dtp.read() ?
	result, _ := ftp.read() ?
	if result != close_data_connection {
		println('`LIST` not ok')
	}
	dtp.close()
	mut dir := []string{}
	sdir := list_dir.bytestr()
	for lfile in sdir.split('\n') {
		if lfile.len > 1 {
			spl := lfile.split(' ')
			dir << spl[spl.len - 1]
		}
	}
	return dir
}

pub fn (mut ftp FTP) get(file string) ?[]byte {
	mut dtp := ftp.pasv() or { return error('Cannot stablish data connection') }
	ftp.write('RETR $file') ?
	code, _ := ftp.read() ?
	if code == denied {
		return error('Permission denied')
	}
	if code != open_data_connection {
		return error('Data connection not ready')
	}
	blob := dtp.read() ?
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
