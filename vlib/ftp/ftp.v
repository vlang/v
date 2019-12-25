/*
	basic ftp module
	RFC-959
	https://tools.ietf.org/html/rfc959

	Methods:
	ftp.connect(host)
	ftp.login(user,passw)
	pwd := ftp.pwd()
	ftp.cd(folder)
	dtp := ftp.pasv()
	ftp.dir()
	ftp.get(file)
	dtp.read()
	dtp.close()
	ftp.close()
*/

module ftp

import net

const (
	CONNECTED = 220
	SPECIFY_PASSWORD = 331
	LOGGED_IN = 230
	LOGIN_FIRST = 503
	ANONYMOUS = 530
	OPEN_DATA_CONNECTION = 150
	CLOSING_DATA_CONNECTION = 226
	COMMAND_OK = 200
	DENIED = 550
	PASSIVE_MODE = 227
	COMPLETE = 226
)


pub struct DTP {
mut:
	sock net.Socket
	ip string
	port int
}

fn (dtp DTP) read() string {
	mut data := ''
	mut buf := ''
	for {
		ptr,len := dtp.sock.recv(1024)
		if len == 0 { break }

		buf = string{
			str: ptr
			len: len
		}

		data += buf
	}

	return data
}

fn (dtp DTP) close() {
	dtp.sock.close() or {}
}

pub struct FTP {
mut:
	sock net.Socket
	buff_sz int
	dbg bool
}

fn new() FTP {
	mut f := FTP{}
	f.buff_sz = 1024
	return f
}

fn (ftp mut FTP) debug() {
	ftp.dbg = !ftp.dbg
}

fn (ftp FTP) write(data string) ?int {
	if ftp.dbg {
		println('FTP.v >>> $data')
	}
	n := ftp.sock.send_string(data + '\n') or {
		return error('cannot send data')
	}
	return n
}

fn (ftp FTP) read2() string {
	ptr,len := ftp.sock.recv(ftp.buff_sz)
	mut data := ''
	data = string{
		str: ptr
		len: len
	}
	return data
}

fn (ftp FTP) read() (int,string) {
	mut data := ftp.sock.read_line()
	if ftp.dbg {
		println('FTP.v <<< $data')
	}

	if data.len < 5 {
		return 0,''
	}

	code := data[0..3].int()
	if data[4] == `-` {
		for {
			data = ftp.sock.read_line()
			if data[0..3].int() == code {
				break
			}
		}
	}

	return code,data
}

fn (ftp mut FTP) connect(ip string) bool {
	sock := net.dial(ip, 21) or {
		return false
	}
	ftp.sock = sock

	code,_ := ftp.read()
	if code == CONNECTED {
		return true
	}

	return false
}

fn (ftp FTP) login(user, passwd string) bool {

	ftp.write('USER '+user) or {
		println('ERROR sending user')
		return false
	}

	mut data := ''
	mut code := 0

	code,data = ftp.read()
	if code == LOGGED_IN {
		return true
	}

	if code != SPECIFY_PASSWORD {
		return false
	}

	ftp.write('PASS '+passwd) or {
		println('ERROR sending password')
		return false
	}

	code,data = ftp.read()

	if code == LOGGED_IN {
		return true
	}

	return false
}

fn (ftp FTP) close() {
	send_quit := 'QUIT\r\n'
	ftp.sock.send_string(send_quit) or {}
	ftp.sock.close() or {}
}

fn (ftp FTP) pwd() string {
	ftp.write('PWD') or {
		return ''
	}
	_,data := ftp.read()
	return data
}

fn (ftp FTP) cd(dir string) {
	ftp.write('CWD '+dir) or { return }
	mut code, mut data := ftp.read()
	if code == DENIED {
		println("CD $dir denied!")
	}
	if code == COMPLETE {
		code,data = ftp.read()
	}
	println('cd $data')
}

fn new_dtp(msg string) ?DTP {
	// it receives a control message 227 like: 
	// '227 Entering Passive Mode (209,132,183,61,48,218)'

	t := msg.split('(')[1].split(')')[0].split(',')
	ip := t[0]+'.'+t[1]+'.'+t[2]+'.'+t[3]
	port := t[4].int()*256+t[5].int()

	sock := net.dial(ip, port) or {
		return error('Cant connect to the data channel')
	}

	dtp := DTP {
		sock : sock 
		ip: ip
		port: port
	}
	return dtp
}

fn (ftp FTP) pasv() ?DTP {
	ftp.write('PASV') or {}
	code,data := ftp.read()
	println("pass: $data")

	if code != PASSIVE_MODE {
		return error('pasive mode not allowed')
	}

	dtp := new_dtp(data)

	return dtp
}

fn (ftp FTP) dir() string {
	ftp.write('LIST') or {}
	code,data := ftp.read()
	if code == DENIED {
		println("LIST denied!")
	}
	if code == OPEN_DATA_CONNECTION {
		println('receiving directory list, open data channel')
	}
	return data
}

fn (ftp FTP)  get(file string) ?string {
	dtp := ftp.pasv() or {
		return error('cant stablish data connection')
	}

	ftp.write('RETR $file') or {}
	code,data := ftp.read()

	if code == DENIED {
		println('denied')
		return error('permission denied')
	}

	if code != OPEN_DATA_CONNECTION {
		println('not open data connection')
		return error('data connection not ready')
	}

	mut blob := ''
	mut buff := ' '
	for buff.len > 0 {
		buff = dtp.read()
		blob += buff
	}

	dtp.close()

	println('$blob.len bytes read')

	return blob
}
