module os

fn C.mkfifo(path &char, mode u32) i32

// fifo_server_nix creates the fifo node and opens it for reading, which
// blocks until a client opens the write end.
fn fifo_server_nix(path string, mode u32) !&Fifo {
	if C.mkfifo(&char(path.str), mode) != 0 {
		return error('os: cannot create fifo `${path}`')
	}
	f := open(path)!
	return &Fifo{
		file:      f
		is_server: true
	}
}

// fifo_client_nix opens the fifo for writing, which blocks until a server
// has it open for reading.
fn fifo_client_nix(path string) !&Fifo {
	f := open_file(path, 'w')!
	return &Fifo{
		file: f
	}
}
