# Named pipes


`fifo_server(path, mode)` creates a named pipe and blocks until a client connects.
`fifo_client(path)` opens an existing endpoint for writing; retry if the server has not created it.
A FIFO transfers bytes from client to server. Use a second pair for bidirectional communication.
`read` returns available bytes or an EOF error when the writer closes.
`write` returns the number of bytes written.
Both operations can complete with fewer bytes than requested. `close` releases an endpoint.
On POSIX, the caller removes the FIFO path after use; mode supplies its permission bits.
Clients never create or truncate ordinary files. Windows pipe names support Unicode.
