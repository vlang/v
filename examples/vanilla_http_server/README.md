# Vanilla

Vanilla is a raw V server.

## Description

This project is a simple server written in the V programming language.
It aims to provide a minimalistic and efficient server implementation.

## Features

- Lightweight and fast
- Minimal dependencies
- Easy to understand and extend

## Installation

To install Vanilla, you need to have the V compiler installed.
You can download it from the [official V website](https://vlang.io).

## Usage

To run the server, use the following command:

```sh
v -prod crun .
```

This will start the server, and you can access it at `http://localhost:3000`.

## Code Overview

### Main Server

The main server logic is implemented in [src/main.v](v/vanilla/src/main.v).
The server is initialized and started in the `main` function:

```v ignore
module main

const port = 3000

fn main() {
	mut server := Server{
		router: setup_router()
	}

	server.socket_fd = create_server_socket(port)
	if server.socket_fd < 0 {
		return
	}
	server.epoll_fd = C.epoll_create1(0)
	if server.epoll_fd < 0 {
		C.perror('epoll_create1 failed'.str)
		C.close(server.socket_fd)
		return
	}

	server.lock_flag.lock()
	if add_fd_to_epoll(server.epoll_fd, server.socket_fd, u32(C.EPOLLIN)) == -1 {
		C.close(server.socket_fd)
		C.close(server.epoll_fd)

		server.lock_flag.unlock()
		return
	}

	server.lock_flag.unlock()

	server.lock_flag.init()
	for i := 0; i < 16; i++ {
		server.threads[i] = spawn process_events(&server)
	}
	println('listening on http://localhost:${port}/')
	event_loop(&server)
}
```

## Test

### CURL

```sh
curl -X GET --verbose http://localhost:3000/ &&
curl -X POST --verbose http://localhost:3000/user &&
curl -X GET --verbose http://localhost:3000/user/1

```

### WRK

```sh
wrk --connection 512 --threads 16 --duration 10s http://localhost:3000
```

### Valgrind
```sh
# Race condition check
v -prod -gc none . 
valgrind --tool=helgrind ./vanilla_http_server
```
