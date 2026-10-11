import pathlib
import socket
import ssl
import sys


def read_line(conn):
    data = bytearray()
    while not data.endswith(b"\r\n"):
        try:
            chunk = conn.recv(1)
        except OSError:
            return bytes(data)
        if not chunk:
            break
        data.extend(chunk)
    return bytes(data)


def record(path, value):
    pathlib.Path(path).write_text(value)


def record_no_auth_unless_already_seen(path):
    if not pathlib.Path(path).exists():
        record(path, "NO_AUTH")


def serve(mode, host, cert, key, ready_path, result_path):
    listener = socket.create_server((host, 0), backlog=1)
    listener.settimeout(10)
    ready = pathlib.Path(ready_path)
    temporary_ready = ready.with_suffix(ready.suffix + ".tmp")
    temporary_ready.write_text(str(listener.getsockname()[1]))
    temporary_ready.replace(ready)
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.load_cert_chain(certfile=cert, keyfile=key)
    conn, _ = listener.accept()
    conn.settimeout(5)
    try:
        if mode == "starttls":
            conn.sendall(b"220 localhost ESMTP ready\r\n")
            if not read_line(conn).startswith(b"EHLO "):
                record(result_path, "ERROR: expected EHLO before STARTTLS")
                return
            conn.sendall(b"250-localhost\r\n250-STARTTLS\r\n250 OK\r\n")
            if read_line(conn).strip().upper() != b"STARTTLS":
                record(result_path, "ERROR: expected STARTTLS")
                return
            conn.sendall(b"220 Ready to start TLS\r\n")
        tls_conn = context.wrap_socket(conn, server_side=True)
        if mode == "implicit":
            tls_conn.sendall(b"220 localhost ESMTP ready\r\n")
            if not read_line(tls_conn).startswith(b"EHLO "):
                record_no_auth_unless_already_seen(result_path)
                return
            tls_conn.sendall(b"250-localhost\r\n250 OK\r\n")
        auth = read_line(tls_conn)
        if not auth:
            record(result_path, "NO_AUTH")
            return
        if not auth.startswith(b"AUTH"):
            record(result_path, f"UNEXPECTED: {auth.hex()}")
            return
        record(result_path, "AUTH")
        if not auth.startswith(b"AUTH PLAIN "):
            return
        tls_conn.sendall(b"235 Authentication successful\r\n")
        if read_line(tls_conn).strip().upper() == b"QUIT":
            tls_conn.sendall(b"221 Bye\r\n")
    except (OSError, TimeoutError, ssl.SSLError):
        record_no_auth_unless_already_seen(result_path)
    finally:
        listener.close()


if __name__ == "__main__":
    serve(*sys.argv[1:])
