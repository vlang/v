"""Local authoritative DNS fixture; no external network or system configuration changes."""
import ipaddress
import pathlib
import socket
import struct
import subprocess
import sys
import threading
import time


def encoded(name):
    return b"".join(bytes([len(part)]) + part.encode("ascii") for part in name.split(".")) + b"\0"


def question(packet):
    offset, parts = 12, []
    while packet[offset]:
        size = packet[offset]
        offset += 1
        parts.append(packet[offset:offset + size].decode("ascii"))
        offset += size
    offset += 1
    kind, class_ = struct.unpack_from("!HH", packet, offset)
    assert class_ == 1
    return ".".join(parts), kind, packet[12:offset + 4]


def response(query, tcp=False):
    name, kind, section = question(query)
    identifier = query[:2]
    flags, answers = 0x8180, []

    def record(owner, typ, data, declared=None):
        address = b"\xc0\x0c" if owner == name else encoded(owner)
        return address + struct.pack("!HHIH", typ, 1, 30, len(data) if declared is None else declared) + data

    def address(owner, v4, v6=None):
        if kind == 1:
            answers.append(record(owner, 1, ipaddress.ip_address(v4).packed))
        elif v6:
            answers.append(record(owner, 28, ipaddress.ip_address(v6).packed))

    if kind == 12:
        if name == "201.2.0.192.in-addr.arpa":
            answers.append(record(name, 12, encoded("reverse.example.test")))
        elif name == "202.2.0.192.in-addr.arpa":
            answers.append(record(name, 5, encoded("reverse-alias.example.test")))
        elif name == "reverse-alias.example.test":
            answers.append(record(name, 12, encoded("reverse-chain.example.test")))
        elif name == "203.2.0.192.in-addr.arpa":
            if not tcp:
                flags |= 0x0200
            else:
                answers.append(record(name, 12, encoded("reverse-tcp.example.test")))
        elif name == ipaddress.ip_address("2001:db8::201").reverse_pointer:
            answers.append(record(name, 12, encoded("reverse6.example.test")))
        elif name == "204.2.0.192.in-addr.arpa":
            offset = 12 + len(section) + 2 + 10
            answers.append(record(name, 12, struct.pack("!H", 0xc000 | offset)))
        elif name == "206.2.0.192.in-addr.arpa":
            answers.append(record(name, 5, encoded(name)))
        elif name == "207.2.0.192.in-addr.arpa":
            answers.append(record(name, 12, encoded("reverse.example.test"), declared=99))
        elif name == "208.2.0.192.in-addr.arpa":
            flags |= 2
        else:
            flags |= 3
    elif name == "a.example.test":
        address(name, "192.0.2.11")
    elif name == "dual.example.test":
        address(name, "192.0.2.12", "2001:db8::12")
    elif name == "search.example.test":
        address(name, "192.0.2.13")
    elif name == "cname.example.test":
        answers.append(record(name, 5, encoded("canonical.example.test")))
        address("canonical.example.test", "192.0.2.14")
    elif name == "chain.example.test":
        answers.append(record(name, 5, encoded("chain2.example.test")))
    elif name == "chain2.example.test":
        address(name, "192.0.2.15")
    elif name == "truncated.example.test":
        if not tcp:
            flags |= 0x0200
        else:
            address(name, "192.0.2.16")
    elif name == "wrongid.example.test":
        address(name, "192.0.2.17")
    elif name == "wrongquestion.example.test":
        address(name, "192.0.2.18")
    elif name == "servfail.example.test":
        flags |= 2
    elif name == "timeout.example.test":
        return None
    elif name == "malformed.example.test":
        answers.append(record(name, 1, b"\x01\x02", declared=4))
    elif name == "badlength.example.test":
        address(name, "192.0.2.19")
        answers.append(record(name, 1, b"\x01\x02\x03"))
    elif name == "cycle.example.test":
        offset = 12 + len(section) + 2 + 10
        answers.append(record(name, 5, struct.pack("!H", 0xc000 | offset)))
    else:
        flags |= 3
    return identifier + struct.pack("!HHHHH", flags, 1, len(answers), 0, 0) + section + b"".join(answers)


def exact(connection, size):
    parts = []
    while size:
        block = connection.recv(size)
        if not block:
            raise EOFError("short DNS TCP query")
        size -= len(block)
        parts.append(block)
    return b"".join(parts)


def main():
    work = pathlib.Path(sys.argv[1])
    work.mkdir(parents=True, exist_ok=True)
    (work / "hosts").write_text(
        "192.0.2.9 canonical.host unit-alias mixedalias\n"
        "2001:db8::9 canonical.host unit-alias mixedalias\n"
        + "".join(f"192.0.2.{i} many.example.test\n" for i in range(130)), encoding="ascii")
    (work / "services").write_text(
        "webtest 4242/tcp webalias\nwebtest 4242/udp webalias\nudp-only 4343/udp\n", encoding="ascii")
    (work / "resolv.conf").write_text(
        "nameserver 127.0.0.1\nsearch example.test\noptions ndots:1 timeout:1 attempts:1\n", encoding="ascii")
    udp = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    udp.bind(("127.0.0.1", 0))
    port = udp.getsockname()[1]
    tcp = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    tcp.bind(("127.0.0.1", port))
    tcp.listen(8)
    udp.settimeout(0.1)
    tcp.settimeout(0.1)
    stopped = threading.Event()
    observed, errors = [], []

    def serve_udp():
        while not stopped.is_set():
            try:
                packet, peer = udp.recvfrom(4096)
            except socket.timeout:
                continue
            try:
                name, kind, _ = question(packet)
                observed.append((name, kind, False))
                reply = response(packet)
                if reply is None:
                    continue
                if name == "wrongid.example.test":
                    udp.sendto(bytes([reply[0] ^ 1]) + reply[1:], peer)
                if name == "wrongquestion.example.test":
                    other = packet[:12] + encoded("unrelated.example.test") + struct.pack("!HH", kind, 1)
                    udp.sendto(response(other), peer)
                udp.sendto(reply, peer)
            except Exception as error:
                errors.append(repr(error))

    def serve_tcp():
        while not stopped.is_set():
            try:
                connection, _ = tcp.accept()
            except socket.timeout:
                continue
            try:
                with connection:
                    connection.settimeout(2)
                    size, = struct.unpack("!H", exact(connection, 2))
                    packet = exact(connection, size)
                    name, kind, _ = question(packet)
                    observed.append((name, kind, True))
                    reply = response(packet, tcp=True)
                    prefix = struct.pack("!H", len(reply))
                    connection.sendall(prefix[:1])
                    time.sleep(0.002)
                    connection.sendall(prefix[1:] + reply[:7])
                    time.sleep(0.002)
                    connection.sendall(reply[7:])
            except Exception as error:
                errors.append(repr(error))

    threads = [threading.Thread(target=serve_udp), threading.Thread(target=serve_tcp)]
    for thread in threads:
        thread.start()
    configured = 0
    try:
        for family, *_ in socket.getaddrinfo(None, "80", socket.AF_UNSPEC, socket.SOCK_STREAM,
                                             0, socket.AI_ADDRCONFIG | socket.AI_PASSIVE):
            configured |= 1 if family == socket.AF_INET else 2
    except socket.gaierror:
        pass
    try:
        process = subprocess.run(sys.argv[2:] + [str(work), str(port), str(configured)],
                                 capture_output=True, text=True, timeout=60)
        sys.stdout.write(process.stdout)
        sys.stderr.write(process.stderr)
        process.check_returncode()
        if errors:
            raise RuntimeError(f"DNS fixture server failed: {errors}")
        expected = {("truncated.example.test", 1, True),
                    ("dual.example.test", 1, False), ("dual.example.test", 28, False),
                    ("203.2.0.192.in-addr.arpa", 12, True),
                    ("reverse-alias.example.test", 12, False),
                    ("search.example.test", 1, False)}
        missing = expected.difference(observed)
        if missing:
            raise RuntimeError(f"missing DNS queries: {sorted(missing)}")
        print(f"DNS UDP/TCP fixture passed ({len(observed)} queries)")
    finally:
        stopped.set()
        for thread in threads:
            thread.join(timeout=3)
        udp.close()
        tcp.close()


if __name__ == "__main__":
    main()
