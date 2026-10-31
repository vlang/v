# SOCKS5 connections

`net.socks` connects through SOCKS5 proxies, with optional username/password
authentication. Destinations may be IPv4 addresses or domain names, followed by a port.
IPv6 destinations are currently unsupported, including bracketed and scoped literals.
Attempting one returns `IPv6 SOCKS5 destinations are not supported` and closes the
connection to the proxy without sending a CONNECT request. Domain names made only of
hexadecimal letters are still encoded as domains.
