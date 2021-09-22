import net

conn := net.dial_tcp('google.com:80') ?
peer_addr := conn.peer_addr() ?
println('$peer_addr')
