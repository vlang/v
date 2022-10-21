import net

conn := net.dial_tcp('[::1]:57000')!
peer_addr := conn.peer_addr()!
println('$peer_addr')
