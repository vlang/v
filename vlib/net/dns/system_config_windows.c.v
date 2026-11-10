module dns

// Windows keeps DNS servers per network adapter and lists them with
// GetAdaptersAddresses. iphlpapi.dll is loaded at run time: the bundled TCC
// ships neither its header nor an import library for it.

fn C.LoadLibraryW(name &u16) voidptr
fn C.GetProcAddress(handle voidptr, procname &u8) voidptr
fn C.FreeLibrary(handle voidptr) bool

@[callconv: 'stdcall']
type GetAdaptersAddressesFn = fn (u32, u32, voidptr, voidptr, &u32) u32

const gaa_flag_skip_unicast = u32(0x1)
const gaa_flag_skip_anycast = u32(0x2)
const gaa_flag_skip_multicast = u32(0x4)
const gaa_flag_skip_friendly_name = u32(0x20)
const windows_error_buffer_overflow = u32(111)
const windows_error_no_data = u32(232)
const windows_if_oper_status_up = u32(1)

// AdapterAddresses mirrors the leading fields of IP_ADAPTER_ADDRESSES, which
// every version of that structure starts with. Only `next`,
// `first_dns_server_address` and `oper_status` are read.
struct AdapterAddresses {
	length                   u32
	if_index                 u32
	next                     &AdapterAddresses = unsafe { nil }
	adapter_name             &char             = unsafe { nil }
	first_unicast_address    voidptr
	first_anycast_address    voidptr
	first_multicast_address  voidptr
	first_dns_server_address &AdapterDnsServer = unsafe { nil }
	dns_suffix               &u16              = unsafe { nil }
	description              &u16              = unsafe { nil }
	friendly_name            &u16              = unsafe { nil }
	physical_address         [8]u8
	physical_address_length  u32
	flags                    u32
	mtu                      u32
	if_type                  u32
	oper_status              u32
}

// AdapterDnsServer mirrors IP_ADAPTER_DNS_SERVER_ADDRESS, whose address is a
// SOCKET_ADDRESS: a SOCKADDR pointer and its length.
struct AdapterDnsServer {
	length       u32
	reserved     u32
	next         &AdapterDnsServer = unsafe { nil }
	sockaddr     voidptr
	sockaddr_len i32
}

// system_config lists the DNS servers of the network adapters that are up,
// in the order Windows returns them, without duplicates. The timeout and the
// attempts keep their defaults.
fn system_config() !ResolvConf {
	// V does not apply `callconv` to function types yet, and on 32-bit x86 a
	// stdcall function called as cdecl leaves the stack unbalanced.
	$if i386 {
		return error('not supported on 32-bit x86 Windows')
	}
	library := C.LoadLibraryW('iphlpapi.dll'.to_wide())
	if library == unsafe { nil } {
		return error('cannot load iphlpapi.dll')
	}
	defer {
		C.FreeLibrary(library)
	}
	symbol := C.GetProcAddress(library, &u8(c'GetAdaptersAddresses'))
	if symbol == unsafe { nil } {
		return error('iphlpapi.dll has no GetAdaptersAddresses')
	}
	get_adapters_addresses := GetAdaptersAddressesFn(symbol)
	flags := gaa_flag_skip_unicast | gaa_flag_skip_anycast | gaa_flag_skip_multicast | gaa_flag_skip_friendly_name
	// Microsoft recommends starting with 15 KB; the size can still grow between
	// the call that reports it and the next one.
	mut size := u32(15 * 1024)
	mut buf := []u8{}
	mut status := windows_error_buffer_overflow
	for _ in 0 .. 3 {
		buf = []u8{len: int(size)}
		status = get_adapters_addresses(0, flags, unsafe { nil }, buf.data, &size)
		if status != windows_error_buffer_overflow {
			break
		}
	}
	if status == windows_error_no_data {
		return error('no network adapter')
	}
	if status != 0 {
		return error('GetAdaptersAddresses failed with error ${status}')
	}
	mut servers := []string{}
	mut adapter := unsafe { &AdapterAddresses(buf.data) }
	for adapter != unsafe { nil } {
		if adapter.oper_status == windows_if_oper_status_up {
			mut dns := adapter.first_dns_server_address
			for dns != unsafe { nil } {
				if dns.sockaddr != unsafe { nil } && dns.sockaddr_len > 0 {
					sockaddr := unsafe { (&u8(dns.sockaddr)).vbytes(dns.sockaddr_len) }
					if server := windows_sockaddr_server(sockaddr) {
						if server !in servers {
							servers << server
						}
					}
				}
				dns = dns.next
			}
		}
		adapter = adapter.next
	}
	if servers.len == 0 {
		return error('no DNS server on a network adapter that is up')
	}
	return ResolvConf{
		servers: servers
	}
}
