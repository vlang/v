module net

const is_windows = true

// Link to Winsock library
#flag -lws2_32
#include <winsock2.h>
#include <ws2tcpip.h>

// Constants that windows needs
pub const fionbio = C.FIONBIO
pub const msg_nosignal = 0
pub const msg_dontwait = 0

pub const error_ewouldblock = int(WsaError.wsaewouldblock)
pub const error_einprogress = int(WsaError.wsaeinprogress)
pub const error_eagain = int(WsaError.wsaewouldblock) // on windows, is also wsaewouldblock
pub const error_eintr = int(WsaError.wsaeintr)

const wsa_v22 = 0x202

// WsaError is all of the socket errors that WSA provides from WSAGetLastError
pub enum WsaError {
	// MessageId: WSAEINTR, A blocking operation was interrupted by a call to WSACancelBlockingCall.
	wsaeintr = 10004
	// MessageId: WSAEBADF, The file handle supplied is not valid.
	wsaebadf = 10009
	// MessageId: WSAEACCES, An attempt was made to access a socket in a way forbidden by its access permissions.
	wsaeacces = 10013
	// MessageId: WSAEFAULT, The system detected an invalid pointer address in attempting to use a pointer argument in a call.
	wsaefault = 10014
	// MessageId: WSAEINVAL, An invalid argument was supplied.
	wsaeinval = 10022
	// MessageId: WSAEMFILE, Too many open sockets.
	wsaemfile = 10024
	// MessageId: WSAEWOULDBLOCK, A non-blocking socket operation could not be completed immediately.
	wsaewouldblock = 10035
	// MessageId: WSAEINPROGRESS, A blocking operation is currently executing.
	wsaeinprogress = 10036
	// MessageId: WSAEALREADY, An operation was attempted on a non-blocking socket that already had an operation in progress.
	wsaealready = 10037
	// MessageId: WSAENOTSOCK, An operation was attempted on something that is not a socket.
	wsaenotsock = 10038
	// MessageId: WSAEDESTADDRREQ, A required address was omitted from an operation on a socket.
	wsaedestaddrreq = 10039
	// MessageId: WSAEMSGSIZE, A message sent on a datagram socket was larger than the internal message buffer or some other network limit, or the buffer used to receive a datagram into was smaller than the datagram itself.
	wsaemsgsize = 10040
	// MessageId: WSAEPROTOTYPE, A protocol was specified in the socket function call that does not support the semantics of the socket type requested.
	wsaeprototype = 10041
	// MessageId: WSAENOPROTOOPT, An unknown, invalid, or unsupported option or level was specified in a getsockopt or setsockopt call.
	wsaenoprotoopt = 10042
	// MessageId: WSAEPROTONOSUPPORT, The requested protocol has not been configured into the system, or no implementation for it exists.
	wsaeprotonosupport = 10043
	// MessageId: WSAESOCKTNOSUPPORT, The support for the specified socket type does not exist in this address family.
	wsaesocktnosupport = 10044
	// MessageId: WSAEOPNOTSUPP, The attempted operation is not supported for the type of object referenced.
	wsaeopnotsupp = 10045
	// MessageId: WSAEPFNOSUPPORT, The protocol family has not been configured into the system or no implementation for it exists.
	wsaepfnosupport = 10046
	// MessageId: WSAEAFNOSUPPORT, An address incompatible with the requested protocol was used.
	wsaeafnosupport = 10047
	// MessageId: WSAEADDRINUSE, Only one usage of each socket address (protocol/network address/port) is normally permitted.
	wsaeaddrinuse = 10048
	// MessageId: WSAEADDRNOTAVAIL, The requested address is not valid in its context.
	wsaeaddrnotavail = 10049
	// MessageId: WSAENETDOWN, A socket operation encountered a dead network.
	wsaenetdown = 10050
	// MessageId: WSAENETUNREACH, A socket operation was attempted to an unreachable network.
	wsaenetunreach = 10051
	// MessageId: WSAENETRESET, The connection has been broken due to keep-alive activity detecting a failure while the operation was in progress.
	wsaenetreset = 10052
	// MessageId: WSAECONNABORTED, An established connection was aborted by the software in your host machine.
	wsaeconnaborted = 10053
	// MessageId: WSAECONNRESET, An existing connection was forcibly closed by the remote host.
	wsaeconnreset = 10054
	// MessageId: WSAENOBUFS, An operation on a socket could not be performed because the system lacked sufficient buffer space or because a queue was full.
	wsaenobufs = 10055
	// MessageId: WSAEISCONN, A connect request was made on an already connected socket.
	wsaeisconn = 10056
	// MessageId: WSAENOTCONN, A request to send or receive data was disallowed because the socket is not connected and (when sending on a datagram socket using a sendto call) no address was supplied.
	wsaenotconn = 10057
	// MessageId: WSAESHUTDOWN, A request to send or receive data was disallowed because the socket had already been shut down in that direction with a previous shutdown call.
	wsaeshutdown = 10058
	// MessageId: WSAETOOMANYREFS, Too many references to some kernel object.
	wsaetoomanyrefs = 10059
	// MessageId: WSAETIMEDOUT, A connection attempt failed because the connected party did not properly respond after a period of time, or established connection failed because connected host has failed to respond.
	wsaetimedout = 10060
	// MessageId: WSAECONNREFUSED, No connection could be made because the target machine actively refused it.
	wsaeconnrefused = 10061
	// MessageId: WSAELOOP, Cannot translate name.
	wsaeloop = 10062
	// MessageId: WSAENAMETOOLONG, Name component or name was too long.
	wsaenametoolong = 10063
	// MessageId: WSAEHOSTDOWN, A socket operation failed because the destination host was down.
	wsaehostdown = 10064
	// MessageId: WSAEHOSTUNREACH, A socket operation was attempted to an unreachable host.
	wsaehostunreach = 10065
	// MessageId: WSAENOTEMPTY, Cannot remove a directory that is not empty.
	wsaenotempty = 10066
	// MessageId: WSAEPROCLIM, A Windows Sockets implementation may have a limit on the number of applications that may use it simultaneously.
	wsaeproclim = 10067
	// MessageId: WSAEUSERS, Ran out of quota.
	wsaeusers = 10068
	// MessageId: WSAEDQUOT, Ran out of disk quota.
	wsaedquot = 10069
	// MessageId: WSAESTALE, File handle reference is no longer available.
	wsaestale = 10070
	// MessageId: WSAEREMOTE, Item is not available locally.
	wsaeremote = 10071
	// MessageId: WSASYSNOTREADY, WSAStartup cannot function at this time because the underlying system it uses to provide network services is currently unavailable.
	wsasysnotready = 10091
	// MessageId: WSAVERNOTSUPPORTED, The Windows Sockets version requested is not supported.
	wsavernotsupported = 10092
	// MessageId: WSANOTINITIALISED, Either the application has not called WSAStartup, or WSAStartup failed.
	wsanotinitialised = 10093
	// MessageId: WSAEDISCON, Returned by WSARecv or WSARecvFrom to indicate the remote party has initiated a graceful shutdown sequence.
	wsaediscon = 10101
	// MessageId: WSAENOMORE, No more results can be returned by WSALookupServiceNext.
	wsaenomore = 10102
	// MessageId: WSAECANCELLED, A call to WSALookupServiceEnd was made while this call was still processing. The call has been canceled.
	wsaecancelled = 10103
	// MessageId: WSAEINVALIDPROCTABLE, The procedure call table is invalid.
	wsaeinvalidproctable = 10104
	// MessageId: WSAEINVALIDPROVIDER, The requested service provider is invalid.
	wsaeinvalidprovider = 10105
	// MessageId: WSAEPROVIDERFAILEDINIT, The requested service provider could not be loaded or initialized.
	wsaeproviderfailedinit = 10106
	// MessageId: WSASYSCALLFAILURE, A system call has failed.
	wsasyscallfailure = 10107
	// MessageId: WSASERVICE_NOT_FOUND, No such service is known. The service cannot be found in the specified name space.
	wsaservice_not_found = 10108
	// MessageId: WSATYPE_NOT_FOUND, The specified class was not found.
	wsatype_not_found = 10109
	// MessageId: WSA_E_NO_MORE, No more results can be returned by WSALookupServiceNext.
	wsa_e_no_more = 10110
	// MessageId: WSA_E_CANCELLED, A call to WSALookupServiceEnd was made while this call was still processing. The call has been canceled.
	wsa_e_cancelled = 10111
	// MessageId: WSAEREFUSED, A database query failed because it was actively refused.
	wsaerefused = 10112
	// MessageId: WSAHOST_NOT_FOUND, No such host is known.
	wsahost_not_found = 11001
	// MessageId: WSATRY_AGAIN, This is usually a temporary error during hostname resolution and means that the local server did not receive a response from an authoritative server.
	wsatry_again = 11002
	// MessageId: WSANO_RECOVERY, A non-recoverable error occurred during a database lookup.
	wsano_recovery = 11003
	// MessageId: WSANO_DATA, The requested name is valid, but no data of the requested type was found.
	wsano_data = 11004
	// MessageId: WSA_QOS_RECEIVERS, At least one reserve has arrived.
	wsa_qos_receivers = 11005
	// MessageId: WSA_QOS_SENDERS, At least one path has arrived.
	wsa_qos_senders = 11006
	// MessageId: WSA_QOS_NO_SENDERS, There are no senders.
	wsa_qos_no_senders = 11007
	// MessageId: WSA_QOS_NO_RECEIVERS, There are no receivers.
	wsa_qos_no_receivers = 11008
	// MessageId: WSA_QOS_REQUEST_CONFIRMED, Reserve has been confirmed.
	wsa_qos_request_confirmed = 11009
	// MessageId: WSA_QOS_ADMISSION_FAILURE, Error due to lack of resources.
	wsa_qos_admission_failure = 11010
	// MessageId: WSA_QOS_POLICY_FAILURE, Rejected for administrative reasons - bad credentials.
	wsa_qos_policy_failure = 11011
	// MessageId: WSA_QOS_BAD_STYLE, Unknown or conflicting style.
	wsa_qos_bad_style = 11012
	// MessageId: WSA_QOS_BAD_OBJECT, Problem with some part of the filterspec or providerspecific buffer in general.
	wsa_qos_bad_object = 11013
	// MessageId: WSA_QOS_TRAFFIC_CTRL_ERROR, Problem with some part of the flowspec.
	wsa_qos_traffic_ctrl_error = 11014
	// MessageId: WSA_QOS_GENERIC_ERROR, General QOS error.
	wsa_qos_generic_error = 11015
	// MessageId: WSA_QOS_ESERVICETYPE, An invalid or unrecognized service type was found in the flowspec.
	wsa_qos_eservicetype = 11016
	// MessageId: WSA_QOS_EFLOWSPEC, An invalid or inconsistent flowspec was found in the QOS structure.
	wsa_qos_eflowspec = 11017
	// MessageId: WSA_QOS_EPROVSPECBUF, Invalid QOS provider-specific buffer.
	wsa_qos_eprovspecbuf = 11018
	// MessageId: WSA_QOS_EFILTERSTYLE, An invalid QOS filter style was used.
	wsa_qos_efilterstyle = 11019
	// MessageId: WSA_QOS_EFILTERTYPE, An invalid QOS filter type was used.
	wsa_qos_efiltertype = 11020
	// MessageId: WSA_QOS_EFILTERCOUNT, An incorrect number of QOS FILTERSPECs were specified in the FLOWDESCRIPTOR.
	wsa_qos_efiltercount = 11021
	// MessageId: WSA_QOS_EOBJLENGTH, An object with an invalid ObjectLength field was specified in the QOS provider-specific buffer.
	wsa_qos_eobjlength = 11022
	// MessageId: WSA_QOS_EFLOWCOUNT, An incorrect number of flow descriptors was specified in the QOS structure.
	wsa_qos_eflowcount = 11023
	// MessageId: WSA_QOS_EUNKOWNPSOBJ, An unrecognized object was found in the QOS provider-specific buffer.
	wsa_qos_eunkownpsobj = 11024
	// MessageId: WSA_QOS_EPOLICYOBJ, An invalid policy object was found in the QOS provider-specific buffer.
	wsa_qos_epolicyobj = 11025
	// MessageId: WSA_QOS_EFLOWDESC, An invalid QOS flow descriptor was found in the flow descriptor list.
	wsa_qos_eflowdesc = 11026
	// MessageId: WSA_QOS_EPSFLOWSPEC, An invalid or inconsistent flowspec was found in the QOS provider specific buffer.
	wsa_qos_epsflowspec = 11027
	// MessageId: WSA_QOS_EPSFILTERSPEC, An invalid FILTERSPEC was found in the QOS provider-specific buffer.
	wsa_qos_epsfilterspec = 11028
	// MessageId: WSA_QOS_ESDMODEOBJ, An invalid shape discard mode object was found in the QOS provider specific buffer.
	wsa_qos_esdmodeobj = 11029
	// MessageId: WSA_QOS_ESHAPERATEOBJ, An invalid shaping rate object was found in the QOS provider-specific buffer.
	wsa_qos_eshaperateobj = 11030
	// MessageId: WSA_QOS_RESERVED_PETYPE, A reserved policy element was found in the QOS provider-specific buffer.
	wsa_qos_reserved_petype = 11031
	// MessageId: WSA_SECURE_HOST_NOT_FOUND, No such host is known securely.
	wsa_secure_host_not_found = 11032
	// MessageId: WSA_IPSEC_NAME_POLICY_ERROR, Name based IPSEC policy could not be added.
	wsa_ipsec_name_policy_error = 11033
}

// wsa_error casts an int to its WsaError value
pub fn wsa_error(code int) WsaError {
	return unsafe { WsaError(code) }
}

// Error code returns the last socket error
pub fn error_code() int {
	return C.WSAGetLastError()
}

pub struct C.WSAData {
mut:
	wVersion       u16
	wHighVersion   u16
	szDescription  [257]u8
	szSystemStatus [129]u8
	iMaxSockets    u16
	iMaxUdpDg      u16
	lpVendorInfo   &u8
}

fn init() {
	mut wsadata := C.WSAData{
		lpVendorInfo: 0
	}
	res := C.WSAStartup(wsa_v22, &wsadata)
	if res != 0 {
		panic('socket: WSAStartup failed')
	}
}
