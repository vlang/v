module fasthttp

#flag windows -lws2_32
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <windows.h>
#include <ws2tcpip.h>

// net also declares accept() for POSIX file descriptors. Keep this wrapper
// module-specific so its pointer-sized Windows SOCKET return type is preserved.
#define v_fasthttp_accept(s) accept((s), NULL, NULL)

type C.DWORD = u32
type C.ULONG_PTR = usize
type C.SOCKET = usize

const iocp_invalid_socket = C.SOCKET(C.INVALID_SOCKET)

@[callconv: 'stdcall']
type IocpTransmitFileFn = fn (C.SOCKET, voidptr, C.DWORD, C.DWORD, &C.OVERLAPPED, voidptr, C.DWORD) int

@[typedef]
struct C.GUID {
mut:
	Data1 u32
	Data2 u16
	Data3 u16
	Data4 [8]u8
}

@[typedef]
struct C.OVERLAPPED {
mut:
	Internal     usize
	InternalHigh usize
	Offset       u32
	OffsetHigh   u32
	hEvent       voidptr
}

@[typedef]
struct C.WSABUF {
mut:
	len u32
	buf &char
}

fn C.WSASocketW(af i32, typ i32, protocol i32, lpProtocolInfo voidptr, g u32, dwFlags u32) C.SOCKET
fn C.WSARecv(s C.SOCKET, lpBuffers &C.WSABUF, dwBufferCount C.DWORD, lpNumberOfBytesRecvd &C.DWORD, lpFlags &C.DWORD, lpOverlapped &C.OVERLAPPED, lpCompletionRoutine voidptr) i32
fn C.WSASend(s C.SOCKET, lpBuffers &C.WSABUF, dwBufferCount C.DWORD, lpNumberOfBytesSent &C.DWORD, dwFlags C.DWORD, lpOverlapped &C.OVERLAPPED, lpCompletionRoutine voidptr) i32
fn C.WSAIoctl(s C.SOCKET, dwIoControlCode C.DWORD, lpvInBuffer voidptr, cbInBuffer C.DWORD, lpvOutBuffer voidptr, cbOutBuffer C.DWORD, lpcbBytesReturned &C.DWORD, lpOverlapped voidptr, lpCompletionRoutine voidptr) i32
fn C.WSAGetLastError() i32
fn C.v_fasthttp_accept(s C.SOCKET) C.SOCKET
fn C.bind(s C.SOCKET, addr voidptr, addrlen int) i32
fn C.listen(s C.SOCKET, backlog int) i32
fn C.send(s C.SOCKET, buf voidptr, len int, flags i32) i32
fn C.setsockopt(s C.SOCKET, level i32, optname i32, optval voidptr, optlen int) i32
fn C.shutdown(s C.SOCKET, how i32) i32
fn C.CreateIoCompletionPort(file_handle voidptr, existing_completion_port voidptr, completion_key C.ULONG_PTR, number_of_concurrent_threads C.DWORD) voidptr
fn C.GetQueuedCompletionStatus(completion_port voidptr, lp_number_of_bytes_transferred &C.DWORD, lp_completion_key &C.ULONG_PTR, lp_overlapped &&C.OVERLAPPED, dw_milliseconds C.DWORD) bool
fn C.PostQueuedCompletionStatus(completion_port voidptr, dw_number_of_bytes_transferred C.DWORD, dw_completion_key C.ULONG_PTR, lp_overlapped &C.OVERLAPPED) bool
fn C.CloseHandle(h_object voidptr) bool
fn C.closesocket(s C.SOCKET) i32
