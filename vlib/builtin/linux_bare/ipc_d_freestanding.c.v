module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_ipc_abi.h"
	fn C.v_ipc_file_ids(uint, &u64, &u64) i32

	@[export: 'v_ipc_semget'; markused]
	fn bare_ipc_semget(key i32, count i32, flags i32) i32 {
		return i32(bare_native_result(sys_call3(u64(C.SYS_semget), u64(key), u64(count), u64(flags))))
	}

	@[export: 'v_ipc_semctl'; markused]
	fn bare_ipc_semctl(id i32, number i32, command i32, argument uint) i32 {
		return i32(bare_native_result(sys_call4(u64(C.SYS_semctl), u64(id), u64(number), u64(command), u64(argument))))
	}

	@[export: 'v_ipc_semop'; markused]
	fn bare_ipc_semop(id i32, operations uint, count uint) i32 {
		return i32(bare_native_result(sys_call3(u64(C.SYS_semop), u64(id), u64(operations), u64(count))))
	}

	@[export: 'v_ipc_semtimedop'; markused]
	fn bare_ipc_semtimedop(id i32, operations uint, count uint, timeout uint) i32 {
		return i32(bare_native_result(sys_call4(u64(C.SYS_semtimedop), u64(id), u64(operations), u64(count), u64(timeout))))
	}

	@[export: 'v_ipc_shmget'; markused]
	fn bare_ipc_shmget(key i32, size uint, flags i32) i32 {
		return i32(bare_native_result(sys_call3(u64(C.SYS_shmget), u64(key), u64(size), u64(flags))))
	}

	@[export: 'v_ipc_shmat'; markused]
	fn bare_ipc_shmat(id i32, address uint, flags i32) uint {
		return uint(bare_native_result(sys_call3(u64(C.SYS_shmat), u64(id), u64(address), u64(flags))))
	}

	@[export: 'v_ipc_shmdt'; markused]
	fn bare_ipc_shmdt(address uint) i32 {
		return i32(bare_native_result(sys_call1(u64(C.SYS_shmdt), u64(address))))
	}

	@[export: 'v_ipc_shmctl'; markused]
	fn bare_ipc_shmctl(id i32, command i32, data uint) i32 {
		return i32(bare_native_result(sys_call3(u64(C.SYS_shmctl), u64(id), u64(command), u64(data))))
	}

	@[export: 'v_ipc_msgget'; markused]
	fn bare_ipc_msgget(key i32, flags i32) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_msgget), u64(key), u64(flags))))
	}

	@[export: 'v_ipc_msgsnd'; markused]
	fn bare_ipc_msgsnd(id i32, message uint, size uint, flags i32) i32 {
		return i32(bare_native_result(u64(bare_cancel_syscall(i64(C.SYS_msgsnd), uint(id), message, size, uint(flags), 0, 0))))
	}

	@[export: 'v_ipc_msgrcv'; markused]
	fn bare_ipc_msgrcv(id i32, message uint, size uint, kind i64, flags i32) i64 {
		return bare_native_result(u64(bare_cancel_syscall(i64(C.SYS_msgrcv), uint(id), message, size, uint(kind), uint(flags), 0)))
	}

	@[export: 'v_ipc_msgctl'; markused]
	fn bare_ipc_msgctl(id i32, command i32, data uint) i32 {
		return i32(bare_native_result(sys_call3(u64(C.SYS_msgctl), u64(id), u64(command), u64(data))))
	}

	@[export: 'v_ipc_ftok'; markused]
	fn bare_ipc_ftok(path uint, project i32) i32 {
		mut device := u64(0)
		mut inode := u64(0)
		if C.v_ipc_file_ids(path, &device, &inode) < 0 { return -1 }
		return i32((u32(project) & 255) << 24 | (u32(device) & 255) << 16 | (u32(inode) & 65535))
	}
}
