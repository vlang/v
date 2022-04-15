module builtin

pub enum Linux_mem {
	page_size = 4096
}

pub const (
	wp_sys_wnohang     = u64(0x00000001)
	wp_sys_wuntraced   = u64(0x00000002)
	wp_sys_wstopped    = u64(0x00000002)
	wp_sys_wexited     = u64(0x00000004)
	wp_sys_wcontinued  = u64(0x00000008)
	wp_sys_wnowait     = u64(0x01000000) // don't reap, just poll status.
	wp_sys___wnothread = u64(0x20000000) // don't wait on children of other threads in this group
	wp_sys___wall      = u64(0x40000000) // wait on all children, regardless of type
	wp_sys___wclone    = u64(0x80000000) // wait only on non-sigchld children
)

// First argument to waitid:
pub enum Wi_which {
	p_all = 0
	p_pid = 1
	p_pgid = 2
}

pub enum Wi_si_code {
	cld_exited = 1 // child has exited
	cld_killed = 2 // child was killed
	cld_dumped = 3 // child terminated abnormally
	cld_trapped = 4 // traced child has trapped
	cld_stopped = 5 // child has stopped
	cld_continued = 6 // stopped child has continued
}

/*
Paraphrased from "man 2 waitid" on Linux

	Upon successful return, waitid() fills in the
	following fields of the siginfo_t structure
	pointed to by infop:

	si_pid, offset 0x10, int index 0x04:
		The process ID of the child.

	si_uid: offset 0x14, int index 0x05
		The real user ID of the child.

	si_signo: offset 0x00, int index 0x00
		Always set to SIGCHLD.

	si_status: ofset 0x18, int index 0x06
		1 the exit status of the child, as given to _exit(2)
			(or exit(3)) (sc_sys.cld_exited)
		2 the signal that caused the child to terminate, stop,
			or continue.
		3 The si_code field can be used to determine how to
			interpret this field.

	si_code, set to one of (enum Wi_si_code), offset 0x08, int index 0x02:
		CLD_EXITED (child called _exit(2));
		CLD_KILLED (child killed by signal);
		CLD_DUMPED (child  killed by signal, and dumped core);
		CLD_STOPPED (child stopped by signal);
		CLD_TRAPPED (traced child has trapped);
		CLD_CONTINUED (child continued by SIGCONT).
*/

pub enum Sig_index {
	si_signo = 0x00
	si_code = 0x02
	si_pid = 0x04
	si_uid = 0x05
	si_status = 0x06
	si_size = 0x80
}

pub enum Signo {
	sighup = 1 // Hangup.
	sigint = 2 // Interactive attention signal.
	sigquit = 3 // Quit.
	sigill = 4 // Illegal instruction.
	sigtrap = 5 // Trace/breakpoint trap.
	sigabrt = 6 // Abnormal termination.
	sigbus = 7
	sigfpe = 8 // Erroneous arithmetic operation.
	sigkill = 9 // Killed.
	sigusr1 = 10
	sigsegv = 11 // Invalid access to storage.
	sigusr2 = 12
	sigpipe = 13 // Broken pipe.
	sigalrm = 14 // Alarm clock.
	sigterm = 15 // Termination request.
	sigstkflt = 16
	sigchld = 17
	sigcont = 18
	sigstop = 19
	sigtstp = 20
	sigttin = 21 // Background read from control terminal.
	sigttou = 22 // Background write to control terminal.
	sigurg = 23
	sigxcpu = 24 // CPU time limit exceeded.
	sigxfsz = 25 // File size limit exceeded.
	sigvtalrm = 26 // Virtual timer expired.
	sigprof = 27 // Profiling timer expired.
	sigwinch = 28
	sigpoll = 29
	sigsys = 31
}

pub const (
	fcntlf_dupfd         = 0x00000000
	fcntlf_exlck         = 0x00000004
	fcntlf_getfd         = 0x00000001
	fcntlf_getfl         = 0x00000003
	fcntlf_getlk         = 0x00000005
	fcntlf_getlk64       = 0x0000000c
	fcntlf_getown        = 0x00000009
	fcntlf_getowner_uids = 0x00000011
	fcntlf_getown_ex     = 0x00000010
	fcntlf_getsig        = 0x0000000b
	fcntlf_ofd_getlk     = 0x00000024
	fcntlf_ofd_setlk     = 0x00000025
	fcntlf_ofd_setlkw    = 0x00000026
	fcntlf_owner_pgrp    = 0x00000002
	fcntlf_owner_pid     = 0x00000001
	fcntlf_owner_tid     = 0x00000000
	fcntlf_rdlck         = 0x00000000
	fcntlf_setfd         = 0x00000002
	fcntlf_setfl         = 0x00000004
	fcntlf_setlk         = 0x00000006
	fcntlf_setlk64       = 0x0000000d
	fcntlf_setlkw        = 0x00000007
	fcntlf_setlkw64      = 0x0000000e
	fcntlf_setown        = 0x00000008
	fcntlf_setown_ex     = 0x0000000f
	fcntlf_setsig        = 0x0000000a
	fcntlf_shlck         = 0x00000008
	fcntlf_unlck         = 0x00000002
	fcntlf_wrlck         = 0x00000001
	fcntllock_ex         = 0x00000002
	fcntllock_mand       = 0x00000020
	fcntllock_nb         = 0x00000004
	fcntllock_read       = 0x00000040
	fcntllock_rw         = 0x000000c0
	fcntllock_sh         = 0x00000001
	fcntllock_un         = 0x00000008
	fcntllock_write      = 0x00000080
	fcntlo_accmode       = 0x00000003
	fcntlo_append        = 0x00000400
	fcntlo_cloexec       = 0x00080000
	fcntlo_creat         = 0x00000040
	fcntlo_direct        = 0x00004000
	fcntlo_directory     = 0x00010000
	fcntlo_dsync         = 0x00001000
	fcntlo_excl          = 0x00000080
	fcntlo_largefile     = 0x00008000
	fcntlo_ndelay        = 0x00000800
	fcntlo_noatime       = 0x00040000
	fcntlo_noctty        = 0x00000100
	fcntlo_nofollow      = 0x00020000
	fcntlo_nonblock      = 0x00000800
	fcntlo_path          = 0x00200000
	fcntlo_rdonly        = 0x00000000
	fcntlo_rdwr          = 0x00000002
	fcntlo_trunc         = 0x00000200
	fcntlo_wronly        = 0x00000001
)

pub enum Errno {
	enoerror = 0x00000000
	e2big = 0x00000007
	eacces = 0x0000000d
	eagain = 0x0000000b
	ebadf = 0x00000009
	ebusy = 0x00000010
	echild = 0x0000000a
	edom = 0x00000021
	eexist = 0x00000011
	efault = 0x0000000e
	efbig = 0x0000001b
	eintr = 0x00000004
	einval = 0x00000016
	eio = 0x00000005
	eisdir = 0x00000015
	emfile = 0x00000018
	emlink = 0x0000001f
	enfile = 0x00000017
	enodev = 0x00000013
	enoent = 0x00000002
	enoexec = 0x00000008
	enomem = 0x0000000c
	enospc = 0x0000001c
	enotblk = 0x0000000f
	enotdir = 0x00000014
	enotty = 0x00000019
	enxio = 0x00000006
	eperm = 0x00000001
	epipe = 0x00000020
	erange = 0x00000022
	erofs = 0x0000001e
	espipe = 0x0000001d
	esrch = 0x00000003
	etxtbsy = 0x0000001a
	exdev = 0x00000012
}

pub enum Mm_prot {
	prot_read = 0x1
	prot_write = 0x2
	prot_exec = 0x4
	prot_none = 0x0
	prot_growsdown = 0x01000000
	prot_growsup = 0x02000000
}

pub enum Map_flags {
	map_shared = 0x01
	map_private = 0x02
	map_shared_validate = 0x03
	map_type = 0x0f
	map_fixed = 0x10
	map_file = 0x00
	map_anonymous = 0x20
	map_huge_shift = 26
	map_huge_mask = 0x3f
}

fn sys_call0(scn u64) u64 {
	res := u64(0)
	asm amd64 {
		syscall
		; =a (res)
		; a (scn)
	}
	return res
}

fn sys_call1(scn u64, arg1 u64) u64 {
	res := u64(0)
	asm amd64 {
		syscall
		; =a (res)
		; a (scn)
		  D (arg1)
	}
	return res
}

fn sys_call2(scn u64, arg1 u64, arg2 u64) u64 {
	res := u64(0)
	asm amd64 {
		syscall
		; =a (res)
		; a (scn)
		  D (arg1)
		  S (arg2)
	}
	return res
}

fn sys_call3(scn u64, arg1 u64, arg2 u64, arg3 u64) u64 {
	res := u64(0)
	asm amd64 {
		syscall
		; =a (res)
		; a (scn)
		  D (arg1)
		  S (arg2)
		  d (arg3)
	}
	return res
}

fn sys_call4(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64) u64 {
	res := u64(0)
	asm amd64 {
		mov r10, arg4
		syscall
		; =a (res)
		; a (scn)
		  D (arg1)
		  S (arg2)
		  d (arg3)
		  r (arg4)
		; r10
	}
	return res
}

fn sys_call5(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64, arg5 u64) u64 {
	res := u64(0)
	asm amd64 {
		mov r10, arg4
		mov r8, arg5
		syscall
		; =a (res)
		; a (scn)
		  D (arg1)
		  S (arg2)
		  d (arg3)
		  r (arg4)
		  r (arg5)
		; r10
		  r8
	}
	return res
}

fn sys_call6(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64, arg5 u64, arg6 u64) u64 {
	res := u64(0)
	asm amd64 {
		mov r10, arg4
		mov r8, arg5
		mov r9, arg6
		syscall
		; =a (res)
		; a (scn)
		  D (arg1)
		  S (arg2)
		  d (arg3)
		  r (arg4)
		  r (arg5)
		  r (arg6)
		; r10
		  r8
		  r9
	}
	return res
}

fn split_int_errno(rc_in u64) (i64, Errno) {
	rc := i64(rc_in)
	if rc < 0 {
		return i64(-1), Errno(-rc)
	}
	return rc, Errno.enoerror
}

// 0 sys_read unsigned int fd char *buf size_t count
pub fn sys_read(fd i64, buf &byte, count u64) (i64, Errno) {
	return split_int_errno(sys_call3(0, u64(fd), u64(buf), count))
}

// 1 sys_write unsigned int fd, const char *buf, size_t count
pub fn sys_write(fd i64, buf &byte, count u64) (i64, Errno) {
	return split_int_errno(sys_call3(1, u64(fd), u64(buf), count))
}

pub fn sys_open(filename &byte, flags i64, mode int) (i64, Errno) {
	// 2 sys_open  const char *filename  int flags int mode
	return split_int_errno(sys_call3(2, u64(filename), u64(flags), u64(mode)))
}

pub fn sys_close(fd i64) Errno {
	// 3 sys_close unsigned int fd
	return Errno(-i64(sys_call1(3, u64(fd))))
}

// 9 sys_mmap unsigned long addr  unsigned long len unsigned long prot  unsigned long flags unsigned long fd  unsigned long off
pub fn sys_mmap(addr &byte, len u64, prot Mm_prot, flags Map_flags, fildes u64, off u64) (&byte, Errno) {
	rc := sys_call6(9, u64(addr), len, u64(prot), u64(flags), fildes, off)
	a, e := split_int_errno(rc)
	return &u8(a), e
}

pub fn sys_munmap(addr voidptr, len u64) Errno {
	// 11 sys_munmap  unsigned long addr  size_t len
	return Errno(-sys_call2(11, u64(addr), len))
}

// 22  sys_pipe  int *filedes
pub fn sys_pipe(filedes &int) Errno {
	return Errno(sys_call1(22, u64(filedes)))
}

// 24 sys_sched_yield
pub fn sys_sched_yield() Errno {
	return Errno(sys_call0(24))
}

pub fn sys_madvise(addr voidptr, len u64, advice int) Errno {
	// 28 sys_madvise unsigned long start size_t len_in int behavior
	return Errno(sys_call3(28, u64(addr), len, u64(advice)))
}

// 39 sys_getpid
pub fn sys_getpid() int {
	return int(sys_call0(39))
}

// 57 sys_fork
pub fn sys_fork() int {
	return int(sys_call0(57))
}

// 58 sys_vfork
pub fn sys_vfork() int {
	return int(sys_call0(58))
}

// 33  sys_dup2  unsigned int oldfd  unsigned int newfd
pub fn sys_dup2(oldfd int, newfd int) (i64, Errno) {
	return split_int_errno(sys_call2(33, u64(oldfd), u64(newfd)))
}

// 59  sys_execve  const char *filename  const char *const argv[]  const char *const envp[]
// pub fn sys_execve(filename byteptr, argv []u8ptr, envp []u8ptr) int {
//  return sys_call3(59, filename, argv, envp)
//}

// 60 sys_exit  int error_code
pub fn sys_exit(ec int) {
	sys_call1(60, u64(ec))
}

// 102 sys_getuid
pub fn sys_getuid() int {
	return int(sys_call0(102))
}

// 247 sys_waitid  int which pid_t upid  struct siginfo *infop int options struct rusage *ru
pub fn sys_waitid(which Wi_which, pid int, infop &int, options int, ru voidptr) Errno {
	return Errno(sys_call5(247, u64(which), u64(pid), u64(infop), u64(options), u64(ru)))
}

/*
A few years old, but still relevant
https://blog.rchapman.org/posts/Linux_System_Call_Table_for_x86_64/

>0 sys_read unsigned int fd char *buf size_t count
>1 sys_write unsigned int fd const char *buf size_t count
>2 sys_open  const char *filename  int flags int mode
>3 sys_close unsigned int fd
4 sys_stat  const char *filename  struct stat *statbuf
5 sys_fstat unsigned int fd struct stat *statbuf
6 sys_lstat fconst char *filename struct stat *statbuf
7 sys_poll  struct poll_fd *ufds  unsigned int nfds long timeout_msecs
8 sys_lseek unsigned int fd off_t offset  unsigned int origin
>9 sys_mmap unsigned long addr  unsigned long len unsigned long prot  unsigned long flags unsigned long fd  unsigned long off
10  sys_mprotect  unsigned long start size_t len  unsigned long prot
>11 sys_munmap  unsigned long addr  size_t len
12  sys_brk unsigned long brk
13  sys_rt_sigaction  int sig const struct sigaction *act struct sigaction *oact  size_t sigsetsize
14  sys_rt_sigprocmask  int how sigset_t *nset  sigset_t *oset  size_t sigsetsize
15  sys_rt_sigreturn  unsigned long __unused
16  sys_ioctl unsigned int fd unsigned int cmd  unsigned long arg
17  sys_pread64 unsigned long fd  char *buf size_t count  loff_t pos
18  sys_pwrite64  unsigned int fd const char *buf size_t count  loff_t pos
19  sys_readv unsigned long fd  const struct iovec *vec unsigned long vlen
20  sys_writev  unsigned long fd  const struct iovec *vec unsigned long vlen
21  sys_access  const char *filename  int mode
>22  sys_pipe  int *filedes
23  sys_select  int n fd_set *inp fd_set *outp  fd_set*exp  struct timeval *tvp
>24 sys_sched_yield
25  sys_mremap  unsigned long addr  unsigned long old_len unsigned long new_len unsigned long flags unsigned long new_addr
26  sys_msync unsigned long start size_t len  int flags
27  sys_mincore unsigned long start size_t len  unsigned char *vec
>28 sys_madvise unsigned long start size_t len_in int behavior
29  sys_shmget  key_t key size_t size int shmflg
30  sys_shmat int shmid char *shmaddr int shmflg
31  sys_shmctl  int shmid int cmd struct shmid_ds *buf
32  sys_dup unsigned int fildes
33  sys_dup2  unsigned int oldfd  unsigned int newfd
34  sys_pause
35  sys_nanosleep struct timespec *rqtp struct timespec *rmtp
36  sys_getitimer int which struct itimerval *value
37  sys_alarm unsigned int seconds
38  sys_setitimer int which struct itimerval *value struct itimerval *ovalue
>39 sys_getpid
40  sys_sendfile  int out_fd  int in_fd off_t *offset size_t count
41  sys_socket  int family  int type  int protocol
42  sys_connect int fd  struct sockaddr *uservaddr  int addrlen
43  sys_accept  int fd  struct sockaddr *upeer_sockaddr int *upeer_addrlen
44  sys_sendto  int fd  void *buff  size_t len  unsigned flags  struct sockaddr *addr int addr_len
45  sys_recvfrom  int fd  void *ubuf  size_t size unsigned flags  struct sockaddr *addr int *addr_len
46  sys_sendmsg int fd  struct msghdr *msg  unsigned flags
47  sys_recvmsg int fd  struct msghdr *msg  unsigned int flags
48  sys_shutdown  int fd  int how
49  sys_bind  int fd  struct sokaddr *umyaddr int addrlen
50  sys_listen  int fd  int backlog
51  sys_getsockname int fd  struct sockaddr *usockaddr  int *usockaddr_len
52  sys_getpeername int fd  struct sockaddr *usockaddr  int *usockaddr_len
53  sys_socketpair  int family  int type  int protocol  int *usockvec
54  sys_setsockopt  int fd  int level int optname char *optval  int optlen
55  sys_getsockopt  int fd  int level int optname char *optval  int *optlen
56  sys_clone unsigned long clone_flags unsigned long newsp void *parent_tid  void *child_tid
>57 sys_fork
>58 sys_vfork
>59 sys_execve  const char *filename  const char *const argv[]  const char *const envp[]
>60 sys_exit  int error_code
61  sys_wait4 pid_t upid  int *stat_addr  int options struct rusage *ru
62  sys_kill  pid_t pid int sig
63  sys_uname struct old_utsname *name
64  sys_semget  key_t key int nsems int semflg
65  sys_semop int semid struct sembuf *tsops  unsigned nsops
66  sys_semctl  int semid int semnum  int cmd union semun arg
67  sys_shmdt char *shmaddr
68  sys_msgget  key_t key int msgflg
69  sys_msgsnd  int msqid struct msgbuf *msgp size_t msgsz  int msgflg
70  sys_msgrcv  int msqid struct msgbuf *msgp size_t msgsz  long msgtyp int msgflg
71  sys_msgctl  int msqid int cmd struct msqid_ds *buf
72  sys_fcntl unsigned int fd unsigned int cmd  unsigned long arg
73  sys_flock unsigned int fd unsigned int cmd
74  sys_fsync unsigned int fd
75  sys_fdatasync unsigned int fd
76  sys_truncate  const char *path  long length
77  sys_ftruncate unsigned int fd unsigned long length
78  sys_getdents  unsigned int fd struct linux_dirent *dirent unsigned int count
79  sys_getcwd  char *buf unsigned long size
80  sys_chdir const char *filename
81  sys_fchdir  unsigned int fd
82  sys_rename  const char *oldname const char *newname
83  sys_mkdir const char *pathname  int mode
84  sys_rmdir const char *pathname
85  sys_creat const char *pathname  int mode
86  sys_link  const char *oldname const char *newname
87  sys_unlink  const char *pathname
88  sys_symlink const char *oldname const char *newname
89  sys_readlink  const char *path  char *buf int bufsiz
90  sys_chmod const char *filename  mode_t mode
91  sys_fchmod  unsigned int fd mode_t mode
92  sys_chown const char *filename  uid_t user  gid_t group
93  sys_fchown  unsigned int fd uid_t user  gid_t group
94  sys_lchown  const char *filename  uid_t user  gid_t group
95  sys_umask int mask
96  sys_gettimeofday  struct timeval *tv  struct timezone *tz
97  sys_getrlimit unsigned int resource struct rlimit *rlim
98  sys_getrusage int who struct rusage *ru
99  sys_sysinfo struct sysinfo *info
100 sys_times struct sysinfo *info
101 sys_ptrace  long request  long pid  unsigned long addr  unsigned long data
>102 sys_getuid
103 sys_syslog  int type  char *buf int len
104 sys_getgid
105 sys_setuid  uid_t uid
106 sys_setgid  gid_t gid
107 sys_geteuid
108 sys_getegid
109 sys_setpgid pid_t pid pid_t pgid
110 sys_getppid
111 sys_getpgrp
112 sys_setsid
113 sys_setreuid  uid_t ruid  uid_t euid
114 sys_setregid  gid_t rgid  gid_t egid
115 sys_getgroups int gidsetsize  gid_t *grouplist
116 sys_setgroups int gidsetsize  gid_t *grouplist
117 sys_setresuid uid_t *ruid uid_t *euid uid_t *suid
118 sys_getresuid uid_t *ruid uid_t *euid uid_t *suid
119 sys_setresgid gid_t rgid  gid_t egid  gid_t sgid
120 sys_getresgid gid_t *rgid gid_t *egid gid_t *sgid
121 sys_getpgid pid_t pid
122 sys_setfsuid  uid_t uid
123 sys_setfsgid  gid_t gid
124 sys_getsid  pid_t pid
125 sys_capget  cap_user_header_t header  cap_user_data_t dataptr
126 sys_capset  cap_user_header_t header  const cap_user_data_t data
127 sys_rt_sigpending sigset_t *set size_t sigsetsize
128 sys_rt_sigtimedwait const sigset_t *uthese  siginfo_t *uinfo  const struct timespec *uts  size_t sigsetsize
129 sys_rt_sigqueueinfo pid_t pid int sig siginfo_t *uinfo
130 sys_rt_sigsuspend sigset_t *unewset size_t sigsetsize
131 sys_sigaltstack const stack_t *uss  stack_t *uoss
132 sys_utime char *filename  struct utimbuf *times
133 sys_mknod const char *filename  umode_t mode  unsigned dev
134 sys_uselib  NOT IMPLEMENTED
135 sys_personality unsigned int personality
136 sys_ustat unsigned dev  struct ustat *ubuf
137 sys_statfs  const char *pathname  struct statfs *buf
138 sys_fstatfs unsigned int fd struct statfs *buf
139 sys_sysfs int option  unsigned long arg1  unsigned long arg2
140 sys_getpriority int which int who
141 sys_setpriority int which int who int niceval
142 sys_sched_setparam  pid_t pid struct sched_param *param
143 sys_sched_getparam  pid_t pid struct sched_param *param
144 sys_sched_setscheduler  pid_t pid int policy  struct sched_param *param
145 sys_sched_getscheduler  pid_t pid
146 sys_sched_get_priority_max  int policy
147 sys_sched_get_priority_min  int policy
148 sys_sched_rr_get_interval pid_t pid struct timespec *interval
149 sys_mlock unsigned long start size_t len
150 sys_munlock unsigned long start size_t len
151 sys_mlockall  int flags
152 sys_munlockall
153 sys_vhangup
154 sys_modify_ldt  int func  void *ptr unsigned long bytecount
155 sys_pivot_root  const char *new_root  const char *put_old
156 sys__sysctl struct __sysctl_args *args
157 sys_prctl int option  unsigned long arg2  unsigned long arg3  unsigned long arg4    unsigned long arg5
158 sys_arch_prctl  struct task_struct *task  int code  unsigned long *addr
159 sys_adjtimex  struct timex *txc_p
160 sys_setrlimit unsigned int resource struct rlimit *rlim
161 sys_chroot  const char *filename
162 sys_sync
163 sys_acct  const char *name
164 sys_settimeofday  struct timeval *tv  struct timezone *tz
165 sys_mount char *dev_name  char *dir_name  char *type  unsigned long flags void *data
166 sys_umount2 const char *target  int flags
167 sys_swapon  const char *specialfile int swap_flags
168 sys_swapoff const char *specialfile
169 sys_reboot  int magic1  int magic2  unsigned int cmd  void *arg
170 sys_sethostname char *name  int len
171 sys_setdomainname char *name  int len
172 sys_iopl  unsigned int level  struct pt_regs *regs
173 sys_ioperm  unsigned long from  unsigned long num int turn_on
174 sys_create_module REMOVED IN Linux 2.6
175 sys_init_module void *umod  unsigned long len const char *uargs
176 sys_delete_module const chat *name_user unsigned int flags
177 sys_get_kernel_syms REMOVED IN Linux 2.6
178 sys_query_module  REMOVED IN Linux 2.6
179 sys_quotactl  unsigned int cmd  const char *special qid_t id  void *addr
180 sys_nfsservctl  NOT IMPLEMENTED
181 sys_getpmsg NOT IMPLEMENTED
182 sys_putpmsg NOT IMPLEMENTED
183 sys_afs_syscall NOT IMPLEMENTED
184 sys_tuxcall NOT IMPLEMENTED
185 sys_security  NOT IMPLEMENTED
186 sys_gettid
187 sys_readahead int fd  loff_t offset size_t count
188 sys_setxattr  const char *pathname  const char *name  const void *value size_t size int flags
189 sys_lsetxattr const char *pathname  const char *name  const void *value size_t size int flags
190 sys_fsetxattr int fd  const char *name  const void *value size_t size int flags
191 sys_getxattr  const char *pathname  const char *name  void *value size_t size
192 sys_lgetxattr const char *pathname  const char *name  void *value size_t size
193 sys_fgetxattr int fd  const har *name void *value size_t size
194 sys_listxattr const char *pathname  char *list  size_t size
195 sys_llistxattr  const char *pathname  char *list  size_t size
196 sys_flistxattr  int fd  char *list  size_t size
197 sys_removexattr const char *pathname  const char *name
198 sys_lremovexattr  const char *pathname  const char *name
199 sys_fremovexattr  int fd  const char *name
200 sys_tkill pid_t pid ing sig
201 sys_time  time_t *tloc
202 sys_futex u32 *uaddr  int op  u32 val struct timespec *utime  u32 *uaddr2 u32 val3
203 sys_sched_setaffinity pid_t pid unsigned int len  unsigned long *user_mask_ptr
204 sys_sched_getaffinity pid_t pid unsigned int len  unsigned long *user_mask_ptr
205 sys_set_thread_area NOT IMPLEMENTED. Use arch_prctl
206 sys_io_setup  unsigned nr_events  aio_context_t *ctxp
207 sys_io_destroy  aio_context_t ctx
208 sys_io_getevents  aio_context_t ctx_id  long min_nr long nr struct io_event *events
209 sys_io_submit aio_context_t ctx_id  long nr struct iocb **iocbpp
210 sys_io_cancel aio_context_t ctx_id  struct iocb *iocb struct io_event *result
211 sys_get_thread_area NOT IMPLEMENTED. Use arch_prctl
212 sys_lookup_dcookie  u64 cookie64  long buf  long len
213 sys_epoll_create  int size
214 sys_epoll_ctl_old NOT IMPLEMENTED
215 sys_epoll_wait_old  NOT IMPLEMENTED
216 sys_remap_file_pages  unsigned long start unsigned long size  unsigned long prot  unsigned long pgoff unsigned long flags
217 sys_getdents64  unsigned int fd struct linux_dirent64 *dirent unsigned int count
218 sys_set_tid_address int *tidptr
219 sys_restart_syscall
220 sys_semtimedop  int semid struct sembuf *tsops  unsigned nsops  const struct timespec *timeout
221 sys_fadvise64 int fd  loff_t offset size_t len  int advice
222 sys_timer_create  const clockid_t which_clock struct sigevent *timer_event_spec timer_t *created_timer_id
223 sys_timer_settime timer_t timer_id  int flags const struct itimerspec *new_setting  struct itimerspec *old_setting
224 sys_timer_gettime timer_t timer_id  struct itimerspec *setting
225 sys_timer_getoverrun  timer_t timer_id
226 sys_timer_delete  timer_t timer_id
227 sys_clock_settime const clockid_t which_clock const struct timespec *tp
228 sys_clock_gettime const clockid_t which_clock struct timespec *tp
229 sys_clock_getres  const clockid_t which_clock struct timespec *tp
230 sys_clock_nanosleep const clockid_t which_clock int flags const struct timespec *rqtp struct timespec *rmtp
231 sys_exit_group  int error_code
232 sys_epoll_wait  int epfd  struct epoll_event *events  int maxevents int timeout
233 sys_epoll_ctl int epfd  int op  int fd  struct epoll_event *event
234 sys_tgkill  pid_t tgid  pid_t pid int sig
235 sys_utimes  char *filename  struct timeval *utimes
236 sys_vserver NOT IMPLEMENTED
237 sys_mbind unsigned long start unsigned long len unsigned long mode  unsigned long *nmask  unsigned long maxnode unsigned flags
238 sys_set_mempolicy int mode  unsigned long *nmask  unsigned long maxnode
239 sys_get_mempolicy int *policy unsigned long *nmask  unsigned long maxnode unsigned long addr  unsigned long flags
240 sys_mq_open const char *u_name  int oflag mode_t mode struct mq_attr *u_attr
241 sys_mq_unlink const char *u_name
242 sys_mq_timedsend  mqd_t mqdes const char *u_msg_ptr size_t msg_len  unsigned int msg_prio const stuct timespec *u_abs_timeout
243 sys_mq_timedreceive mqd_t mqdes char *u_msg_ptr size_t msg_len  unsigned int *u_msg_prio  const struct timespec *u_abs_timeout
244 sys_mq_notify mqd_t mqdes const struct sigevent *u_notification
245 sys_mq_getsetattr mqd_t mqdes const struct mq_attr *u_mqstat  struct mq_attr *u_omqstat
246 sys_kexec_load  unsigned long entry unsigned long nr_segments struct kexec_segment *segments  unsigned long flags
>247 sys_waitid  int which pid_t upid  struct siginfo *infop int options struct rusage *ru
248 sys_add_key const char *_type const char *_description  const void *_payload  size_t plen
249 sys_request_key const char *_type const char *_description  const char *_callout_info key_serial_t destringid
250 sys_keyctl  int option  unsigned long arg2  unsigned long arg3  unsigned long arg4  unsigned long arg5
251 sys_ioprio_set  int which int who int ioprio
252 sys_ioprio_get  int which int who
253 sys_inotify_init
254 sys_inotify_add_watch int fd  const char *pathname  u32 mask
255 sys_inotify_rm_watch  int fd  __s32 wd
256 sys_migrate_pages pid_t pid unsigned long maxnode const unsigned long *old_nodes  const unsigned long *new_nodes
257 sys_openat  int dfd const char *filename  int flags int mode
258 sys_mkdirat int dfd const char *pathname  int mode
259 sys_mknodat int dfd const char *filename  int mode  unsigned dev
260 sys_fchownat  int dfd const char *filename  uid_t user  gid_t group int flag
261 sys_futimesat int dfd const char *filename  struct timeval *utimes
262 sys_newfstatat  int dfd const char *filename  struct stat *statbuf  int flag
263 sys_unlinkat  int dfd const char *pathname  int flag
264 sys_renameat  int oldfd const char *oldname int newfd const char *newname
265 sys_linkat  int oldfd const char *oldname int newfd const char *newname int flags
266 sys_symlinkat const char *oldname int newfd const char *newname
267 sys_readlinkat  int dfd const char *pathname  char *buf int bufsiz
268 sys_fchmodat  int dfd const char *filename  mode_t mode
269 sys_faccessat int dfd const char *filename  int mode
270 sys_pselect6  int n fd_set *inp fd_set *outp  fd_set *exp struct timespec *tsp  void *sig
271 sys_ppoll struct pollfd *ufds unsigned int nfds struct timespec *tsp  const sigset_t *sigmask size_t sigsetsize
272 sys_unshare unsigned long unshare_flags
273 sys_set_robust_list struct robust_list_head *head size_t len
274 sys_get_robust_list int pid struct robust_list_head **head_ptr  size_t *len_ptr
275 sys_splice  int fd_in loff_t *off_in  int fd_out  loff_t *off_out size_t len  unsigned int flags
276 sys_tee int fdin  int fdout size_t len  unsigned int flags
277 sys_sync_file_range long fd loff_t offset loff_t bytes  long flags
278 sys_vmsplice  int fd  const struct iovec *iov unsigned long nr_segs unsigned int flags
279 sys_move_pages  pid_t pid unsigned long nr_pages  const void **pages  const int *nodes  int *status int flags
280 sys_utimensat int dfd const char *filename  struct timespec *utimes int flags
281 sys_epoll_pwait int epfd  struct epoll_event *events  int maxevents int timeout const sigset_t *sigmask size_t sigsetsize
282 sys_signalfd  int ufd sigset_t *user_mask size_t sizemask
283 sys_timerfd_create  int clockid int flags
284 sys_eventfd unsigned int count
285 sys_fallocate long fd long mode loff_t offset loff_t len
286 sys_timerfd_settime int ufd int flags const struct itimerspec *utmr struct itimerspec *otmr
287 sys_timerfd_gettime int ufd struct itimerspec *otmr
288 sys_accept4 int fd  struct sockaddr *upeer_sockaddr int *upeer_addrlen  int flags
289 sys_signalfd4 int ufd sigset_t *user_mask size_t sizemask int flags
290 sys_eventfd2  unsigned int count  int flags
291 sys_epoll_create1 int flags
292 sys_dup3  unsigned int oldfd  unsigned int newfd  int flags
293 sys_pipe2 int *filedes  int flags
294 sys_inotify_init1 int flags
295 sys_preadv  unsigned long fd  const struct iovec *vec unsigned long vlen  unsigned long pos_l unsigned long pos_h
296 sys_pwritev unsigned long fd  const struct iovec *vec unsigned long vlen  unsigned long pos_l unsigned long pos_h
297 sys_rt_tgsigqueueinfo pid_t tgid  pid_t pid int sig siginfo_t *uinfo
298 sys_perf_event_open struct perf_event_attr *attr_uptr pid_t pid int cpu int group_fd  unsigned long flags
299 sys_recvmmsg  int fd  struct msghdr *mmsg unsigned int vlen unsigned int flags  struct timespec *timeout
300 sys_fanotify_init unsigned int flags  unsigned int event_f_flags
301 sys_fanotify_mark long fanotify_fd  long flags  __u64 mask  long dfd  long pathname
302 sys_prlimit64 pid_t pid unsigned int resource const struct rlimit64 *new_rlim struct rlimit64 *old_rlim
303 sys_name_to_handle_at int dfd const char *name  struct file_handle *handle  int *mnt_id int flag
304 sys_open_by_handle_at int dfd const char *name  struct file_handle *handle  int *mnt_id int flags
305 sys_clock_adjtime clockid_t which_clock struct timex *tx
306 sys_syncfs  int fd
307 sys_sendmmsg  int fd  struct mmsghdr *mmsg  unsigned int vlen unsigned int flags
308 sys_setns int fd  int nstype
309 sys_getcpu  unsigned *cpup  unsigned *nodep struct getcpu_cache *unused
310 sys_process_vm_readv  pid_t pid const struct iovec *lvec  unsigned long liovcnt const struct iovec *rvec  unsigned long riovcnt unsigned long flags
311 sys_process_vm_writev pid_t pid const struct iovec *lvec  unsigned long liovcnt const struct iovcc *rvec  unsigned long riovcnt unsigned long flags
312 sys_kcmp  pid_t pid1  pid_t pid2  int type  unsigned long idx1  unsigned long idx2
313 sys_finit_module  int fd  const char __user *uargs  int flags
314 sys_sched_setattr pid_t pid struct sched_attr __user *attr  unsigned int flags
315 sys_sched_getattr pid_t pid struct sched_attr __user *attr  unsigned int size unsigned int flags
316 sys_renameat2 int olddfd  const char __user *oldname  int newdfd  const char __user *newname  unsigned int flags
317 sys_seccomp unsigned int op unsigned int flags  const char __user *uargs
318 sys_getrandom char __user *buf  size_t count  unsigned int flags
319 sys_memfd_create  const char __user *uname_ptr  unsigned int flags
320 sys_kexec_file_load int kernel_fd int initrd_fd unsigned long cmdline_len const char __user *cmdline_ptr  unsigned long flags
321 sys_bpf int cmd union bpf_attr *attr  unsigned int size
322 stub_execveat int dfd const char __user *filename const char __user *const __user *argv const char __user *const __user *envp int flags
323 userfaultfd int flags
324 membarrier  int cmd int flags
325 mlock2  unsigned long start size_t len  int flags
326 copy_file_range int fd_in loff_t __user *off_in int fd_out  loff_t __user * off_out size_t len  unsigned int flags
327 preadv2 unsigned long fd  const struct iovec __user *vec  unsigned long vlen  unsigned long pos_l unsigned long pos_h int flags
328 pwritev2  unsigned long fd  const struct iovec __user *vec  unsigned long vlen  unsigned long pos_l unsigned long pos_h int flags
*/
