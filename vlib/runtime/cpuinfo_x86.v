module runtime

// https://github.com/klauspost/cpuid
// https://github.com/torvalds/linux/blob/master/arch/x86/include/asm/cpufeatures.h
import bitfield

#flag -I @VEXEROOT/vlib/runtime/asm
#insert "@VEXEROOT/vlib/runtime/asm/cpuinfo.h"
$if msvc {
	// msvc doesn't support embedded asm, so include a pre-compiled obj
	$if x64 {
		#flag @VEXEROOT/vlib/runtime/asm/cpuinfo_amd64.obj
	} $else {
		#flag @VEXEROOT/vlib/runtime/asm/cpuinfo_i386.obj
	}
}
fn C.cpuidex_asm([4]u32, u32, u32)
fn C.xgetbv_asm([2]u32, u32)
fn C.rdtscp_asm([4]u32)

// vfmt off
// Vendor_X86 is a representation of a X86 CPU vendor.
pub enum Vendor_X86 {
	vendor_unknown
	intel
	amd
	via
	transmeta
	nsc
	kvm	// Kernel-based Virtual Machine
	msvm	// Microsoft Hyper-V or Windows Virtual PC
	vmware
	xenhvm
	bhyve
	hygon
	sis
	rdc
}

// FeatureID_X86 is the ID of a X86 CPU feature.
pub enum FeatureID_X86 {
	unknown = -1
	// generated from https://github.com/torvalds/linux/blob/master/arch/x86/include/asm/cpufeatures.h
// Intel-defined CPU features, CPUID level 0x00000001 (EDX), word 0
	fpu                 // bit00 Onboard FPU
	vme                 // bit01 Virtual Mode Extensions
	de                  // bit02 Debugging Extensions
	pse                 // bit03 Page Size Extensions
	tsc                 // bit04 Time Stamp Counter
	msr                 // bit05 Model-Specific Registers
	pae                 // bit06 Physical Address Extensions
	mce                 // bit07 Machine Check Exception
	cx8                 // bit08 CMPXCHG8 instruction
	apic                // bit09 Onboard APIC
	sep                 // bit11 SYSENTER/SYSEXIT
	mtrr                // bit12 Memory Type Range Registers
	pge                 // bit13 Page Global Enable
	mca                 // bit14 Machine Check Architecture
	cmov                // bit15 CMOV instructions (plus FCMOVcc, FCOMI with FPU)
	pat                 // bit16 Page Attribute Table
	pse36               // bit17 36-bit PSEs
	pn                  // bit18 Processor serial number
	clflush             // bit19 CLFLUSH instruction
	dts                 // bit21 Debug Store
	acpi                // bit22 ACPI via MSR
	mmx                 // bit23 Multimedia Extensions
	fxsr                // bit24 FXSAVE/FXRSTOR, CR4.OSFXSR
	sse                 // bit25 
	sse2                // bit26 
	ss                  // bit27 CPU self snoop
	ht                  // bit28 Hyper-Threading
	tm                  // bit29 Automatic clock control
	ia64                // bit30 IA-64 processor
	pbe                 // bit31 Pending Break Enable

// AMD-defined CPU features, CPUID level 0x80000001, word 1
	syscall             // bit11 SYSCALL/SYSRET
	mp                  // bit19 MP Capable
	nx                  // bit20 Execute Disable
	mmxext              // bit22 AMD MMX extensions
	fxsr_opt            // bit25 FXSAVE/FXRSTOR optimizations
	pdpe1gb             // bit26 GB pages
	rdtscp              // bit27 RDTSCP
	lm                  // bit29 Long Mode (x86-64, 64-bit support)
	amd_3dnowext        // bit30 AMD 3DNow extensions
	amd_3dnow           // bit31 3DNow

// Intel-defined CPU features, CPUID level 0x00000001 (ECX), word 4
	sse3                // bit00 SSE-3
	pclmulqdq           // bit01 PCLMULQDQ instruction
	dtes64              // bit02 64-bit Debug Store
	monitor             // bit03 MONITOR/MWAIT support
	ds_cpl              // bit04 CPL-qualified (filtered) Debug Store
	vmx                 // bit05 Hardware virtualization
	smx                 // bit06 Safer Mode eXtensions
	est                 // bit07 Enhanced SpeedStep
	tm2                 // bit08 Thermal Monitor 2
	ssse3               // bit09 Supplemental SSE-3
	cid                 // bit10 Context ID
	sdbg                // bit11 Silicon Debug
	fma                 // bit12 Fused multiply-add
	cx16                // bit13 CMPXCHG16B instruction
	xtpr                // bit14 Send Task Priority Messages
	pdcm                // bit15 Perf/Debug Capabilities MSR
	pcid                // bit17 Process Context Identifiers
	dca                 // bit18 Direct Cache Access
	sse4_1              // bit19 SSE-4.1
	sse4_2              // bit20 SSE-4.2
	x2apic              // bit21 X2APIC
	movbe               // bit22 MOVBE instruction
	popcnt              // bit23 POPCNT instruction
	tsc_deadline_timer  // bit24 TSC deadline timer
	aes                 // bit25 AES instructions
	xsave               // bit26 XSAVE/XRSTOR/XSETBV/XGETBV instructions
	osxsave             // bit27 XSAVE instruction enabled in the OS
	avx                 // bit28 Advanced Vector Extensions
	f16c                // bit29 16-bit FP conversions
	rdrand              // bit30 RDRAND instruction
	hypervisor          // bit31 Running on a hypervisor

// More extended AMD flags: CPUID level 0x80000001, ECX, word 6
	lahf_lm             // bit00 LAHF/SAHF in long mode
	cmp_legacy          // bit01 If yes HyperThreading not valid
	svm                 // bit02 Secure Virtual Machine
	extapic             // bit03 Extended APIC space
	cr8_legacy          // bit04 CR8 in 32-bit mode
	abm                 // bit05 Advanced bit manipulation
	sse4a               // bit06 SSE-4A
	misalignsse         // bit07 Misaligned SSE mode
	amd_3dnowprefetch   // bit08 3DNow prefetch instructions
	osvw                // bit09 OS Visible Workaround
	ibs                 // bit10 Instruction Based Sampling
	xop                 // bit11 extended AVX instructions
	skinit              // bit12 SKINIT/STGI instructions
	wdt                 // bit13 Watchdog timer
	lwp                 // bit15 Light Weight Profiling
	fma4                // bit16 4 operands MAC instructions
	tce                 // bit17 Translation Cache Extension
	nodeid_msr          // bit19 NodeId MSR
	tbm                 // bit21 Trailing Bit Manipulations
	topoext             // bit22 Topology extensions CPUID leafs
	perfctr_core        // bit23 Core performance counter extensions
	perfctr_nb          // bit24 NB performance counter extensions
	bpext               // bit26 Data breakpoint extension
	ptsc                // bit27 Performance time-stamp counter
	perfctr_llc         // bit28 Last Level Cache performance counter extensions
	mwaitx              // bit29 MWAIT extension (MONITORX/MWAITX instructions)

// Intel-defined CPU features, CPUID level 0x00000007:0 (EBX), word 9
	fsgsbase            // bit00 RDFSBASE, WRFSBASE, RDGSBASE, WRGSBASE instructions
	tsc_adjust          // bit01 TSC adjustment MSR 0x3B
	sgx                 // bit02 Software Guard Extensions
	bmi1                // bit03 1st group bit manipulation extensions
	hle                 // bit04 Hardware Lock Elision
	avx2                // bit05 AVX2 instructions
	fdp_excptn_only     // bit06 FPU data pointer updated only on x87 exceptions
	smep                // bit07 Supervisor Mode Execution Protection
	bmi2                // bit08 2nd group bit manipulation extensions
	erms                // bit09 Enhanced REP MOVSB/STOSB instructions
	invpcid             // bit10 Invalidate Processor Context ID
	rtm                 // bit11 Restricted Transactional Memory
	cqm                 // bit12 Cache QoS Monitoring
	zero_fcs_fds        // bit13 Zero out FPU CS and FPU DS
	mpx                 // bit14 Memory Protection Extension
	rdt_a               // bit15 Resource Director Technology Allocation
	avx512f             // bit16 AVX-512 Foundation
	avx512dq            // bit17 AVX-512 DQ (Double/Quad granular) Instructions
	rdseed              // bit18 RDSEED instruction
	adx                 // bit19 ADCX and ADOX instructions
	smap                // bit20 Supervisor Mode Access Prevention
	avx512ifma          // bit21 AVX-512 Integer Fused Multiply-Add instructions
	clflushopt          // bit23 CLFLUSHOPT instruction
	clwb                // bit24 CLWB instruction
	intel_pt            // bit25 Intel Processor Trace
	avx512pf            // bit26 AVX-512 Prefetch
	avx512er            // bit27 AVX-512 Exponential and Reciprocal
	avx512cd            // bit28 AVX-512 Conflict Detection
	sha_ni              // bit29 SHA1/SHA256 Instruction Extensions
	avx512bw            // bit30 AVX-512 BW (Byte/Word granular) Instructions
	avx512vl            // bit31 AVX-512 VL (128/256 Vector Length) Extensions

// Extended state features, CPUID level 0x0000000d:1 (EAX), word 10
	xsaveopt            // bit00 XSAVEOPT instruction
	xsavec              // bit01 XSAVEC instruction
	xgetbv1             // bit02 XGETBV with ECX = 1 instruction
	xsaves              // bit03 XSAVES/XRSTORS instructions
	xfd                 // bit04 eXtended Feature Disabling

// Intel-defined CPU features, CPUID level 0x00000007:1 (EAX), word 12
	avx_vnni            // bit04 AVX VNNI instructions
	avx512_bf16         // bit05 AVX512 BFLOAT16 instructions
	cmpccxadd           // bit07 CMPccXADD instructions
	arch_perfmon_ext    // bit08 Intel Architectural PerfMon Extension
	fzrm                // bit10 Fast zero-length REP MOVSB
	fsrs                // bit11 Fast short REP STOSB
	fsrc                // bit12 Fast short REP {CMPSB,SCASB}
	lkgs                // bit18 
	amx_fp16            // bit21 AMX fp16 Support
	avx_ifma            // bit23 Support for VPMADD52[H,L]UQ
	lam                 // bit26 Linear Address Masking

// AMD-defined CPU features, CPUID level 0x80000008 (EBX), word 13
	clzero              // bit00 CLZERO instruction
	irperf              // bit01 Instructions Retired Count
	xsaveerptr          // bit02 Always save/restore FP error pointers
	rdpru               // bit04 Read processor register at user level
	wbnoinvd            // bit09 WBNOINVD instruction
	amd_ibpb            // bit12 Indirect Branch Prediction Barrier
	amd_ibrs            // bit14 Indirect Branch Restricted Speculation
	amd_stibp           // bit15 Single Thread Indirect Branch Predictors
	amd_stibp_always_on // bit17 Single Thread Indirect Branch Predictors always-on preferred
	amd_ppin            // bit23 Protected Processor Inventory Number
	amd_ssbd            // bit24 Speculative Store Bypass Disable
	virt_ssbd           // bit25 Virtualized Speculative Store Bypass Disable
	amd_ssb_no          // bit26 Speculative Store Bypass is fixed in hardware.
	cppc                // bit27 Collaborative Processor Performance Control
	amd_psfd            // bit28 Predictive Store Forwarding Disable
	btc_no              // bit29 Not vulnerable to Branch Type Confusion
	brs                 // bit31 Branch Sampling available

// Thermal and Power Management Leaf, CPUID level 0x00000006 (EAX), word 14
	dtherm              // bit00 Digital Thermal Sensor
	ida                 // bit01 Intel Dynamic Acceleration
	arat                // bit02 Always Running APIC Timer
	pln                 // bit04 Intel Power Limit Notification
	pts                 // bit06 Intel Package Thermal Status
	hwp                 // bit07 Intel Hardware P-states
	hwp_notify          // bit08 HWP Notification
	hwp_act_window      // bit09 HWP Activity Window
	hwp_epp             // bit10 HWP Energy Perf. Preference
	hwp_pkg_req         // bit11 HWP Package Level Request
	hfi                 // bit19 Hardware Feedback Interface

// AMD SVM Feature Identification, CPUID level 0x8000000a (EDX), word 15
	npt                 // bit00 Nested Page Table support
	lbrv                // bit01 LBR Virtualization support
	svm_lock            // bit02 SVM locking MSR
	nrip_save           // bit03 SVM next_rip save
	tsc_scale           // bit04 TSC scaling support
	vmcb_clean          // bit05 VMCB clean bits support
	flushbyasid         // bit06 flush-by-ASID support
	decodeassists       // bit07 Decode Assists support
	pausefilter         // bit10 filtered pause intercept
	pfthreshold         // bit12 pause filter threshold
	avic                // bit13 Virtual Interrupt Controller
	v_vmsave_vmload     // bit15 Virtual VMSAVE VMLOAD
	vgif                // bit16 Virtual GIF
	x2avic              // bit18 Virtual x2apic
	v_spec_ctrl         // bit20 Virtual SPEC_CTRL
	vnmi                // bit25 Virtual NMI
	svme_addr_chk       // bit28 SVME addr check

// Intel-defined CPU features, CPUID level 0x00000007:0 (ECX), word 16
	avx512vbmi          // bit01 AVX512 Vector Bit Manipulation instructions
	umip                // bit02 User Mode Instruction Protection
	pku                 // bit03 Protection Keys for Userspace
	ospke               // bit04 OS Protection Keys Enable
	waitpkg             // bit05 UMONITOR/UMWAIT/TPAUSE Instructions
	avx512_vbmi2        // bit06 Additional AVX512 Vector Bit Manipulation Instructions
	shstk               // bit07 Shadow stack
	gfni                // bit08 Galois Field New Instructions
	vaes                // bit09 Vector AES
	vpclmulqdq          // bit10 Carry-Less Multiplication Double Quadword
	avx512_vnni         // bit11 Vector Neural Network Instructions
	avx512_bitalg       // bit12 Support for VPOPCNT[B,W] and VPSHUF-BITQMB instructions
	tme                 // bit13 Intel Total Memory Encryption
	avx512_vpopcntdq    // bit14 POPCNT for vectors of DW/QW
	la57                // bit16 5-level page tables
	rdpid               // bit22 RDPID instruction
	bus_lock_detect     // bit24 Bus Lock detect
	cldemote            // bit25 CLDEMOTE instruction
	movdiri             // bit27 MOVDIRI instruction
	movdir64b           // bit28 MOVDIR64B instruction
	enqcmd              // bit29 ENQCMD and ENQCMDS instructions
	sgx_lc              // bit30 Software Guard Extensions Launch Control

// AMD-defined CPU features, CPUID level 0x80000007 (EBX), word 17
	overflow_recov      // bit00 MCA overflow recovery support
	succor              // bit01 Uncorrectable error containment and recovery
	smca                // bit03 Scalable MCA

// Intel-defined CPU features, CPUID level 0x00000007:0 (EDX), word 18
	avx512_4vnniw       // bit02 AVX-512 Neural Network Instructions
	avx512_4fmaps       // bit03 AVX-512 Multiply Accumulation Single precision
	fsrm                // bit04 Fast Short Rep Mov
	avx512_vp2intersect // bit08 AVX-512 Intersect for D/Q
	srbds_ctrl          // bit09 SRBDS mitigation MSR available
	md_clear            // bit10 VERW clears CPU buffers
	rtm_always_abort    // bit11 RTM transaction always aborts
	tsx_force_abort     // bit13 TSX_FORCE_ABORT
	serialize           // bit14 SERIALIZE instruction
	hybrid_cpu          // bit15 This part has CPUs of more than one type
	tsxldtrk            // bit16 TSX Suspend Load Address Tracking
	pconfig             // bit18 Intel PCONFIG
	arch_lbr            // bit19 Intel ARCH LBR
	ibt                 // bit20 Indirect Branch Tracking
	amx_bf16            // bit22 AMX bf16 Support
	avx512_fp16         // bit23 AVX512 FP16
	amx_tile            // bit24 AMX tile Support
	amx_int8            // bit25 AMX int8 Support
	spec_ctrl           // bit26 Speculation Control (IBRS + IBPB)
	intel_stibp         // bit27 Single Thread Indirect Branch Predictors
	flush_l1d           // bit28 Flush L1D cache
	arch_capabilities   // bit29 IA32_ARCH_CAPABILITIES MSR (Intel)
	core_capabilities   // bit30 IA32_CORE_CAPABILITIES MSR
	spec_ctrl_ssbd      // bit31 Speculative Store Bypass Disable

// AMD-defined memory encryption features, CPUID level 0x8000001f (EAX), word 19
	sme                 // bit00 AMD Secure Memory Encryption
	sev                 // bit01 AMD Secure Encrypted Virtualization
	vm_page_flush       // bit02 VM Page Flush MSR is supported
	sev_es              // bit03 AMD Secure Encrypted Virtualization - Encrypted State
	v_tsc_aux           // bit09 Virtual TSC_AUX
	sme_coherent        // bit10 AMD hardware-enforced cache coherency
	debug_swap          // bit14 AMD SEV-ES full debug state swap support

// AMD-defined Extended Feature 2 EAX, CPUID level 0x80000021 (EAX), word 20
	no_nested_data_bp   // bit00 No Nested Data Breakpoints
	wrmsr_xx_base_ns    // bit01 WRMSR to {FS,GS,KERNEL_GS}_BASE is non-serializing
	lfence_rdtsc        // bit02 LFENCE always serializing / synchronizes RDTSC
	null_sel_clr_base   // bit06 Null Selector Clears Base
	autoibrs            // bit08 Automatic IBRS
	no_smm_ctl_msr      // bit09 SMM_CTL MSR is not present

	sbpb                // bit27 Selective Branch Prediction Barrier
	ibpb_brtype         // bit28 MSR_PRED_CMD[IBPB] flushes all branch type predictions
	srso_no             // bit29 CPU is not affected by SRSO
	
	// Keep it last. It automatically defines the size of feature_set
	last_id
}

// vfmt on

// CPUInfo_X86 contains information about the detected system CPU.
// If system have multiple cores, the CPUInfo_X86 only contains
// information of the core which current process running on.
pub struct CPUInfo_X86 {
pub mut:
	brand_name       string            // Brand name reported by the CPU
	vendor_id        Vendor_X86        // Comparable CPU vendor ID
	vendor_string    string            // Raw vendor string.
	feature_set      bitfield.BitField // Features of the CPU
	physical_cores   int // Number of physical processor cores in your CPU. Will be 0 if undetectable.
	threads_per_core int = 1 // Number of threads per physical core. Will be 1 if undetectable.
	logical_cores    int // Number of physical cores times threads that can run on each core through the use of hyperthreading. Will be 0 if undetectable.
	family           int // CPU family number
	model            int // CPU model number
	stepping         int // CPU stepping info
	cache_line       int // Cache line size in bytes. Will be 0 if undetectable.
	hz               i64 // Clock speed, if known, 0 otherwise. Will attempt to contain base clock speed.
	boost_freq       i64 // Max clock speed, if known, 0 otherwise
	cache            struct {
	pub mut:
		l1i int = -1 // L1 Instruction Cache (per core or shared). Will be -1 if undetected
		l1d int = -1 // L1 Data Cache (per core or shared). Will be -1 if undetected
		l2  int = -1 // L2 Cache (per core or shared). Will be -1 if undetected
		l3  int = -1 // L3 Cache (per core, per ccx or shared). Will be -1 if undetected
	}

	sgx                SGXSupport
	amd_mem_encryption AMDMemEncryptionSupport
	max_func           u32
	max_ex_func        u32
}

// cpuidex will call `cpuidex` instruction, return eax,ebx,ecx,edx
pub fn cpuidex(op u32, op2 u32) (u32, u32, u32, u32) {
	$if i386 || amd64 {
		$if msvc {
			mut result := [4]u32{}

			// C.__cpuidex(result, op, op2)
			C.cpuidex_asm(result, op, op2)
			return result[0], result[1], result[2], result[3]
		} $else {
			mut eax := u32(0)
			mut ebx := u32(0)
			mut ecx := u32(0)
			mut edx := u32(0)
			asm amd64 {
				mov eax, op
				mov ecx, op2
				cpuid
				; =a (eax)
				  =b (ebx)
				  =c (ecx)
				  =d (edx)
				; r (op)
				  r (op2)
			}
			return eax, ebx, ecx, edx
		}
	}

	return 0, 0, 0, 0
}

fn cpuid(op u32) (u32, u32, u32, u32) {
	return cpuidex(op, 0)
}

// xgetbv will call `xgetbv` instruction, return eax,edx
pub fn xgetbv(index u32) (u32, u32) {
	$if i386 || amd64 {
		$if msvc {
			mut result := [2]u32{}
			C.xgetbv_asm(result, index)
			return result[0], result[1]
		} $else {
			mut eax := u32(0)
			mut edx := u32(0)
			asm amd64 {
				mov ecx, index
				.byte 0x0f, 0x01, 0xd0 // XGETBV
				; =a (eax)
				  =d (edx)
				; r (index)
				; rcx
			}
			return eax, edx
		}
	}
	return 0, 0
}

// rdtscp will call `rdtscp` instruction, return eax,ebx,ecx,edx
pub fn rdtscp() (u32, u32, u32, u32) {
	$if i386 || amd64 {
		$if msvc {
			mut result := [4]u32{}
			C.rdtscp_asm(result)
			return result[0], result[1], result[2], result[3]
		} $else {
			mut eax := u32(0)
			mut ebx := u32(0)
			mut ecx := u32(0)
			mut edx := u32(0)
			asm amd64 {
				.byte 0x0F, 0x01, 0xF9 // RDTSCP
				; =a (eax)
				  =b (ebx)
				  =c (ecx)
				  =d (edx)
			}
			return eax, ebx, ecx, edx
		}
	}
	return 0, 0, 0, 0
}

fn darwin_has_avx512() bool {
	return false
}

// detect_x86 will detect current CPU info.
pub fn detect_x86() CPUInfo_X86 {
	mut cpu := CPUInfo_X86{}
	$if i386 || amd64 {
		// Set defaults
		cpu.threads_per_core = 1
		cpu.cache.l1i = -1
		cpu.cache.l1d = -1
		cpu.cache.l2 = -1
		cpu.cache.l3 = -1

		cpu.max_func = max_function_id()
		cpu.max_ex_func = max_extended_function()
		cpu.brand_name = brand_name()
		cpu.cache_line = cache_line()
		cpu.family, cpu.model, cpu.stepping = family_model()
		cpu.feature_set = support_x86()
		cpu.sgx = has_sgx(cpu.has(.sgx), cpu.has(.sgx_lc))
		cpu.amd_mem_encryption = has_amd_mem_encryption(cpu.has(.sme) || cpu.has(.sev))
		cpu.threads_per_core = threads_per_core()
		cpu.logical_cores = logical_cores()
		cpu.physical_cores = physical_cores()
		cpu.vendor_id, cpu.vendor_string = vendor_id()
		cpu.cache_size()
		cpu.frequencies()
	}
	return cpu
}

// has returns whether the CPU supports one or more of the requested features.
@[inline]
pub fn (mut c CPUInfo_X86) has(ids ...FeatureID_X86) bool {
	for id in ids {
		if c.feature_set.has(int(id)) {
			return true
		}
	}
	return false
}

// all returns whether the CPU supports all of the requested features.
@[inline]
pub fn (mut c CPUInfo_X86) all(ids ...FeatureID_X86) bool {
	for id in ids {
		if !c.feature_set.has(int(id)) {
			return false
		}
	}
	return true
}

// amd64_level returns the microarchitecture level detected on the CPU.
// If features are lacking or non amd64 mode, 0 is returned.
// See https://en.wikipedia.org/wiki/X86-64#Microarchitecture_levels
// See https://github.com/Jordan-JD-Peterson/x86-64-level/blob/develop/README.md
pub fn (mut c CPUInfo_X86) amd64_level() int {
	$if i386 || amd64 {
		if c.all(.avx512f, .avx512bw, .avx512cd, .avx512dq, .avx512vl) {
			return 4
		}
		if c.all(.avx, .avx2, .bmi1, .bmi2, .f16c, .fma, .movbe, .osxsave) {
			return 3
		}
		if c.all(.cx16, .lahf_lm, .popcnt, .sse3, .sse4_1, .sse4_2, .ssse3) {
			return 2
		}
		if c.all(.cmov, .cx8, .fpu, .fxsr, .mmx, .sse, .sse2) {
			return 1
		}
	}
	return 0
}

// feature_set returns all available features as strings.
pub fn (c CPUInfo_X86) feature_set() []string {
	mut s := []string{}
	for i in 0 .. c.feature_set.get_size() {
		if c.feature_set.get_bit(i) == 1 {
			s << unsafe { FeatureID_X86(i).str() }
		}
	}
	return s
}

// rt_counter returns the 64-bit time-stamp counter
// Uses the RDTSCP instruction. The value 0 is returned
// if the CPU does not support the instruction.
pub fn (mut c CPUInfo_X86) rt_counter() u64 {
	if !c.has(.rdtscp) {
		return 0
	}
	a, _, _, d := rdtscp()
	return u64(a) | (u64(d) << 32)
}

// ia32_tsc_aux returns the IA32_TSC_AUX part of the RDTSCP.
// This variable is OS dependent, but on Linux contains information
// about the current cpu/core the code is running on.
// If the RDTSCP instruction isn't supported on the CPU, the value 0 is returned.
pub fn (mut c CPUInfo_X86) ia32_tsc_aux() u32 {
	if !c.has(.rdtscp) {
		return 0
	}
	_, _, ecx, _ := rdtscp()
	return ecx
}

// logical_cpu will return the Logical CPU the code is currently executing on.
// This is likely to change when the OS re-schedules the running thread
// to another CPU.
// If the current core cannot be detected, -1 will be returned.
pub fn (c CPUInfo_X86) logical_cpu() int {
	if c.max_func < 1 {
		return -1
	}
	_, ebx, _, _ := cpuid(1)
	return int(ebx >> 24)
}

// frequencies tries to compute the clock speed of the CPU. If leaf 15 is
// supported, use it, otherwise parse the brand string. Yes, really.
pub fn (mut c CPUInfo_X86) frequencies() {
	c.hz, c.boost_freq = 0, 0
	mfi := max_function_id()
	if mfi >= 0x15 {
		eax, ebx, ecx, _ := cpuid(0x15)
		if eax != 0 && ebx != 0 && ecx != 0 {
			c.hz = (i64(ecx) * i64(ebx)) / i64(eax)
		}
	}
	if mfi >= 0x16 {
		a, b, _, _ := cpuid(0x16)

		// Base...
		if a & 0xffff > 0 {
			c.hz = i64(a & 0xffff) * 1_000_000
		}

		// Boost...
		if b & 0xffff > 0 {
			c.boost_freq = i64(b & 0xffff) * 1_000_000
		}
	}
	if c.hz > 0 {
		return
	}

	// computeHz determines the official rated speed of a CPU from its brand
	// string. This insanity is *actually the official documented way to do
	// this according to Intel*, prior to leaf 0x15 existing. The official
	// documentation only shows this working for exactly `x.xx` or `xxxx`
	// cases, e.g., `2.50GHz` or `1300MHz`; this parser will accept other
	// sizes.
	model := c.brand_name
	hz := model.index_last('Hz') or { return }
	mut multiplier := i64(0)
	if model[hz - 1] == `M` {
		multiplier = 1000 * 1000
	} else if model[hz - 1] == `G` {
		multiplier = 1000 * 1000 * 1000
	} else if model[hz - 1] == `T` {
		multiplier = 1000 * 1000 * 1000 * 1000
	}
	if multiplier == 0 {
		return
	}
	mut freq := i64(0)
	mut divisor := i64(0)
	mut decimal_shift := i64(1)
	mut i := hz - 2
	for i >= 0 && model[i] != ` ` {
		if model[i] >= `0` && model[i] <= `9` {
			freq += i64(model[i] - `0`) * decimal_shift
			decimal_shift *= 10
		} else if model[i] == `.` {
			if divisor != 0 {
				return
			}
			divisor = decimal_shift
		} else {
			return
		}
		i--
	}

	// we didn't find a space
	if i < 0 {
		return
	}
	if divisor != 0 {
		c.hz = (freq * multiplier) / divisor
		return
	}
	c.hz = freq * multiplier
}

// vm Will return true if the cpu id indicates we are in a virtual machine.
pub fn (mut c CPUInfo_X86) vm() bool {
	return c.has(.hypervisor)
}

fn max_extended_function() u32 {
	eax, _, _, _ := cpuid(0x80000000)
	return eax
}

fn max_function_id() u32 {
	a, _, _, _ := cpuid(0)
	return a
}

fn brand_name() string {
	if max_extended_function() >= 0x80000004 {
		mut v := []u32{}
		for i in 0 .. 3 {
			a, b, c, d := cpuid(0x80000002 + i)
			v << [a, b, c, d]
		}
		return val_as_string(...v).trim_space()
	}
	return 'unknown'
}

fn threads_per_core() int {
	mfi := max_function_id()
	vend, _ := vendor_id()

	if mfi < 0x4 || (vend != .intel && vend != .amd) {
		return 1
	}

	if mfi < 0xb {
		if vend != .intel {
			return 1
		}
		_, b, _, d := cpuid(1)
		if (d & (1 << 28)) != 0 {
			// v will contain logical core count
			v := (b >> 16) & 255
			if v > 1 {
				a4, _, _, _ := cpuid(4)

				// physical cores
				v2 := (a4 >> 26) + 1
				if v2 > 0 {
					return int(v) / int(v2)
				}
			}
		}
		return 1
	}
	_, b, _, _ := cpuidex(0xb, 0)
	if b & 0xffff == 0 {
		if vend == .amd {
			// Workaround for AMD returning 0, assume 2 if >= Zen 2
			// It will be more correct than not.
			fam, _, _ := family_model()
			_, _, _, d := cpuid(1)
			if (d & (1 << 28)) != 0 && fam >= 23 {
				return 2
			}
		}
		return 1
	}
	return int(b & 0xffff)
}

fn logical_cores() int {
	mfi := max_function_id()
	v, _ := vendor_id()
	match v {
		.intel {
			// Use this on old Intel processors
			if mfi < 0xb {
				if mfi < 1 {
					return 0
				}

				// CPUID.1:EBX[23:16] represents the maximum number of addressable IDs (initial APIC ID)
				// that can be assigned to logical processors in a physical package.
				// The value may not be the same as the number of logical processors that are present in the hardware of a physical package.
				_, ebx, _, _ := cpuid(1)
				logical := (ebx >> 16) & 0xff
				return int(logical)
			}
			_, b, _, _ := cpuidex(0xb, 1)
			return int(b & 0xffff)
		}
		.amd, .hygon {
			_, b, _, _ := cpuid(1)
			return int((b >> 16) & 0xff)
		}
		else {
			return 0
		}
	}
}

fn family_model() (int, int, int) {
	if max_function_id() < 0x1 {
		return 0, 0, 0
	}
	eax, _, _, _ := cpuid(1)

	// If BaseFamily[3:0] is less than Fh then ExtendedFamily[7:0] is reserved and Family is equal to BaseFamily[3:0].
	mut family := int((eax >> 8) & 0xf)
	mut ext_fam := family == 0x6 // Intel is 0x6, needs extended model.
	if family == 0xf {
		// Add ExtFamily
		family += int((eax >> 20) & 0xff)
		ext_fam = true
	}

	// If BaseFamily[3:0] is less than 0Fh then ExtendedModel[3:0] is reserved and Model is equal to BaseModel[3:0].
	mut model := int((eax >> 4) & 0xf)
	if ext_fam {
		// Add ExtModel
		model += int((eax >> 12) & 0xf0)
	}
	stepping := int(eax & 0xf)
	return family, model, stepping
}

fn physical_cores() int {
	v, _ := vendor_id()
	match v {
		.intel {
			return logical_cores() / threads_per_core()
		}
		.amd, .hygon {
			lc := logical_cores()
			tpc := threads_per_core()
			if lc > 0 && tpc > 0 {
				return lc / tpc
			}

			// The following is inaccurate on AMD EPYC 7742 64-Core Processor
			if max_extended_function() >= 0x80000008 {
				_, _, c, _ := cpuid(0x80000008)
				if c & 0xff > 0 {
					return int(c & 0xff) + 1
				}
			}
		}
		else {
			return 0
		}
	}
	return 0
}

// Except from http://en.wikipedia.org/wiki/CPUID#EAX.3D0:_Get_vendor_ID
const vendor_mapping = {
	'AMDisbetter!': Vendor_X86.amd
	'AuthenticAMD': Vendor_X86.amd
	'CentaurHauls': Vendor_X86.via
	'GenuineIntel': Vendor_X86.intel
	'TransmetaCPU': Vendor_X86.transmeta
	'GenuineTMx86': Vendor_X86.transmeta
	'Geode by NSC': Vendor_X86.nsc
	'VIA VIA VIA ': Vendor_X86.via
	'KVMKVMKVMKVM': Vendor_X86.kvm
	'Microsoft Hv': Vendor_X86.msvm
	'VMwareVMware': Vendor_X86.vmware
	'XenVMMXenVMM': Vendor_X86.xenhvm
	'bhyve bhyve ': Vendor_X86.bhyve
	'HygonGenuine': Vendor_X86.hygon
	'Vortex86 SoC': Vendor_X86.sis
	'SiS SiS SiS ': Vendor_X86.sis
	'RiseRiseRise': Vendor_X86.sis
	'Genuine  RDC': Vendor_X86.rdc
}

fn vendor_id() (Vendor_X86, string) {
	_, b, c, d := cpuid(0)
	v := val_as_string(b, d, c)
	if vend := runtime.vendor_mapping[v] {
		return vend, v
	} else {
		return Vendor_X86.vendor_unknown, v
	}
}

fn cache_line() int {
	if max_function_id() < 0x1 {
		return 0
	}

	_, ebx, _, _ := cpuid(1)
	mut cache := (ebx & 0xff00) >> 5 // cflush size
	if cache == 0 && max_extended_function() >= 0x80000006 {
		_, _, ecx, _ := cpuid(0x80000006)
		cache = ecx & 0xff // cacheline size
	}

	// TODO: Read from Cache and TLB Information
	return int(cache)
}

fn (mut c CPUInfo_X86) cache_size() {
	c.cache.l1d = -1
	c.cache.l1i = -1
	c.cache.l2 = -1
	c.cache.l3 = -1
	vendor, _ := vendor_id()
	match vendor {
		.intel {
			if max_function_id() < 4 {
				return
			}
			c.cache.l1i, c.cache.l1d, c.cache.l2, c.cache.l3 = 0, 0, 0, 0
			mut i := u32(0)
			for {
				eax, ebx, ecx, _ := cpuidex(4, i)
				i++
				cache_type := eax & 15
				if cache_type == 0 {
					break
				}
				cache_level := (eax >> 5) & 7
				coherency := int(ebx & 0xfff) + 1
				partitions := int((ebx >> 12) & 0x3ff) + 1
				associativity := int((ebx >> 22) & 0x3ff) + 1
				sets := int(ecx) + 1
				size := associativity * partitions * coherency * sets
				match cache_level {
					1 {
						if cache_type == 1 {
							// 1 = Data Cache
							c.cache.l1d = size
						} else if cache_type == 2 {
							// 2 = instruction cache
							c.cache.l1i = size
						} else {
							if c.cache.l1d < 0 {
								c.cache.l1i = size
							}
							if c.cache.l1i < 0 {
								c.cache.l1i = size
							}
						}
					}
					2 {
						c.cache.l2 = size
					}
					3 {
						c.cache.l3 = size
					}
					else {}
				}
			}
		}
		.amd, .hygon {
			// Untested.
			mut eax := u32(0)
			mut ebx := u32(0)
			mut ecx := u32(0)
			mut edx := u32(0)
			if max_extended_function() < 0x80000005 {
				return
			}
			_, _, ecx, edx = cpuid(0x80000005)
			c.cache.l1d = int(((ecx >> 24) & 0xff) * 1024)
			c.cache.l1i = int(((edx >> 24) & 0xff) * 1024)

			if max_extended_function() < 0x80000006 {
				return
			}
			_, _, ecx, _ = cpuid(0x80000006)
			c.cache.l2 = int(((ecx >> 16) & 0xffff) * 1024)

			// CPUID Fn8000_001D_EAX_x[N:0] Cache Properties
			if max_extended_function() < 0x8000001d
				|| !c.feature_set.has(int(FeatureID_X86.topoext)) {
				return
			}

			// Xen Hypervisor is buggy and returns the same entry no matter ECX value.
			// Hack: When we encounter the same entry 100 times we break.
			mut n_same := 0
			mut last := u32(0)
			for i in u32(0) .. u32(-1) {
				eax, ebx, ecx, _ = cpuidex(0x8000001D, i)

				level := (eax >> 5) & 7
				cache_num_sets := ecx + 1
				cache_line_size := 1 + (ebx & 2047)
				cache_phys_partitions := 1 + ((ebx >> 12) & 511)
				cache_num_ways := 1 + ((ebx >> 22) & 511)

				typ := eax & 15
				size := int(cache_num_sets * cache_line_size * cache_phys_partitions * cache_num_ways)
				if typ == 0 {
					return
				}

				// Check for the same value repeated.
				comb := eax ^ ebx ^ ecx
				if comb == last {
					n_same++
					if n_same == 100 {
						return
					}
				}
				last = comb

				match level {
					1 {
						match typ {
							1 {
								// data cache
								c.cache.l1d = size
							}
							2 {
								// inst cache
								c.cache.l1i = size
							}
							else {
								if c.cache.l1d < 0 {
									c.cache.l1i = size
								}
								if c.cache.l1i < 0 {
									c.cache.l1i = size
								}
							}
						}
					}
					2 {
						c.cache.l2 = size
					}
					3 {
						c.cache.l3 = size
					}
					else {}
				}
			}
		}
		else {}
	}
}

pub struct SGXEPCSection {
pub mut:
	base_address u64
	epc_size     u64
}

pub struct SGXSupport {
pub mut:
	available               bool
	launch_control          bool
	sgx1_supported          bool
	sgx2_supported          bool
	max_enclave_size_not_64 u64
	max_enclave_size_64     u64
	epc_sections            []SGXEPCSection
}

fn has_sgx(available bool, lc bool) SGXSupport {
	mut rval := SGXSupport{}
	rval.available = available

	if !available {
		return rval
	}

	rval.launch_control = lc

	a, _, _, d := cpuidex(0x12, 0)
	rval.sgx1_supported = a & 0x01 != 0
	rval.sgx2_supported = a & 0x02 != 0
	rval.max_enclave_size_not_64 = 1 << (d & 0xff) // pow 2
	rval.max_enclave_size_64 = 1 << ((d >> 8) & 0xff) // pow 2
	rval.epc_sections = []SGXEPCSection{}

	for subleaf in u32(0) .. 10 {
		eax, ebx, ecx, edx := cpuidex(0x12, subleaf)
		leaf_type := eax & 0xf

		if leaf_type == 0 {
			// Invalid subleaf, stop iterating
			break
		} else if leaf_type == 1 {
			// EPC Section subleaf
			base_address := u64(eax & 0xfffff000) + (u64(ebx & 0x000fffff) << 32)
			size := u64(ecx & 0xfffff000) + (u64(edx & 0x000fffff) << 32)

			section := SGXEPCSection{
				base_address: base_address
				epc_size: size
			}
			rval.epc_sections << section
		}
	}

	return rval
}

pub struct AMDMemEncryptionSupport {
pub mut:
	available            bool
	c_bit_possition      u32
	num_vmpl             u32
	phys_addr_reduction  u32
	num_entrypted_guests u32
	min_sev_no_es_asid   u32
}

fn has_amd_mem_encryption(available bool) AMDMemEncryptionSupport {
	mut rval := AMDMemEncryptionSupport{}
	rval.available = available
	if !available {
		return rval
	}

	_, b, c, d := cpuidex(0x8000001f, 0)

	rval.c_bit_possition = b & 0x3f
	rval.phys_addr_reduction = (b >> 6) & 0x3f
	rval.num_vmpl = (b >> 12) & 0xf
	rval.num_entrypted_guests = c
	rval.min_sev_no_es_asid = d

	return rval
}

fn support_x86() bitfield.BitField {
	mut fs := bitfield.new(int(FeatureID_X86.last_id) + 8)
	$if i386 || amd64 {
		mfi := max_function_id()
		if mfi < 0x1 {
			return fs
		}

		mut eax := u32(0)
		mut ebx := u32(0)
		mut ecx := u32(0)
		mut edx := u32(0)

		// Intel-defined CPU features, CPUID level 0x00000001 (EDX), word 0
		eax, ebx, ecx, edx = cpuidex(0x00000001, 0)
		fs.set_if(edx & (1 << 0) != 0, int(FeatureID_X86.fpu)) // bit00 Onboard FPU
		fs.set_if(edx & (1 << 1) != 0, int(FeatureID_X86.vme)) // bit01 Virtual Mode Extensions
		fs.set_if(edx & (1 << 2) != 0, int(FeatureID_X86.de)) // bit02 Debugging Extensions
		fs.set_if(edx & (1 << 3) != 0, int(FeatureID_X86.pse)) // bit03 Page Size Extensions
		fs.set_if(edx & (1 << 4) != 0, int(FeatureID_X86.tsc)) // bit04 Time Stamp Counter
		fs.set_if(edx & (1 << 5) != 0, int(FeatureID_X86.msr)) // bit05 Model-Specific Registers
		fs.set_if(edx & (1 << 6) != 0, int(FeatureID_X86.pae)) // bit06 Physical Address Extensions
		fs.set_if(edx & (1 << 7) != 0, int(FeatureID_X86.mce)) // bit07 Machine Check Exception
		fs.set_if(edx & (1 << 8) != 0, int(FeatureID_X86.cx8)) // bit08 CMPXCHG8 instruction
		fs.set_if(edx & (1 << 9) != 0, int(FeatureID_X86.apic)) // bit09 Onboard APIC
		fs.set_if(edx & (1 << 11) != 0, int(FeatureID_X86.sep)) // bit11 SYSENTER/SYSEXIT
		fs.set_if(edx & (1 << 12) != 0, int(FeatureID_X86.mtrr)) // bit12 Memory Type Range Registers
		fs.set_if(edx & (1 << 13) != 0, int(FeatureID_X86.pge)) // bit13 Page Global Enable
		fs.set_if(edx & (1 << 14) != 0, int(FeatureID_X86.mca)) // bit14 Machine Check Architecture
		fs.set_if(edx & (1 << 15) != 0, int(FeatureID_X86.cmov)) // bit15 CMOV instructions (plus FCMOVcc, FCOMI with FPU)
		fs.set_if(edx & (1 << 16) != 0, int(FeatureID_X86.pat)) // bit16 Page Attribute Table
		fs.set_if(edx & (1 << 17) != 0, int(FeatureID_X86.pse36)) // bit17 36-bit PSEs
		fs.set_if(edx & (1 << 18) != 0, int(FeatureID_X86.pn)) // bit18 Processor serial number
		fs.set_if(edx & (1 << 19) != 0, int(FeatureID_X86.clflush)) // bit19 CLFLUSH instruction
		fs.set_if(edx & (1 << 21) != 0, int(FeatureID_X86.dts)) // bit21 Debug Store
		fs.set_if(edx & (1 << 22) != 0, int(FeatureID_X86.acpi)) // bit22 ACPI via MSR
		fs.set_if(edx & (1 << 23) != 0, int(FeatureID_X86.mmx)) // bit23 Multimedia Extensions
		fs.set_if(edx & (1 << 24) != 0, int(FeatureID_X86.fxsr)) // bit24 FXSAVE/FXRSTOR, CR4.OSFXSR
		fs.set_if(edx & (1 << 25) != 0, int(FeatureID_X86.sse)) // bit25
		fs.set_if(edx & (1 << 26) != 0, int(FeatureID_X86.sse2)) // bit26
		fs.set_if(edx & (1 << 27) != 0, int(FeatureID_X86.ss)) // bit27 CPU self snoop
		fs.set_if(edx & (1 << 28) != 0, int(FeatureID_X86.ht)) // bit28 Hyper-Threading
		fs.set_if(edx & (1 << 29) != 0, int(FeatureID_X86.tm)) // bit29 Automatic clock control
		fs.set_if(edx & (1 << 30) != 0, int(FeatureID_X86.ia64)) // bit30 IA-64 processor
		fs.set_if(edx & (1 << 31) != 0, int(FeatureID_X86.pbe)) // bit31 Pending Break Enable

		// AMD-defined CPU features, CPUID level 0x80000001, word 1
		eax, ebx, ecx, edx = cpuidex(0x80000001, 0)
		fs.set_if(edx & (1 << 11) != 0, int(FeatureID_X86.syscall)) // bit11 SYSCALL/SYSRET
		fs.set_if(edx & (1 << 19) != 0, int(FeatureID_X86.mp)) // bit19 MP Capable
		fs.set_if(edx & (1 << 20) != 0, int(FeatureID_X86.nx)) // bit20 Execute Disable
		fs.set_if(edx & (1 << 22) != 0, int(FeatureID_X86.mmxext)) // bit22 AMD MMX extensions
		fs.set_if(edx & (1 << 25) != 0, int(FeatureID_X86.fxsr_opt)) // bit25 FXSAVE/FXRSTOR optimizations
		fs.set_if(edx & (1 << 26) != 0, int(FeatureID_X86.pdpe1gb)) // bit26 GB pages
		fs.set_if(edx & (1 << 27) != 0, int(FeatureID_X86.rdtscp)) // bit27 RDTSCP
		fs.set_if(edx & (1 << 29) != 0, int(FeatureID_X86.lm)) // bit29 Long Mode (x86-64, 64-bit support)
		fs.set_if(edx & (1 << 30) != 0, int(FeatureID_X86.amd_3dnowext)) // bit30 AMD 3DNow extensions
		fs.set_if(edx & (1 << 31) != 0, int(FeatureID_X86.amd_3dnow)) // bit31 3DNow

		// Intel-defined CPU features, CPUID level 0x00000001 (ECX), word 4
		eax, ebx, ecx, edx = cpuidex(0x00000001, 0)
		fs.set_if(ecx & (1 << 0) != 0, int(FeatureID_X86.sse3)) // bit00 SSE-3
		fs.set_if(ecx & (1 << 1) != 0, int(FeatureID_X86.pclmulqdq)) // bit01 PCLMULQDQ instruction
		fs.set_if(ecx & (1 << 2) != 0, int(FeatureID_X86.dtes64)) // bit02 64-bit Debug Store
		fs.set_if(ecx & (1 << 3) != 0, int(FeatureID_X86.monitor)) // bit03 MONITOR/MWAIT support
		fs.set_if(ecx & (1 << 4) != 0, int(FeatureID_X86.ds_cpl)) // bit04 CPL-qualified (filtered) Debug Store
		fs.set_if(ecx & (1 << 5) != 0, int(FeatureID_X86.vmx)) // bit05 Hardware virtualization
		fs.set_if(ecx & (1 << 6) != 0, int(FeatureID_X86.smx)) // bit06 Safer Mode eXtensions
		fs.set_if(ecx & (1 << 7) != 0, int(FeatureID_X86.est)) // bit07 Enhanced SpeedStep
		fs.set_if(ecx & (1 << 8) != 0, int(FeatureID_X86.tm2)) // bit08 Thermal Monitor 2
		fs.set_if(ecx & (1 << 9) != 0, int(FeatureID_X86.ssse3)) // bit09 Supplemental SSE-3
		fs.set_if(ecx & (1 << 10) != 0, int(FeatureID_X86.cid)) // bit10 Context ID
		fs.set_if(ecx & (1 << 11) != 0, int(FeatureID_X86.sdbg)) // bit11 Silicon Debug
		fs.set_if(ecx & (1 << 12) != 0, int(FeatureID_X86.fma)) // bit12 Fused multiply-add
		fs.set_if(ecx & (1 << 13) != 0, int(FeatureID_X86.cx16)) // bit13 CMPXCHG16B instruction
		fs.set_if(ecx & (1 << 14) != 0, int(FeatureID_X86.xtpr)) // bit14 Send Task Priority Messages
		fs.set_if(ecx & (1 << 15) != 0, int(FeatureID_X86.pdcm)) // bit15 Perf/Debug Capabilities MSR
		fs.set_if(ecx & (1 << 17) != 0, int(FeatureID_X86.pcid)) // bit17 Process Context Identifiers
		fs.set_if(ecx & (1 << 18) != 0, int(FeatureID_X86.dca)) // bit18 Direct Cache Access
		fs.set_if(ecx & (1 << 19) != 0, int(FeatureID_X86.sse4_1)) // bit19 SSE-4.1
		fs.set_if(ecx & (1 << 20) != 0, int(FeatureID_X86.sse4_2)) // bit20 SSE-4.2
		fs.set_if(ecx & (1 << 21) != 0, int(FeatureID_X86.x2apic)) // bit21 X2APIC
		fs.set_if(ecx & (1 << 22) != 0, int(FeatureID_X86.movbe)) // bit22 MOVBE instruction
		fs.set_if(ecx & (1 << 23) != 0, int(FeatureID_X86.popcnt)) // bit23 POPCNT instruction
		fs.set_if(ecx & (1 << 24) != 0, int(FeatureID_X86.tsc_deadline_timer)) // bit24 TSC deadline timer
		fs.set_if(ecx & (1 << 25) != 0, int(FeatureID_X86.aes)) // bit25 AES instructions
		fs.set_if(ecx & (1 << 26) != 0, int(FeatureID_X86.xsave)) // bit26 XSAVE/XRSTOR/XSETBV/XGETBV instructions
		fs.set_if(ecx & (1 << 27) != 0, int(FeatureID_X86.osxsave)) // bit27 XSAVE instruction enabled in the OS
		fs.set_if(ecx & (1 << 28) != 0, int(FeatureID_X86.avx)) // bit28 Advanced Vector Extensions
		fs.set_if(ecx & (1 << 29) != 0, int(FeatureID_X86.f16c)) // bit29 16-bit FP conversions
		fs.set_if(ecx & (1 << 30) != 0, int(FeatureID_X86.rdrand)) // bit30 RDRAND instruction
		fs.set_if(ecx & (1 << 31) != 0, int(FeatureID_X86.hypervisor)) // bit31 Running on a hypervisor

		// More extended AMD flags: CPUID level 0x80000001, ECX, word 6
		eax, ebx, ecx, edx = cpuidex(0x80000001, 0)
		fs.set_if(ecx & (1 << 0) != 0, int(FeatureID_X86.lahf_lm)) // bit00 LAHF/SAHF in long mode
		fs.set_if(ecx & (1 << 1) != 0, int(FeatureID_X86.cmp_legacy)) // bit01 If yes HyperThreading not valid
		fs.set_if(ecx & (1 << 2) != 0, int(FeatureID_X86.svm)) // bit02 Secure Virtual Machine
		fs.set_if(ecx & (1 << 3) != 0, int(FeatureID_X86.extapic)) // bit03 Extended APIC space
		fs.set_if(ecx & (1 << 4) != 0, int(FeatureID_X86.cr8_legacy)) // bit04 CR8 in 32-bit mode
		fs.set_if(ecx & (1 << 5) != 0, int(FeatureID_X86.abm)) // bit05 Advanced bit manipulation
		fs.set_if(ecx & (1 << 6) != 0, int(FeatureID_X86.sse4a)) // bit06 SSE-4A
		fs.set_if(ecx & (1 << 7) != 0, int(FeatureID_X86.misalignsse)) // bit07 Misaligned SSE mode
		fs.set_if(ecx & (1 << 8) != 0, int(FeatureID_X86.amd_3dnowprefetch)) // bit08 3DNow prefetch instructions
		fs.set_if(ecx & (1 << 9) != 0, int(FeatureID_X86.osvw)) // bit09 OS Visible Workaround
		fs.set_if(ecx & (1 << 10) != 0, int(FeatureID_X86.ibs)) // bit10 Instruction Based Sampling
		fs.set_if(ecx & (1 << 11) != 0, int(FeatureID_X86.xop)) // bit11 extended AVX instructions
		fs.set_if(ecx & (1 << 12) != 0, int(FeatureID_X86.skinit)) // bit12 SKINIT/STGI instructions
		fs.set_if(ecx & (1 << 13) != 0, int(FeatureID_X86.wdt)) // bit13 Watchdog timer
		fs.set_if(ecx & (1 << 15) != 0, int(FeatureID_X86.lwp)) // bit15 Light Weight Profiling
		fs.set_if(ecx & (1 << 16) != 0, int(FeatureID_X86.fma4)) // bit16 4 operands MAC instructions
		fs.set_if(ecx & (1 << 17) != 0, int(FeatureID_X86.tce)) // bit17 Translation Cache Extension
		fs.set_if(ecx & (1 << 19) != 0, int(FeatureID_X86.nodeid_msr)) // bit19 NodeId MSR
		fs.set_if(ecx & (1 << 21) != 0, int(FeatureID_X86.tbm)) // bit21 Trailing Bit Manipulations
		fs.set_if(ecx & (1 << 22) != 0, int(FeatureID_X86.topoext)) // bit22 Topology extensions CPUID leafs
		fs.set_if(ecx & (1 << 23) != 0, int(FeatureID_X86.perfctr_core)) // bit23 Core performance counter extensions
		fs.set_if(ecx & (1 << 24) != 0, int(FeatureID_X86.perfctr_nb)) // bit24 NB performance counter extensions
		fs.set_if(ecx & (1 << 26) != 0, int(FeatureID_X86.bpext)) // bit26 Data breakpoint extension
		fs.set_if(ecx & (1 << 27) != 0, int(FeatureID_X86.ptsc)) // bit27 Performance time-stamp counter
		fs.set_if(ecx & (1 << 28) != 0, int(FeatureID_X86.perfctr_llc)) // bit28 Last Level Cache performance counter extensions
		fs.set_if(ecx & (1 << 29) != 0, int(FeatureID_X86.mwaitx)) // bit29 MWAIT extension (MONITORX/MWAITX instructions)

		// Intel-defined CPU features, CPUID level 0x00000007:0 (EBX), word 9
		eax, ebx, ecx, edx = cpuidex(0x00000007, 0)
		fs.set_if(ebx & (1 << 0) != 0, int(FeatureID_X86.fsgsbase)) // bit00 RDFSBASE, WRFSBASE, RDGSBASE, WRGSBASE instructions
		fs.set_if(ebx & (1 << 1) != 0, int(FeatureID_X86.tsc_adjust)) // bit01 TSC adjustment MSR 0x3B
		fs.set_if(ebx & (1 << 2) != 0, int(FeatureID_X86.sgx)) // bit02 Software Guard Extensions
		fs.set_if(ebx & (1 << 3) != 0, int(FeatureID_X86.bmi1)) // bit03 1st group bit manipulation extensions
		fs.set_if(ebx & (1 << 4) != 0, int(FeatureID_X86.hle)) // bit04 Hardware Lock Elision
		fs.set_if(ebx & (1 << 5) != 0, int(FeatureID_X86.avx2)) // bit05 AVX2 instructions
		fs.set_if(ebx & (1 << 6) != 0, int(FeatureID_X86.fdp_excptn_only)) // bit06 FPU data pointer updated only on x87 exceptions
		fs.set_if(ebx & (1 << 7) != 0, int(FeatureID_X86.smep)) // bit07 Supervisor Mode Execution Protection
		fs.set_if(ebx & (1 << 8) != 0, int(FeatureID_X86.bmi2)) // bit08 2nd group bit manipulation extensions
		fs.set_if(ebx & (1 << 9) != 0, int(FeatureID_X86.erms)) // bit09 Enhanced REP MOVSB/STOSB instructions
		fs.set_if(ebx & (1 << 10) != 0, int(FeatureID_X86.invpcid)) // bit10 Invalidate Processor Context ID
		fs.set_if(ebx & (1 << 11) != 0, int(FeatureID_X86.rtm)) // bit11 Restricted Transactional Memory
		fs.set_if(ebx & (1 << 12) != 0, int(FeatureID_X86.cqm)) // bit12 Cache QoS Monitoring
		fs.set_if(ebx & (1 << 13) != 0, int(FeatureID_X86.zero_fcs_fds)) // bit13 Zero out FPU CS and FPU DS
		fs.set_if(ebx & (1 << 14) != 0, int(FeatureID_X86.mpx)) // bit14 Memory Protection Extension
		fs.set_if(ebx & (1 << 15) != 0, int(FeatureID_X86.rdt_a)) // bit15 Resource Director Technology Allocation
		fs.set_if(ebx & (1 << 16) != 0, int(FeatureID_X86.avx512f)) // bit16 AVX-512 Foundation
		fs.set_if(ebx & (1 << 17) != 0, int(FeatureID_X86.avx512dq)) // bit17 AVX-512 DQ (Double/Quad granular) Instructions
		fs.set_if(ebx & (1 << 18) != 0, int(FeatureID_X86.rdseed)) // bit18 RDSEED instruction
		fs.set_if(ebx & (1 << 19) != 0, int(FeatureID_X86.adx)) // bit19 ADCX and ADOX instructions
		fs.set_if(ebx & (1 << 20) != 0, int(FeatureID_X86.smap)) // bit20 Supervisor Mode Access Prevention
		fs.set_if(ebx & (1 << 21) != 0, int(FeatureID_X86.avx512ifma)) // bit21 AVX-512 Integer Fused Multiply-Add instructions
		fs.set_if(ebx & (1 << 23) != 0, int(FeatureID_X86.clflushopt)) // bit23 CLFLUSHOPT instruction
		fs.set_if(ebx & (1 << 24) != 0, int(FeatureID_X86.clwb)) // bit24 CLWB instruction
		fs.set_if(ebx & (1 << 25) != 0, int(FeatureID_X86.intel_pt)) // bit25 Intel Processor Trace
		fs.set_if(ebx & (1 << 26) != 0, int(FeatureID_X86.avx512pf)) // bit26 AVX-512 Prefetch
		fs.set_if(ebx & (1 << 27) != 0, int(FeatureID_X86.avx512er)) // bit27 AVX-512 Exponential and Reciprocal
		fs.set_if(ebx & (1 << 28) != 0, int(FeatureID_X86.avx512cd)) // bit28 AVX-512 Conflict Detection
		fs.set_if(ebx & (1 << 29) != 0, int(FeatureID_X86.sha_ni)) // bit29 SHA1/SHA256 Instruction Extensions
		fs.set_if(ebx & (1 << 30) != 0, int(FeatureID_X86.avx512bw)) // bit30 AVX-512 BW (Byte/Word granular) Instructions
		fs.set_if(ebx & (1 << 31) != 0, int(FeatureID_X86.avx512vl)) // bit31 AVX-512 VL (128/256 Vector Length) Extensions

		// Extended state features, CPUID level 0x0000000d:1 (EAX), word 10
		eax, ebx, ecx, edx = cpuidex(0x0000000d, 1)
		fs.set_if(eax & (1 << 0) != 0, int(FeatureID_X86.xsaveopt)) // bit00 XSAVEOPT instruction
		fs.set_if(eax & (1 << 1) != 0, int(FeatureID_X86.xsavec)) // bit01 XSAVEC instruction
		fs.set_if(eax & (1 << 2) != 0, int(FeatureID_X86.xgetbv1)) // bit02 XGETBV with ECX = 1 instruction
		fs.set_if(eax & (1 << 3) != 0, int(FeatureID_X86.xsaves)) // bit03 XSAVES/XRSTORS instructions
		fs.set_if(eax & (1 << 4) != 0, int(FeatureID_X86.xfd)) // bit04 eXtended Feature Disabling

		// Intel-defined CPU features, CPUID level 0x00000007:1 (EAX), word 12
		eax, ebx, ecx, edx = cpuidex(0x00000007, 1)
		fs.set_if(eax & (1 << 4) != 0, int(FeatureID_X86.avx_vnni)) // bit04 AVX VNNI instructions
		fs.set_if(eax & (1 << 5) != 0, int(FeatureID_X86.avx512_bf16)) // bit05 AVX512 BFLOAT16 instructions
		fs.set_if(eax & (1 << 7) != 0, int(FeatureID_X86.cmpccxadd)) // bit07 CMPccXADD instructions
		fs.set_if(eax & (1 << 8) != 0, int(FeatureID_X86.arch_perfmon_ext)) // bit08 Intel Architectural PerfMon Extension
		fs.set_if(eax & (1 << 10) != 0, int(FeatureID_X86.fzrm)) // bit10 Fast zero-length REP MOVSB
		fs.set_if(eax & (1 << 11) != 0, int(FeatureID_X86.fsrs)) // bit11 Fast short REP STOSB
		fs.set_if(eax & (1 << 12) != 0, int(FeatureID_X86.fsrc)) // bit12 Fast short REP {CMPSB,SCASB}
		fs.set_if(eax & (1 << 18) != 0, int(FeatureID_X86.lkgs)) // bit18
		fs.set_if(eax & (1 << 21) != 0, int(FeatureID_X86.amx_fp16)) // bit21 AMX fp16 Support
		fs.set_if(eax & (1 << 23) != 0, int(FeatureID_X86.avx_ifma)) // bit23 Support for VPMADD52[H,L]UQ
		fs.set_if(eax & (1 << 26) != 0, int(FeatureID_X86.lam)) // bit26 Linear Address Masking

		// AMD-defined CPU features, CPUID level 0x80000008 (EBX), word 13
		eax, ebx, ecx, edx = cpuidex(0x80000008, 0)
		fs.set_if(ebx & (1 << 0) != 0, int(FeatureID_X86.clzero)) // bit00 CLZERO instruction
		fs.set_if(ebx & (1 << 1) != 0, int(FeatureID_X86.irperf)) // bit01 Instructions Retired Count
		fs.set_if(ebx & (1 << 2) != 0, int(FeatureID_X86.xsaveerptr)) // bit02 Always save/restore FP error pointers
		fs.set_if(ebx & (1 << 4) != 0, int(FeatureID_X86.rdpru)) // bit04 Read processor register at user level
		fs.set_if(ebx & (1 << 9) != 0, int(FeatureID_X86.wbnoinvd)) // bit09 WBNOINVD instruction
		fs.set_if(ebx & (1 << 12) != 0, int(FeatureID_X86.amd_ibpb)) // bit12 Indirect Branch Prediction Barrier
		fs.set_if(ebx & (1 << 14) != 0, int(FeatureID_X86.amd_ibrs)) // bit14 Indirect Branch Restricted Speculation
		fs.set_if(ebx & (1 << 15) != 0, int(FeatureID_X86.amd_stibp)) // bit15 Single Thread Indirect Branch Predictors
		fs.set_if(ebx & (1 << 17) != 0, int(FeatureID_X86.amd_stibp_always_on)) // bit17 Single Thread Indirect Branch Predictors always-on preferred
		fs.set_if(ebx & (1 << 23) != 0, int(FeatureID_X86.amd_ppin)) // bit23 Protected Processor Inventory Number
		fs.set_if(ebx & (1 << 24) != 0, int(FeatureID_X86.amd_ssbd)) // bit24 Speculative Store Bypass Disable
		fs.set_if(ebx & (1 << 25) != 0, int(FeatureID_X86.virt_ssbd)) // bit25 Virtualized Speculative Store Bypass Disable
		fs.set_if(ebx & (1 << 26) != 0, int(FeatureID_X86.amd_ssb_no)) // bit26 Speculative Store Bypass is fixed in hardware.
		fs.set_if(ebx & (1 << 27) != 0, int(FeatureID_X86.cppc)) // bit27 Collaborative Processor Performance Control
		fs.set_if(ebx & (1 << 28) != 0, int(FeatureID_X86.amd_psfd)) // bit28 Predictive Store Forwarding Disable
		fs.set_if(ebx & (1 << 29) != 0, int(FeatureID_X86.btc_no)) // bit29 Not vulnerable to Branch Type Confusion
		fs.set_if(ebx & (1 << 31) != 0, int(FeatureID_X86.brs)) // bit31 Branch Sampling available

		// Thermal and Power Management Leaf, CPUID level 0x00000006 (EAX), word 14
		eax, ebx, ecx, edx = cpuidex(0x00000006, 0)
		fs.set_if(eax & (1 << 0) != 0, int(FeatureID_X86.dtherm)) // bit00 Digital Thermal Sensor
		fs.set_if(eax & (1 << 1) != 0, int(FeatureID_X86.ida)) // bit01 Intel Dynamic Acceleration
		fs.set_if(eax & (1 << 2) != 0, int(FeatureID_X86.arat)) // bit02 Always Running APIC Timer
		fs.set_if(eax & (1 << 4) != 0, int(FeatureID_X86.pln)) // bit04 Intel Power Limit Notification
		fs.set_if(eax & (1 << 6) != 0, int(FeatureID_X86.pts)) // bit06 Intel Package Thermal Status
		fs.set_if(eax & (1 << 7) != 0, int(FeatureID_X86.hwp)) // bit07 Intel Hardware P-states
		fs.set_if(eax & (1 << 8) != 0, int(FeatureID_X86.hwp_notify)) // bit08 HWP Notification
		fs.set_if(eax & (1 << 9) != 0, int(FeatureID_X86.hwp_act_window)) // bit09 HWP Activity Window
		fs.set_if(eax & (1 << 10) != 0, int(FeatureID_X86.hwp_epp)) // bit10 HWP Energy Perf. Preference
		fs.set_if(eax & (1 << 11) != 0, int(FeatureID_X86.hwp_pkg_req)) // bit11 HWP Package Level Request
		fs.set_if(eax & (1 << 19) != 0, int(FeatureID_X86.hfi)) // bit19 Hardware Feedback Interface

		// AMD SVM Feature Identification, CPUID level 0x8000000a (EDX), word 15
		eax, ebx, ecx, edx = cpuidex(0x8000000a, 0)
		fs.set_if(edx & (1 << 0) != 0, int(FeatureID_X86.npt)) // bit00 Nested Page Table support
		fs.set_if(edx & (1 << 1) != 0, int(FeatureID_X86.lbrv)) // bit01 LBR Virtualization support
		fs.set_if(edx & (1 << 2) != 0, int(FeatureID_X86.svm_lock)) // bit02 SVM locking MSR
		fs.set_if(edx & (1 << 3) != 0, int(FeatureID_X86.nrip_save)) // bit03 SVM next_rip save
		fs.set_if(edx & (1 << 4) != 0, int(FeatureID_X86.tsc_scale)) // bit04 TSC scaling support
		fs.set_if(edx & (1 << 5) != 0, int(FeatureID_X86.vmcb_clean)) // bit05 VMCB clean bits support
		fs.set_if(edx & (1 << 6) != 0, int(FeatureID_X86.flushbyasid)) // bit06 flush-by-ASID support
		fs.set_if(edx & (1 << 7) != 0, int(FeatureID_X86.decodeassists)) // bit07 Decode Assists support
		fs.set_if(edx & (1 << 10) != 0, int(FeatureID_X86.pausefilter)) // bit10 filtered pause intercept
		fs.set_if(edx & (1 << 12) != 0, int(FeatureID_X86.pfthreshold)) // bit12 pause filter threshold
		fs.set_if(edx & (1 << 13) != 0, int(FeatureID_X86.avic)) // bit13 Virtual Interrupt Controller
		fs.set_if(edx & (1 << 15) != 0, int(FeatureID_X86.v_vmsave_vmload)) // bit15 Virtual VMSAVE VMLOAD
		fs.set_if(edx & (1 << 16) != 0, int(FeatureID_X86.vgif)) // bit16 Virtual GIF
		fs.set_if(edx & (1 << 18) != 0, int(FeatureID_X86.x2avic)) // bit18 Virtual x2apic
		fs.set_if(edx & (1 << 20) != 0, int(FeatureID_X86.v_spec_ctrl)) // bit20 Virtual SPEC_CTRL
		fs.set_if(edx & (1 << 25) != 0, int(FeatureID_X86.vnmi)) // bit25 Virtual NMI
		fs.set_if(edx & (1 << 28) != 0, int(FeatureID_X86.svme_addr_chk)) // bit28 SVME addr check

		// Intel-defined CPU features, CPUID level 0x00000007:0 (ECX), word 16
		eax, ebx, ecx, edx = cpuidex(0x00000007, 0)
		fs.set_if(ecx & (1 << 1) != 0, int(FeatureID_X86.avx512vbmi)) // bit01 AVX512 Vector Bit Manipulation instructions
		fs.set_if(ecx & (1 << 2) != 0, int(FeatureID_X86.umip)) // bit02 User Mode Instruction Protection
		fs.set_if(ecx & (1 << 3) != 0, int(FeatureID_X86.pku)) // bit03 Protection Keys for Userspace
		fs.set_if(ecx & (1 << 4) != 0, int(FeatureID_X86.ospke)) // bit04 OS Protection Keys Enable
		fs.set_if(ecx & (1 << 5) != 0, int(FeatureID_X86.waitpkg)) // bit05 UMONITOR/UMWAIT/TPAUSE Instructions
		fs.set_if(ecx & (1 << 6) != 0, int(FeatureID_X86.avx512_vbmi2)) // bit06 Additional AVX512 Vector Bit Manipulation Instructions
		fs.set_if(ecx & (1 << 7) != 0, int(FeatureID_X86.shstk)) // bit07 Shadow stack
		fs.set_if(ecx & (1 << 8) != 0, int(FeatureID_X86.gfni)) // bit08 Galois Field New Instructions
		fs.set_if(ecx & (1 << 9) != 0, int(FeatureID_X86.vaes)) // bit09 Vector AES
		fs.set_if(ecx & (1 << 10) != 0, int(FeatureID_X86.vpclmulqdq)) // bit10 Carry-Less Multiplication Double Quadword
		fs.set_if(ecx & (1 << 11) != 0, int(FeatureID_X86.avx512_vnni)) // bit11 Vector Neural Network Instructions
		fs.set_if(ecx & (1 << 12) != 0, int(FeatureID_X86.avx512_bitalg)) // bit12 Support for VPOPCNT[B,W] and VPSHUF-BITQMB instructions
		fs.set_if(ecx & (1 << 13) != 0, int(FeatureID_X86.tme)) // bit13 Intel Total Memory Encryption
		fs.set_if(ecx & (1 << 14) != 0, int(FeatureID_X86.avx512_vpopcntdq)) // bit14 POPCNT for vectors of DW/QW
		fs.set_if(ecx & (1 << 16) != 0, int(FeatureID_X86.la57)) // bit16 5-level page tables
		fs.set_if(ecx & (1 << 22) != 0, int(FeatureID_X86.rdpid)) // bit22 RDPID instruction
		fs.set_if(ecx & (1 << 24) != 0, int(FeatureID_X86.bus_lock_detect)) // bit24 Bus Lock detect
		fs.set_if(ecx & (1 << 25) != 0, int(FeatureID_X86.cldemote)) // bit25 CLDEMOTE instruction
		fs.set_if(ecx & (1 << 27) != 0, int(FeatureID_X86.movdiri)) // bit27 MOVDIRI instruction
		fs.set_if(ecx & (1 << 28) != 0, int(FeatureID_X86.movdir64b)) // bit28 MOVDIR64B instruction
		fs.set_if(ecx & (1 << 29) != 0, int(FeatureID_X86.enqcmd)) // bit29 ENQCMD and ENQCMDS instructions
		fs.set_if(ecx & (1 << 30) != 0, int(FeatureID_X86.sgx_lc)) // bit30 Software Guard Extensions Launch Control

		// AMD-defined CPU features, CPUID level 0x80000007 (EBX), word 17
		eax, ebx, ecx, edx = cpuidex(0x80000007, 0)
		fs.set_if(ebx & (1 << 0) != 0, int(FeatureID_X86.overflow_recov)) // bit00 MCA overflow recovery support
		fs.set_if(ebx & (1 << 1) != 0, int(FeatureID_X86.succor)) // bit01 Uncorrectable error containment and recovery
		fs.set_if(ebx & (1 << 3) != 0, int(FeatureID_X86.smca)) // bit03 Scalable MCA

		// Intel-defined CPU features, CPUID level 0x00000007:0 (EDX), word 18
		eax, ebx, ecx, edx = cpuidex(0x00000007, 0)
		fs.set_if(edx & (1 << 2) != 0, int(FeatureID_X86.avx512_4vnniw)) // bit02 AVX-512 Neural Network Instructions
		fs.set_if(edx & (1 << 3) != 0, int(FeatureID_X86.avx512_4fmaps)) // bit03 AVX-512 Multiply Accumulation Single precision
		fs.set_if(edx & (1 << 4) != 0, int(FeatureID_X86.fsrm)) // bit04 Fast Short Rep Mov
		fs.set_if(edx & (1 << 8) != 0, int(FeatureID_X86.avx512_vp2intersect)) // bit08 AVX-512 Intersect for D/Q
		fs.set_if(edx & (1 << 9) != 0, int(FeatureID_X86.srbds_ctrl)) // bit09 SRBDS mitigation MSR available
		fs.set_if(edx & (1 << 10) != 0, int(FeatureID_X86.md_clear)) // bit10 VERW clears CPU buffers
		fs.set_if(edx & (1 << 11) != 0, int(FeatureID_X86.rtm_always_abort)) // bit11 RTM transaction always aborts
		fs.set_if(edx & (1 << 13) != 0, int(FeatureID_X86.tsx_force_abort)) // bit13 TSX_FORCE_ABORT
		fs.set_if(edx & (1 << 14) != 0, int(FeatureID_X86.serialize)) // bit14 SERIALIZE instruction
		fs.set_if(edx & (1 << 15) != 0, int(FeatureID_X86.hybrid_cpu)) // bit15 This part has CPUs of more than one type
		fs.set_if(edx & (1 << 16) != 0, int(FeatureID_X86.tsxldtrk)) // bit16 TSX Suspend Load Address Tracking
		fs.set_if(edx & (1 << 18) != 0, int(FeatureID_X86.pconfig)) // bit18 Intel PCONFIG
		fs.set_if(edx & (1 << 19) != 0, int(FeatureID_X86.arch_lbr)) // bit19 Intel ARCH LBR
		fs.set_if(edx & (1 << 20) != 0, int(FeatureID_X86.ibt)) // bit20 Indirect Branch Tracking
		fs.set_if(edx & (1 << 22) != 0, int(FeatureID_X86.amx_bf16)) // bit22 AMX bf16 Support
		fs.set_if(edx & (1 << 23) != 0, int(FeatureID_X86.avx512_fp16)) // bit23 AVX512 FP16
		fs.set_if(edx & (1 << 24) != 0, int(FeatureID_X86.amx_tile)) // bit24 AMX tile Support
		fs.set_if(edx & (1 << 25) != 0, int(FeatureID_X86.amx_int8)) // bit25 AMX int8 Support
		fs.set_if(edx & (1 << 26) != 0, int(FeatureID_X86.spec_ctrl)) // bit26 Speculation Control (IBRS + IBPB)
		fs.set_if(edx & (1 << 27) != 0, int(FeatureID_X86.intel_stibp)) // bit27 Single Thread Indirect Branch Predictors
		fs.set_if(edx & (1 << 28) != 0, int(FeatureID_X86.flush_l1d)) // bit28 Flush L1D cache
		fs.set_if(edx & (1 << 29) != 0, int(FeatureID_X86.arch_capabilities)) // bit29 IA32_ARCH_CAPABILITIES MSR (Intel)
		fs.set_if(edx & (1 << 30) != 0, int(FeatureID_X86.core_capabilities)) // bit30 IA32_CORE_CAPABILITIES MSR
		fs.set_if(edx & (1 << 31) != 0, int(FeatureID_X86.spec_ctrl_ssbd)) // bit31 Speculative Store Bypass Disable

		// AMD-defined memory encryption features, CPUID level 0x8000001f (EAX), word 19
		eax, ebx, ecx, edx = cpuidex(0x8000001f, 0)
		fs.set_if(eax & (1 << 0) != 0, int(FeatureID_X86.sme)) // bit00 AMD Secure Memory Encryption
		fs.set_if(eax & (1 << 1) != 0, int(FeatureID_X86.sev)) // bit01 AMD Secure Encrypted Virtualization
		fs.set_if(eax & (1 << 2) != 0, int(FeatureID_X86.vm_page_flush)) // bit02 VM Page Flush MSR is supported
		fs.set_if(eax & (1 << 3) != 0, int(FeatureID_X86.sev_es)) // bit03 AMD Secure Encrypted Virtualization - Encrypted State
		fs.set_if(eax & (1 << 9) != 0, int(FeatureID_X86.v_tsc_aux)) // bit09 Virtual TSC_AUX
		fs.set_if(eax & (1 << 10) != 0, int(FeatureID_X86.sme_coherent)) // bit10 AMD hardware-enforced cache coherency
		fs.set_if(eax & (1 << 14) != 0, int(FeatureID_X86.debug_swap)) // bit14 AMD SEV-ES full debug state swap support

		// AMD-defined Extended Feature 2 EAX, CPUID level 0x80000021 (EAX), word 20
		eax, ebx, ecx, edx = cpuidex(0x80000021, 0)
		fs.set_if(eax & (1 << 0) != 0, int(FeatureID_X86.no_nested_data_bp)) // bit00 No Nested Data Breakpoints
		fs.set_if(eax & (1 << 1) != 0, int(FeatureID_X86.wrmsr_xx_base_ns)) // bit01 WRMSR to {FS,GS,KERNEL_GS}_BASE is non-serializing
		fs.set_if(eax & (1 << 2) != 0, int(FeatureID_X86.lfence_rdtsc)) // bit02 LFENCE always serializing / synchronizes RDTSC
		fs.set_if(eax & (1 << 6) != 0, int(FeatureID_X86.null_sel_clr_base)) // bit06 Null Selector Clears Base
		fs.set_if(eax & (1 << 8) != 0, int(FeatureID_X86.autoibrs)) // bit08 Automatic IBRS
		fs.set_if(eax & (1 << 9) != 0, int(FeatureID_X86.no_smm_ctl_msr)) // bit09 SMM_CTL MSR is not present
		fs.set_if(eax & (1 << 27) != 0, int(FeatureID_X86.sbpb)) // bit27 Selective Branch Prediction Barrier
		fs.set_if(eax & (1 << 28) != 0, int(FeatureID_X86.ibpb_brtype)) // bit28 MSR_PRED_CMD[IBPB] flushes all branch type predictions
		fs.set_if(eax & (1 << 29) != 0, int(FeatureID_X86.srso_no)) // bit29 CPU is not affected by SRSO
	}
	return fs
}

fn val_as_string(values ...u32) string {
	mut r := []u8{len: 4 * values.len}
	for i, v in values {
		mut dst := unsafe { r[i * 4..] }
		dst[0] = u8(v & 0xff)
		dst[1] = u8((v >> 8) & 0xff)
		dst[2] = u8((v >> 16) & 0xff)
		dst[3] = u8((v >> 24) & 0xff)
		if dst[0] == 0 || dst[1] == 0 || dst[2] == 0 || dst[3] == 0 {
			break
		}
	}
	return unsafe { tos_clone(&r[0]) }
}
