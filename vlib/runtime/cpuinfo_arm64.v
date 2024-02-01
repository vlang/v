module runtime

// https://github.com/klauspost/cpuid
// https://www.kernel.org/doc/html/latest/arch/arm64/cpu-feature-registers.html
import bitfield

$if arm64 {
	#flag -I@VMODROOT/vlib/runtime/asm
	#include "cpuinfo.h"
	// doesn't support arm64 embedded asm, so include a pre-compiled obj
	#flag @VMODROOT/vlib/runtime/asm/cpuinfo_arm64.o
}
fn C.read_aarch64_features(voidptr)

// vfmt off
// Vendor_ARM64 is a representation of an ARM64 CPU vendor.
pub enum Vendor_ARM64 {
	vendor_unknown
	ampere
	arm
	broadcom
	cavium
	dec
	fujitsu
	infineon
	intel
	motorola
	nvidia
	amcc
	qualcomm
	marvell
	phytium
}

// FeatureID_ARM64 is the ID of an ARM64 CPU feature.
pub enum FeatureID_ARM64 {
	unknown = -1
	// ID_AA64PFR0_EL1, AArch64 Processor Feature Register 0
	fp        // Floating-point
	fphp      // Half-precision floating point support
	asimd     // Advanced SIMD
	asimdhp   // Advanced SIMD half-precision support
	//gic3_gic4 // GIC CPU interface system registers 3.0 and 4.0
	//gic_4p1   // GIC CPU interface system registers 4.1
	ras       // RAS Extension 1.0 implemented
	ras_1p1   // RAS Extension 1.1 implemented 
	sve       // Scalable Vector Extension
	sel2      // Secure EL2 is implemented
	mpam_0p1  // MPAM extension is 0.1
	mpam_1p0  // MPAM extension is 1.0
	mpam_1p1  // MPAM extension is 1.1
	amu_v1    // Activity Monitors Extension v1 is implemented
	amu_v1p1  // Activity Monitors Extension v1.1 is implemented
	dit       // Data independent timing
	csv2      // Speculative use of out of context branch targets csv2
	csv2_2    // Speculative use of out of context branch targets csv2_2
	csv2_1p1  // Speculative use of out of context branch targets csv2_1p1
	csv2_1p2  // Speculative use of out of context branch targets csv2_1p2
	csv3      // Speculative use of faulting data
	
	// ID_AA64PFR1_EL1, AArch64 Processor Feature Register 1
	bti       // The Branch Target Identification mechanism is implemented
	ssbs      // Speculative Store Bypass Safe PSTATE bit
	ssbs2     // Speculative Store Bypassing controls, adds the MSR and MRS instructions
	mte       // Instruction-only Memory Tagging Extension is implemented
	mte2      // Full Memory Tagging Extension is implemented
	mte3      // Memory Tagging Extension is implemented with support for asymmetric Tag Check Fault handling
	ras_frac  // RAS Extension fractional field
	mpam_frac // The minor version number of the MPAM extension is 0/1
	rndr_trap // Trapping of RNDR and RNDRRS to EL3 is supported
	csv2_frac // CSV2 fractional field
	nmi       // Non-maskable Interrupt
	
	// ID_AA64ISAR0_EL1, AArch64 Instruction Set Attribute Register 0
	aes       // Hardware-accelerated Advanced Encryption Standard
	pmull     // Polynomial multiply long (PMULL/PMULL2)
	sha1      // Hardware-accelerated SHA1 (SHA1C, etc)
	sha256    // Hardware-accelerated SHA2-256 (SHA256H, etc)
	sha512    // Hardware-accelerated SHA512
	crc32     // Hardware-accelerated CRC-32
	atomics   // Armv8.1 atomic instructions
	asimdrdm  // Rounding Double Multiply Accumulate/Subtract (SQRDMLAH/SQRDMLSH)
	sha3      // Hardware-accelerated SHA3
	sm3       // Hardware-accelerated SM3
	sm4       // Hardware-accelerated SM4
	asimddp   // Dot product instruction
	asimdfhm  // Additional half-precision instructions FMLAL and FMLSL instructions
	flagm    // Flag manipulation instructions CFINV, RMIF, SETF16, and SETF8 instructions are implemented
	flagm2   // Additional flag manipulation instructions
	tlbios    // Outer shareable TLB maintenance instructions are implemented
	tlbirange // Outer shareable and TLB range maintenance instructions are implemented
	rng       // True random number generator support
	
	// ID_AA64ISAR1_EL1, AArch64 Instruction Set Attribute Register 1
	dpb             // Data Persistence writeback(DC CVAP)
	dpb2            // Data Persistence writeback(DC CVAP/DC CVADP)
	pacqarma5       // Address Authentication using the QARMA5 algorithm
	apa_pauth       // Address Authentication using the QARMA5 algorithm
	apa_epac        // Address Authentication using the QARMA5 algorithm
	apa_pauth2      // Address Authentication using the QARMA5 algorithm
	apa_fpac        // Address Authentication using the QARMA5 algorithm
	apa_fpaccombine // Address Authentication using the QARMA5 algorithm
	pacimp          // IMPLEMENTATION DEFINED algorithm is implemented
	api_pauth       // Address Authentication using an IMPLEMENTATION DEFINED algorithm
	api_epac        // Address Authentication using an IMPLEMENTATION DEFINED algorithm
	api_pauth2      // Address Authentication using an IMPLEMENTATION DEFINED algorithm
	api_fpac        // Address Authentication using an IMPLEMENTATION DEFINED algorithm
	api_fpaccombine // Address Authentication using an IMPLEMENTATION DEFINED algorithm
	jscvt           // Support for JavaScript conversion (FJCVTZS)
	fcma            // Floating point complex numbers
	lrcpc           // Support for weaker release consistency (LDAPR, etc)
	ilrcpc          // Additional support for weaker release consistency
	gpa             // Generic Authentication using the QARMA5 algorithm
	gpi             // Generic Authentication using an IMPLEMENTATION DEFINED algorithm
	frintts         // Floating point to integer rounding FRINT32Z, FRINT32X, FRINT64Z, and FRINT64X instructions
	sb              // Speculation barrier
	specres         // CFP RCTX, DVP RCTX, and CPP RCTX instructions
	bf16            // BFloat16 instructions
	dgh             // Data Gathering Hint instruction
	i8mm            // Int8 matrix multiplication instructions
	xs              // The XS attribute, the TLBI and DSB instructions with the nXS qualifier, and the HCRX_EL2.{FGTnXS, FnXS} fields are supported
	ls64            // LD64B and ST64B instructions
	ls64_v          // The LD64B, ST64B, and ST64BV instructions, and their associated traps are supported
	ls64_accdata    // The LD64B, ST64B, ST64BV, and ST64BV0 instructions, the ACCDATA_EL1 register, and their associated traps are supported
	
	// ID_AA64ISAR2_EL1, AArch64 Instruction Set Attribute Register 2
	wfxt             // WFET and WFIT are supported
	rpres            // 12-bit reciprocal (square root) estimate precision
	gpa3             // Generic Authentication using the QARMA3 algorithm is implemented
	pacqarma3        // The QARMA3 algorithm is implemented
	apa3_pauth       // Address Authentication using the QARMA3 algorithm is implemented
	apa3_epac        // Address Authentication using the QARMA3 algorithm is implemented
	apa3_pauth2      // Address Authentication using the QARMA3 algorithm is implemented
	apa3_fpac        // Address Authentication using the QARMA3 algorithm is implemented
	apa3_fpaccombine // Address Authentication using the QARMA3 algorithm is implemented
	mops             // Standardized memory operations
	bc               // BC instruction
	constpacfield    // ConstPACField() returns TRUE
	
	// ID_AA64DFR0_EL1, AArch64 Debug Feature Register 0
	debug_v8	  // Armv8 debug architecture
	debug_v8_vhe  // Armv8 debug architecture with Virtualization Host Extensions
	debug_v8p2    // Armv8.2 debug architecture
	debug_v8p4    // Armv8.4 debug architecture
	debug_v8p8    // Armv8.8 debug architecture
	trace         // PE trace unit System registers implemented
	pmu_v3        // Performance Monitors Extension, PMUv3 implemented
	pmu_v3p1      // Performance Monitors Extension, PMUv3.1 implemented
	pmu_v3p4      // Performance Monitors Extension, PMUv3.4 implemented
	pmu_v3p5      // Performance Monitors Extension, PMUv3.5 implemented
	pmu_v3p7      // Performance Monitors Extension, PMUv3.7 implemented
	pmu_v3p8      // Performance Monitors Extension, PMUv3.8 implemented
	spe           // Statistical Profiling Extension implemented
	spe_v1p1      // Statistical Profiling Extension 1.1 implemented
	spe_v1p2      // Statistical Profiling Extension 1.2 implemented
	spe_v1p3      // Statistical Profiling Extension 1.3 implemented
	doublelock    // OS Double Lock implemented. OSDLR_EL1 is RW
	trf           // Armv8.4 Self-hosted Trace Extension implemented
	mtpmu         // Multi-threaded PMU extension
	hpmn0         // Zero PMU event counters for a Guest operating system
	
	// ID_AA64MMFR0_EL1, AArch64 Memory Model Feature Register 0
	pa_range_4g   // Physical Address range 4GB
	pa_range_64g  // Physical Address range 64GB
	pa_range_1t   // Physical Address range 1TB
	pa_range_4t   // Physical Address range 4TB
	pa_range_16t  // Physical Address range 16TB
	pa_range_256t // Physical Address range 256TB
	pa_range_4p   // Physical Address range 4PB
	asid_8        // Number of ASID bits is 8
	asid_16       // Number of ASID bits is 16
	bigend        // Mixed-endian support
	snsmem        // support for a distinction between Secure and Non-secure Memory
	bigendel0     // Mixed-endian support at EL0
	tgran16       // 16KB granule supported
	tgran64       // 64KB granule supported
	tgran4        // 4KB granule supported
	tgran16_2     // 16KB granule supported at stage 2
	tgran64_2     // 64KB granule supported at stage 2
	tgran4_2      // 4KB granule supported at stage 
	exs           // support for disabling context synchronizing exception entry and exit
	fgt           // The fine-grained trap controls are implemented
	ecv           // Enhanced Counter Virtualization

	// ID_AA64MMFR1_EL1, AArch64 Memory Model Feature Register 1
	hafdbs      // Hardware update of the Access flag is supported
	vmidbits_8  // Number of VMID bits is 8
	vmidbits_16 // Number of VMID bits is 16
	vh          // Virtualization Host Extensions
	hpds        // Hierarchical Permission Disables
	hpds2       // Hierarchical Permission Disables 2
	lo          // LORegions
	pan         // Privileged Access Never
	pan2        // Privileged Access Never 2
	pan3        // Privileged Access Never 3
	specsei     // The PE might generate an SError interrupt due to an External abort on a speculative read
	xnx         // Distinction between EL0 and EL1 execute-never control at stage 2 supported
	twed        // Configurable delayed trapping of WFE is supported
	ets         // Enhanced Translation Synchronization is supported
	hcx         // HCRX_EL2 and its associated EL3 trap are supported
	afp         // Alternate floating-point behaviour
	ntlbpa      // support for intermediate caching of translation table walks
	tidcp1      // SCTLR_EL1.TIDCP bit is implemented. If EL2 is implemented, SCTLR_EL2.TIDCP bit is implemented
	cmow        // support for cache maintenance instruction permission
	
	// ID_AA64MMFR2_EL1, AArch64 Memory Model Feature Register 2
	cnp        // Common not Private translations supported
	uao        // User Access Override
	lsm        // LSMAOE and nTLSMD bits supported
	iesb       // IESB bit in the SCTLR_ELx registers is supported
	lva        // VMSAv8-64 supports 52-bit VAs when using the 64KB translation granule
	ccidx      // 64-bit format implemented for all levels of the CCSIDR_EL
	nv         // Nested Virtualization
	ttst       // support for small translation tables
	uscat      // support for unaligned single-copy atomicity and atomic functions
	idst       // All exceptions generated by an AArch64 read access to the feature ID space are reported by ESR_ELx.EC == 0x18
	s2fwb      // HCR_EL2.FWB is supported
	ttl        // support for TTL field in address operations
	bbm_level0 // Level 0 support for changing block size is supported
	bbm_level1 // Level 1 support for changing block size is supported
	bbm_level2 // Level 2 support for changing block size is supported
	evt        // Enhanced Virtualization Traps
	e0pd       // E0PDx mechanism is implemented
	
	// ID_AA64ZFR0_EL1, SVE Feature ID register 0
	svebf16       // SVE BFloat16 instructions
	svei8mm       // SVE Int8 matrix multiplication instructions
	svef32mm      // SVE FP32 matrix multiplication instruction
	svef64mm      // SVE FP64 matrix multiplication instructions
	
	// not used
	cpuid      // Some CPU ID registers readable at user-level
	evtstrm    // Generic timer
	
	// Keep it last. It automatically defines the size of feature_set
	last_id
}

// vfmt on

pub struct AArch64Features {
pub mut:
	midr_el1         u64 // MIDR_EL1, Main ID Register
	id_aa64dfr0_el1  u64 // AArch64 Debug Feature Register 0
	id_aa64dfr1_el1  u64 // AArch64 Debug Feature Register 1
	id_aa64isar0_el1 u64 // AArch64 Instruction Set Attribute Register 0
	id_aa64isar1_el1 u64 // AArch64 Instruction Set Attribute Register 1
	id_aa64isar2_el1 u64 // AArch64 Instruction Set Attribute Register 2
	id_aa64mmfr0_el1 u64 // AArch64 Memory Model Feature Register 0
	id_aa64mmfr1_el1 u64 // AArch64 Memory Model Feature Register 1
	id_aa64mmfr2_el1 u64 // AArch64 Memory Model Feature Register 2
	id_aa64pfr0_el1  u64 // AArch64 Processor Feature Register 0
	id_aa64pfr1_el1  u64 // AArch64 Processor Feature Register 1
	id_aa64zfr0_el1  u64 // SVE Feature ID register 0
}

// CPUInfo_ARM64 contains information about the detected system CPU.
// If system have multiple cores, the CPUInfo_ARM64 only contains
// information of the core which current process running on.
pub struct CPUInfo_ARM64 {
pub mut:
	vendor_id        Vendor_ARM64      // Comparable CPU vendor ID
	vendor_string    string            // Raw vendor string.
	feature_set      bitfield.BitField // Features of the CPU
	physical_cores   int // Number of physical processor cores in your CPU. Will be 0 if undetectable.
	threads_per_core int = 1 // Number of threads per physical core. Will be 1 if undetectable.
	logical_cores    int // Number of physical cores times threads that can run on each core through the use of hyperthreading. Will be 0 if undetectable.
	variant          int // CPU variant number
	architecture     int // CPU architecture number
	part_num         int // Primary Part Number for the device
	revision         int // Revision number for the device
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

	aarch64 AArch64Features
}

// has returns whether the CPU supports one or more of the requested features.
@[inline]
pub fn (mut c CPUInfo_ARM64) has(ids ...FeatureID_ARM64) bool {
	for id in ids {
		if c.feature_set.has(int(id)) {
			return true
		}
	}
	return false
}

// all returns whether the CPU supports all of the requested features.
@[inline]
pub fn (mut c CPUInfo_ARM64) all(ids ...FeatureID_ARM64) bool {
	for id in ids {
		if !c.feature_set.has(int(id)) {
			return false
		}
	}
	return true
}

// feature_set returns all available features as strings.
pub fn (c CPUInfo_ARM64) feature_set() []string {
	mut s := []string{}
	for i in 0 .. c.feature_set.get_size() {
		if c.feature_set.get_bit(i) == 1 {
			s << unsafe { FeatureID_ARM64(i).str() }
		}
	}
	return s
}

// logical_cpu will return the Logical CPU the code is currently executing on.
fn (c CPUInfo_ARM64) logical_cpu() int {
	// TODO: add support
	return -1
}

// frequencies tries to compute the clock speed of the CPU.
fn (mut c CPUInfo_ARM64) frequencies() {
	// TODO: add support
}

// detect_arm will detect current CPU info.
pub fn detect_arm() CPUInfo_ARM64 {
	mut cpu := CPUInfo_ARM64{}
	$if arm64 {
		// Set defaults
		cpu.threads_per_core = 1
		cpu.cache.l1i = -1
		cpu.cache.l1d = -1
		cpu.cache.l2 = -1
		cpu.cache.l3 = -1
		support_arm(mut cpu)
	}
	return cpu
}

// support_arm decode the feature from sys regs
fn support_arm(mut c CPUInfo_ARM64) {
	mut fs := bitfield.new(int(FeatureID_ARM64.last_id) + 8)

	$if arm64 {
		mut aarch64 := AArch64Features{}
		C.read_aarch64_features(&aarch64)
		c.aarch64 = aarch64

		midr := aarch64.midr_el1 // Main ID Register
		dfr0 := aarch64.id_aa64dfr0_el1 // AArch64 Debug Feature Register 0
		// dfr1 := aarch64.id_aa64dfr1_el1 // AArch64 Debug Feature Register 1, not used yet
		isar0 := aarch64.id_aa64isar0_el1 // AArch64 Instruction Set Attribute Register 0
		isar1 := aarch64.id_aa64isar1_el1 // AArch64 Instruction Set Attribute Register 1
		isar2 := aarch64.id_aa64isar2_el1 // AArch64 Instruction Set Attribute Register 2
		mmfr0 := aarch64.id_aa64mmfr0_el1 // AArch64 Memory Model Feature Register 0
		mmfr1 := aarch64.id_aa64mmfr1_el1 // AArch64 Memory Model Feature Register 1
		mmfr2 := aarch64.id_aa64mmfr2_el1 // AArch64 Memory Model Feature Register 2
		pfr0 := aarch64.id_aa64pfr0_el1 // AArch64 Processor Feature Register 0
		pfr1 := aarch64.id_aa64pfr1_el1 // AArch64 Processor Feature Register 1
		zfr0 := aarch64.id_aa64zfr0_el1 // SVE Feature ID register 0

		// MIDR_EL1 - Main ID Register
		// https://developer.arm.com/docs/ddi0595/h/aarch64-system-registers/midr_el1
		//  x--------------------------------------------------x
		//  | Name                         |  bits   | visible |
		//  |--------------------------------------------------|
		//  | Implementer                  | [31-24] |    y    |
		//  |--------------------------------------------------|
		//  | Variant                      | [23-20] |    y    |
		//  |--------------------------------------------------|
		//  | Architecture                 | [19-16] |    y    |
		//  |--------------------------------------------------|
		//  | PartNum                      | [15-4]  |    y    |
		//  |--------------------------------------------------|
		//  | Revision                     | [3-0]   |    y    |
		//  x--------------------------------------------------x
		match (midr >> 24) & 0xff {
			0x41 {
				c.vendor_string = 'Arm Limited'
				c.vendor_id = .arm
			}
			0x42 {
				c.vendor_string = 'Broadcom Corporation'
				c.vendor_id = .broadcom
			}
			0x43 {
				c.vendor_string = 'Cavium Inc'
				c.vendor_id = .cavium
			}
			0x44 {
				c.vendor_string = 'Digital Equipment Corporation'
				c.vendor_id = .dec
			}
			0x46 {
				c.vendor_string = 'Fujitsu Ltd'
				c.vendor_id = .fujitsu
			}
			0x49 {
				c.vendor_string = 'Infineon Technologies AG'
				c.vendor_id = .infineon
			}
			0x4D {
				c.vendor_string = 'Motorola or Freescale Semiconductor Inc'
				c.vendor_id = .motorola
			}
			0x4E {
				c.vendor_string = 'NVIDIA Corporation'
				c.vendor_id = .nvidia
			}
			0x50 {
				c.vendor_string = 'Applied Micro Circuits Corporation'
				c.vendor_id = .amcc
			}
			0x51 {
				c.vendor_string = 'Qualcomm Inc'
				c.vendor_id = .qualcomm
			}
			0x56 {
				c.vendor_string = 'Marvell International Ltd'
				c.vendor_id = .marvell
			}
			0x69 {
				c.vendor_string = 'Intel Corporation'
				c.vendor_id = .intel
			}
			0x70 {
				c.vendor_string = 'phytium.com.cn'
				c.vendor_id = .phytium
			}
			0xC0 {
				c.vendor_string = 'Ampere Computing'
				c.vendor_id = .ampere
			}
			else {}
		}

		// Variant, bits [23:20]
		// An IMPLEMENTATION DEFINED variant number.
		// Typically, this field is used to distinguish between different product variants, or major revisions of a product.
		c.variant = int(midr >> 20) & 0xf

		// Architecture, bits [19:16]
		// 0b0001		Armv4.
		// 0b0010		Armv4T.
		// 0b0011		Armv5 (obsolete).
		// 0b0100		Armv5T.
		// 0b0101		Armv5TE.
		// 0b0110		Armv5TEJ.
		// 0b0111		Armv6.
		// 0b1111		Architectural features are individually identified in the ID_* registers, see 'ID registers'.
		// TODO: more accurate method detect architecture
		c.architecture = if int(midr >> 16) & 0xf == 0b1111 { 8 } else { 0 }

		// PartNum, bits [15:4]
		// An IMPLEMENTATION DEFINED primary part number for the device.
		// On processors implemented by Arm, if the top four bits of the primary
		// part number are 0x0 or 0x7, the variant and architecture are encoded differently.
		// Revision, bits [3:0]
		// An IMPLEMENTATION DEFINED revision number for the device.
		c.part_num = int(midr >> 4) & 0xfff

		// Revision number for the device, bits [3:0]
		// This field has an IMPLEMENTATION DEFINED value.
		c.revision = int(midr >> 0) & 0x0f

		// ID_AA64DFR0_EL1, AArch64 Debug Feature Register 0
		//  x--------------------------------------------------x
		//  | Name                         |  bits   | visible |
		//  |--------------------------------------------------|
		//  | HPMN0                        | [63-60] |    y    |
		//  |--------------------------------------------------|
		//  | MTPMU                        | [51-48] |    y    |
		//  |--------------------------------------------------|
		//  | TraceFilt                    | [23-20] |    y    |
		//  |--------------------------------------------------|
		//  | DoubleLock                   | [39-36] |    y    |
		//  |--------------------------------------------------|
		//  | PMSVer                       | [35-32] |    y    |
		//  |--------------------------------------------------|
		//  | CTX_CMPs                     | [31-28] |    y    |
		//  |--------------------------------------------------|
		//  | WRPs                         | [23-20] |    y    |
		//  |--------------------------------------------------|
		//  | BRPs                         | [15-12] |    y    |
		//  |--------------------------------------------------|
		//  | PMUVer                       | [11-8]  |    y    |
		//  |--------------------------------------------------|
		//  | TraceVer                     | [7-4]   |    y    |
		//  |--------------------------------------------------|
		//  | DebugVer                     | [3-0]   |    y    |
		//  x--------------------------------------------------x
		fs.set_if((dfr0 >> 0) & 0xf == 0b0110, int(FeatureID_ARM64.debug_v8))
		fs.set_if((dfr0 >> 0) & 0xf == 0b0111, int(FeatureID_ARM64.debug_v8_vhe))
		fs.set_if((dfr0 >> 0) & 0xf == 0b1000, int(FeatureID_ARM64.debug_v8p2))
		fs.set_if((dfr0 >> 0) & 0xf == 0b1001, int(FeatureID_ARM64.debug_v8p4))
		fs.set_if((dfr0 >> 0) & 0xf == 0b1010, int(FeatureID_ARM64.debug_v8p8))

		fs.set_if((dfr0 >> 4) & 0xf != 0b0000, int(FeatureID_ARM64.trace))

		fs.set_if((dfr0 >> 8) & 0xf == 0b0001, int(FeatureID_ARM64.pmu_v3))
		fs.set_if((dfr0 >> 8) & 0xf == 0b0100, int(FeatureID_ARM64.pmu_v3p1))
		fs.set_if((dfr0 >> 8) & 0xf == 0b0101, int(FeatureID_ARM64.pmu_v3p4))
		fs.set_if((dfr0 >> 8) & 0xf == 0b0110, int(FeatureID_ARM64.pmu_v3p5))
		fs.set_if((dfr0 >> 8) & 0xf == 0b0111, int(FeatureID_ARM64.pmu_v3p7))
		fs.set_if((dfr0 >> 8) & 0xf == 0b1000, int(FeatureID_ARM64.pmu_v3p8))

		fs.set_if((dfr0 >> 32) & 0xf == 0b0001, int(FeatureID_ARM64.spe))
		fs.set_if((dfr0 >> 32) & 0xf == 0b0010, int(FeatureID_ARM64.spe_v1p1))
		fs.set_if((dfr0 >> 32) & 0xf == 0b0011, int(FeatureID_ARM64.spe_v1p2))
		fs.set_if((dfr0 >> 32) & 0xf == 0b0100, int(FeatureID_ARM64.spe_v1p3))

		fs.set_if((dfr0 >> 36) & 0xf == 0b0000, int(FeatureID_ARM64.doublelock))
		fs.set_if((dfr0 >> 40) & 0xf != 0b0000, int(FeatureID_ARM64.trf))
		fs.set_if((dfr0 >> 48) & 0xf != 0b0000, int(FeatureID_ARM64.mtpmu))
		fs.set_if((dfr0 >> 60) & 0xf != 0b0000, int(FeatureID_ARM64.hpmn0))
		// ID_AA64DFR1_EL1, AArch64 Debug Feature Register 1, not used yet
		// ID_AA64MMFR0_EL1, AArch64 Memory Model Feature Register 0
		//  x--------------------------------------------------x
		//  | Name                         |  bits   | visible |
		//  |--------------------------------------------------|
		//  | ECV                          | [63-60] |    y    |
		//  |--------------------------------------------------|
		//  | FGT                          | [59-56] |    y    |
		//  |--------------------------------------------------|
		//  | ExS                          | [47-44] |    y    |
		//  |--------------------------------------------------|
		//  | TGran4_2                     | [43-40] |    y    |
		//  |--------------------------------------------------|
		//  | TGran64_2                    | [39-36] |    y    |
		//  |--------------------------------------------------|
		//  | TGran16_2                    | [35-32] |    y    |
		//  |--------------------------------------------------|
		//  | TGran4                       | [31-28] |    y    |
		//  |--------------------------------------------------|
		//  | TGran64                      | [27-24] |    y    |
		//  |--------------------------------------------------|
		//  | TGran16                      | [23-20] |    y    |
		//  |--------------------------------------------------|
		//  | BigEndEL0                    | [19-16] |    y    |
		//  |--------------------------------------------------|
		//  | SNSMem                       | [15-12] |    y    |
		//  |--------------------------------------------------|
		//  | BigEnd                       | [11-8]  |    y    |
		//  |--------------------------------------------------|
		//  | ASIDBits                     | [7-4]   |    y    |
		//  |--------------------------------------------------|
		//  | PARange                      | [3-0]   |    y    |
		//  x--------------------------------------------------x
		fs.set_if((mmfr0 >> 0) & 0xf == 0b0000, int(FeatureID_ARM64.pa_range_4g))
		fs.set_if((mmfr0 >> 0) & 0xf == 0b0001, int(FeatureID_ARM64.pa_range_64g))
		fs.set_if((mmfr0 >> 0) & 0xf == 0b0010, int(FeatureID_ARM64.pa_range_1t))
		fs.set_if((mmfr0 >> 0) & 0xf == 0b0011, int(FeatureID_ARM64.pa_range_4t))
		fs.set_if((mmfr0 >> 0) & 0xf == 0b0100, int(FeatureID_ARM64.pa_range_16t))
		fs.set_if((mmfr0 >> 0) & 0xf == 0b0101, int(FeatureID_ARM64.pa_range_256t))
		fs.set_if((mmfr0 >> 0) & 0xf == 0b0110, int(FeatureID_ARM64.pa_range_4p))
		fs.set_if((mmfr0 >> 4) & 0xf == 0b0000, int(FeatureID_ARM64.asid_8))
		fs.set_if((mmfr0 >> 4) & 0xf == 0b0010, int(FeatureID_ARM64.asid_16))
		fs.set_if((mmfr0 >> 8) & 0xf != 0b0000, int(FeatureID_ARM64.bigend))
		fs.set_if((mmfr0 >> 12) & 0xf != 0b0000, int(FeatureID_ARM64.snsmem))
		fs.set_if((mmfr0 >> 16) & 0xf != 0b0000, int(FeatureID_ARM64.bigendel0))
		fs.set_if((mmfr0 >> 20) & 0xf != 0b0000, int(FeatureID_ARM64.tgran16))
		fs.set_if((mmfr0 >> 24) & 0xf == 0b0000, int(FeatureID_ARM64.tgran64))
		fs.set_if((mmfr0 >> 28) & 0xf != 0b1111, int(FeatureID_ARM64.tgran4))
		fs.set_if((mmfr0 >> 32) & 0xf != 0b0001, int(FeatureID_ARM64.tgran16_2))
		fs.set_if((mmfr0 >> 36) & 0xf != 0b0001, int(FeatureID_ARM64.tgran64_2))
		fs.set_if((mmfr0 >> 40) & 0xf != 0b0001, int(FeatureID_ARM64.tgran4_2))
		fs.set_if((mmfr0 >> 44) & 0xf != 0b0000, int(FeatureID_ARM64.exs))
		fs.set_if((mmfr0 >> 56) & 0xf != 0b0000, int(FeatureID_ARM64.fgt))
		fs.set_if((mmfr0 >> 60) & 0xf != 0b0000, int(FeatureID_ARM64.ecv))

		// ID_AA64MMFR1_EL1, AArch64 Memory Model Feature Register 1
		//  x--------------------------------------------------x
		//  | Name                         |  bits   | visible |
		//  |--------------------------------------------------|
		//  | CMOW                         | [59-56] |    y    |
		//  |--------------------------------------------------|
		//  | TIDCP1                       | [55-52] |    y    |
		//  |--------------------------------------------------|
		//  | nTLBPA                       | [51-48] |    y    |
		//  |--------------------------------------------------|
		//  | AFP                          | [47-44] |    y    |
		//  |--------------------------------------------------|
		//  | HCX                          | [43-40] |    y    |
		//  |--------------------------------------------------|
		//  | ETS                          | [39-36] |    y    |
		//  |--------------------------------------------------|
		//  | TWED                         | [35-32] |    y    |
		//  |--------------------------------------------------|
		//  | XNX                          | [31-28] |    y    |
		//  |--------------------------------------------------|
		//  | SpecSEI                      | [27-24] |    y    |
		//  |--------------------------------------------------|
		//  | PAN                          | [23-20] |    y    |
		//  |--------------------------------------------------|
		//  | LO                           | [19-16] |    y    |
		//  |--------------------------------------------------|
		//  | HPDS                         | [15-12] |    y    |
		//  |--------------------------------------------------|
		//  | VH                           | [11-8]  |    y    |
		//  |--------------------------------------------------|
		//  | VMIDBits                     | [7-4]   |    y    |
		//  |--------------------------------------------------|
		//  | HAFDBS                       | [3-0]   |    y    |
		//  x--------------------------------------------------x
		fs.set_if((mmfr1 >> 0) & 0xf != 0b0000, int(FeatureID_ARM64.hafdbs))
		fs.set_if((mmfr1 >> 4) & 0xf == 0b0000, int(FeatureID_ARM64.vmidbits_8))
		fs.set_if((mmfr1 >> 4) & 0xf == 0b0010, int(FeatureID_ARM64.vmidbits_16))
		fs.set_if((mmfr1 >> 8) & 0xf != 0b0000, int(FeatureID_ARM64.vh))
		fs.set_if((mmfr1 >> 12) & 0xf == 0b0001, int(FeatureID_ARM64.hpds))
		fs.set_if((mmfr1 >> 12) & 0xf == 0b0010, int(FeatureID_ARM64.hpds2))
		fs.set_if((mmfr1 >> 16) & 0xf != 0b0000, int(FeatureID_ARM64.lo))
		fs.set_if((mmfr1 >> 20) & 0xf == 0b0001, int(FeatureID_ARM64.pan))
		fs.set_if((mmfr1 >> 20) & 0xf == 0b0010, int(FeatureID_ARM64.pan2))
		fs.set_if((mmfr1 >> 20) & 0xf == 0b0011, int(FeatureID_ARM64.pan3))
		fs.set_if((mmfr1 >> 24) & 0xf != 0b0000, int(FeatureID_ARM64.specsei))
		fs.set_if((mmfr1 >> 28) & 0xf != 0b0000, int(FeatureID_ARM64.xnx))
		fs.set_if((mmfr1 >> 32) & 0xf != 0b0000, int(FeatureID_ARM64.twed))
		fs.set_if((mmfr1 >> 36) & 0xf != 0b0000, int(FeatureID_ARM64.ets))
		fs.set_if((mmfr1 >> 40) & 0xf != 0b0000, int(FeatureID_ARM64.hcx))
		fs.set_if((mmfr1 >> 44) & 0xf != 0b0000, int(FeatureID_ARM64.afp))
		fs.set_if((mmfr1 >> 48) & 0xf != 0b0000, int(FeatureID_ARM64.ntlbpa))
		fs.set_if((mmfr1 >> 52) & 0xf != 0b0000, int(FeatureID_ARM64.tidcp1))
		fs.set_if((mmfr1 >> 56) & 0xf != 0b0000, int(FeatureID_ARM64.cmow))

		// ID_AA64MMFR2_EL1, AArch64 Memory Model Feature Register 2
		//  x--------------------------------------------------x
		//  | Name                         |  bits   | visible |
		//  |--------------------------------------------------|
		//  | E0PD                         | [63-60] |    y    |
		//  |--------------------------------------------------|
		//  | EVT                          | [59-56] |    y    |
		//  |--------------------------------------------------|
		//  | BBM                          | [55-52] |    y    |
		//  |--------------------------------------------------|
		//  | TTL                          | [51-48] |    y    |
		//  |--------------------------------------------------|
		//  | FWB                          | [43-40] |    y    |
		//  |--------------------------------------------------|
		//  | IDS                          | [39-36] |    y    |
		//  |--------------------------------------------------|
		//  | AT                           | [35-32] |    y    |
		//  |--------------------------------------------------|
		//  | ST                           | [31-28] |    y    |
		//  |--------------------------------------------------|
		//  | NV                           | [27-24] |    y    |
		//  |--------------------------------------------------|
		//  | CCIDX                        | [23-20] |    y    |
		//  |--------------------------------------------------|
		//  | VARange                      | [19-16] |    y    |
		//  |--------------------------------------------------|
		//  | IESB                         | [15-12] |    y    |
		//  |--------------------------------------------------|
		//  | LSM                          | [11-8]  |    y    |
		//  |--------------------------------------------------|
		//  | UAO                          | [7-4]   |    y    |
		//  |--------------------------------------------------|
		//  | CnP                          | [3-0]   |    y    |
		//  x--------------------------------------------------x
		fs.set_if((mmfr2 >> 0) & 0xf != 0b0000, int(FeatureID_ARM64.cnp))
		fs.set_if((mmfr2 >> 4) & 0xf != 0b0000, int(FeatureID_ARM64.uao))
		fs.set_if((mmfr2 >> 8) & 0xf != 0b0000, int(FeatureID_ARM64.lsm))
		fs.set_if((mmfr2 >> 12) & 0xf != 0b0000, int(FeatureID_ARM64.iesb))
		fs.set_if((mmfr2 >> 16) & 0xf != 0b0000, int(FeatureID_ARM64.lva))
		fs.set_if((mmfr2 >> 20) & 0xf != 0b0000, int(FeatureID_ARM64.ccidx))
		fs.set_if((mmfr2 >> 24) & 0xf != 0b0000, int(FeatureID_ARM64.nv))
		fs.set_if((mmfr2 >> 28) & 0xf != 0b0000, int(FeatureID_ARM64.ttst))
		fs.set_if((mmfr2 >> 32) & 0xf != 0b0000, int(FeatureID_ARM64.uscat))
		fs.set_if((mmfr2 >> 36) & 0xf != 0b0000, int(FeatureID_ARM64.idst))
		fs.set_if((mmfr2 >> 40) & 0xf != 0b0000, int(FeatureID_ARM64.s2fwb))
		fs.set_if((mmfr2 >> 48) & 0xf != 0b0000, int(FeatureID_ARM64.ttl))
		fs.set_if((mmfr2 >> 52) & 0xf == 0b0000, int(FeatureID_ARM64.bbm_level0))
		fs.set_if((mmfr2 >> 52) & 0xf == 0b0001, int(FeatureID_ARM64.bbm_level1))
		fs.set_if((mmfr2 >> 52) & 0xf == 0b0010, int(FeatureID_ARM64.bbm_level2))
		fs.set_if((mmfr2 >> 56) & 0xf != 0b0000, int(FeatureID_ARM64.evt))
		fs.set_if((mmfr2 >> 60) & 0xf != 0b0000, int(FeatureID_ARM64.e0pd))

		// ID_AA64PFR0_EL1 - Processor Feature Register 0
		// x--------------------------------------------------x
		// | Name                         |  bits   | visible |
		// |--------------------------------------------------|
		// | CSV3                         | [63-60] |    y    |
		// |--------------------------------------------------|
		// | CSV2                         | [59-56] |    y    |
		// |--------------------------------------------------|
		// | DIT                          | [51-48] |    y    |
		// |--------------------------------------------------|
		// | AMU                          | [47-44] |    y    |
		// |--------------------------------------------------|
		// | MPAM                         | [43-40] |    y    |
		// |--------------------------------------------------|
		// | SEL2                         | [39-36] |    y    |
		// |--------------------------------------------------|
		// | SVE                          | [35-32] |    y    |
		// |--------------------------------------------------|
		// | RAS                          | [31-28] |    y    |
		// |--------------------------------------------------|
		// | GIC                          | [27-24] |    n    |
		// |--------------------------------------------------|
		// | AdvSIMD                      | [23-20] |    y    |
		// |--------------------------------------------------|
		// | FP                           | [19-16] |    y    |
		// x--------------------------------------------------x
		fs.set_if((pfr0 >> 16) & 0xf != 0b1111, int(FeatureID_ARM64.fp))
		fs.set_if((pfr0 >> 16) & 0xf == 0b0001, int(FeatureID_ARM64.fphp))

		fs.set_if((pfr0 >> 20) & 0xf != 0b1111, int(FeatureID_ARM64.asimd))
		fs.set_if((pfr0 >> 20) & 0xf == 0b0001, int(FeatureID_ARM64.asimdhp))

		// fs.set_if((pfr0 >> 24) & 0xf == 0b0001, int(FeatureID_ARM64.gic3_gic4))
		// fs.set_if((pfr0 >> 24) & 0xf == 0b0011, int(FeatureID_ARM64.gic_4p1))

		ras := (pfr0 >> 28) & 0xf != 0b0000
		fs.set_if(ras, int(FeatureID_ARM64.ras))

		fs.set_if((pfr0 >> 32) & 0xf != 0b0000, int(FeatureID_ARM64.sve))
		fs.set_if((pfr0 >> 36) & 0xf != 0b0000, int(FeatureID_ARM64.sel2))

		mpam_major := (pfr0 >> 40) & 0xf
		mpam_minor := (pfr1 >> 16) & 0xf
		fs.set_if(mpam_major == 0 && mpam_minor == 1, int(FeatureID_ARM64.mpam_0p1))
		fs.set_if(mpam_major == 1 && mpam_minor == 0, int(FeatureID_ARM64.mpam_1p0))
		fs.set_if(mpam_major == 1 && mpam_minor == 1, int(FeatureID_ARM64.mpam_1p1))

		fs.set_if((pfr0 >> 44) & 0xf == 0b0001, int(FeatureID_ARM64.amu_v1))
		fs.set_if((pfr0 >> 44) & 0xf == 0b0010, int(FeatureID_ARM64.amu_v1p1))

		fs.set_if((pfr0 >> 48) & 0xf == 0b0001, int(FeatureID_ARM64.dit))
		csv2 := (pfr0 >> 56) & 0xf == 0b0001
		fs.set_if(csv2, int(FeatureID_ARM64.csv2))
		fs.set_if((pfr0 >> 56) & 0xf == 0b0010, int(FeatureID_ARM64.csv2_2))
		fs.set_if((pfr0 >> 60) & 0xf != 0b0000, int(FeatureID_ARM64.csv3))

		// ID_AA64PFR1_EL1 - Processor Feature Register 1
		// x--------------------------------------------------x
		// | Name                         |  bits   | visible |
		// |--------------------------------------------------|
		// | RES0                         | [63-40] |    y    |
		// |--------------------------------------------------|
		// | NMI                          | [39-36] |    y    |
		// |--------------------------------------------------|
		// | CSV2_frac                    | [35-32] |    y    |
		// |--------------------------------------------------|
		// | RNDR_trap                    | [31-28] |    y    |
		// |--------------------------------------------------|
		// | RES0                         | [27-20] |    y    |
		// |--------------------------------------------------|
		// | MPAM_frac                    | [19-16] |    y    |
		// |--------------------------------------------------|
		// | RAS_frac                     | [15-12] |    y    |
		// |--------------------------------------------------|
		// | MTE                          | [11-8]  |    y    |
		// |--------------------------------------------------|
		// | SSBS                         | [7-4]   |    y    |
		// |--------------------------------------------------|
		// | BT                           | [3-0]   |    y    |
		// x--------------------------------------------------x

		fs.set_if((pfr1 >> 0) & 0xf != 0b0000, int(FeatureID_ARM64.bti))
		fs.set_if((pfr1 >> 4) & 0xf == 0b0001, int(FeatureID_ARM64.ssbs))
		fs.set_if((pfr1 >> 4) & 0xf == 0b0010, int(FeatureID_ARM64.ssbs2))

		fs.set_if((pfr1 >> 8) & 0xf == 0b0001, int(FeatureID_ARM64.mte))
		fs.set_if((pfr1 >> 8) & 0xf == 0b0010, int(FeatureID_ARM64.mte2))
		fs.set_if((pfr1 >> 8) & 0xf == 0b0011, int(FeatureID_ARM64.mte3))

		fs.set_if((pfr1 >> 12) & 0xf == 0b0001 && ras, int(FeatureID_ARM64.ras_1p1))
		fs.set_if((pfr1 >> 28) & 0xf != 0b0000, int(FeatureID_ARM64.rndr_trap))
		fs.set_if((pfr1 >> 32) & 0xf == 0b0001 && csv2, int(FeatureID_ARM64.csv2_1p1))
		fs.set_if((pfr1 >> 32) & 0xf == 0b0010 && csv2, int(FeatureID_ARM64.csv2_1p2))
		fs.set_if((pfr1 >> 36) & 0xf != 0b0000, int(FeatureID_ARM64.nmi))

		// https://developer.arm.com/docs/ddi0595/b/aarch64-system-registers/id_aa64isar0_el1
		//
		// ID_AA64ISAR0_EL1 - Instruction Set Attribute Register 0
		// x--------------------------------------------------x
		// | Name                         |  bits   | visible |
		// |--------------------------------------------------|
		// | RNDR                         | [63-60] |    y    |
		// |--------------------------------------------------|
		// | TLB                          | [59-56] |    y    |
		// |--------------------------------------------------|
		// | TS                           | [55-52] |    y    |
		// |--------------------------------------------------|
		// | FHM                          | [51-48] |    y    |
		// |--------------------------------------------------|
		// | DP                           | [47-44] |    y    |
		// |--------------------------------------------------|
		// | SM4                          | [43-40] |    y    |
		// |--------------------------------------------------|
		// | SM3                          | [39-36] |    y    |
		// |--------------------------------------------------|
		// | SHA3                         | [35-32] |    y    |
		// |--------------------------------------------------|
		// | RDM                          | [31-28] |    y    |
		// |--------------------------------------------------|
		// | ATOMICS                      | [23-20] |    y    |
		// |--------------------------------------------------|
		// | CRC32                        | [19-16] |    y    |
		// |--------------------------------------------------|
		// | SHA2                         | [15-12] |    y    |
		// |--------------------------------------------------|
		// | SHA1                         | [11-8]  |    y    |
		// |--------------------------------------------------|
		// | AES                          | [7-4]   |    y    |
		// x--------------------------------------------------x

		fs.set_if((isar0 >> 4) & 0xf != 0b0000, int(FeatureID_ARM64.aes))
		fs.set_if((isar0 >> 4) & 0xf == 0b0010, int(FeatureID_ARM64.pmull))
		fs.set_if((isar0 >> 8) & 0xf == 0b0001, int(FeatureID_ARM64.sha1))
		fs.set_if((isar0 >> 12) & 0xf != 0b0000, int(FeatureID_ARM64.sha256))
		fs.set_if((isar0 >> 12) & 0xf == 0b0010, int(FeatureID_ARM64.sha512))
		fs.set_if((isar0 >> 16) & 0xf == 0b0001, int(FeatureID_ARM64.crc32))
		fs.set_if((isar0 >> 20) & 0xf != 0b0000, int(FeatureID_ARM64.atomics))
		fs.set_if((isar0 >> 28) & 0xf != 0b0000, int(FeatureID_ARM64.asimdrdm))
		fs.set_if((isar0 >> 32) & 0xf != 0b0000, int(FeatureID_ARM64.sha3))
		fs.set_if((isar0 >> 36) & 0xf != 0b0000, int(FeatureID_ARM64.sm3))
		fs.set_if((isar0 >> 40) & 0xf != 0b0000, int(FeatureID_ARM64.sm4))
		fs.set_if((isar0 >> 44) & 0xf != 0b0000, int(FeatureID_ARM64.asimddp))
		fs.set_if((isar0 >> 48) & 0xf != 0b0000, int(FeatureID_ARM64.asimdfhm))
		fs.set_if((isar0 >> 52) & 0xf == 0b0001, int(FeatureID_ARM64.flagm))
		fs.set_if((isar0 >> 52) & 0xf == 0b0010, int(FeatureID_ARM64.flagm2))
		fs.set_if((isar0 >> 56) & 0xf == 0b0001, int(FeatureID_ARM64.tlbios))
		fs.set_if((isar0 >> 56) & 0xf == 0b0010, int(FeatureID_ARM64.tlbirange))
		fs.set_if((isar0 >> 60) & 0xf != 0b0000, int(FeatureID_ARM64.rng))

		// https://developer.arm.com/docs/ddi0595/b/aarch64-system-registers/id_aa64isar1_el1
		//
		// ID_AA64ISAR1_EL1 - Instruction set attribute register 1
		// x--------------------------------------------------x
		// | Name                         |  bits   | visible |
		// |--------------------------------------------------|
		// | LS64                         | [63-60] |    y    |
		// |--------------------------------------------------|
		// | XS                           | [59-56] |    y    |
		// |--------------------------------------------------|
		// | I8MM                         | [55-52] |    y    |
		// |--------------------------------------------------|
		// | DGH                          | [51-48] |    y    |
		// |--------------------------------------------------|
		// | BF16                         | [47-44] |    y    |
		// |--------------------------------------------------|
		// | SPECRES                      | [43-40] |    y    |
		// |--------------------------------------------------|
		// | SB                           | [39-36] |    y    |
		// |--------------------------------------------------|
		// | FRINTTS                      | [35-32] |    y    |
		// |--------------------------------------------------|
		// | GPI                          | [31-28] |    y    |
		// |--------------------------------------------------|
		// | GPA                          | [27-24] |    y    |
		// |--------------------------------------------------|
		// | LRCPC                        | [23-20] |    y    |
		// |--------------------------------------------------|
		// | FCMA                         | [19-16] |    y    |
		// |--------------------------------------------------|
		// | JSCVT                        | [15-12] |    y    |
		// |--------------------------------------------------|
		// | API                          | [11-8]  |    y    |
		// |--------------------------------------------------|
		// | APA                          | [7-4]   |    y    |
		// |--------------------------------------------------|
		// | DPB                          | [3-0]   |    y    |
		// x--------------------------------------------------x

		fs.set_if((isar1 >> 0) & 0xf == 0b0001, int(FeatureID_ARM64.dpb))
		fs.set_if((isar1 >> 0) & 0xf == 0b0010, int(FeatureID_ARM64.dpb2))

		fs.set_if((isar1 >> 4) & 0xf != 0b0000, int(FeatureID_ARM64.pacqarma5))
		fs.set_if((isar1 >> 4) & 0xf == 0b0001, int(FeatureID_ARM64.apa_pauth))
		fs.set_if((isar1 >> 4) & 0xf == 0b0010, int(FeatureID_ARM64.apa_epac))
		fs.set_if((isar1 >> 4) & 0xf == 0b0011, int(FeatureID_ARM64.apa_pauth2))
		fs.set_if((isar1 >> 4) & 0xf == 0b0100, int(FeatureID_ARM64.apa_fpac))
		fs.set_if((isar1 >> 4) & 0xf == 0b0101, int(FeatureID_ARM64.apa_fpaccombine))

		fs.set_if((isar1 >> 8) & 0xf != 0b0000, int(FeatureID_ARM64.pacimp))
		fs.set_if((isar1 >> 8) & 0xf == 0b0001, int(FeatureID_ARM64.api_pauth))
		fs.set_if((isar1 >> 8) & 0xf == 0b0010, int(FeatureID_ARM64.api_epac))
		fs.set_if((isar1 >> 8) & 0xf == 0b0011, int(FeatureID_ARM64.api_pauth2))
		fs.set_if((isar1 >> 8) & 0xf == 0b0100, int(FeatureID_ARM64.api_fpac))
		fs.set_if((isar1 >> 8) & 0xf == 0b0101, int(FeatureID_ARM64.api_fpaccombine))

		fs.set_if((isar1 >> 12) & 0xf != 0b0000, int(FeatureID_ARM64.jscvt))
		fs.set_if((isar1 >> 16) & 0xf != 0b0000, int(FeatureID_ARM64.fcma))

		fs.set_if((isar1 >> 20) & 0xf == 0b0001, int(FeatureID_ARM64.lrcpc))
		fs.set_if((isar1 >> 20) & 0xf == 0b0010, int(FeatureID_ARM64.ilrcpc))
		fs.set_if((isar1 >> 24) & 0xf != 0b0000, int(FeatureID_ARM64.gpa))
		fs.set_if((isar1 >> 28) & 0xf != 0b0000, int(FeatureID_ARM64.gpi))
		fs.set_if((isar1 >> 32) & 0xf != 0b0000, int(FeatureID_ARM64.frintts))
		fs.set_if((isar1 >> 36) & 0xf != 0b0000, int(FeatureID_ARM64.sb))
		fs.set_if((isar1 >> 40) & 0xf != 0b0000, int(FeatureID_ARM64.specres))
		fs.set_if((isar1 >> 44) & 0xf != 0b0000, int(FeatureID_ARM64.bf16))
		fs.set_if((isar1 >> 48) & 0xf != 0b0000, int(FeatureID_ARM64.dgh))
		fs.set_if((isar1 >> 52) & 0xf != 0b0000, int(FeatureID_ARM64.i8mm))
		fs.set_if((isar1 >> 56) & 0xf != 0b0000, int(FeatureID_ARM64.xs))
		fs.set_if((isar1 >> 60) & 0xf == 0b0001, int(FeatureID_ARM64.ls64))
		fs.set_if((isar1 >> 60) & 0xf == 0b0010, int(FeatureID_ARM64.ls64_v))
		fs.set_if((isar1 >> 60) & 0xf == 0b0011, int(FeatureID_ARM64.ls64_accdata))

		// ID_AA64ISAR2_EL1 - Instruction set attribute register 2
		// x--------------------------------------------------x
		// | Name                         |  bits   | visible |
		// |--------------------------------------------------|
		// | PAC_frac                     | [27-24] |    y    |
		// |--------------------------------------------------|
		// | BC                           | [23-20] |    y    |
		// |--------------------------------------------------|
		// | MOPS                         | [19-16] |    y    |
		// |--------------------------------------------------|
		// | APA3                         | [15-12] |    y    |
		// |--------------------------------------------------|
		// | GPA3                         | [11-8]  |    y    |
		// |--------------------------------------------------|
		// | RPRES                        | [7-4]   |    y    |
		// |--------------------------------------------------|
		// | WFxT                         | [3-0]   |    y    |
		// x--------------------------------------------------x

		fs.set_if((isar2 >> 0) & 0xf != 0b0000, int(FeatureID_ARM64.wfxt))
		fs.set_if((isar2 >> 4) & 0xf != 0b0000, int(FeatureID_ARM64.rpres))
		fs.set_if((isar2 >> 8) & 0xf != 0b0000, int(FeatureID_ARM64.gpa3))
		fs.set_if((isar2 >> 12) & 0xf != 0b0000, int(FeatureID_ARM64.pacqarma3))
		fs.set_if((isar2 >> 12) & 0xf == 0b0001, int(FeatureID_ARM64.apa3_pauth))
		fs.set_if((isar2 >> 12) & 0xf == 0b0010, int(FeatureID_ARM64.apa3_epac))
		fs.set_if((isar2 >> 12) & 0xf == 0b0011, int(FeatureID_ARM64.apa3_pauth2))
		fs.set_if((isar2 >> 12) & 0xf == 0b0100, int(FeatureID_ARM64.apa3_fpac))
		fs.set_if((isar2 >> 12) & 0xf == 0b0101, int(FeatureID_ARM64.apa3_fpaccombine))
		fs.set_if((isar2 >> 16) & 0xf != 0b0000, int(FeatureID_ARM64.mops))
		fs.set_if((isar2 >> 20) & 0xf != 0b0000, int(FeatureID_ARM64.bc))
		fs.set_if((isar2 >> 24) & 0xf != 0b0000, int(FeatureID_ARM64.constpacfield))

		// ID_AA64ZFR0_EL1, SVE Feature ID register 0
		// x--------------------------------------------------x
		// | Name                         |  bits   | visible |
		// |--------------------------------------------------|
		// | F64MM                        | [59-56] |    y    |
		// |--------------------------------------------------|
		// | F32MM                        | [55-52] |    y    |
		// |--------------------------------------------------|
		// | I8MM                         | [47-44] |    y    |
		// |--------------------------------------------------|
		// | BF16                         | [23-20] |    y    |
		// |--------------------------------------------------|
		// | SVEver                       | [3-0]   |    y    |
		// x--------------------------------------------------x
		fs.set_if((zfr0 >> 20) & 0xf != 0b0000, int(FeatureID_ARM64.svebf16))
		fs.set_if((zfr0 >> 44) & 0xf != 0b0000, int(FeatureID_ARM64.svei8mm))
		fs.set_if((zfr0 >> 52) & 0xf != 0b0000, int(FeatureID_ARM64.svef32mm))
		fs.set_if((zfr0 >> 56) & 0xf != 0b0000, int(FeatureID_ARM64.svef64mm))
	}
	c.feature_set = fs
}
