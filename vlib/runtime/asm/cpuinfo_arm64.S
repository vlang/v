/* as -march=armv9.3-a cpuinfo_arm64.S -o cpuinfo_arm64.o */
.text
.align 2
/*
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
*/
.global read_aarch64_features
.type read_aarch64_features,%function
read_aarch64_features:
	mrs x1, midr_el1
	str x1, [x0]
	add x0, x0, #8
	
	mrs x1, id_aa64dfr0_el1
	str x1, [x0]
	add x0, x0, #8
	
	mrs x1, id_aa64dfr1_el1
	str x1, [x0]
	add x0, x0, #8
	
	mrs x1, id_aa64isar0_el1
	str x1, [x0]
	add x0, x0, #8

	mrs x1, id_aa64isar1_el1
	str x1, [x0]
	add x0, x0, #8
	
	mrs x1, id_aa64isar2_el1
	str x1, [x0]
	add x0, x0, #8

	mrs x1, id_aa64mmfr0_el1
	str x1, [x0]
	add x0, x0, #8
	
	mrs x1, id_aa64mmfr1_el1
	str x1, [x0]
	add x0, x0, #8

	mrs x1, id_aa64mmfr2_el1
	str x1, [x0]
	add x0, x0, #8
	
	mrs x1, id_aa64pfr0_el1
	str x1, [x0]
	add x0, x0, #8

	mrs x1, id_aa64pfr1_el1
	str x1, [x0]
	add x0, x0, #8
	
	mrs x1, id_aa64zfr0_el1
	str x1, [x0]
	add x0, x0, #8

	ret
