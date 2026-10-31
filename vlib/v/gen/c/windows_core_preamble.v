module c

// headerless_windows_core_declarations supplies the storage used by the core Windows
// bindings. Native helpers compile their implementation against the Windows SDK.
fn (mut g FlatGen) headerless_windows_core_declarations() {
	g.writeln('#ifdef _WIN32')
	g.writeln('#define SRWLOCK_INIT { 0 }')
	g.writeln('#define FILE_ATTRIBUTE_READONLY 0x00000001U')
	g.writeln('#define FILE_WRITE_ATTRIBUTES 0x00000100U')
	g.writeln('#define FILE_FLAG_BACKUP_SEMANTICS 0x02000000U')
	g.writeln('#define ERROR_INVALID_PARAMETER 87')
	g.writeln('#define ERROR_INSUFFICIENT_BUFFER 122')
	g.writeln('typedef union ULARGE_INTEGER { struct { u32 LowPart; u32 HighPart; }; u64 QuadPart; } ULARGE_INTEGER;')
	g.writeln('typedef struct _FILETIME { u32 dwLowDateTime; u32 dwHighDateTime; } FILETIME;')
	g.writeln('typedef struct _SYSTEM_INFO { union { u32 dwOemId; struct { u16 wProcessorArchitecture; u16 wReserved; }; }; u32 dwPageSize; void* lpMinimumApplicationAddress; void* lpMaximumApplicationAddress; uintptr_t dwActiveProcessorMask; u32 dwNumberOfProcessors; u32 dwProcessorType; u32 dwAllocationGranularity; u16 wProcessorLevel; u16 wProcessorRevision; } SYSTEM_INFO;')
	g.writeln('typedef struct _MEMORYSTATUS { u32 dwLength; u32 dwMemoryLoad; size_t dwTotalPhys; size_t dwAvailPhys; size_t dwTotalPageFile; size_t dwAvailPageFile; size_t dwTotalVirtual; size_t dwAvailVirtual; } MEMORYSTATUS;')
	g.writeln('typedef struct _MEMORY_BASIC_INFORMATION { void* BaseAddress; void* AllocationBase; u32 AllocationProtect;')
	g.writeln('#ifdef _WIN64\nu16 PartitionId;\n#endif')
	g.writeln('size_t RegionSize; u32 State; u32 Protect; u32 Type; } MEMORY_BASIC_INFORMATION;')
	g.writeln('typedef struct PROCESS_MEMORY_COUNTERS { u32 cb; u32 PageFaultCount; size_t PeakWorkingSetSize; size_t WorkingSetSize; size_t QuotaPeakPagedPoolUsage; size_t QuotaPagedPoolUsage; size_t QuotaPeakNonPagedPoolUsage; size_t QuotaNonPagedPoolUsage; size_t PagefileUsage; size_t PeakPagefileUsage; } PROCESS_MEMORY_COUNTERS;')
	g.writeln('#ifdef _WIN64\ntypedef unsigned long long __v_windows_size_t;\n#else\ntypedef unsigned long __v_windows_size_t;\n#endif')
	g.writeln('HANDLE WINAPI CreateThread(SECURITY_ATTRIBUTES*, __v_windows_size_t, DWORD (WINAPI *)(void*), void*, DWORD, DWORD*);')
	if g.target.os == 'windows' {
		g.emit_c_runtime_declarations(['GetLastError', 'WaitForSingleObject', 'CloseHandle'])
	}
	g.writeln('#endif')
	if g.target.os == 'windows' {
		g.inlined_c_structs['_FILETIME'] = true
		g.collect_preserved_c_fns(['CreateThread'])
	}
}
