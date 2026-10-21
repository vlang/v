// TinyCC's Windows headers target Windows XP (its _mingw.h sets _WIN32_WINNT to 0x502), and its
// <synchapi.h> declares the slim reader/writer locks and the condition variables, which came
// with Vista, only for _WIN32_WINNT >= 0x0600. Every Windows that V runs on has them. Without a
// declaration, TinyCC warns about each call, and takes the result of TryAcquireSRWLock* (a
// BOOLEAN, of which only the low byte is set) for an int. These are synchapi.h's declarations,
// except that a condition variable is a PVOID: TinyCC's <windef.h> has a CONDITION_VARIABLE of
// its own, which V uses, and which is not the RTL_CONDITION_VARIABLE of PCONDITION_VARIABLE.
#if defined(_WIN32) && defined(__TINYC__) && defined(_WIN32_WINNT) && _WIN32_WINNT < 0x0600
#ifndef V_SYNC_WINDOWS_TCC_H
#define V_SYNC_WINDOWS_TCC_H
WINBASEAPI VOID WINAPI InitializeSRWLock(PSRWLOCK SRWLock);
VOID WINAPI ReleaseSRWLockExclusive(PSRWLOCK SRWLock);
VOID WINAPI ReleaseSRWLockShared(PSRWLOCK SRWLock);
VOID WINAPI AcquireSRWLockExclusive(PSRWLOCK SRWLock);
VOID WINAPI AcquireSRWLockShared(PSRWLOCK SRWLock);
WINBASEAPI BOOLEAN WINAPI TryAcquireSRWLockExclusive(PSRWLOCK SRWLock);
WINBASEAPI BOOLEAN WINAPI TryAcquireSRWLockShared(PSRWLOCK SRWLock);
WINBASEAPI VOID WINAPI InitializeConditionVariable(PVOID ConditionVariable);
WINBASEAPI VOID WINAPI WakeConditionVariable(PVOID ConditionVariable);
WINBASEAPI VOID WINAPI WakeAllConditionVariable(PVOID ConditionVariable);
WINBASEAPI WINBOOL WINAPI SleepConditionVariableSRW(PVOID ConditionVariable,
	PSRWLOCK SRWLock, DWORD dwMilliseconds, ULONG Flags);
#endif
#endif
