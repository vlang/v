// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

#ifndef V_TOOLCACHE_WINDOWS_HELPERS_H
#define V_TOOLCACHE_WINDOWS_HELPERS_H

#include <windows.h>

/* The bundled TinyCC ships no aclapi.h/accctrl.h, so the bootstrap (makev.bat -tcc,
 * vc/v_win.c) would fail with "include file 'aclapi.h' not found". Only two items
 * from those headers are used below; declare them when the header is unavailable.
 * The functions themselves live in advapi32, which is linked either way. */
#if defined(__has_include)
#if __has_include(<aclapi.h>)
#define V_TOOLCACHE_HAS_ACLAPI_H 1
#endif
#else
#define V_TOOLCACHE_HAS_ACLAPI_H 1
#endif

#ifdef V_TOOLCACHE_HAS_ACLAPI_H
#include <aclapi.h>
#else
typedef enum {
	SE_UNKNOWN_OBJECT_TYPE = 0,
	SE_FILE_OBJECT = 1
} SE_OBJECT_TYPE;
#endif

#ifdef __TINYC__
/* The bundled TinyCC import library (lib/advapi32.def) exports only a handful of
 * registry and crypto entry points, so the security functions below would fail to
 * link. Resolve them from advapi32.dll at run time instead; every wrapper reports
 * failure when a lookup fails, which makes v_toolcache_root_is_private() answer
 * "not private", the conservative side. */
/* advapi32 is already linked into the process, so the module handle lookup is
 * cheap; a function-local cache would be file-static state, which the parallel
 * C compilation splitter refuses to replicate across translation units. */
static FARPROC v_toolcache_advapi32_proc(const char *name) {
	HMODULE advapi32 = GetModuleHandleW(L"advapi32.dll");
	if (advapi32 == NULL) {
		advapi32 = LoadLibraryW(L"advapi32.dll");
	}
	return advapi32 == NULL ? NULL : GetProcAddress(advapi32, name);
}

static BOOL WINAPI v_toolcache_equal_sid(PSID a, PSID b) {
	typedef BOOL (WINAPI *fn_t)(PSID, PSID);
	fn_t fn = (fn_t)v_toolcache_advapi32_proc("EqualSid");
	return fn == NULL ? FALSE : fn(a, b);
}

static BOOL WINAPI v_toolcache_is_well_known_sid(PSID sid, WELL_KNOWN_SID_TYPE type) {
	typedef BOOL (WINAPI *fn_t)(PSID, WELL_KNOWN_SID_TYPE);
	fn_t fn = (fn_t)v_toolcache_advapi32_proc("IsWellKnownSid");
	return fn == NULL ? FALSE : fn(sid, type);
}

static DWORD WINAPI v_toolcache_get_named_security_info_w(LPCWSTR object_name,
	SE_OBJECT_TYPE object_type, SECURITY_INFORMATION security_info, PSID *owner,
	PSID *group, PACL *dacl, PACL *sacl, PSECURITY_DESCRIPTOR *security_descriptor) {
	typedef DWORD (WINAPI *fn_t)(LPCWSTR, SE_OBJECT_TYPE, SECURITY_INFORMATION, PSID *,
		PSID *, PACL *, PACL *, PSECURITY_DESCRIPTOR *);
	fn_t fn = (fn_t)v_toolcache_advapi32_proc("GetNamedSecurityInfoW");
	return fn == NULL ? ERROR_PROC_NOT_FOUND
		: fn(object_name, object_type, security_info, owner, group, dacl, sacl,
			security_descriptor);
}

static BOOL WINAPI v_toolcache_open_process_token(HANDLE process, DWORD desired_access,
	PHANDLE token) {
	typedef BOOL (WINAPI *fn_t)(HANDLE, DWORD, PHANDLE);
	fn_t fn = (fn_t)v_toolcache_advapi32_proc("OpenProcessToken");
	return fn == NULL ? FALSE : fn(process, desired_access, token);
}

static BOOL WINAPI v_toolcache_get_token_information(HANDLE token,
	TOKEN_INFORMATION_CLASS information_class, LPVOID information, DWORD length,
	PDWORD returned_length) {
	typedef BOOL (WINAPI *fn_t)(HANDLE, TOKEN_INFORMATION_CLASS, LPVOID, DWORD, PDWORD);
	fn_t fn = (fn_t)v_toolcache_advapi32_proc("GetTokenInformation");
	return fn == NULL ? FALSE
		: fn(token, information_class, information, length, returned_length);
}

static BOOL WINAPI v_toolcache_get_acl_information(PACL acl, LPVOID information,
	DWORD length, ACL_INFORMATION_CLASS information_class) {
	typedef BOOL (WINAPI *fn_t)(PACL, LPVOID, DWORD, ACL_INFORMATION_CLASS);
	fn_t fn = (fn_t)v_toolcache_advapi32_proc("GetAclInformation");
	return fn == NULL ? FALSE : fn(acl, information, length, information_class);
}

static BOOL WINAPI v_toolcache_get_ace(PACL acl, DWORD index, LPVOID *ace) {
	typedef BOOL (WINAPI *fn_t)(PACL, DWORD, LPVOID *);
	fn_t fn = (fn_t)v_toolcache_advapi32_proc("GetAce");
	return fn == NULL ? FALSE : fn(acl, index, ace);
}

#define EqualSid v_toolcache_equal_sid
#define IsWellKnownSid v_toolcache_is_well_known_sid
#define GetNamedSecurityInfoW v_toolcache_get_named_security_info_w
#define OpenProcessToken v_toolcache_open_process_token
#define GetTokenInformation v_toolcache_get_token_information
#define GetAclInformation v_toolcache_get_acl_information
#define GetAce v_toolcache_get_ace
#elif !defined(V_TOOLCACHE_HAS_ACLAPI_H)
DWORD WINAPI GetNamedSecurityInfoW(LPCWSTR object_name, SE_OBJECT_TYPE object_type,
	SECURITY_INFORMATION security_info, PSID *owner, PSID *group, PACL *dacl,
	PACL *sacl, PSECURITY_DESCRIPTOR *security_descriptor);
#endif

static int v_toolcache_move_file_ex_w(const unsigned short *existing,
	const unsigned short *replacement, unsigned int flags) {
	return MoveFileExW((LPCWSTR)existing, (LPCWSTR)replacement, (DWORD)flags);
}

static void *v_toolcache_create_file_w(const unsigned short *path,
	unsigned int desired_access, unsigned int share_mode,
	unsigned int creation_disposition, unsigned int flags_and_attributes) {
	return CreateFileW((LPCWSTR)path, (DWORD)desired_access, (DWORD)share_mode,
		NULL, (DWORD)creation_disposition, (DWORD)flags_and_attributes, NULL);
}

static int v_toolcache_get_file_information(void *handle, void *information) {
	return GetFileInformationByHandle((HANDLE)handle,
		(LPBY_HANDLE_FILE_INFORMATION)information);
}

static int v_toolcache_close_handle(void *handle) {
	return CloseHandle((HANDLE)handle);
}

static int v_toolcache_sid_can_write(PSID sid, PSID owner) {
	return EqualSid(sid, owner)
		|| IsWellKnownSid(sid, WinCreatorOwnerSid)
		|| IsWellKnownSid(sid, WinLocalSystemSid)
		|| IsWellKnownSid(sid, WinBuiltinAdministratorsSid)
		|| IsWellKnownSid(sid, (WELL_KNOWN_SID_TYPE)71); /* WinOwnerRightsSid */
}

static int v_toolcache_root_is_private(const unsigned short *path) {
	PSECURITY_DESCRIPTOR security_descriptor = NULL;
	PSID owner = NULL;
	PACL dacl = NULL;
	HANDLE token = NULL;
	int result = 0;
	DWORD status = GetNamedSecurityInfoW((LPWSTR)path, SE_FILE_OBJECT,
		OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION, &owner, NULL, &dacl,
		NULL, &security_descriptor);
	if (status != ERROR_SUCCESS || owner == NULL || dacl == NULL
		|| security_descriptor == NULL) {
		goto cleanup;
	}
	if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token)) {
		goto cleanup;
	}
	union {
		void *alignment;
		unsigned char bytes[128];
	} token_information;
	DWORD returned_length = 0;
	if (!GetTokenInformation(token, TokenUser, &token_information,
		(DWORD)sizeof(token_information), &returned_length)) {
		goto cleanup;
	}
	TOKEN_USER *token_user = (TOKEN_USER *)&token_information;
	if (token_user->User.Sid == NULL || !EqualSid(owner, token_user->User.Sid)) {
		goto cleanup;
	}

	ACL_SIZE_INFORMATION acl_information;
	if (!GetAclInformation(dacl, &acl_information, sizeof(acl_information),
		AclSizeInformation)) {
		goto cleanup;
	}
	const DWORD write_permissions = GENERIC_ALL | GENERIC_WRITE | WRITE_DAC
		| WRITE_OWNER | DELETE | FILE_WRITE_DATA | FILE_APPEND_DATA | FILE_WRITE_EA
		| FILE_DELETE_CHILD | FILE_WRITE_ATTRIBUTES;
	for (DWORD index = 0; index < acl_information.AceCount; index++) {
		ACE_HEADER *header = NULL;
		if (!GetAce(dacl, index, (void **)&header) || header == NULL) {
			goto cleanup;
		}
		if (header->AceType != ACCESS_ALLOWED_ACE_TYPE
			&& header->AceType != ACCESS_ALLOWED_COMPOUND_ACE_TYPE
			&& header->AceType != ACCESS_ALLOWED_OBJECT_ACE_TYPE
			&& header->AceType != ACCESS_ALLOWED_CALLBACK_ACE_TYPE
			&& header->AceType != ACCESS_ALLOWED_CALLBACK_OBJECT_ACE_TYPE) {
			continue;
		}
		ACCESS_ALLOWED_ACE *allowed = (ACCESS_ALLOWED_ACE *)header;
		if ((allowed->Mask & write_permissions) == 0) {
			continue;
		}
		/* Object and compound ACEs have a variable SID offset; reject writable ones. */
		if (header->AceType == ACCESS_ALLOWED_COMPOUND_ACE_TYPE
			|| header->AceType == ACCESS_ALLOWED_OBJECT_ACE_TYPE
			|| header->AceType == ACCESS_ALLOWED_CALLBACK_OBJECT_ACE_TYPE) {
			goto cleanup;
		}
		if (!v_toolcache_sid_can_write((PSID)&allowed->SidStart, owner)) {
			goto cleanup;
		}
	}
	result = 1;

cleanup:
	if (token != NULL) {
		CloseHandle(token);
	}
	if (security_descriptor != NULL) {
		LocalFree(security_descriptor);
	}
	return result;
}

#ifdef __TINYC__
#undef EqualSid
#undef IsWellKnownSid
#undef GetNamedSecurityInfoW
#undef OpenProcessToken
#undef GetTokenInformation
#undef GetAclInformation
#undef GetAce
#endif
#undef V_TOOLCACHE_HAS_ACLAPI_H

#endif
