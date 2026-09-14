// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

#ifndef V_TOOLCACHE_WINDOWS_HELPERS_H
#define V_TOOLCACHE_WINDOWS_HELPERS_H

#include <windows.h>
#include <aclapi.h>

static int v_toolcache_move_file_ex_w(const unsigned short *existing,
	const unsigned short *replacement, unsigned int flags) {
	return MoveFileExW((LPCWSTR)existing, (LPCWSTR)replacement, (DWORD)flags);
}

static void *v_toolcache_create_file_w(const unsigned short *path,
	unsigned int desired_access, unsigned int share_mode, void *security_attributes,
	unsigned int creation_disposition, unsigned int flags_and_attributes,
	void *template_file) {
	return CreateFileW((LPCWSTR)path, (DWORD)desired_access, (DWORD)share_mode,
		(LPSECURITY_ATTRIBUTES)security_attributes, (DWORD)creation_disposition,
		(DWORD)flags_and_attributes, (HANDLE)template_file);
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

#endif
