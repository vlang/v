#include "cmd/v/toolcache_windows_helpers.h"
#include <stdio.h>
#include <stdlib.h>

typedef DWORD (WINAPI *ProbeGetSecurity)(LPCWSTR, SE_OBJECT_TYPE,
    SECURITY_INFORMATION, PSID *, PSID *, PACL *, PACL *, PSECURITY_DESCRIPTOR *);
typedef BOOL (WINAPI *ProbeOpenToken)(HANDLE, DWORD, PHANDLE);
typedef BOOL (WINAPI *ProbeTokenInfo)(HANDLE, TOKEN_INFORMATION_CLASS, LPVOID, DWORD, PDWORD);
typedef BOOL (WINAPI *ProbeAclInfo)(PACL, LPVOID, DWORD, ACL_INFORMATION_CLASS);
typedef BOOL (WINAPI *ProbeGetAce)(PACL, DWORD, LPVOID *);
typedef BOOL (WINAPI *ProbeEqualSid)(PSID, PSID);
typedef BOOL (WINAPI *ProbeSidText)(PSID, LPSTR *);

static ProbeGetSecurity probe_security;
static ProbeOpenToken probe_open_token;
static ProbeTokenInfo probe_token_info;
static ProbeAclInfo probe_acl_info;
static ProbeGetAce probe_get_ace;
static ProbeEqualSid probe_equal_sid;
static ProbeSidText probe_sid_text;

static int load_probe_functions(void) {
    HMODULE library = GetModuleHandleW(L"advapi32.dll");
    if (library == NULL) library = LoadLibraryW(L"advapi32.dll");
    if (library == NULL) {
        fprintf(stderr, "LoadLibraryW(advapi32.dll): %lu\n", (unsigned long)GetLastError());
        return 0;
    }
    probe_security = (ProbeGetSecurity)GetProcAddress(library, "GetNamedSecurityInfoW");
    probe_open_token = (ProbeOpenToken)GetProcAddress(library, "OpenProcessToken");
    probe_token_info = (ProbeTokenInfo)GetProcAddress(library, "GetTokenInformation");
    probe_acl_info = (ProbeAclInfo)GetProcAddress(library, "GetAclInformation");
    probe_get_ace = (ProbeGetAce)GetProcAddress(library, "GetAce");
    probe_equal_sid = (ProbeEqualSid)GetProcAddress(library, "EqualSid");
    probe_sid_text = (ProbeSidText)GetProcAddress(library, "ConvertSidToStringSidA");
    if (!probe_security || !probe_open_token || !probe_token_info || !probe_acl_info
        || !probe_get_ace || !probe_equal_sid || !probe_sid_text) {
        fputs("A required read-only advapi32 query is unavailable\n", stderr);
        return 0;
    }
    return 1;
}

static void print_sid(const char *name, PSID sid) {
    LPSTR text = NULL;
    if (sid == NULL) {
        printf("%s=<null>\n", name);
    } else if (probe_sid_text(sid, &text)) {
        printf("%s=%s\n", name, text);
        LocalFree(text);
    } else {
        printf("%s=<conversion failed: %lu>\n", name, (unsigned long)GetLastError());
    }
}

static void *read_token(HANDLE token, TOKEN_INFORMATION_CLASS kind) {
    DWORD size = 0;
    probe_token_info(token, kind, NULL, 0, &size);
    if (size == 0 || size > 65536) {
        fprintf(stderr, "GetTokenInformation size (%d): %lu, error=%lu\n",
            (int)kind, (unsigned long)size, (unsigned long)GetLastError());
        return NULL;
    }
    void *data = calloc(1, size);
    if (data == NULL) return NULL;
    if (!probe_token_info(token, kind, data, size, &size)) {
        fprintf(stderr, "GetTokenInformation (%d): %lu\n",
            (int)kind, (unsigned long)GetLastError());
        free(data);
        return NULL;
    }
    return data;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fputs("usage: windows_toolcache_acl_probe DIRECTORY\n", stderr);
        return 2;
    }
    if (!load_probe_functions()) return 2;
    int length = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, argv[1], -1, NULL, 0);
    if (length == 0) return 2;
    WCHAR *path = (WCHAR *)calloc((size_t)length, sizeof(WCHAR));
    if (path == NULL) return 2;
    if (!MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, argv[1], -1, path, length)) {
        free(path);
        return 2;
    }
    printf("path=%s\n", argv[1]);
    printf("production_root_is_private=%d\n",
        v_toolcache_root_is_private((const unsigned short *)path));

    PSECURITY_DESCRIPTOR descriptor = NULL;
    PSID owner = NULL;
    PACL dacl = NULL;
    DWORD status = probe_security(path, SE_FILE_OBJECT,
        OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION,
        &owner, NULL, &dacl, NULL, &descriptor);
    free(path);
    printf("GetNamedSecurityInfoW=%lu\n", (unsigned long)status);
    if (status != ERROR_SUCCESS) return 2;

    HANDLE token = NULL;
    if (!probe_open_token(GetCurrentProcess(), TOKEN_QUERY, &token)) {
        fprintf(stderr, "OpenProcessToken: %lu\n", (unsigned long)GetLastError());
        LocalFree(descriptor);
        return 2;
    }
    TOKEN_USER *user = (TOKEN_USER *)read_token(token, TokenUser);
    TOKEN_OWNER *default_owner = (TOKEN_OWNER *)read_token(token, TokenOwner);
    if (user == NULL || default_owner == NULL) {
        free(user);
        free(default_owner);
        CloseHandle(token);
        LocalFree(descriptor);
        return 2;
    }
    print_sid("filesystem_owner", owner);
    print_sid("token_user", user->User.Sid);
    print_sid("token_default_owner", default_owner->Owner);
    printf("owner_equals_token_user=%d\n",
        owner != NULL && probe_equal_sid(owner, user->User.Sid));
    printf("owner_equals_token_default_owner=%d\n",
        owner != NULL && probe_equal_sid(owner, default_owner->Owner));

    int result = 0;
    ACL_SIZE_INFORMATION information;
    if (dacl == NULL) {
        puts("DACL=<null>");
    } else if (!probe_acl_info(dacl, &information, sizeof(information), AclSizeInformation)) {
        fprintf(stderr, "GetAclInformation: %lu\n", (unsigned long)GetLastError());
        result = 2;
    } else {
        printf("ace_count=%lu\n", (unsigned long)information.AceCount);
        for (DWORD index = 0; index < information.AceCount; ++index) {
            ACE_HEADER *header = NULL;
            if (!probe_get_ace(dacl, index, (void **)&header) || header == NULL) {
                fprintf(stderr, "GetAce(%lu): %lu\n",
                    (unsigned long)index, (unsigned long)GetLastError());
                result = 2;
                continue;
            }
            printf("ace[%lu].type=%u flags=0x%02x bytes=%u\n", (unsigned long)index,
                (unsigned int)header->AceType, (unsigned int)header->AceFlags,
                (unsigned int)header->AceSize);
            if (header->AceSize >= sizeof(ACE_HEADER) + sizeof(DWORD)) {
                ACCESS_ALLOWED_ACE *entry = (ACCESS_ALLOWED_ACE *)header;
                printf("ace[%lu].mask=0x%08lx\n", (unsigned long)index,
                    (unsigned long)entry->Mask);
                if (header->AceType == ACCESS_ALLOWED_ACE_TYPE
                    || header->AceType == ACCESS_DENIED_ACE_TYPE
                    || header->AceType == ACCESS_ALLOWED_CALLBACK_ACE_TYPE) {
                    PSID sid = (PSID)&entry->SidStart;
                    print_sid("ace.sid", sid);
                    printf("ace.matches_token_user=%d\n", probe_equal_sid(sid, user->User.Sid));
                    printf("ace.writer_allowed_by_production_helper=%d\n",
                        owner != NULL && v_toolcache_sid_can_write(sid, owner));
                }
            }
        }
    }
    free(user);
    free(default_owner);
    CloseHandle(token);
    LocalFree(descriptor);
    return result;
}
