#ifndef V_MULTIWINDOW_PROBE_WATCHDOG_WINDOWS_HELPERS_H
#define V_MULTIWINDOW_PROBE_WATCHDOG_WINDOWS_HELPERS_H

#include <windows.h>

typedef struct v_multiwindow_watchdog_process {
    HANDLE job;
    HANDLE process;
    HANDLE stdout_read;
    HANDLE stderr_read;
    DWORD pid;
} v_multiwindow_watchdog_process;

static DWORD v_multiwindow_watchdog_error = ERROR_SUCCESS;

static void v_multiwindow_watchdog_close_handle(HANDLE* handle) {
    if (*handle != NULL && *handle != INVALID_HANDLE_VALUE) {
        CloseHandle(*handle);
        *handle = NULL;
    }
}

static void v_multiwindow_watchdog_dispose_process(
    v_multiwindow_watchdog_process* state) {
    if (state == NULL) {
        return;
    }
    v_multiwindow_watchdog_close_handle(&state->stdout_read);
    v_multiwindow_watchdog_close_handle(&state->stderr_read);
    v_multiwindow_watchdog_close_handle(&state->process);
    v_multiwindow_watchdog_close_handle(&state->job);
    HeapFree(GetProcessHeap(), 0, state);
}

static DWORD v_multiwindow_watchdog_wait_process(HANDLE process,
    DWORD timeout_ms) {
    DWORD wait_result = WAIT_FAILED;
    if (process == NULL || process == INVALID_HANDLE_VALUE) {
        v_multiwindow_watchdog_error = ERROR_INVALID_HANDLE;
        return wait_result;
    }
    wait_result = WaitForSingleObject(process, timeout_ms);
    if (wait_result == WAIT_FAILED) {
        v_multiwindow_watchdog_error = GetLastError();
    }
    return wait_result;
}

static v_multiwindow_watchdog_process* v_multiwindow_watchdog_spawn(
    const unsigned short* application, unsigned short* command_line,
    const unsigned short* work_directory, unsigned short* environment) {
    SECURITY_ATTRIBUTES security;
    STARTUPINFOW startup;
    PROCESS_INFORMATION process_info;
    HANDLE stdout_read = NULL;
    HANDLE stdout_write = NULL;
    HANDLE stderr_read = NULL;
    HANDLE stderr_write = NULL;
    HANDLE job = NULL;
    v_multiwindow_watchdog_process* state = NULL;

    ZeroMemory(&security, sizeof(security));
    security.nLength = sizeof(security);
    security.bInheritHandle = TRUE;
    if (!CreatePipe(&stdout_read, &stdout_write, &security, 0)
        || !SetHandleInformation(stdout_read, HANDLE_FLAG_INHERIT, 0)
        || !CreatePipe(&stderr_read, &stderr_write, &security, 0)
        || !SetHandleInformation(stderr_read, HANDLE_FLAG_INHERIT, 0)) {
        v_multiwindow_watchdog_error = GetLastError();
        goto fail;
    }

    job = CreateJobObjectW(NULL, NULL);
    if (job == NULL) {
        v_multiwindow_watchdog_error = GetLastError();
        goto fail;
    }
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;
    ZeroMemory(&limits, sizeof(limits));
    limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if (!SetInformationJobObject(job, JobObjectExtendedLimitInformation,
        &limits, sizeof(limits))) {
        v_multiwindow_watchdog_error = GetLastError();
        goto fail;
    }

    ZeroMemory(&startup, sizeof(startup));
    startup.cb = sizeof(startup);
    startup.dwFlags = STARTF_USESTDHANDLES;
    startup.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    startup.hStdOutput = stdout_write;
    startup.hStdError = stderr_write;
    ZeroMemory(&process_info, sizeof(process_info));
    if (!CreateProcessW((LPCWSTR)application, (LPWSTR)command_line, NULL, NULL,
        TRUE, CREATE_SUSPENDED | CREATE_UNICODE_ENVIRONMENT, environment,
        (LPCWSTR)work_directory, &startup, &process_info)) {
        v_multiwindow_watchdog_error = GetLastError();
        goto fail;
    }
    v_multiwindow_watchdog_close_handle(&stdout_write);
    v_multiwindow_watchdog_close_handle(&stderr_write);

    if (!AssignProcessToJobObject(job, process_info.hProcess)) {
        v_multiwindow_watchdog_error = GetLastError();
        DWORD spawn_error = v_multiwindow_watchdog_error;
        TerminateProcess(process_info.hProcess, 125);
        (void)v_multiwindow_watchdog_wait_process(process_info.hProcess, 2000);
        v_multiwindow_watchdog_error = spawn_error;
        CloseHandle(process_info.hThread);
        CloseHandle(process_info.hProcess);
        goto fail;
    }

    state = (v_multiwindow_watchdog_process*)HeapAlloc(GetProcessHeap(),
        HEAP_ZERO_MEMORY, sizeof(v_multiwindow_watchdog_process));
    if (state == NULL) {
        v_multiwindow_watchdog_error = ERROR_NOT_ENOUGH_MEMORY;
        DWORD spawn_error = v_multiwindow_watchdog_error;
        TerminateJobObject(job, 125);
        (void)v_multiwindow_watchdog_wait_process(process_info.hProcess, 2000);
        v_multiwindow_watchdog_error = spawn_error;
        CloseHandle(process_info.hThread);
        CloseHandle(process_info.hProcess);
        goto fail;
    }
    state->job = job;
    state->process = process_info.hProcess;
    state->stdout_read = stdout_read;
    state->stderr_read = stderr_read;
    state->pid = process_info.dwProcessId;

    if (ResumeThread(process_info.hThread) == (DWORD)-1) {
        v_multiwindow_watchdog_error = GetLastError();
        DWORD spawn_error = v_multiwindow_watchdog_error;
        TerminateJobObject(job, 125);
        (void)v_multiwindow_watchdog_wait_process(process_info.hProcess, 2000);
        v_multiwindow_watchdog_error = spawn_error;
        CloseHandle(process_info.hThread);
        v_multiwindow_watchdog_dispose_process(state);
        return NULL;
    }
    CloseHandle(process_info.hThread);
    v_multiwindow_watchdog_error = ERROR_SUCCESS;
    return state;

fail:
    v_multiwindow_watchdog_close_handle(&stdout_read);
    v_multiwindow_watchdog_close_handle(&stdout_write);
    v_multiwindow_watchdog_close_handle(&stderr_read);
    v_multiwindow_watchdog_close_handle(&stderr_write);
    v_multiwindow_watchdog_close_handle(&job);
    return NULL;
}

static DWORD v_multiwindow_watchdog_pid(v_multiwindow_watchdog_process* state) {
    return state == NULL ? 0 : state->pid;
}

static int v_multiwindow_watchdog_leader_alive(
    v_multiwindow_watchdog_process* state) {
    DWORD wait_result = WAIT_FAILED;
    if (state == NULL || state->process == NULL) {
        v_multiwindow_watchdog_error = ERROR_INVALID_HANDLE;
        return -1;
    }
    wait_result = v_multiwindow_watchdog_wait_process(state->process, 0);
    if (wait_result == WAIT_TIMEOUT) {
        return 1;
    }
    if (wait_result == WAIT_OBJECT_0) {
        return 0;
    }
    v_multiwindow_watchdog_error = GetLastError();
    return -1;
}

static int v_multiwindow_watchdog_wait_leader(
    v_multiwindow_watchdog_process* state, DWORD timeout_ms, DWORD* exit_code) {
    DWORD wait_result = WAIT_FAILED;
    if (state == NULL || state->process == NULL || exit_code == NULL) {
        v_multiwindow_watchdog_error = ERROR_INVALID_HANDLE;
        return 0;
    }
    wait_result = v_multiwindow_watchdog_wait_process(state->process, timeout_ms);
    if (wait_result != WAIT_OBJECT_0) {
        v_multiwindow_watchdog_error = wait_result == WAIT_TIMEOUT
            ? ERROR_TIMEOUT : GetLastError();
        return 0;
    }
    if (!GetExitCodeProcess(state->process, exit_code)) {
        v_multiwindow_watchdog_error = GetLastError();
        return 0;
    }
    v_multiwindow_watchdog_error = ERROR_SUCCESS;
    return 1;
}

static int v_multiwindow_watchdog_release_leader(
    v_multiwindow_watchdog_process* state) {
    HANDLE leader = NULL;
    if (state == NULL || state->process == NULL) {
        v_multiwindow_watchdog_error = ERROR_INVALID_HANDLE;
        return 0;
    }
    leader = state->process;
    state->process = NULL;
    if (!CloseHandle(leader)) {
        v_multiwindow_watchdog_error = GetLastError();
        return 0;
    }
    v_multiwindow_watchdog_error = ERROR_SUCCESS;
    return 1;
}

static int v_multiwindow_watchdog_read_pipe(HANDLE pipe, char* buffer,
    DWORD capacity) {
    DWORD available = 0;
    DWORD read = 0;
    if (pipe == NULL || capacity == 0) {
        return 0;
    }
    if (!PeekNamedPipe(pipe, NULL, 0, NULL, &available, NULL)) {
        DWORD error = GetLastError();
        if (error == ERROR_BROKEN_PIPE) {
            return 0;
        }
        v_multiwindow_watchdog_error = error;
        return -1;
    }
    if (available == 0) {
        return 0;
    }
    DWORD requested = available < capacity ? available : capacity;
    if (!ReadFile(pipe, buffer, requested, &read, NULL)) {
        DWORD error = GetLastError();
        if (error == ERROR_BROKEN_PIPE) {
            return 0;
        }
        v_multiwindow_watchdog_error = error;
        return -1;
    }
    return (int)read;
}

static int v_multiwindow_watchdog_read_stdout(
    v_multiwindow_watchdog_process* state, char* buffer, DWORD capacity) {
    return state == NULL ? -1 : v_multiwindow_watchdog_read_pipe(
        state->stdout_read, buffer, capacity);
}

static int v_multiwindow_watchdog_read_stderr(
    v_multiwindow_watchdog_process* state, char* buffer, DWORD capacity) {
    return state == NULL ? -1 : v_multiwindow_watchdog_read_pipe(
        state->stderr_read, buffer, capacity);
}

static int v_multiwindow_watchdog_terminate_job(
    v_multiwindow_watchdog_process* state) {
    if (state == NULL || state->job == NULL) {
        v_multiwindow_watchdog_error = ERROR_INVALID_HANDLE;
        return 0;
    }
    if (!TerminateJobObject(state->job, 124)) {
        v_multiwindow_watchdog_error = GetLastError();
        return 0;
    }
    v_multiwindow_watchdog_error = ERROR_SUCCESS;
    return 1;
}

static int v_multiwindow_watchdog_active_processes(
    v_multiwindow_watchdog_process* state) {
    if (state == NULL || state->job == NULL) {
        v_multiwindow_watchdog_error = ERROR_INVALID_HANDLE;
        return -1;
    }
    JOBOBJECT_BASIC_ACCOUNTING_INFORMATION accounting;
    ZeroMemory(&accounting, sizeof(accounting));
    if (!QueryInformationJobObject(state->job,
        JobObjectBasicAccountingInformation, &accounting,
        sizeof(accounting), NULL)) {
        v_multiwindow_watchdog_error = GetLastError();
        return -1;
    }
    return (int)accounting.ActiveProcesses;
}

static void v_multiwindow_watchdog_close(
    v_multiwindow_watchdog_process* state) {
    v_multiwindow_watchdog_dispose_process(state);
}

static int v_multiwindow_watchdog_process_exists(DWORD pid) {
    HANDLE process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, pid);
    if (process == NULL) {
        return GetLastError() == ERROR_ACCESS_DENIED;
    }
    DWORD code = 0;
    int alive = GetExitCodeProcess(process, &code) && code == STILL_ACTIVE;
    CloseHandle(process);
    return alive;
}

static DWORD v_multiwindow_watchdog_last_error(void) {
    return v_multiwindow_watchdog_error;
}

#endif
