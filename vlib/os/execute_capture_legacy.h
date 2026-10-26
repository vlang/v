#ifndef V_OS_EXECUTE_CAPTURE_LEGACY_H
#define V_OS_EXECUTE_CAPTURE_LEGACY_H

// Older C backends assume these helpers are declared by an inserted header.
// Keep the signatures consistent with the bodyless V bindings in os and fastc.
int v_os_execute_capture_start(char *cmd, int *child_pid, int *read_fd);
int v_os_exec_capture_start(char **argv, int *child_pid, int *read_fd);
int v_os_exec_capture_input_start(char **argv, int *child_pid, int *read_fd, int *write_fd);
void v_os_fd_write_all(int fd, char *data, size_t len);

#endif
