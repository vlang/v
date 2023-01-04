module builder

// TODO move this to msvc_default.v
struct MsvcResult {
	full_cl_exe_path    string
	exe_path            string
	um_lib_path         string
	ucrt_lib_path       string
	vs_lib_path         string
	um_include_path     string
	ucrt_include_path   string
	vs_include_path     string
	shared_include_path string
	valid               bool
}
