import os
import rand

const vexe = @VEXE

fn test_mbedtls_compiles_with_tcc_on_arm64_macos() {
	$if !(macos && arm64) {
		return
	}
	workdir := os.join_path(os.vtmp_dir(), 'v_mbedtls_tcc_arm64_${rand.ulid()}')
	os.mkdir_all(workdir) or { panic(err) }
	defer {
		os.rmdir_all(workdir) or {}
	}
	vcache := os.join_path(workdir, 'vcache')
	os.mkdir_all(vcache) or { panic(err) }
	src := os.join_path(workdir, 'main.v')
	out := os.join_path(workdir, 'main')
	os.write_file(src, 'import net.mbedtls as _\n\nfn main() {}\n') or { panic(err) }
	cmd := 'env VCACHE=${os.quoted_path(vcache)} ${os.quoted_path(vexe)} -nocache -cc tcc -gc none -no-retry-compilation -o ${os.quoted_path(out)} ${os.quoted_path(src)}'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		panic('command failed:\n${cmd}\n${res.output}')
	}
}
