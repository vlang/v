#!/usr/bin/env python3
"""Check that CI scheduling changes retain every suite and all shard indices."""

import itertools
from pathlib import Path
import re


ROOT = Path(__file__).resolve().parents[2]


def require(condition, message):
    if not condition:
        raise SystemExit(message)


def job(workflow, name):
    match = re.search(
        rf"^  {re.escape(name)}:\n(.*?)(?=^  [\w-]+:\n|\Z)",
        workflow,
        re.MULTILINE | re.DOTALL,
    )
    require(match is not None, f"missing job: {name}")
    return match.group(1)


def field(text, name, indent):
    values = re.findall(rf"^{' ' * indent}{re.escape(name)}: (.+)$", text, re.MULTILINE)
    require(len(values) == 1, f"expected exactly one {name} field")
    return values[0]


def axis(text, name):
    value = field(text, name, 8)
    require(value.startswith("[") and value.endswith("]"), f"invalid {name} axis")
    return [part.strip() for part in value[1:-1].split(",")]


def command_step(text, command):
    steps = re.split(r"^      - ", text, flags=re.MULTILINE)[1:]
    matches = [step for step in steps if f"        run: {command}\n" in step]
    require(len(matches) == 1, f"expected exactly one step for: {command}")
    return matches[0]


def routed_command(text, command, condition):
    step = command_step(text, command)
    require(field(step, "if", 8) == condition, f"wrong suite routing: {command}")


def script_step(text, command):
    steps = re.split(r"^      - ", text, flags=re.MULTILINE)[1:]
    matches = [
        step for step in steps
        if re.search(rf"^          {re.escape(command)}$", step, re.MULTILINE)
    ]
    require(len(matches) == 1, f"expected exactly one script step for: {command}")
    return matches[0]


def check_linux(workflow):
    tcc = job(workflow, "tcc-linux")
    require(axis(tcc, "shard") == ["0", "1", "2"], "Linux TCC must schedule all three shards")
    require(field(tcc, "fail-fast", 6) == "false", "Linux TCC must run every shard")
    require("include:" not in tcc and "exclude:" not in tcc, "unexpected Linux TCC matrix overrides")
    tests = command_step(tcc, "v run ci/linux_ci.vsh self_tests_vprod_tcc")
    require(field(tests, "VTEST_SELF_SHARD_INDEX", 10) == "${{ matrix.shard }}", "Linux TCC shard index")
    require(field(tests, "VTEST_SELF_SHARD_COUNT", 10) == "'3'", "Linux TCC shard count")
    require("VTEST_ONLY" not in tcc, "Linux TCC must retain the full self-test selection")
    condition = field(tests, "if", 8)
    require("steps.install_dependencies.outcome == 'success'" in condition, "all Linux TCC shards need dependencies")
    require("matrix.shard != 0 || steps.test_inline_assembly.outcome == 'success'" in condition,
            "nonzero Linux TCC shards must run without the serial checks")
    for task in ("test_v_to_c_tcc", "build_examples_tcc"):
        step = command_step(tcc, f"v run ci/linux_ci.vsh {task}")
        require("matrix.shard == 0" in field(step, "if", 8), f"{task} must run once")
    binaries = command_step(tcc, "v run ci/linux_ci.vsh build_vbinaries_tcc")
    require(field(binaries, "VFLAGS", 10) == "${{ env.VFLAGS }} -parallel-cc",
            "Linux TCC production build checks need parallel C compilation")

    for compiler, prod_task in [("gcc", "prod_gcc"), ("clang", "vprod_clang")]:
        name = f"{compiler}-linux-self-tests"
        lane = job(workflow, name)
        require(field(lane, "if", 4) == "github.ref == 'refs/heads/master'", name)
        require(field(lane, "fail-fast", 6) == "false", f"{name} must run every shard")
        require(field(lane, "timeout-minutes", 4) == "121", f"{name} timeout changed")
        require("include:" not in lane, f"{name} has unexpected matrix additions")
        cases = list(itertools.product(axis(lane, "suite"), axis(lane, "shard")))
        excluded = re.findall(
            r"^          - suite: (\w+)\n            shard: (\d+)$",
            lane,
            re.MULTILINE,
        )
        require(len(excluded) == 3 and len(set(excluded)) == 3, f"{name} exclusions")
        require(set(excluded) <= set(cases), f"{name} excludes nonexistent cases")
        cases = [case for case in cases if case not in excluded]
        expected = [(suite, str(shard)) for suite in ["default", "prod"] for shard in range(4)]
        expected.append(("strict", "0"))
        require(sorted(cases) == sorted(expected), f"{name} loses or duplicates suite coverage")
        require(field(lane, "VFLAGS", 6) == f"-cc {compiler}", f"{name} compiler")
        require(field(lane, "VTEST_SELF_SHARD_INDEX", 6) == "${{ matrix.shard }}", name)
        require(
            field(lane, "VTEST_SELF_SHARD_COUNT", 6)
            == "${{ matrix.suite == 'strict' && '1' || '4' }}",
            f"{name} shard count must match the scheduled indices",
        )
        require("VTEST_ONLY" not in lane, f"{name} must retain the full suite selection")
        require(field(lane, "V_MACOS_V3_NO_FALLBACK", 6) == "1", name)
        setup = "uses: ./.github/actions/setup-v"
        dependencies = f"v run ci/linux_ci.vsh install_dependencies_for_examples_and_tools_{compiler}"
        command_step(lane, dependencies)
        require(setup in lane and lane.index(setup) < lane.index(dependencies), f"{name} setup order")
        for suite, task in [
            ("default", compiler),
            ("prod", prod_task),
            ("strict", f"cstrict_{compiler}"),
        ]:
            command = f"v run ci/linux_ci.vsh self_tests_{task}"
            routed_command(lane, command, f"matrix.suite == '{suite}'")
            require(lane.index(dependencies) < lane.index(command), f"{name} dependency order")
        if compiler == "clang":
            build = "v -cc clang -o vprod -prod cmd/v"
            routed_command(lane, build, "matrix.suite == 'strict'")
            require(
                lane.index(dependencies) < lane.index(build) < lane.index("run: v run ci/linux_ci.vsh self_tests_cstrict_clang"),
                "clang strict suite must build its own vprod after dependency setup",
            )
        serial = job(workflow, f"{compiler}-linux")
        require("steps.self_tests" not in serial, f"{compiler} checks depend on a moved step")
        examples = command_step(serial, f"v run ci/linux_ci.vsh build_examples_{compiler}")
        require(
            "steps.test_pure_v_math_module.outcome == 'success'" in field(examples, "if", 8),
            f"{compiler} examples must still run after the serial checks",
        )


def check_macos_self_tests(workflow):
    checks = job(workflow, "clang-macos")
    shards = job(workflow, "clang-macos-self-tests")
    images = ["macos-15", "macos-26"]
    require(axis(checks, "os") == images, "macOS checks must cover both runner images")
    require(axis(shards, "os") == images, "macOS shards must cover both runner images")
    require(axis(shards, "shard") == ["1", "2", "3", "4"], "macOS needs four independent shard jobs")
    require(field(checks, "VTEST_SELF_SHARD_INDEX", 6) == "'0'", "macOS checks must retain shard 0")
    require(field(shards, "VTEST_SELF_SHARD_INDEX", 6) == "${{ matrix.shard }}", "macOS shard index")
    require(field(shards, "timeout-minutes", 4) == "90", "macOS shard deadline changed")
    for lane in [checks, shards]:
        require("include:" not in lane and "exclude:" not in lane, "macOS must schedule every shard")
        require(field(lane, "fail-fast", 6) == "false", "macOS must run every shard after a failure")
        require(field(lane, "VTEST_SELF_SHARD_COUNT", 6) == "'5'", "macOS shard counts must match")
        require(field(lane, "VJOBS", 6) == "'3'", "macOS workers must fit the hosted runner")
        require(field(lane, "VFLAGS", 6) == "-cc clang", "macOS test compiler flags changed")
        require(field(lane, "V_MACOS_V3_NO_FALLBACK", 6) == "1", "macOS must use V3")
        require(
            re.search(r"^      VTEST_ONLY(?:_FN)?:", lane, re.MULTILINE) is None,
            "macOS must retain the full self-test selection",
        )
        setup = "uses: ./.github/actions/setup-v"
        require(lane.count(setup) == 1, "macOS needs the shared optimized compiler setup")
        require(field(lane, "use-prebuilt", 10) == "'true'", "macOS must reuse exact-commit vprod")
        dependencies = "v run ci/macos_ci.vsh install_iconv"
        command_step(lane, dependencies)
        tests = "v run ci/macos_ci.vsh self_tests"
        step = command_step(lane, tests)
        require("        if:" not in step, "macOS shard must run after successful setup")
        require("VTEST_SELF_SHARD_INDEX" not in step, "macOS must run only its assigned shard")
        require(lane.index(setup) < lane.index(dependencies) < lane.index(tests), "macOS setup order")


def check_macos(workflow):
    lane = job(workflow, "tools-macos")
    cases = list(itertools.product(axis(lane, "cc"), axis(lane, "suite"), axis(lane, "shard")))
    expected = list(itertools.product(["clang"], ["default", "strict"], ["0", "1"]))
    require(sorted(cases) == sorted(expected), "macOS tools loses or duplicates suite coverage")
    require("exclude:" not in lane and "include:" not in lane, "unexpected macOS matrix overrides")
    require(field(lane, "fail-fast", 6) == "false", "macOS must run every shard")
    require(field(lane, "VTEST_SELF_SHARD_INDEX", 6) == "${{ matrix.shard }}", "macOS shard index")
    require(field(lane, "VTEST_SELF_SHARD_COUNT", 6) == "'2'", "macOS shard count")
    require("VTEST_ONLY" not in lane, "macOS must retain the full tool suite selection")
    routed_command(lane, "./v -silent -N -W -check build-tools", "matrix.suite == 'default' && matrix.shard == 0")
    routed_command(lane, "./v -silent test-self cmd", "matrix.suite == 'default'")
    routed_command(lane, "./v -silent -W -cstrict test-self cmd", "matrix.suite == 'strict'")


def check_riscv(workflow):
    lane = job(workflow, "riscv64_linux")
    require(field(lane, "timeout-minutes", 4) == "180", "RISC-V needs time for tests after bootstrap")
    commands = [
        "            make\n",
        "            V_CI_SOURCE_BUILD=1 sh .github/workflows/prepare_v.sh\n",
        "            ./v test vlib/builtin vlib/os vlib/encoding/binary\n",
        "            ./v test vlib/v/slow_tests/assembly\n",
        "            VTEST_ONLY=closure ./v test vlib/v/tests\n",
    ]
    require(all(lane.count(command) == 1 for command in commands), "RISC-V coverage changed")
    positions = [lane.index(command) for command in commands]
    require(positions == sorted(positions), "RISC-V native bootstrap and test order changed")


def check_openbsd(workflow):
    lane = job(workflow, "tools-openbsd")
    require(field(lane, "timeout-minutes", 4) == "35", "OpenBSD needs time after VM bootstrap")
    cases = list(itertools.product(axis(lane, "cc"), axis(lane, "suite"), axis(lane, "shard")))
    excluded = re.findall(
        r"^          - cc: (\w+)\n            suite: (\w+)$", lane, re.MULTILINE
    )
    require(len(re.findall(r"^          - ", lane, re.MULTILINE)) == 1, "unexpected OpenBSD exclusions")
    require(excluded == [("tcc", "strict")], "only unsupported TCC strict cases may be excluded")
    require("include:" not in lane, "unexpected OpenBSD matrix additions")
    cases = [case for case in cases if case[:2] not in excluded]
    expected = [
        (compiler, suite, str(shard))
        for compiler, suite in [("tcc", "default"), ("clang", "default"), ("clang", "strict")]
        for shard in range(3)
    ]
    require(sorted(cases) == sorted(expected), "OpenBSD loses or duplicates suite coverage")
    require(field(lane, "fail-fast", 6) == "false", "OpenBSD must run every shard")
    require(field(lane, "V_CI_SUITE", 6) == "${{ matrix.suite }}", "OpenBSD suite selection")
    require(field(lane, "VTEST_SELF_SHARD_INDEX", 6) == "${{ matrix.shard }}", "OpenBSD shard index")
    require(field(lane, "VTEST_SELF_SHARD_COUNT", 6) == "'3'", "OpenBSD shard count")
    forwarded = field(lane, "environment_variables", 10).split()
    require(
        {"V_CI_SUITE", "VTEST_SELF_SHARD_INDEX", "VTEST_SELF_SHARD_COUNT"} <= set(forwarded),
        "OpenBSD VM must receive its complete suite/shard selection",
    )


def check_windows_gcc(workflow):
    lane = job(workflow, "gcc-windows-self-tests")
    require(axis(lane, "shard") == ["0", "1", "2", "3"], "Windows GCC must schedule all four shards")
    require("exclude:" not in lane and "include:" not in lane, "unexpected Windows GCC matrix overrides")
    require(field(lane, "fail-fast", 6) == "false", "Windows GCC must run every shard")
    require(field(lane, "timeout-minutes", 4) == "150", "Windows GCC deadline changed")
    require(field(lane, "VFLAGS", 6) == "-cc gcc", "Windows GCC compiler changed")
    require(field(lane, "VJOBS", 6) == "'4'", "Windows GCC worker count changed")
    require(field(lane, "VTEST_SELF_SHARD_INDEX", 6) == "${{ matrix.shard }}", "Windows GCC shard index")
    require(field(lane, "VTEST_SELF_SHARD_COUNT", 6) == "'4'", "Windows GCC shard count")
    require("VTEST_ONLY" not in lane, "Windows GCC must retain the full self-test selection")
    cache = r".\.github\workflows\setup_windows_tool_cache.ps1"
    bootstrap = r".\makev.bat -gcc"
    symlink = r".\v.exe symlink"
    optimized = "uses: ./.github/actions/setup-v"
    freetype = "v retry -- v setup-freetype"
    sqlite = r".\.github\workflows\windows-install-sqlite.bat"
    tests = "v -silent test-self vlib"
    command_step(lane, cache)
    command_step(lane, tests)
    for command in [bootstrap, symlink, freetype, sqlite]:
        script_step(lane, command)
    require(optimized in lane, "Windows GCC needs the optimized compiler")
    order = [lane.index(command) for command in [cache, bootstrap, symlink, optimized, freetype, sqlite, tests]]
    require(order == sorted(order), "Windows GCC setup must complete before self tests")
    serial = job(workflow, "gcc-windows")
    require("steps.self_tests" not in serial, "Windows GCC still depends on the moved self-test step")
    javascript = command_step(serial, "v -old-compiler -b js -o hi.js examples/js_hello_world.v && node hi.js")
    require(
        "steps.test_pure_v_math_module.outcome == 'success'" in field(javascript, "if", 8),
        "Windows GCC remaining checks must still run after pure math",
    )


def check_windows_msvc(workflow):
    lane = job(workflow, "msvc-windows-self-tests")
    require(axis(lane, "shard") == ["0", "1", "2", "3"], "Windows MSVC must schedule all four shards")
    require("exclude:" not in lane and "include:" not in lane, "unexpected Windows MSVC matrix overrides")
    require(field(lane, "fail-fast", 6) == "false", "Windows MSVC must run every shard")
    require(field(lane, "VFLAGS", 6) == "-cc msvc", "Windows MSVC compiler changed")
    require(field(lane, "VTEST_SELF_SHARD_INDEX", 6) == "${{ matrix.shard }}", "Windows MSVC shard index")
    require(field(lane, "VTEST_SELF_SHARD_COUNT", 6) == "'4'", "Windows MSVC shard count")
    require("VTEST_ONLY" not in lane, "Windows MSVC must retain the full self-test selection")
    serial = job(workflow, "msvc-windows")
    setup = ["checkout", "prepare_tool_cache", "install_openssl", "msvc_dev_cmd",
             "verify_msvc_tools"]
    require(re.findall(r"^      - \*(\w+)$", lane, re.M) == setup, "Windows MSVC shard setup order")
    require(re.findall(r"^      - &(\w+)$", serial, re.M) == setup, "Windows MSVC shared setup")
    tests = "v -silent test-self vlib"
    command_step(lane, tests)
    positions = [lane.index(part) for part in ("uses: ./.github/actions/setup-v",
                                               "run: *install_test_dependencies", tests)]
    require(positions == sorted(positions), "Windows MSVC test prerequisites")
    require("makev.bat" not in lane, "Windows MSVC shards should reuse the prepared compiler")
    require(tests not in serial and "steps.self_tests" not in serial, "Windows MSVC serial checks depend on moved tests")


def check_sanitizer_shards(workflow):
    lane = job(workflow, "sanitize-linux")
    matrix = lane.split("\n    name:", 1)[0]
    sharded = ["undefined-clang", "undefined-gcc", "address-gcc"]
    standalone = ["address-clang", "memory-clang", "memory-clang-compiler", "address-clang-without-gc"]
    require(axis(matrix, "name") == sharded, "wrong sharded sanitizer suites")
    require(axis(matrix, "shard") == ["0", "1", "2", "3"], "sanitizers must schedule all four shards")
    require("exclude:" not in matrix, "sanitizer matrix must retain all shards")
    entries = [entry.split("\n", 1) for entry in re.split(r"^          - name: ", matrix, flags=re.M)[1:]]
    require(sorted(name for name, _ in entries) == sorted(sharded + standalone), "sanitizer coverage changed")
    for name, row in entries:
        require("            shard:" not in row, f"{name} must not override individual shards")
        if name in sharded:
            sanitizer, cc = name.split("-")
            require(field(row, "shard_count", 12) == "4", f"{name} shard count")
            require(field(row, "cc", 12) == cc, f"{name} compiler changed")
            require(field(row, "sanitizer", 12) == sanitizer, f"{name} sanitizer changed")
            require(field(row, "gc", 12) == "default", f"{name} GC mode changed")
            require(field(row, "vflags", 12) == f"-cc {cc} -cflags -fno-omit-frame-pointer", f"{name} flags changed")
        else:
            require("            shard_count:" not in row, f"{name} must run its full suite")
    require(field(lane, "fail-fast", 6) == "false", "sanitizers must run every shard")
    require(field(lane, "VJOBS", 6) == "${{ matrix.shard_count && 2 || 1 }}",
            "only sharded sanitizer suites should use two workers")
    require(field(lane, "SANITIZER_GITHUB_JOB", 6) == "sanitize-${{ matrix.name }}", "sanitizer identity changed")
    require(field(lane, "VTEST_SELF_SHARD_INDEX", 6) == "${{ matrix.shard || 0 }}", "sanitizer shard index")
    require(field(lane, "VTEST_SELF_SHARD_COUNT", 6) == "${{ matrix.shard_count || 1 }}", "sanitizer shard count")
    build = "./v -prod -cflags -fsanitize=undefined -o v2 cmd/v"
    tests = "./v2 -cflags -fsanitize=undefined -silent test-self vlib"
    step = script_step(lane, build)
    require(step == script_step(lane, tests) and step.index(build) < step.index(tests), "UBSan tests need their instrumented compiler")
    require(field(step, "if", 8) == "${{ matrix.sanitizer == 'undefined' }}", "UBSan self-test routing")
    routed_command(lane, 'GITHUB_JOB="$SANITIZER_GITHUB_JOB" ./v2 -silent build-examples',
                   "${{ matrix.sanitizer == 'undefined' && matrix.shard == 0 }}")
    routed_command(
        lane, 'GITHUB_JOB="$SANITIZER_GITHUB_JOB" ./v -silent build-examples',
        "${{ matrix.name == 'address-clang' || (matrix.name == 'address-gcc' && matrix.shard == 0) }}",
    )


def check_msvc(workflow):
    lane = job(workflow, "sanitize-address-msvc")
    require(sorted(axis(lane, "stage")) == ["fmt", "vet"], "MSVC must retain both cleancode stages")
    require("exclude:" not in lane and "include:" not in lane, "unexpected MSVC matrix overrides")
    require(field(lane, "fail-fast", 6) == "false", "MSVC must run both cleancode stages")
    require(field(lane, "timeout-minutes", 4) == "45", "MSVC deadline changed")
    require(field(lane, "VTEST_CLEANCODE_STAGE", 6) == "${{ matrix.stage }}", "MSVC stage selection")
    require(field(lane, "VJOBS", 6) == "4", "MSVC cleancode must not serialize all files")
    require(field(lane, "VTEST_SHOW_START", 6) == "'1'", "MSVC must report the active file")
    cache = r".\.github\workflows\setup_windows_tool_cache.ps1"
    checks = r".\v.exe -silent test-cleancode"
    command_step(lane, cache)
    command_step(lane, checks)
    routed_command(lane, cache + " -Verify", "matrix.stage == 'vet'")
    positions = [lane.index(part) for part in (cache, "uses: ./.github/actions/setup-v",
                                               cache + " -Verify", checks)]
    require(positions == sorted(positions), "MSVC cleancode cache setup order")


if __name__ == "__main__":
    check_linux((ROOT / ".github/workflows/linux_ci.yml").read_text())
    check_macos_self_tests((ROOT / ".github/workflows/macos_ci.yml").read_text())
    check_macos((ROOT / ".github/workflows/tools_ci.yml").read_text())
    check_openbsd((ROOT / ".github/workflows/tools_ci.yml").read_text())
    check_windows_gcc((ROOT / ".github/workflows/windows_ci_gcc.yml").read_text())
    check_windows_msvc((ROOT / ".github/workflows/windows_ci_msvc.yml").read_text())
    check_sanitizer_shards((ROOT / ".github/workflows/sanitized_ci.yml").read_text())
    check_riscv((ROOT / ".github/workflows/riscv64_linux_ci.yml").read_text())
    check_msvc((ROOT / ".github/workflows/sanitized_ci.yml").read_text())
    print("CI suite routing, complete shard coverage, and bootstrap prerequisites passed")
