## V 0.4.7
*26 Jul 2024*

#### Improvements in the language
- Add support for `-d ident=value` and retrieval in code via `$d('ident', <default value>)`
- `-warn-about-allocs` for debugging allocations when using manual memory management
- `@[freed]` attribute for assign statements (for the above mode)
- Implement `&&=` and `||=` operators  (#21678)
- Improve C var args interop, allow for `fn f(some int, ...) {` (#21812)
- A new flag `-n` for skipping notes (similar to `-w` for skipping warnings)
- Cross compilation to FreeBSD

#### Breaking changes
**none**

#### Checker improvements/fixes
- Fix unknown fixed array size for `const n = int(sizeof(u64)); _ = [n]int{}` (fix #21544) (#21548)
- Fix checking of default field initialisations, that are part of unions of structs tagged with `@[noinit]` (#21587)
- Disallow sum type with `Result` variants (#21620)
- Add error for `field map` (i.e. a plain untyped map), used inside a struct (#21625)
- Fix missing concrete type checking on a generic type specifier (#21614)
- Fix missing option and result wrong type return type definition (#21626)
- Turn warnings for private fields into errors (#21296)
- Support nested labeled for statements (#21658)
- Fix the return type of overloaded operators, defined on aliases of primitive types (fix #21654) (#21663)
- Fix match expr with auto promote number (#21696)
- Fix sorting compare fn with mut reference parameter (fix #21662) (#21706)
- Fix `$if x {` condition, using `const x = $d('ident', false)` (fix #21709) (#21713)
- Fix generic variable resolution on generic func return assignment (#21712)
- Fix result call or_block with multi-statements (fix #21504) (#21717)
- Allow `\uxxxxx` in raw strings (#21724)
- Limit recursion in Checker.ensure_type_exists/2 to 40 levels (it is usually 4 or less) (#21734)
- Add error for `field [$d('x', 2)]int = [1, 2]!` (#21741)
- Fix nested if expr method call (#21773)
- Fix generic inference in if expressions used in assignments (#21781)
- Disallow nil in non-nil arrays and vice versa  (#21786)
- Check expr evaluated but not used (fix #21436) (#21816)
- Fix wrong receiver generic resolution with embed types (#21833)
- Check mismatch of fn call mut argument (fix #21857) (#21873)
- Fix global fixed array key resolution when it is a constant ident (#21900)
- Improve checks for embed in anon struct  (#21877)
- Fix builtin fn var resolving (#21899)
- Fix spawn when calling undefined function (#21906)
- Require enum values to be declared, before using them in other enum values (#21913)
- Check enum field value duplicate (#21920)
- Allow embed of type alias anon struct  (#21928)

#### Parser improvements
- Register selective import aliases as used (#21613)
- Allow multiple flag values in enum.from() for flag enums (fix #21569) (#21615)
- Disallow duplicate operator overload  (#21675)
- Support `[$d('s', 4)]int{}`, move resolving to method on `ComptimeCall` (#21701)
- Support `$d()` in fixed size array `struct` fields (#21731)
- Suggest using type alias when defining methods on non-local types (#21803)
- Sugggest map initialization with the correct syntax  (#21817)
- Fix parameter collision for generated `@[flag]` enum methods, with modules named `flag` (#21844)
- Fix high order generic fn resolution (#21871)
- Fix recognition of `mod.Enum.val` inside fn args (#21908)

#### Compiler internals
- tools,cgen,pref: add `-coverage` support + `vcover`  tool (#21154)
- v.utils: allow to set the names of the compared items, when diffing strings (#21650)
- v.pref: fix regression of command flags not working, when there is a subfolder, named after the command, in the current working folder (#21647)
- transformer: handle `enum_variant = some_const + 10` (fix #21777) (#21779)
- v.builder: print the suggestion for using `v xyz` instead of `v xyz.v` just once (#21801)
- v.builder: improve the C compilation output on cgen errors (#21885)
- v.scanner: use table lookups for very frequently done character checks (#21931)
- markused: mark explicitly all used array and map methods with `-skip-unused` (fix #21907) (#21914)

#### Standard library
- builtin,v: reduce overhead and memory usage for very frequently called methods (#21540)
- builtin: reduce allocations in s.index_kmp/1 and s.replace/2 (#21561)
- os: remove mut declarions for unchanged vars in `os_nix.c.v` (#21564)
- os: make minior improvement to C function semantics and related code (#21565)
- builtin: simplify splint_nth methods (#21563)
- toml: fix `@[toml: ]`, support `@[skip]` (#21571)
- builtin:  update `last_index_u8`, deprecate `index_u8_last` string methods, make consistent with `last_index` (#21604)
- builtin: implement a JS version of `string.split_any` (#21612)
- crypto: add a `crypto.sha3` hash and extended output functions (#21664)
- crypto.sha3: remove unnecessary return at the end of the write function (#21669)
- builtin: fix string.find_between(), when not found end string return '' (#21670)
- builtin: add string.camel_to_snake/0 and string.snake_to_camel/0 methods (#21679)
- cgen,x.json2: fix generic map decoding (#21704)
- encoding.base32: fix warning of implicit clone of array, by using an explicit unsafe block (#21728)
- vlib: simplify byte character conditions by using methods like is_capital, is_lower, is_letter etc (#21725)
- gg: add Context.draw_cubic_bezier_recursive/2 and Context.draw_cubic_bezier_recursive_scalar/9 (#21749)
- ci: fix deprecations_consts.out (the warning for math.max_i8 is now an error; it was deprecated since 2023-12-31)
- math: add divide_truncated/2, divide_euclid/2, divide_floored/2 and modulo_truncated/2, modulo_euclid/2, modulo_floored/2 (#21759)
- math: avoid unused calculations for math.modulo_floored/2 and math.modulo_euclid/2
- crypto.blowfish: apply `@[direct_array_access]` to very commonly used functions (#21771)
- encoding.binary: little_endian_f32_at
- arrays: add arrays.chunk_while/2, where `arrays.chunk_while([0,9,2,2,3,2],fn(x int,y int)bool{return x<=y})==[[0,9],[2,2,3],[2]]`
- sokol: reduce _SGL_DEFAULT_MAX_VERTICES back to 1<<17 (1<<22 in 811ac12, leads to ~140MB of memory usage for gg/minimal.v, instead of just ~38MB)
- vrepl: fix os.input() (#21811)
- Improve consistency and behavior regarding explicit mutability (#21566)
- builtin: add `pub fn arguments() []string {`; make `os.args` use it, remove edge case in cgen (#21852)
- builtin: support `-d use_bundled_libgc` to make it easier to debug/upgrade reliably libgc cloned from ivmai/bdwgc (#21869)
- time: increase http_header_string performance (#21894)
- builtin: add s.trim_space_left/0 and s.trim_space_right/0 methods (#21903)
- strconv,math.bits: eliminate bounds checking in commonly used routines
- arrays: fix arrays.fold, when the `init` value in the call, is an array (#21921)
- string: fix leak in replace_once

#### Web
- net.http: change default http.Server listening address to :9009, to avoid conflicts with tools, that start their own http servers on 8080 like bytehound (#21570)
- net.urllib: update parse host to allow for better error handling (#21602)
- net.html: quick workaround for a cgen problem for `const x = int(0x80000000)`
- net.http:  implement http.download_file_with_progress/2, saving each chunk, as it is received, without growing the memory usage (#21633)
- veb: update import name in docs (#21668)
- Add `m3u8` MIME type `application/vnd.apple.mpegurl` for multimedia playlists (#21688)
- veb.auth: use constant time comparision in compare_password_with_hash (#21693)
- net.http: correct `Response.status()` method comment, to indicate returning of a `Status` enum field, instead of struct (#21735)
- net: fix TcpConn.peer_ip/0 to only return the ip address, without the port number (#21831)
- Add mime type `text/x-vlang`, for `.v` and `.vsh` file extensions (#21851)
- net.conv: add conv.reverse_bytes_u64/u32/u16 (#21917)

#### ORM
- orm: fix subquery without where expr (#21598)
- orm: ilike for case insensitive text search
- orm: update readme with more specifics (#21697)

#### Database drivers

#### Native backend

#### C backend
- Fix printing of floating point values in asserts (#21538)
- Fix array fixed initialization on struct from call (#21568)
- Put the `Interface(unsafe{nil})` change for now behind -experimental (#21593)
- Fix indexexpr with orexpr (fix #21591) (#21592)
- Fix generic struct init when it is a reference &T (fix #21594) (#21596)
- Fix generated code for handling fn call with result/option return on match conditional (#21608)
- Fix missing call to a function returning option, when called inside a print (fix #21616) (#21623)
- Support a `-d no_bool` option, for cgen compatibility with the C23 language standard (since `bool` is a builtin type and a keyword in it) (#21637)
- Fix or expr with non option fn call return (fix #21660) (#21661)
- Generate pragmas for gcc-14, to make it more relaxed by default. Support `-no-relaxed-gcc14` for turning them off. (#21680)
- Fix spawn code generated when calling conditional function (fix #19352) (#21681)
- Fix comptime generic arg resolution (allow several different struct fields in generic functions using comptime) (fix #18110) (#21682)
- Fix dumping array of reference (#21694)
- Implement g.trace_last_lines/2 to help debugging cgen problems
- Support inc cond for c style for loop with alias types  (#21708)
- Fix fn call with mut reference args (fix #21265) (#21719)
- Fix auto dereference mut variable in if expr (fix #21309) (#21720)
- Fix smartcasting a reference to a sumtype value (#21730)
- Fix assert checking fn option ret with `none` (#21726)
- Fix generics map with the reference argument (#21737)
- Fix shortcircuiting of infix and/or expressions (#21740)
- Simplify in infix_expr_and_or_op() (#21745)
- Fix reference variable str() method call (#21753)
- Fix scope command when `$dbg` breakpoint is on or-expr (fix #21772) (#21747)
- Fix array fixed empty struct code generated on clang (fix #21761) (#21764)
- Allow alias to array fixed to be initialized like `[n]int{}` (#21785)
- Fix comptime call argument auto conversion for indexexpr (fix #15232) (#21796)
- Fix const name without main prefix in `-translated` mode (#21789)
- Fix closure variable with optional reference params (fix #21827) (#21829)
- Fix auto str for fn struct member (#21825)
- Fix option string struct member init with autofree (#21859)
- Fix fn mut argument of sumtype reference (#21874)
- Fix generic type init syntax for primitive types  (#21872)
- Fix missing `&` in string representation of `map[X]&Y` (#21890)
- Fix optional closure direct call (#21892)
- Fix wrong missing unsafe block checking when calling generic functions with @[unsafe] attr (#21898)
- Fix typeof(var.typ) with comptime $for variables (#21901)
- Fix enum with const value (#21919)
- Fix generic option/result reference return (#21922)

#### vfmt
- Allow single line `defer {}`, just like `or {}`
- Don't break `it` variable name in match expression function calls (#21600)
- Exit with error code if encountering diffs with `-diff` flag (#21603)
- Add fn_with_mut_ref_params_keep.vv (#21711)
- Implement wrapping function's super long arguments  (fix #15545, fix #21643) (#21782)
- Keep manual newlines after fn parameters in fn declarations, do not auto wrap (#21795)

#### Tools
- Fix spurious ending quote in the output of show_manual_release_cmd.vsh
- Dockerfile.alpine: update to alpine:3.20, add more tools like gdb, libunwind, gc, to ease producing and debugging static executable in more situations (#21583)
- Add `v download URL` for downloading single files, to avoid curl/wget etc, which are not available uniformly. (#21597)
- ci: use shebang with better portability in shell scripts, format shell scripts (#21605)
- Don't write vpm logfiles in CI runs (#21649)
- ci: make vmodules_overrides_test.v failures more informative
- Fix measurements for `./v repeat "./vold -o x project/" "./vnew -o x project/"`
- Make the summary output of `v repeat` more informative
- Disallow self referencing function alias types like `type FnType = fn(string) FnType` and `type FnType = fn (FnType) string` (#21733)
- vrepl: fix method call (fix #21788) (#21792)
- ci: restore v_apps_and_modules_compile_ci.yml (#21791)
- vrepl: fix output of the fn call (related #21792) (#21800)
- vrepl: fix variable name starts with print (#21806)
- v.pref: make pref_test.v be less sensitive on the specific C compiler used as a backend (#21813)
- vrepl: fix arbitrary script execute (#21818)
- ci: disable c2v_ci.yml temporarily, until it is fixed
- vrepl: lots of fixes in REPL
- Add a TOTAL line, to the output of -show-timings, to minimise the need for external tools on windows (#21847)

#### Operating System support
- veb: support sendfile() syscall on FreeBSD (#21541)
- os: use 1 instead of C.SW_SHOWNORMAL to workaround a cross compilation issue from linux to windows
- v.builder: minimise the amount of linked libs for cross compilation to FreeBSD
- v.builder: support downloading the FreeBSD cross compilation files on first use
- Add a test for crosscompiling hw, on linux and macos, to FreeBSD (#21553)
- vlib: make `./v -Wimpure-v -W test vlib/` pass on Linux (#21554)
- os: fix debugger_present() for non Windows OSes (#21573)
- ci: extend coverage, move app test from macos_ci to v_apps_and_modules_ci (#21584)
- gc: fix tcc on macos
- os.font: return the "segoeui.ttf" font, if its file exists on Windows (#21655)
- readline: fix s := read_line('Message\nName:') on unix/linux (#21930)



## V 0.4.6
*20 May 2024*

#### Improvements in the language
- Experimental `x.vweb` is now `veb`, the official and recommended way to build web apps in V. Built on top of pico.v.
- autofree: -print_autofree_vars command line flag for printing variables autofree couldn't free
- Add `@VMODHASH` comptime variable to store the commit sha of a V module (#21091)
- Fix sumtype support for option variant types (#21101)
- Utilize new diff functions in errors (expected/found) (#21431)
- Add @[_linker_section] for global variables

#### Breaking changes
- checker: disallow initializing private struct fields outside structs module (#21183)

#### Checker improvements/fixes
- Ambiguous expression notice for `& << >>`, similar to `&& ||`
- Disallow using aliases of ?Type as !Type (#21128)
- Fix option checker arg validation for ptr passing on non expected ptr (#21087)
- Optimize option and result type check, add more typinfo to error details (#21105)
- Move error handling for `any` type to the checker to resolve parsing issues (#21146)
- Move error handling for user defined type duplicates to the checker to resolve parsing issues (#21147)
- Detect redundant ref when assigning call expr with ref return (#21141)
- Fix multi return using nil and voidptrfix (fix #17343) (#21144)
- Fix C struct embedded init fields checking (#21137)
- Remove resolved vfmt workaround and TODO (#21175)
- Move more match validation from the parser into the checker, add error for match without branches (#21181)
- Improve the error message for an unknown type (#21207)
- Allow passing `none` to `fn f(arg ?&Type) {` (fix #21213) (#21231)
- Fix -parallel-cc regression (part 1, workaround .filter(fn[c]) used in checker/orm.v) (#21238)
- Detect and error on unreachable code in loops, after `continue` and `break` statements (#21294)
- Disallow `Result` callbacks functions like `map/filter/all/any` (#21055)
- Fix missing check for or expr on string interpolation (#17566)
- Allow pass array as mut param to spawn fn (#21283)
- Turn array assign warning into error (#21341)
- Ignore last semicolon on or-expr (#21361)
- Disallow structs with `@[params]` attribute as mutable function parameters (#21206)
- Set auto Return pos correctly; cgen: autofree fix for optional returns
- Disallow invalid ptr operations  (#21515)
- Fix wrong checking for heap struct (#21511)
- Allow alias enum flagged to have bit operations  (#21532)
- Simplify, utilize pref.vroot (#21096)
- Fix option interface member checking when `none` is passed (#21509)

#### Parser improvements
- Fix option as possible match case for sumtype (#21079)
- orm: disallow invalid infix for where clause in `delete` and `update`  (#21113)
- Fix case of falsely registering imports as used, remove unused imports (#21156)
- Remove redundant comments_mode field (#21198)
- Update file path fields; use more expressive name for file path, remove obsolete `file_name_dir` field (#21202)
- Add missing docstrings for vlib/v/ast/comptime_const_values.v  functions (#21219)
- Allow struct init on `for in Iterator{} {` (fix #21179) (#21282)
- Fix `for x in Iterator{} {`, when there are no field initialisations (#21333)
- Add check for result type on chan init (#21363)
- Fix comptime panic for `$tmpl("x.html")`, when the template file contained % at the end (#21402)
- Parse string and array typ idx of `ScopeVar` and `Ident` (#21523)

#### Compiler internals
- v.util: use tmp instead of cache dir for temporary diff files (#21075)
- v.util: fix module lookup if module import parts end with the word `modules` (#21083)
- v.util: update `githash` to be able to get the githash of every passed project (#21178)
- v.util: improve detection for opendiff when automatically searching difftools (#21241)
- v.util: improve color_compare_files (#21247)
- v.util: improve find_diff_cmd: don't add spaces to result without env opts (#21242)
- v.util: fix diff coloring, add test (#21260)
- v.util: polish off diff utils after recent updates and fixes, add doc comments to pub fns (#21275)
- v.builder: suggest using `v wipe-cache`, when the object files are not recognized
- pref: be more conservative when generating code using `-cross`, allow for `$if cross ? {`
- builder: use cc enum in CcompilerOptions, fix cc detection, enable cc guessing without prod flag (#21370)
- pref: fix version flag handling (#21377)
- pref: make minor performance related changes / simplify (#21379)
- builder: simplify generic cc detection (#21380)
- pref: extract architecture related code into `arch.c.v`; rename `pref.c.v` to `pref.v` (#21387)
- pref: update `os_from_string`, add missing `qnx`, remove deprecated wasm options that used `-` instead of `_` (#21390)
- v.util: rewrite diff module, deprecate old functions (#21403)
- v.util: fix color when auto tool is `diff` (#21435)
- v.util: make diff_test.v more robust to the color settings for the chosen local diff tool
- v.util: fix performance with `v test-cleancode`, when a slower diff tool is installed (#21447)
- v.util: remove fast path in `diff.compare_text` (#21458)
- v.pref: error for `v file.v --unknown-option` (#21391)

#### Standard library
- builtin,dlmalloc: fixes for `v vlib/v/gen/c/coutput_test.v` for gcc14.1, which is stricter
- Min window width and height
- builtin: str.last_index(); pref: hide-auto-str;
- toml: update the alexcrichton and BurntSushi repos to their successors toml-rs, and toml-test, record new exceptions (#21152)
- breaking,vlib: update handling of imports whose symbols are not directly used in imported file, remove `pub const is_used = 1` workarounds (#21160)
- json: allow `i32` decoding and encoding (#21162)
- json2: add ability to decode arrays (#21163)
- json2,checker,toml: allow field.typ compile-time checking with MatchExpr and add array of option checking (#21171)
- gg: draw_text with custom fonts
- x.json2: add a way to decode an array (#21186)
- os: clarify some doc comments (#21209)
- os: fix double free in os.get_raw_line() (used by os.input), with `-autofree` (#21204)
- time: extract Duration related code into duration.v (#21229)
- builtin: implement an `at_exit(cb)` wrapper for C.atexit (part 1) (#21254)
- os: format readme, fix markdown inside html (#21286)
- time: update unix time acces, fix issues related to deviating unix times (#21293)
- vlib: refactor empty string checks to use `s == ''` or `s != ''`, instead of `s.len == 0` (#21300)
- cli: update `command_test.v` (#21307)
- cli: extend control over defaults (#21308)
- thirdparty/sokol: bump _SGL_DEFAULT_MAX_VERTICES and _SGL_DEFAULT_MAX_COMMANDS again; mark them with `__v_ start` and `__v_ end`
- sync: add Gentoo paths for libatomic
- sync.stdatomic: add flag lines for gcc 14 too
- gg: make `PenConfig` fields public (#21353)
- builtin: fix undefined read s[0], from ''.is_lower() and ''.is_upper() in c7af2c2
- builtin: fix empty string lower / upper assert (#21358)
- cli: simplify flag parsing (#21392)
- os,runtime: workaround for v.c generation instability
- datatypes: fix for set `-` operator, union and intersection, now they no longer change the receiver (fix #21315) (#21362)
- sync.stdatomic: add paths for compilation with musl on Gentoo (#21400)
- os: fix os.execute stderr redirection (fix #20986) (#21404)
- time: fix the string representation of a negative Duration (#21407)
- cli: make program outputs using the cli module testable in `cli/testdata` (#21456)
- math.unsigned: permit _ separators in Uint128 decimal strings passed to uint128_from_dec_str (#21461)
- cli: fix default flags when their command equivalents are disabled (#21469)
- toml: simplify `decode_quoted_escapes` (#21472)
- os: fix join-path (#21425)
- builtin: simplify MessageError.msg() (#21524)
- all: replace usages of C.atexit(cb) with `at_exit(cb) or {}` (part 2) (#21263)
- math.unsigned: fix some Uint256 bugs and add tests (#21528)


#### Web
- Update `mbedtls` to latest compatible version v3.3.0 (#21118)
- veb.auth: a minor find_token fix
- Improve descriptions (#21155)
- ci: change listen ports in vweb_should_listen_on_both_ipv4_and_ipv6_by_default_test.v for both vweb and x.vweb, to reduce probability of network errors
- ci: mark both vweb and x.vweb versions of vweb_should_listen_on_both_ipv4_and_ipv6_by_default_test.v as flaky
- breaking,net.ftp: allow to choose a different port than port 21 (change FTP.connect to accept `host:port`, not just a `host` address) (#21185)
- x.vweb: accept query params as method arguments (#21201)
- net.http.file: support index_file (`index.html` by default), and auto_index (true by default) parameters to file.serve()
- veb: copy x.vweb to veb, and make it work with comptime
- ci: fix the ubuntu-docker-musl job by skipping veb_app_test.v
- pref: support a shortcut: `v -http` for `v -e "import net.http.file; file.serve()"` .
- net: add a .port()! method for net.Addr (#21412)
- net: improve error message in .port()
- picoev: handle `EAGAIN` or `EWOULDBLOCK` quietly (#21480)
- net.unix: remove debug/trace eprintln (#21517)

#### ORM
- Add error for unchecked option multi return types, fix undefined behavior (#21106)

#### Database drivers
- db.mysql: fix invalid memory access in exec_one for returned rows with NULL fields (#21317)

#### C backend
- Enable autofree for option (#21051)
- Force C struct types which does not implement str() to be passed as ptr (#21054)
- Improve diagnostic information for ORM queries with invalid types
- Allow static call on generic type (#21071)
- Fix code generation for a struct field, having a type of fixed array of options `field [5]?Type` (#21082)
- Add the `_M_ARM64` macro to endianness check (#21109)
- Fix return code when returning interface result type (fix #21115) (#21130)
- Fix const initialized with array (#21131)
- Fix infix array heap comparison (#21145)
- Fix C struct sumtype support (#21129)
- Add `autofree` comptime check (#21197)
- Fix comptime `$if !autofree {` (#21218)
- Allow `for mut v in [12, 13, 14] { v+= 2 }`  (#21237)
- Allow op overload for type with generic parent  (#21262)
- Optimize .map(), .any(), .filter() and .all() when using closure (#21256)
- Fix `none` passed to a generic option cast expression (fix #21215) (#21276)
- Fix `-fsanitize=undefined` used with `[]int{}.sort()` (#21331)
- Fix `myarr [1]C.mytype` fixed array fields, for `pub type C.mytype = voidptr` (#21266)
- Fix comptime ref argument passing (#21335)
- Use the real C line number instead of `#line 1000000 ...` in the C footer with `-g` (#21388)
- Fine tune the line count reset for the C footer (fix off by 1 error in 00dd0bf)
- Fix array.delete_last call generation (#21439)
- Fix option ptr unwrapping (#21415)
- Fix C struct option alias printing (#21496)
- Handle auto deref var for index when the array element is an interface or a sumtype (#21491)
- Fix C struct init when it has default expr (#21510)
- Fix sumtype field naming (when they are the same as a C keyword) (#21527)

#### vfmt
- Update determining of struct field comments (#21066)
- Inform about invalid interop function bodies instead of removing them (#21070)
- Parse methods on JS interfaces, write JS method bodies (#21088)
- Improve module detection when formatting imports (#21134)
- Don't change paths when formatting imports (#21148)
- Use fixed size array for max_len const (#21140)
- Simplify const name formatting (#21143)
- Improve import processing, add test (#21172)
- Fix duplicates remove import comments (#21177)
- Extend import import alias reference map for submodules (#21200)

#### Tools
- doc: fix vup/vself replacement scripts (#21092)
- Prevent module updates during `v build-tools`, when VTEST_SANDBOXED_PACKAGING is set (#21094)
- ci: update the reference to the v-analyzer repo
- ci: retry all setup commands that need network access, till they succeed (so the CI jobs have less false positives) (#21103)
- changelog: escape `@NAME` entries, to avoid mentioning unrelated github accounts
- Add `v retry apt update`, intended to replace the retry.sh script, for more robust CI jobs (#21104)
- vpm: show the final path where a module is installed, improve color contrast for white on black terminal sessions
- vet: print help when passing no files or `--help`/`-help` flag after vet command (#21108)
- Fix `v build-tools` (it skipped all tools in subfolders of cmd/tools/ after 6a4f293) (#21120)
- .gitignore: ignore generated .NET files in bench/vectors (#21136)
- vet: optimize performance for path detection, when vetting files (#21139)
- vet: allow to overwrite excluded dirs (#21142)
- ci: increase wait time for the xvfb check, to reduce the chance of false positives
- Fix `v run cmd/tools/measure/parser_speed.v file.v`
- Add `v run cmd/tools/measure/fmt_speed.v file.v`
- ci: move build step for VPM site, into apps and modules ci, add concurrency config (#21191)
- tools.vpm: debug-log to `$VMODULES/cache/vpm.log` if not running in debug mode (#21192)
- vpm: optimize performance by adding filter when cloning (#21216)
- vdoc: don't add _docs directory when an out path is specified (#21233)
- ci: prefer dedicated tiggers for platform workflows, so sporadic fails can be retried quicker (#21251)
- v.util: improve code related to diff tool specified via environment, add check if the diff tool exists (#21240)
- vpm: check for git version before adding `--also-filter-submodules` flag (#21259)
- ci: add logging to .github/workflows/retry.sh
- Revise `vtest-self.v`: remove dead paths, fix json2 essential test path (#21267)
- Add check for unavailable files in vtest (#21272)
- ci: reactive app prod builds (#21295)
- ci: add a m1 runner for testing the prebuilt zips too
- ci: add workflow_run: event in prebuilt.yml
- ci: simplify prebuilt.yml, make it usable for manual runs too
- vpm: fix regression, so `v install sdl && v run ~/.vmodules/sdl/setup.vsh` works again
- ci: fix outdated_test.v (--unshallow is not needed now)
- ci: continue testing independent V apps, even if one fails, to get feedback for breaking changes faster (#21302)
- ci: optimize apps and modules (#21303)
- ci: test `v ~/.vmodules/sdl/setup.vsh`, to prevent future vpm regressions (#21306)
- ci: prevent unintended deployment workflow steps (#21313)
- Add a 2024.html page to https://fast.vlang.io/
- vdoc: rewrite and extend vdocignore (#21314)
- ci: fix the commit labels for the vc repo
- Support `v run cmd/tools/oldv.v --show_VC_commit weekly.2024.03`
- ci: use latest upstream `discord.v` in apps and modules test (#21322)
- vdoc: remove obsolete entries from `.vdocignore` (#21320)
- v: update linguist languages, add .vdocignore (#21321)
- ci: update deployment workflow (#21323)
- Allow for selectively running `v test-self vlib/` and `v test-self cmd/` (#21326)
- Rewrite test-self arg handling (#21327)
- ci: restore `v install elliotchance.vsql` in v_apps_and_modules_compile_ci.yml
- ci: use `v retry -- cmd` to replace `.github/workflows/retry.sh cmd`, where possible in `v_apps_and_modules_compile_ci.yml` (#21336)
- ci: update workflow conditions (#21338)
- Improve `v symlink -githubci` diagnostic message, when used outside CIs or with sudo (#21340)
- ci: update detection of workflow cancellation scenarios (#21342)
- Fix compiling vwhere with `-cc gcc -cstrict` (#21347)
- ci: remove the separate `-gc boehm` job (since `-gc boehm` is the default now) (#21352)
- ci: add a separate cmd/tools testing job (#21344)
- Update fast.v and fast_job.v to update docs.vlang.io and fast.vlang.io on each commit to master.
- Make fast.v more robust to independent remote changes in vlang/docs
- Utilize environment specific files for vsymlink  (#21360)
- ci: update `binary_artifact.yml` (#21364)
- ci: add docker to tools workflow, update ignore paths (#21368)
- ci: split up vsl / vtl run, reducing the tool change CI time from ~19min to ~10min (#21372)
- ci: fix binary_artifact.yml (#21373)
- Refine `check_os_api_parity.v` (#21371)
- ci: update native backend ci matrix (#21375)
- ci: update symlink ci, add matrix (#21376)
- ci: workaround defer codegen failing with nested if blocks and -cstrict in vdoc_file_test.v
- ci: update detection of accidentally added gpl licenses (#21384)
- ci: set cancel-in-progress to false in bootstrapping_ci.yml to avoid false positives
- ci: do trigger bootstrapping_ci.yml periodically, but just on changes
- ci: speed up bootstrapping_ci.yml, by using the default tcc when possible
- ci: update `bootstrapping_ci.yml` trigger paths (#21394)
- ci: pass a good commit to oldv.v in `bootstrapping_ci.yml` (#21393)
- Be more verbose when doing `v up` in V folder produced by extracting a .zip release file
- Exclude thirdparty/tcc from the git clean operation, that vup does in case of a missing .git folder
- Protect from cleaning during `v up`, only ./v , not any matching folder
- Use proper ignore/exclude patterns in the git clean, that `v up` does
- Use more meaningful names in vup.v
- Be verbose, when a git commands that `v up` executes fails
- ci: add a v-up-works-ubuntu job, to ensure more robust `v up` runs (#21401)
- ci: ensure v master is available when trying to check out its commits to build oldv (#21414)
- Rewrite vet error handling (improve parser performance extend vvet) p1 (#21417)
- Move dynamic const array check from parser into vet (#21423)
- v.help: update help for `fmt` (#21430)
- Move array_init_one_val checks from parser into vet (#21422)
- Remove `vet_errors` and `vet_notices` from parser (#21424)
- ci: temporary fix for gitly compilation
- Remove vetting for spaces after / before parens (#21437)
- Add `.github/workflows/show_manual_release_cmd.vsh`, to make cross platform testing of the release process easier
- ci: merge docker_alpine and docker_ubuntu workflows in `docker_ci.yml` (#21446)
- Move now obsolete vlib vet module to cmd vet (#21445)
- Use `parse_file`, remove `parse_vet_file` (#21444)
- ci: update binary artifact workflow, add matrix (#21378)
- ci: add workflow_dispatch: to gen_vc_ci.yml
- ci: fix vinix_ci.yml by using `./v symlink -githubci` in vinix_ci.yml
- ci: port changes from Vinix's check.yml at 8231e569 to vinix_ci.yml
- tools.vet: move error methods to `vvet/errors.v` (#21449)
- ci: reduce false negatives for tcp_test.v, retry it 3 times before failing
- Improve performance of `v test-cleancode` and `v fmt -inprocess -verify .` (#21450)
- Make `./v symlink` work platform independent in CI (part 1) (#21453)
- ci: replace .github/workflows/retry.sh usages in the CI with the shorter `v retry --`, move `v test-cleancode` upwards to save time for unformatted PRs (#21452)
- Capitalize information output of `v up` (#21464)
- ci: use `v symlink` without `-githubci` for regular symlinking (#21455)
- ci: add a linter for the .yml workflow files (#21459)
- ci: update symlink ci, extend tested cases (#21466)
- tools.vet: update diff comparison in `vet_test.v` (#21457)
- Call mkdir_all before set_output_path to avoid a vpm panic when ~/.vmodules/cache does not exist (#21463)
- ci: make issue template title consistent, fix linter error regarding labels (#21460)
- tools.vet: reduce nesting in `vet_fn_documentation`, skip vetting empty lines (#21465)
- Print info to use v symlink instead of `v symlink -githubci` (#21471)
- Move _test.v files for vdoc at the same level (#21473)
- ci: update the helper script, for getting the most recent sqlite-amalgamation-3380200.zip (#21474)
- vdoc: fix handling of .vdocignore files in subdirectories (#21514)
- ci: run build-module-docs, when changes to the source of the `v doc` tool happen too
- ci: use g++ not g++-11 in misc-tooling (g++-11 can not be found and installed on the CI runner anymore for some reason)
- ci: update g++ to g++-10 in other_ci.yml, add workflow_dispatch: trigger for easier local future diagnostic
- vdoc: improve vdocignore file handling by walking all .vdocignore sub-paths in IgnoreRules.get, add test (#21521)
- ci: run `v fmt -w cmd/tools/vdoc/vdoc_test.v`
- ci: make sure that unformatted code in just cmd/ is not allowed
- ci: mark again tcp_test.v as flaky (it had 3 unrelated failures on the CI over the last week)
- v: vet for empty string conditions (#21529)
- tools.vet: add notice for empty strings conditions (#21421)

#### Operating System support
- ci: improve test robustness on windows (#21116)
- v.pkgconfig: add pkgconfig path `/usr/libdata/pkgconfig` for FreeBSD base packages (#21151)
- v.util: add diff tool color support detection (tested on linux and freebsd) (#21244)
- v.util.diff: return diff options with the diff command for FreeBSD/OpenBSD (#21271)
- v.pkgconfig: fix load_paths with `;` split char on windows (#21291)
- Fix vpm on macos, when using the bundled git executable (#21292)
- ci: fix the bundled tcc for macos arm64 (#21299)
- ci: update the runner for build-macos-arm64 to `macos-14` too, so it runs on M1
- Fix hot code reloading on windows (#21351)
- Fix building vpm on the FreeBSD instance, that runs fast.v
- Fix `v install` for the FreeBSD instance that updates docs.vlang.io .
- ci: use macos-13 for cross_ci.yml to force the old non m1 runner
- v.builder: update macos->linux cross compile message (~22MB -> ~77MB)
- v.pref: fix new pref test on FreeBSD (#21385)
- ci: stop earlier on vc/v.c files, that may break on systems != linux (#21397)
- Fix compilation on macos-arm with `-cstrict`; run macos ci also on the arm runner (#21408)
- ci: use `v` instead of `./v` in the platform linux/macos/windows/_ci.yml files (#21454)
- ci: add a retry to vcreate_init_test.v (it is sporadically flaky on macos)
- sync,os,thirdparty: fix cross compilation from macos to windows (#21484)
- os: rename os_structs_stat_windows.v to os_structs_stat_windows.c.v to fix `v -Wimpure-v -os windows vlib/os/os_stat_test.v`
- Default to `-cc clang` on FreeBSD in `cmd/tools/vtest_test.v` (#21534)



## V 0.4.5
*20 March 2024*

#### Improvements in the language
- Add map update-init syntax: `new_map := {...old_map, 'k1': 1, 'k2': 5}` (#20561)
- Improve coroutines, Photon vcpu creation, coroutines + GC fixes (#20549)
- Update Boehm GC libs/headers to the latest version 8.3.0 (#20772)
- $dbg statement - native V debugger REPL (#20533)
- Implement `@[_linker_section]` attribute (#20629)
- Enable `@[export]` for global variables too (#20649)
- Add callstack support on v.debug (#20680)

#### Breaking changes
- sokol: the sokol wrapper was updated, to match its upstream version at commit 058a4c5, several of its APIs no longer exist
- templating.dtm: compile_template_file is no longer public
- v.trace_calls: `pub fn on_c_main() {`, is now `pub fn on_c_main(should_trace_c_main bool) {`
- v.transformer: Transformer.fn_decl is now Transformer.fn_decl_trace_calls
- x.vweb: Context.redirect(url string, redirect_type RedirectType) is now Context.redirect(url string, params RedirectParams)

#### Checker improvements/fixes
- Check invalid lambda expr (#20461)
- Fix comptime if with comptime smartcast (#20466)
- Fix anon struct init with const fields (fix #20452) (#20463)
- Disallow `option` or `result` return type, for infix operator overloading (#20494)
- Cleanup the const variable evaluate for fixed array fields of structs (#20503)
- Fix missing check for struct initialization with `@[noinit]` attribute, through using `T{}` (#20516)
- Fix mark methods into used-list, when generics as receivers (fix #20509) (#20527)
- Modify comments on generic receiver type storage (#20539)
- Fix checking give const map as default or init value to struct fields (fix #20512) (#20546)
- Fix return map index with or_block (#20544)
- Cleanup the generic tests (#20553)
- Fix `@[deprecated]` attribute for consts (fix #20523) (#20550)
- Cleanup in method_call() (#20554)
- Disallow `non_opt_array << optvalue` (#20573)
- Fix non dereferenced enum in match statements (fixes #10045) (#20591)
- Fix .variant smartcast on non-comptime variables (#20575)
- Disallow static maps: `mut static x := map[string]int{}` (#20596)
- Allow `#define X` and `asm riscv64 {}` blocks in .v files, with `-freestanding` (make embedded usecases easier)
- Add cast overflow checks (#20641)
- Disallow assigning none to _ (#20646)
- Fix checking for option matching with non-option (#20673)
- Disallow `(x) := 10` (#20695)
- Disallow `none` as match cond (#20688)
- Fix comptime ident checking on defer stmt (fix #20719) (#20723)
- Add error for `x as Y.field`, suggesting using `(x as Y).field` instead for clarity (#20725)
- Disallow sum type holding alias ptrs (#20786)
- Optimise out calls to `arg_typ_sym.embed_name()` when there are no errors (#20820)
- Fix if branch option type mismatch (fix #20809) (#20830)
- Fix auto deref arg when fn expects ref (#20846)
- Fix struct field init with generic fn variable (fix #20847) (#20878)
- Cleanup in assign_stmt() (#20880)
- Check assigning immutable reference struct field (fix #20814) (#20883)
- Fix struct field init with generic anon fn (add the test from #18294) (#20888)
- Fix checking match branch call expr twice (#20910)
- Support `Flags.bit ^ Flags.bit1` and `~Flags.bit` (flagged enums) (fix #20925) (#20929)
- Fix some specific interface generic inference within generics struct and method (#20932)
- Remove notice when shifting signed int for `@[translated]` (#20935)
- Silence "assigning 0 to a reference field" and "uninitialized fn struct fields" notices for `@[translated]\nmodule ...` files (#20938)
- Fix missing check for interface cast of option type (#20961)
- Silence more warnings for `@[translated]` files (#20964)
- Fix comptimeselector passing to generic argument (#20985)
- Remove unnecessary string interpolation in deprecation method calls (#21007)
- Disallow void return value lambdas in array.map method calls (#21011)
- Cleanup and simplify `check_ref_fields_initialized` methods (#21016)
- Cleanup and simplify struct processing p1 (#21009)
- Add support for deprecation messages for structs and struct fields (#21017)
- Cleanup and simplify struct processing p2, extend test (#21025)
- Fix undefined reference to interface struct field regression (after #21025) (#21030)
- Add test for interface embedding and interface with erroneous implementation (test related to #21030) (#21033)
- Disallow `Optional` and `Result` high val in a `for x in low..high {` loop  (#21043)
- Fix missing incompatible pushval type for chan <- operator (#21040)

#### Parser improvements
- Fix close_scope() missing, when field.name is `sort` or `sorted` (fix#20436) (#20485)
- Check non-generic interface defining generic method (#20545)
- vast,ast: output map init update expression (#20574)
- Implement `MyEnum.from(1)!` generic static method (#20411)
- Fix `MyEnum.from(0)!`, implement `MyFlaggedEnum.zero()` (#20623)
- vfmt,parser: keep the original import name in ast.Import, and use it without modifications for paths unders ~/.vmodules
- Allow double quotes in `@include` template directives (#20628)
- Fn type declaration does not check already registered name (#20732)
- Fix global const ordering with string inter literal (fix #20760) (#20770)
- Disallow option alias with option parent type  (#20769)
- Make Scope.find methods more robust, when called on default initialised `scope &Scope = unsafe { nil }` fields (#20774)
- Fix parsing of cgen.v, in normal mode, when the table is empty (no files in `builtin` are preparsed) (fix #20606) (#20611)
- Fix infinite loop in Parser.sql stmt in `-silent -print-watched-files` mode (used by `v watch`) (#20873)
- Disallow defining map key more than once  (#20905)
- Fix formatting comptime if expr, after inc expr (fix #20927) (#20931)
- Fix for comptime with fully type name (fix #20948) (#20988)
- Allow lock prefix instructions and numbered reg in inline asm blocks (#21022)
- Add better error for mut variadic fn argument  (#21063)

#### Compiler internals
- v.util: make launch_tool failures more detailed (for the `Text file busy; code: 26` case), bump tool_recompile_retry_max_count from 3 to 7
- v.util: make launch_tool more robust, when multiple `v -b js file.v` commands are run at the same time (all trying to recompile the JS backend program) (#20631)
- builder: allow for `./v -freestanding -cc riscv64-elf-gcc -d no_main -no-builtin -no-preludes -showcc -keepc x.v`
- pref: support file_notd_freestanding.v + file_d_freestanding.v, remove dependency to `os`, of $embed_file(), when compiling with -freestanding (#20712)
- v.builder: reduce the default noise from successfully rebuilding cached thirdparty object files
- pref: allow fetching the photonwrapper .so (for the coroutines) with curl too, or print details for manual download (#20855)
- scanner: disallow a shebang line, that is not at the top of a file (#21029)
- strings.textscanner: fix off-by-one error in skip method (#21045)

#### Standard library
- x.crypto.chacha20: speed up the core functionality of the ChaCha20 stream cipher (#20470)
- log: enhance log time format setting (#20484)
- encoding.csv: add a new utility fn `new_reader_from_file/2` (#20530)
- readline: add completion support (#20559)
- builtin: add `is_hex()`, `is_int()`, `is_bin()`, and `is_oct()` methods to the string type (#20540)
- builtin: add empty string verification for the new string .is_oct() etc methods, suggested on PR #20540 (#20564)
- json: fix struct with option enum field (fix #20597) #20597
- x.json2: fix panic on calling json2.decode of an optional enum (fix #20593) (#20603)
- vlib: add a compress.zstd module (#20616)
- io: ensure BufferedReader.read_line() returns `io.Eof`, instead of `error('none')` (#20619)
- log: add support for l.set_short_tag/1 (#20652)
- Update error checking for new error io.Eof (#20656)
- io: allow BufferedReader.read_line() to accept custom line delimiters (#20655)
- builtin: implement unbuffer_stdout/0 (#20662)
- x.crypto: add sm4 module (#20651)
- crypto.aes: optimise performance (#20674)
- os: add proper process termination with p.signal_term() (#20671)
- os: simplify and unify os.join_path and os.join_path_single, and add more tests (#21494)
- bitfield: enhance operation with multiple flags (#20683)
- os: fix File.read() in JS backends (fix #20501) (#20633)
- os: add error_posix() and error_win32() for explicit platform error handling and default behavior (#20694)
- log: implement set_always_flush/1 for log.Log, log.ThreadSafeLog and log.Logger (#20698)
- x.vweb: error() and simpler redirect(); comptime: a clearer error
- builtin: add a string.u8_array() method (#20736)
- os: add os.stat() and helpers (#20739)
- os: make os.SystemError struct public so the os.error_* functions can be used by other modules (#20754)
- os: refactor to use os.stat and os.lstat instead of unsafe C calls (#20759)
- os: make os_stat_test.v more robust to reporting false positives
- x.crypto: add poly1305 message authentication code (mac) in pure v (#20752)
- encoding.binary: add _fixed variants for the conversion fns, that accept fixed arrays (#20766)
- x.crypto.sm4: make sm4 use the encoding.binary _fixed fns (#20773)
- builtin: add gc_collect/0, gc_get_warn_proc/0, gc_set_warn_proc/1. Use them to turn off GC warnings by default. (#20788)
- builtin: support `-d gc_warn_on_stderr`, to show the GC warnings, without installing a custom warn fn callback
- x.crypto: add AEAD ChaCha20Poly1305 algorithm in pure v (#20817)
- x.crypto.chacha20: remove deprecated `math.max_u32` in favour of builtin `max_u32`, remove unneceseary bits, reorder (#20838)
- json: fix decode struct ptr (#20828)
- time: add a .http_header_string() method on Time (#20861)
- json2: reorganize encode string (#20862)
- vlib: add `encoding.txtar` (port of Go's txtar module) (#20874)
- gg: handle dpi change, when moving window to another monitor (#20886)
- time: add a tm_gmtoff field to `C.tm` (a BSD and GNU extension) (#20907)
- x.json2: add skippable field attr `@[json: '-']` (improve backwards compatibility with the `json` module) (#20892)
- time: rename identifiers and parameter names (#20913)
- io: add a `string_reader` submodule (#20893)
- toml: return an error from toml.parse_file(), when the passed file path does not exist (#20912)
- x.json2: fix encoder commas (#20916)
- time: microoptimise the Time formatting methods (use custom number->string conversion, instead of string interpolation) (#20917)
- x.json2: improve performance of string encoding for unicode special characters and emojis (#20867)
- x.json2: minor performance improvement, update benchmark recommendations (#20954)
- os: workaround a `-prod -cc gcc` bug, affecting os.open_file (fix #20923) (related to #20872) (#20960)
- cli: add pluralization to err msg, if required number of args is not met (#20971)
- os: remove repetitive words in comments (#20981)
- gg: fix empty circle in native; http: post_form_with_cookies; veb: print veb action in html errors
- io.reader: make read_all constants public (#20997)
- builtin: expose gc_disable(), gc_enable(), gc_is_enabled(), in addition to the existing gc_collect() (#21002)
- x.json2: improve error message upon missing comma (#20602)
- builtin: fix a few grammar errors in builtin.string comments (#21010)
- io.string_reader: fix needs_fill_until check (#21005)
- builtin: add missing return type to fn signature for C.GC_get_stack_base
- x.json2: predefine buffer capacity for encoding to avoid reallocations (#20920)
- rand: add PRNG.fill_buffer_from_set/2 (#21037)
- sokol.gfx: update the PixelFormat V enum, to exactly match the C sg_pixel_format from thirdparty/sokol/sokol_gfx.h

#### Web
- net: fix vlib/net/tcp_test.v (since .listen_tcp with af .unix, is now expected to return an error) (#20472)
- net: remove unused import in tcp_test.v
- x.vweb: add error, when static directory does not exist (#20455)
- net.urllib: fix parsing url error, when querypath is '//' (fix #20476) (#20504)
- vweb: unify listen address from tcp and print (#20448)
- net.unix: make the error messages in unix_test.v more specific (#20537)
- vweb: add an optional Context.before_accept_loop/0 method, to make testing easier and more robust (#20538)
- vweb: fix routes without results in vweb_app_test.v (#20548)
- vweb: make vweb_test.v more robust and faster, by embedding its server as a module
- Small fixes and backport changes from vweb (#20584)
- net.smtp: implement mail attachment support (fix #19920) (#20640)
- vweb: fix quickstart docs in the module's README.md on how to create a new vweb project (#20644)
- net.http: add a temporary fix for the intermittent segfault with http.get_ text/1 and `-prod -cc gcc` 13.2.0 (fix #20506) (#20660)
- x.vweb: support HTTP 1.1 persistent connections (#20658)
- x.vweb: use `$if debug_ev_callback ? {` for the `[vweb] error: write event on connection should be closed` message
- x.vweb: add cors middleware (#20713)
- x.vweb: add new sessions module (#20642)
- net: fix non-blocking read/write (#20438)
- net: reduce flakiness of tcp test (#20761)
- picoev: renaming, doc (#20567)
- x.vweb: add full static host support, for urls ending with /folder/ , where the folder backing it, has `index.html` inside (#20784)
- x.sessions: change session Store interface to use results instead of options (#20796)
- net: fix function name in split_address doc comment (#20794)
- doc: x.vweb static website capabilities (#20808)
- thirdparty: update picohttpparser (#20843)
- picohttpparser: restore formatting for g_digits_lut, after f09826e (#20844)
- x.vweb: fix handling of static URL paths like `/sub.folder/a_folder` (#20863)
- veb: a new module veb.auth for authentication logic (tokens, hashed passwords)
- veb.auth: make work with any db
- net: workaround a `-prod -cc gcc` bug (#20872)
- picoev: add more logging of errors (#20558)
- picoev: remove fmt off tags (#20569)

#### ORM
- orm: fix checking invalid recursive structs (fix #20285) (#20491)
- orm: fix checking invalid recursive structs in sql stmts (fix #20278) (#20505)
- orm: fix orm insert issue if table missing [Issue : #20017] (#20580)
- orm: fix orm_option_time_test.v after 2d0ed2c made insert in parent tables with child ones missing fail
- orm: insert expressions returning id

#### Database drivers
- db.sqlite: fix exec_param_many bug (#21008)

#### C backend
- Fix multidimensional fixed array size expression evaluation (fix #20311) (#20458)
- Fix fixed array handling with operator overloading call (fix #20467) (#20469)
- Fix C code, generated for generic option fixed array return type (fix #20465) (#20479)
- Fix fixed array handling, on generic result return, and on or block (#20492)
- Fix generated code for fixed array cast (fix #20454) (#20480)
- Change `x.filter(cond).len > 0` to `x.any(cond)`, and `x.filter(cond) == 0` to `x.all(!cond)` (#20513)
- Fix code generation wrong, when '?foo.array or {}' as a 'for-in' condition (fix #20528) (#20542)
- Add a necessary clone, when the closure param is string/array with -autofree (fix #20498) (#20557)
- Fix wrong cgen, when auto_heap var, is used as a closed over arg in a closure (fix #20208) (#20566)
- Initialize closures in shared library mode (#20630)
- Fix interface generic smartcast (#20609)
- Support inter-dependent function types (#20638)
- Write the profile file out, even upon CTRL-C or kill (#20677)
- Fix as cast as selector (fix #20710) (#20718)
- Fix method call checking against `none` (fix #20711) (#20717)
- Fix interface on multi return func (fix #20720) (#20721)
- Fix premature variable release by autofree (#20731)
- Fix return with option on orexpr (#20728)
- Fix auto str for map with ptr str (#20741)
- Remove `ull` suffix, which looks like the cause for the first scanner error in PR#20726 (#20750)
- Fix comptime smartcast as receiver on method call (#20749)
- Fix unwrap on or-expr, when calling f() ?Type (fix #20756) (#20758)
- Builtin,coroutines,cgen: fix using coroutines with boehm GC, by using a stack pointer corrector (#20771)
- Fix interface casting (#20789)
- Fix auto_eq for option eq operator overload (#20795)
- Fix from_string fn generation missing mod name (#20807)
- Fix const fixed array initialization handling (#20812)
- Fix unwrapped option selector assigning (#20816)
- Fix map methods call with generic types (fix #20827) (#20829)
- Fix codegen for a.index/1, where a is []Fn (#20849)
- Fix thread return type generation (fix #20836) (#20850)
- Fix code generated for anon struct default value (fix #20839) (#20851)
- Fix comptime selector of interface (#20876)
- Fix multiple fixed array variable init (fix #20895) (#20902)
- Ast,checker,cgen: fix generics function with embedded structs, ensure correct link generation in cgen (#20900)
- Fix returning option call in non-option fn (#20943)
- Fix global initializer of fixed array on gcc (#20934)
- Fix comptime `is` condition when using interface (#20952)
- Fix const fixed array init with index (#20950)
- Fix generic map inferring key and value types (#20959)
- Fix missing scope enclosing for const init which needs temp variables (#20973)
- Fix fixed array return on fn with option generic return (#20974)
- Fix code generated to comptime passed to generic arg (#20994)
- Fix match for alias  (#21028)
- Add ability to mark a global as `@[hidden]` (#21035)
- Fix _str name generated for C struct which define str() method (#21042)
- Fix for/in codegen when iterating over C structs (#21052)

#### JavaScript backend
- Fix javascript backend treating u32 as i32 (fix #20499) (#20618)
- Fix u16 cast handling in the JS backend (#20620)
- Make program_test.v not flaky anymore, so that it is more sensitive to early errors. Fix compilation error for u16.v . Make u32.out match the current reality (the bug will be filed separately)
- Fix inconsistent output (u32) in JS backend (#20691)

#### vfmt
- v.fmt: drop newline in end comments for const (#20672)
- Fix alias array no cast init (#20898)
- Fix removal of used selective and alias imports in modules in `$VMODULES` dirs (#20977)
- Improve submodule type alias lookup; fix formatting of modules in `$VMODULES` (#20989)
- Fix type names for casts (#21036)
- Insert auto imports after shebang (#21038)
- Fix autoimport with shebang and comments above other imports (#21050)
- Fix formatting for imports of submodule from module `src` dir (#21060)
- tools.fmt: extend exit codes to allow spotting unformatted files next to internal errors (#21058)
- Fix parsing of interface with comment above `mut:` keyword (#21062)

#### Tools
- repl: support executing shell commands on the REPL with `!sh [COMMAND]` (#20496)
- repl: fix an issue with `print` and println after the execution of `for` or `if` (fix #20524) (#20525)
- tools: bump too_long_line_length_table to 160, to avoid warnings for just `v check-md doc/docs.md` (most common)
- tools: bump too_long_line_length_link to 250, to avoid warnings for very common long links
- ci: simplify time_ci.yml, use more descriptive CI job names, for easier judging of CI failures
- debug: fix variable dereferencing (#20594)
- tools: support setting another SCANNER_MODE=parse_comments in parser_speed.v and scanner_speed.v
- testing: fix warning for compiling `./v cmd/tools/vtest.v`
- docs: add a section about modules specifics (#20653)
- github: add dependabot.yml (#20800)
- vtest,pref: add ability to have platform specific _test.v files (#20810)
- ci: change spaceface777/cancel-workflow-action to styfle/cancel-workflow-action (#20806)
- tools: use the same same skipping logic for the platform specific _test.v files in `v test-self` too (#20815)
- tools: make the output of `v check-md .` more informative (#20819)
- v.debug: implement tracing hook feature (#20818)
- ci: mark db_store_test.v as flaky
- ci: add a vtcc step (check that vtcc, continues to be able to compile with v, and v itself can be compiled with vtcc) (#21000)
- v.util: simplify vtest (#21013)
- vtest-self: add sandboxed packaging case (#21059)

#### Operating System support
- v.builder: allow for `v -shared -os windows file.v` on Linux/macos (fix #20445) (#20453)
- Add windows dll support, fix (#20447) (#20459)
- sync: add mutex.try*lock functions for FreeBSD too (#20482)
- sync: fix FreeBSD implementation of sync functions (#20483)
- os: make os.cache_dir() and os.vtmp_dir() more robust to parallel test executions on windows (#20495)
- builder: replace "-rdynamic" for clang on macos with "-Wl,-export_dynamic" (fix #20510) (#20511)
- builder: restore ability to use tcc, without fallback to cc on macos
- v.builder: use a more uniq prefix for the generated temporary file names, to further reduce the chances of collision and sporadic CI failures on windows (#20551)
- encoding.csv: fix bug in RandomAccessReader, spotted on windows with mingw32 (#20571)
- builtin: use `#pkgconfig bdw-gc-threaded` where available, instead of `#pkgconfig bdw-gc` (on FreeBSD)
- db.pg: add include and lib paths for PostgreSQL on FreeBSD (#20582)
- thirdparty: fix `v cmd/tools/vcompress.v` on FreeBSD
- os: fix an error in Process.win_spawn_process, not using stdout pipe in a cmd environment on 32bit Windows (#20613)
- testing: retry 1 additional time sporadic silent test run failures on macos
- builder: add '-lelf' to linker flags on freebsd (fix #20481) (#20643)
- GNUmakefile: use standard default RM make variable to fix MSYS2 env on windows (#20701)
- x.vweb: add the missing include for C.sendfile to sendfile_linux.c.v
- clipboard: fix notice in clipboard_windows.c.v (#20733)
- ci: update macos runners to macos-14, to make use of the much faster m1 instances (#20747)
- builder: support musl-gcc on macos
- builtin: link to user32 to fix boehm GC compilation on Windows with clang released from the LLVM project (fix #20724) (#20767)
- pref: download correct photonwrapper.so for linux (#20783)
- ci: improve repo CI robustness, by marking dynamic_template_manager_cache_system_test.v as flaky, and only failing db_store_test.v on !windows
- tools.vpm: fix remove command on Windows, add test (#20852)
- os: don't check rdev equality on FreeBSD, inside vlib/os/os_stat_test.v (#20885)
- sync: support more gcc version specific search locations on linux with tcc


## V 0.4.4
*9 January 2024*

#### Improvements in the language
- Implement `@[aligned]` and `@[aligned:8]` attributes for structs and unions (#19915)
- Update attributes to use new syntax
- Update remaining deprecated attr syntax (#19908)
- Support `$if T is $array_dynamic {` and `$if T is $array_fixed {` in addition to `$if T is $array {` (#19882)
- Prepare for making `-W impure-v` the default (#19940)
- Assigning `0` to reference fields now requires unsafe blocks (fix #14911) (#19955)
- Unwrap const() blocks
- Implement $for comptime T.variants (#20193)
- Add `r` and `R` switches for repeating in string interpolation, `'${"abc":3r}'` == 'abcabcabc' (#20197)
- Comptime refactor and cleanup (#20196)
- Allow comptime-for to iterate over comptime variables, add `$string` comptime type, cleanup (#20233)
- Unwrap an option value automatically, inside `if o != none {` (#20275)
- Complete support for smartcasting to a comptime variable type (#20270)
- Improve comptime var checking with `is` operator and smartcasting (#20315)

#### Breaking changes
*none*

#### Checker improvements/fixes
- Disallow `$for i in struct.values` and `$for i in enum.fields` (#19845)
- Parser, checker: fix var scope in lambda(fix #19860) (#19871)
- Change the warning `more than 1000 possibilities in match range`  to a notice (#19862)
- Fix inability to use multiple `import some modname as _` in the same .v file (fix #19899) (#19900)
- Disallow casting strings to pointers outside `unsafe` (#19977)
- Disallow directly indexing sumtype and interface, when using as parameters(fix #19811) (#19982)
- Fix loop on aggregates of arrays (in match branches) of sumtypes (fix #18548) (#19988)
- Disallow indexing mut struct, passed as a fn parameter (#19992)
- Enhance err msg for unknown types for comptime `$for` (#20057)
- Fix .clone()/.move() with shared maps (#20083)
- Fix generics method call with struct short syntax args(fix #20030) (#20100)
- Fix unwrap, when generic structs are used as arguments, in uncalled methods (fix #20132) (#20135)
- Fix generic fn with generic fn call returning generic map (fix #20106) (#20150)
- Cast sumtype to its variant generic type (#20166)
- Refactor `string` to `enum` error check, handle `EnumName(string_variable)` too (#20210)
- Fix generic array method call with multi-types (#20237)
- Remove unnecessary struct ref field initialization checks and notifications at map initializing(fix #20245) (#20251)
- Add a notice, for accessing by key, map values, that contain pointers (to use unsafe or an `or {}` block) (#20266)
- Fix mismatch checking when a function returns sumtype as an argument (fix #19325) (#20264)
- Fix and cleanup uninitialized checks for array initialisers with `len:` (fix #20272) (#20279)
- Give an error for `.free()` method calls, when used on fixed arrays  (#20320)
- Fix type mismatch checking for assignments with generics (fix #20298) (#20327)
- Fix too strict checking with generics in assignment type mismatch (fix #20335) (#20346)
- Disallow `string` to `voidptr` cast entirely (#20351)
- Fix generic method calls with multi generic types (fix #20330) (#20360)

#### Parser improvements
- parser: fix parsing comments after new attribute syntax
- parser: fix failures found with fuzzing (#19873)
- parser: deprecate old attribute syntax & update remaining (missed) attributes (#19879)
- parser: fix infix expr handling with cast on left side of << operator (#19985)
- ast: fix generic structs with multiple levels of generic embedding (#20042)
- parser: implement thread returns result and multi_returns (fix #19281) (#20194)
- parser: fix formatting struct decl with comments (#20207)
- parser: fix formatting enum and interface decl with comments (#20216)
- parser: fix fn call with newline opening brace (fix #20258) (#20267)
- parser: fix parse_vet_file() with vfmt off/on flag (#20273)

#### Compiler internals
- scanner: implement support for UTF-32 escape codes in string literals (#19911)
- scanner: add new_silent_scanner/0, Scanner.prepare_for_new_text/1, make .ident_char/0, .ident_string/0 and .text_scan/0 public (#20045)
- pref: support VNORUN=1, to enable running of tests, vsh files etc (i.e. just compile them, for debugging later)
- scanner: fix backslashes followed directly by newline in string literals (fix #20291) (#20296)
- scanner: fix escape character handling in character/rune literals (fix #20301) (#20304)
- pref: disable the -macosx_version_min clang flag by default (#20297)
- builder: remove passing `-fno-strict-aliasing`, for `-prod` to gcc/icc (#20368)
- markused: add `-skip-unused` for programs that `import x.vweb` too (do not skip unused routing methods)

#### Standard library
- json: fix recursive pointer encoding (#19840)
- os,picohttpparser,sokol,strconv: prepare for making `-W impure-v` the default (#19846)
- os: add fast path to mkdir_all, when the given folder already exists (#19869)
- os: ignore empty path segments in `join_path` (#19877)
- os: fix bootstrapping for OpenBSD
- x.json2: replace deprecated type byte with u8 in the tests (#19909)
- vlib: change byte to u8 (#19930)
- sync: add a FreeBSD specific version of vlib/sync/sync_default.c.v (#19962)
- datatypes: add push_many for doubly and singly linked list + add insert_many for heap (#19975)
- datatypes: make `Direction` pub and fix and add tests for `push_many` (#19983)
- gg: fn (data voidptr, e &Event) for events, allows methods
- vlib: add a `compress.szip` module, deprecate the `szip` one after 2023-12-31 (#20003)
- os: create the folder described by `XDG_CACHE_HOME`, *if it is non empty, and it does not exist yet*, when calling `os.cache_dir()` (#20046)
- vlib: use the builtin flush functions, instead of the C. ones (#20108)
- crypto: add blake2s and blake2b hashes (#20149)
- os: fix `mv_by_cp` for directories (#20154)
- os: update mv fns, improve performance, add params struct to control overwrite behavior (#20156)
- gg: fix overlapping slices in `draw_slice_filled()` (#20182)
- json: fix option sumtype handling (#20186)
- builtin: add `@[direct_array_access]` to js string trim_right method (#20222)
- json2: add encoder support for `time.Time`  (#20228)
- json2: fix encoding of 💀🐈 etc emojis (fix #20243) (#20247)
- json2: make public the generic function `map_from/1` (#20294)
- json2: optimise encoding to be faster than cJSON with -prod (#20052)
- json2: support sumtype encoding in a more robust way (#20093)
- json2: strict module (#17927)
- crypto: fix notices/errors for `v -N test vlib/crypto`
- crypto: add blake3 hash (#20319)
- sokol: fix compiling gg and other graphical examples on OpenBSD (#20333)
- csv: Add a sequential reader too (suitable for very large .csv files, it does not read everything at once) (#20140)

#### Web
- net.mbedtls: use `char` and `usize` types for describing more precisely the C API of mbedtls (#19837)
- vweb: add the mime type for .toml files (#19875)
- net.openssl: use actual C values for the SSLError enum (#19945)
- vweb: .html('custom_template.html')
- vweb: add an optional parameter to the .redirect/2 method, to be able to set the http code for the redirects (#20082)
- x.vweb: fix large payload (#20155)
- x.vweb: reimplement csrf module (#20160)
- net: make net.fionbio and net.msg_nosignal constants public in net_windows.c.v (#20183)
- net.http: remove unused `read_set_cookies` function (#20187)
- os, net.http.file: add a folder listing to the http static file server, started by file.serve/1 (#20192)
- websocket: enable using an already existing connection (from vweb or another http server) (#20103)
- x.vweb: fix fsanitize-address test for SSE, improve documentation on the usage of `takeover_conn` (#20249)
- net.http: support `-d no_vschannel` on windows, to fix long waits, while connecting on some systems (#20265)
- x.vweb: fix `$vweb.html()` integration in cgen for the newer `x.vweb` module (fix #20204)
- net: support only ip and ip6 in net.tcp_listener (#20336)
- x.vweb.assets: reimplement assets module for x.vweb (#20280)
- x.vweb.sse: reimplement SSE module for x.vweb (#20203)
- js.dom: add querySelector[All] and NodeList (#20240)

#### ORM
- orm: fix code generation for an option time.Time field (#20031)
- orm: fix the generated SQL for the "not equal" operator (#20321)

#### Database drivers
- db.mysql: add support for the FreeBSD name of the mariadb client library (#20039)
- db.pg: fix using postgresql on windows, improve instructions for installing it (#20053)
- db.mysql: add ability to prepare and execute statements separately (#20146)
- db.pg: fix compilation error with the msvc compiler on windows, and add readme (#20326)

#### Native backend

#### C backend
- Fix generic fn returning fixed array (#19885)
- Fix arrays alias built-in methods call(fix #19896) (#19910)
- Fix generic array initialization (fix #19903) (#19916)
- Fix option sumtype auto deref (#19919)
- Ast, checker, cgen: fix interface embedded methods call(fix #16496) (#19936)
- Fix ref and deref when an interface is used as a function parameter (fix #19947) (#19966)
- Fix auto str for interface struct member which implements str method (#19970)
- Fix generics call with interface arg (fix #19976) (#20002)
- Fix lambda initialization on option struct field (fix #19474) (#19995)
- Fix live mode on windows (#20041)
- Fix the static from_string method of Enum across mods(fix #20050) (#20068)
- Fix `@[if xyz?] fn init() {}`, add tests (#20096)
- Fix assignment to the elements of an array of fixed arrays (#20133)
- Fix mutable comptimeselector usage (fix #20027) (#20134)
- Fix generics chans select (#20159)
- Fix string interp with zero characters (fix #20199) (#20200)
- Fix interface eq method with option and ref (fix #19441) (#20201)
- Fix infix expr in method of mut receiver variable (#20225)
- Fix cgen for thread wrappers, when spawning fns with with anon-fn array args and mut interfaces (fix #19425) (#20241)
- Fix fixed array return when returning fixed array initialization (#20262)
- Fix function generated code, when returning from match (#20263)
- Fix in expression with mut and ref (fix #20268) (#20271)
- Fix initialization of const string arrays on msvc (fix #20287) (#20289)
- Fix code generation when 'in array init' is used as an if condition (fix #20300) (#20302)
- Escape table names (fix #20313) (#20322)
- Add missing clear method for generic maps (#20340)
- Fix auto unwrapping option fn type (#20332)
- Fix option initialization with default struct initialization to not be `none` (#20349)
- Fix auto str for arr options with possible circular reference (#20354)
- Fix code generation when the function returns mut fixed array (fix #20366) (#20367)

#### vfmt
- vfmt: automate transition from the old `[attribute]` to the new `@[attribute]` syntax (#19912)
- vfmt: remove empty `__global()` (#20004)

#### Tools
- tools: fix already installed detection when running v install --once without args (#19838)
- compress.gzip: change the endianness for validation to conform to the gzip file specification (fix #19839) (#19849)
- tools: support `v doc -run-examples math`, to ensure that all `// Example: code` doc comments are working (#19852)
- Fix `v help` in the prebuilt v executables from the zips in the releases
- ci,tools: remove skips for the wasm backend, since binaryen is not required anymore (#19883)
- tools.vpm: support again `http` installs, when installing from an url (workaround) (#19914)
- tools.vpm: improve version detection of installed modules (#19933)
- tools: fix `v up`, by not deleting `./v` when missing a `.git` folder (#19965)
- tools.vpm: fix installing of modules with conflicting names, extend tests (#19961)
- tools.vpm: evaluate dependencies earlier to fix potential recursive install loop (#19987)
- tools.vpm: add support for ssh and hg version installations (#20125)
- tools: simplify and remove redundancies in vshader.v (#20161)
- ci: add new workflow, for doing the slower tests in vpm specifically with `-d network` (#20177)
- tools.vpm: improve detection of already parsed modules (#20223)
- scanner: change `-d debugscanner` to `-d trace_scanner` for uniformity with the other tracing options, described in CONTRIBUTING.md
- v.pref: support a `-n` option, silencing only notices (#20331)
- ci: add vsql to v_apps_and_modules_compile_ci.yml too (#20341)
- ci: fix the workflow for Vinix, using the rules in its own .yml file (#20371)
- Support -? as alias to -help (implement #20355) (#20358)
- vdoc: filter testdata and tests folders by default, reduce filesystem stats calls

#### Operating System support
- os: small cleanup in the FreeBSD branch of os.executable/0: use fixed array for the sysctl params, instead of allocating a dynamic one (#20353)
- os: improve os.executable() on OpenBSD (#20356)
- v.util.diff: support OpenBSD's default `diff` tool (#20369)
- os: fix os.open_file/3 `wb` mode creation of text files containing crlf on Windows (#20101)
- os: fix File.tell for files > 2GB on windows, by using C._telli64(f.fd) (#20072)

#### Examples
- encoding.xml: make functions public, add documentation, tests, fix attribute parsing for self-closing tags  (#19901)
- examples: show how to turn on CORS in a vweb server app
- examples: serve the wasm mandelbrot project using a v web server (#19937)
- examples: increase the resolution of the generated image in examples/wasm/mandelbrot
- docs: update null convention in ORM example, since `@[nonull]` is no longer needed (#20286)
- docs: add an example of a nullable ORM field (#20292)
- example: add a path finding algorithm visualizer using gg (#20060)
- examples: add an even smaller gg usage example, demonstrating how to always show the builtin fps counter, and how to avoid importing gx





## V 0.4.3
*11 November 2023*

#### Improvements in the language
- A new `encoding.xml` module with parser, validation, entity encoding, unit tests (#19708)
- Better couroutines support (IO, networking)
- Allocations in vweb apps reduced by 80%
- Implement `@VCURRENTHASH` to replace `C.V_CURRENT_COMMIT_HASH` (#19514)
- int => i64 on 64 bit, i32 on 32 bit (start of the transition)
- Fix new int type promotion rules and cleanup native gen() (#19535)

#### Breaking changes
- `Request.cookies` map has been deprecated. Replaced with `Request.cookie()` and `Request.add_cookie()`.
- Stricter rules for C types (they always have to be declared now)

#### Checker improvements/fixes
- Turn the option/result split warning into an error
- Turn propagation warning into an error (finishes the option/result split)
- Fix fn call with option call argument in autofree mode (#19515)
- Bring back pascal case check for aliases
- C.xx = C.yy aliases
- Allow casted integral types in match ranges (#19572)
- Warn about byte deprecation, when used as a fn parameter (#19629)
- Allow size of fixed array to be integral casts (#19663)
- Fix generic array append (#19658)
- Check error of implementing other module private interface (fix #19620) (#19688)
- Extend byte deprecation warning to array init types (#19671)
- Extend byte deprecation warnings to return types (#19668)
- Fix negative cap, len checks in array init (#19694)
- Turn warning for var and param module name duplicates into error (#19645)
- Fix closure in if guard, including with multi_return (#19765)
- Fix comptime enumdata value property access (#19768)
- Fix `field ?&Type` without default value (#19786)
- Avoid nil assign to option var (#19746)
- Allow for a shared variable to be whole reassigned (keeping the same mutex state) (fix #15649) (#19751)

#### Parser improvements
- Fix assigning static method to anon fn (#19499)
- ast: fix formatting a struct declaration with a nested struct (#19592)
- Add `set_all` + `clear_all` methods to `[flag]` enum bitfields (#19717)
- ast: reduce cost of frequently called functions by using constants (#19733)
- Warn on unused imports, even when they are declared via `import math { sin, cos }`   (#19738)
- ast: add missing docstrings for the public fns in vlib/v/ast/types.v (#19752)
- Give a friendly error when misusing if over $if (#19810)
- Add multiple struct attributes error for new attribute syntax

#### Compiler internals
- checker, builder, pref: support `-dump-defines -` to help explore all the available user and system defines for a given program (#19576)
- pref,builder: add support for `-macosx-version-min 10.2` and `-macosx-version-min 0` (with default of 10.7) (#19626)
- pref: fix unintended file extensions in default output names, allow for `v run file.c.v` (#19745)
- transformer: fix using a constant, instead of a fn parameter with the same name (fix #19766) (#19773)
- maps: add maps.merge() and maps.merge_in_place() generic utility functions (#19776)
- coroutines: only attempt to add/remove roots when GC is on.
- markused: cleanup in mark_used(), use robust index names, instead of the much more brittle integer values (#19543)

#### Standard library
- builtin: add an `unsafe { a.reset() }` method, for quickly setting all bytes in an array to 0
- math.fractions: use operator overloading and deprecate old functions (#19547)
- gg: fix the alignment of the bottom border of draw_rounded_rect_empty on macos and linux
- crypto.bcrypt: fix bcrypt failure for valid pass and hash (fix #19558) (#19569)
- sokol: update sokol to the latest version
- builtin: fix sizeof(C.BOOL) (windows specific) (#19589)
- math.big: fix incorrect division with negative numbers (fix #19585) (#19587)
- os: add a convenient way to ignore certain system signals (#19632)
- os: fix os.ls(folder) on windows, when a protected folder can not be opened (#19647)
- os: add a convenient way to ignore certain system signals (#19639)
- crypto.sha: fix calculating the same hash values when .sum() is called several times for sha1/256/512 (fix #19696) (#19697)
- crypto.md5: fix calculating the same hash values, when .sum() is called several times (#19703)
- os: add a new function `execute_opt` (#19723)
- os: add os.page_size() (#19770)
- os: implement os.fd_is_pending/1, os.Process.pipe_read/1, os.Process.is_pending/1 (#19787)
- builtin: copy min/max integer values consts from `math` to builtin so that the entire math module doesn't have to be imported(#19809)
- json2: add support for nested structs (#19579)

#### Web
- vweb: add mime type support for static .md files
- net.conv: add varinttou64 and u64tovarint functions, for the variable unsigned integer encoding, described in rfc9000 (for QUIC) (#19568)
- net.http: support passing on_running, on_stopped, on_closed callback functions to http.Server{}, as well as show_startup_message: false. (#19591)
- net: fix handling of spurious wake-up signals, lost when calling select() in mbedtls and openssl (continue on C.EINTR) (#19600)
- net: use conv.hton* consistently, instead of `$if tinyc { conv.hton16(port) } $else { u16(C.htons(port)) }`
- net.http: support passing an HTTP proxy server in http.fetch (#19606)
- net.http: add a retry mechanism to http.fetch(), when the socket inevitably errors (#19660)
- wasm: implement inline assembly (#19686)
- net.http: increase max_redirects to 16 (#19743)
- picoev: implement raw mode (#19771)
- flag,json,net: handle C calls in .v files (part of enabling `-W impure-v` as default) (#19779)
- net.http: add socks5|http(s) proxy support [Linux] (#19676)

#### ORM
- orm: add null handling and option fields (#19379)
- orm: make is_null/is_not_null unary ops; don't bind null in where (#19635)

#### Database drivers
- pg: handle C calls, move to .c.v files (#19739)

#### Native backend
- native: support `-no-builtin` (generate executables < 1KB Linux with `v -no-builtin -b native examples/hello_world.v`)
- native: use i32 instead of int

#### C backend
- Fix printing fixed array of options (#19479)
- Fix struct field of fixed array init (fix #19483) (#19487)
- Fix struct init with multi option fn type (#19491)
- Ast, native, cgen: add support for `$if native {}` (#19500)
- Fix maps with i32 keys
- Fix for c stmt with option or result calls (#19641)
- Fix infix op when handling comptime selector (#19691)
- Fix array contains method with interface(fix #19670) (#19675)
- Reduce expense in repetitively called functions by using consts (#19732)
- Fix closure parameter judgment when var cross assign inside anon fn(fix #19734) (#19736)
- Only generate free in wrapper for spawn and not go (#19780)
- Fix g.obf_table data missing(fix #19695) (#19778)
- Fix closure variable in smartcast (#19796)

#### vfmt
- Remove additional line breaks after call_expr before params struct args (#19795)
- Fix map value alignment when using keys with uft8 symbols (#19689)
- Align ternary expressions in const blocks (#19721)
- Respect range index expressions in match branches (#19684)
- Respect raw strings in `$embed_file(r'/some/path')` expressions (#19753)
- Fix formatting of struct field with default value and new attr syntax (#19683)
- Recognize or blocks in call args (#19690)

#### Tools
- all: add support for `@LOCATION`, for more convenient logging/tracing, without needing to combine `@FILE`, `@LINE` at runtime (#19488)
- benchmark: add new methods b.record_measure/1 and b.all_recorded_measures/0 (#19561)
- ci: update c2v workflow, translate doom on macOS (#19562)
- strings: add Bulder.write_decimal/1 method (write a decimal number, without additional allocations) (#19625)
- testing: add temporary file hash to prevent accidental collisions with test file binaries (#19710)
- ci: compile VTL and VSL in their own CI job, with `VFLAGS=-no-parallel`
- tools: fix windows install of an already existing module with vpm (#19761)
- tools: use `VPM_NO_INCREMENT` env var to skip dl count increment when testing vpm (#19756)
- tools.vpm: improve handling of urls that end with .git (#19758)
- tools: fix resolving external dependencies in vpm, add test (#19772)
- tools: cleanup and simplify vcreate, for upcoming fixes and features (#19794)
- tools: improve error messages, add color coding and debug logging (#19781)
- tools: fix `v build-tools`, make `v test` more robust (#19803)
- tools: add parse_query to vpm (#19814)
- ci: add macos arm64 binary release (#19823)
- Require the presence of a `v.mod` file, to install external urls via vpm (#19825)
- vcreate: fix `v init` with project names containing dashes (#19619)

#### Operating System support

#### Examples
- tests: workaround name conflict, causing false positives with msvc on windows, when both tests were executed at the same time (locked executable)


## V 0.4.2
*30 September 2023*

#### Improvements in the language
- Short lambda expressions like `a.sorted(|x,y| x > y)` (#19390)
- Support `-os plan9`, `$if plan9 {`, and `_plan9.c.v` (stage 1 for plan9) (#19389)
- fmt: simplify the processing logic for removing inline comments (#19297)
- Align the custom values of the enum fields (#19331)
- Do not warn/error for `import flag as _`
- Keep anon struct decl fields in interfaces (#19461)
- Support -N, turning all notices into errors, to ease the process of finding places that may need attention/correction

#### Breaking changes
- Remove inline comments (#19263)

#### Checker improvements/fixes
- Disallow module name duplicates in local names (#18118)
- Check enum fields with duplicate value (fix #19309) (#19310)
- Disallow alias ptr cast of a map value (#19336)
- Require `else` branch in `[flag]` enum match (#19375)
- Disallow assigning pointer values to option struct fields (#19380)
- Fix generic comparison for conditional assignment (#19401)
- Allow using ! and ~ on aliased bool and integral types (#19403)
- Warn -> error for uninitialized ref fields
- Parser, checker: allow lambdas anywhere anonymous functions are expected (#19436)
- Allow for `each(a, |x| println(x))`, i.e. using lambda expressions, when expecting `fn (x int)`
- Check fixed array init with default expression (#19472)
- Allow for `const source = $embed_file(@FILE).to_string()`
- Fix C.type in imported modules

#### Parser improvements
- parser: fix fixed array with eval const size (#19269)
- parser: disallow using `sql` as name (#19298)
- parser: fix `;` support for `module x;`
- parser: fix fixed array of option values (`_ := [10]?int{}`) (#19392)
- parser: fix assigning with in another module sumtypes 2 (#19415)
- Support `;` statements, allowing for oneliners like `./v -e 'import os; println( os.ls(os.args[1])!.sorted(a > b) )' vlib/math` (#19345)
- v.ast: improve Stmt.str(), showing more details about ast.Block, ast.DeferStmt, ast.ForInStmt, ast.GlobalDecl

#### Compiler internals
- pref: support `-fast-math`, passing either -ffast-math or /fp:fast (for msvc) to the C backend, and `$if fast_math {` to detect it at comptime
- parser, transformer: fix transformer.infix_expr() and cleanup parse_types.v (related #19269) (#19276)
- pref,builder: support -use-os-system-to-run to workaround segfaults using not fully updated xcode command line tools
- v.builder: fix compiling code, that imports modules from both `src/modules` and `modules` (#19437)
- os, v.builder: show more details, when a program ran by `v run file.v`, exits by a signal (fix #19412) (#19471)

#### Standard library
- math: speedup the pure V math.pow implementation for non-fractional powers (#19270)
- math: add more C. fn overrides, for the default C backend (speed up examples/path_tracing.v) (#19271)
- time: add `MMM` support for parse_format() (#19284)
- os: include sys/sysctl.h on FreeBSD to avoid implicit definition of sysctl function (#19293)
- crypto.md5: change the Digest.write return type, from `?int` to `!int` (#19311)
- v.help: use os.executable() instead of `@VEXE` as an anchor, so `v help` will work more robustly.
- toml: fix custom `to_toml` for complex structs (#19338)
- vlib: add net.http.file, allowing for `v -e "import net.http.file; file.serve()"` (#19348)
- vlib: remove functions and fields, deprecated before 2023-03-20
- toml: fix toml encoding of complex types (#19408)
- arrays: add a partition function, that splits a given array, based on a criteria, passed as a callback fn (#19417)
- toml: add decoding for struct fields of type map[string]T (#19447)
- arrays: add arrays.each, arrays.each_indexed, and tests for them
- encoding.html: implement `unescape()` (#19267)

#### Web
- net.http: fix http.fetch(), without explicit method (default again to .get, not to .acl)
- net.http: default explicitly to Method.get for http.Request and http.FetchConfig too
- examples: add examples/fetch_ip.v, showing how to contact http://ifconfig.co/json and parse the result
- net.http: fix post error with https on windows (#19334)
- net.ssl: implement SSLConn.peer_addr() (#19333)

#### ORM
- orm: add `references` attribute to allow foreign key declarations on fields (#19349)
- orm: support different foreign key types, not just an integer id (#19337)
- orm: add support for V enum struct fields (#19374)
- orm: quote table and field name in `[references]` (#19387)

#### Database drivers
- db.pg: allow postgres connection using service definitions (#19288)

#### Native backend
- native: make native tests pass on windows; refactor PE file generation (#19140)
- native: parse dll files to resolve extern symbols (#19433)

#### C backend
- Fix printing struct with thread field (#19320)
- Fix the logic around the definition of VNORETURN and VUNREACHABLE (less warnings on FreeBSD) (#19316)
- Add support for `-d trace_cgen_stmt`, document it in CONTRIBUTING.md
- Fix printing smartcast interface variable (fix #18886) (#19372)
- Fix interface with multiple embedded fields (#19377)
- Fix channel of interface (fix #19382) (#19383)
- Fix fixed array of option type default (#19397)
- Fix interface with option field (#19434)
- Fix promoting an alias typed value, to a sumtype of the alias's base type (fix #19407) (#19423)
- Remove the special plan9 support code, treat it instead as a posix system in cheaders.v (#19445)
- Fix printing slice of fn call string (#19450)
- Fix `type VType = &C.CType` (#19452)
- Fix array of fns index call with direct_array_access mode (#19460)

#### Tools
- bench: a new bench/ directory for language benchmarks
- ci: test the pure V math versions without .c.v overrides on the CI too (#19292)
- github: add automatically info about voting to all new issues (#19351)
- tools: add -E flag to `v test-parser`, that will show the partial source that caused the parser to fail with `-no-builtin -check-syntax file.v`
- ci: bring back gitly
- github: improve the voting message for issues (#19448)
- vcreate: update templates, add `lib` (#19444)

#### Operating System support
- builtin: use `libgc-threaded` on FreeBSD, to get the threaded version of libgc (#19294)

#### Examples
- examples: add more .obj files for 06_obj_viewer (#19406)

## V 0.4.1
*3 September 2023*

#### Improvements in the language
- Pure `array.sorted()` and `array.sorted_with_compare()` methods, that do not modify their receivers (#19251)
- UB overflow has been removed
- Implement `Enum.from_string(name string)` for converting strings to enum values (#19156)
- Disallow casting string to enum, suggest using Enum.from_string() instead (#19260)
- Use autocasting in complex conditions (#18839)
- Allow alias as fixed array on return (#18817)
- Do not allow uninitialized function pointers
- Fix compiling V programs with latest clang 16 on windows (clang 16 is stricter than clang 14) (#19095)
- Fix anonymous struct with default expr (#19257)
- Allow using consts as enum values (#19193)
- `@[attr]` syntax to replace `[attr]` (`[]` is used for too many things). Most likely to be replaced with `@attr()` in the future.
- Allow `none` for not first values on map initialization (#18821)
- Make all .trace() methods generic on the type of the passed expression

#### Breaking changes
- `arr[1..4]` now requires `unsafe` if the slice can modify the original immutable array.

#### Checker improvements/fixes
- Disallow assigning `nil` to struct fields (#18725)
- Use autocasting in complex if conditions (#18753)
- Disallow invalid prefix on left side of assign stmt (#18750)
- Allow no return in compile_error else block (#18758)
- Fix interface param resolution (#18780)
- Add an error for `$tmpl` function type mismatches (#18826)
- Disallow voidptr cast to struct (#18845)
- Fix type checker on auto deref var (#18842)
- Check generic sumtype declaration (fix #18741) (#18865)
- Fix closure with inherited sumtype variable (#18894)
- "v -line-info" for a quick run to fetch info about objects on one line
- Make sure vweb actions return vweb.Result
- Do not allow modifying immutable vars via arrays with refs
- Support `@STRUCT` in static methods
- Fix generic struct field init recursively (related #19014) (#19025)
- Fix struct field fntype value call (#19067)
- Explicitly disallow creating type aliases of `none`, i.e. `type Abc = none` (#19078)
- Fix assigning an array slice (fix #19120) (#19137)
- Fix assigning array slice in struct init (#19150)
- Check enum static from_string arguments errors (#19163)
- Disallow taking the address of consts with int literal values (#19160)
- Check struct embed with wrong position (#19245)
- Optimize out needless string interpolations from the most common case in `Checker.expr_or_block_err`
- Check error for or_expr inside infix expression (#19213)
- Disallow `thread` as var name (#19174)
- Check error for sumtype in array (#19183)
- Disallow an empty `chan` type (#19167)

#### Parser improvements
- Change warn to error, for const names with upper letter (fix #18838) (#18840)
- Disallow declaring static functions as method receivers (#19007)
- Disallow having builtin type as type names for `enum`, `sum type` and `alias` (#19043)
- Support `const x := 123`, to make extracting locals as constants less annoying while prototyping
- Fix struct field fn type with default value (fix #19099) (#19106)
- Fix `for i++; i<10; i++ {` (fix #18445) (#19035)
- Fix fn return alias of fixed array (#19116)
- Fix generic struct init (Stack[&Person]{}) (fix #19119) (#19122)

#### Compiler internals
- pref: make -autofree work without -gc none
- builder,pref: allow thirdparty objects compilation with CPP compiler (#19124)
- scanner: fix string interpolation with nested string interpolation in inner quotes p. 3 (#19121)
- scanner: error early on an unsupported escape sequence in a string, like `\_` (fix #19131) (#19134)
- v.token: add inline next_to() and cleanup related calls (#19226)

#### Standard library
- eventbus: add generic support for event name (#18805)
- readline: add support for ctlr+w and ctrl+u shortcuts (#18921)
- strconv.atoi: fix string.int() returning numbers for non number characters (fix #18875) (#18925)
- builtin: reduce the number of array allocations for consts in all V programs (#18889)
- builtin: move array.data to the top of the struct
- os.notify: implement the kqueue backend for notify.FdNotifier (#19057)
- vlib: add a new module `builtin.wchar`, to ease dealing with C APIs that accept `wchar_t*` (#18794)
- arrays: add more util functions and tests for them - find_first, find_last, join_to_string (#18784)
- vlib: use sync.new_mutex() consistently for initialising all vlib structures containing mutex fields
- crypto.pem: add a static method `Block.new`, to replace `new` (#18846)
- crypto.pem: add decode_only and general improvements to decoding (#18908)
- log: improve the most common use case, it's no longer necessary to create a `Log` instance (#19242)
- crypto.sha512: make the new384/0, new512_256/0, new512_224/0 functions public
- json: fix option alias support (#18801)
- time: fix `parse_format` with `YY` (#18887)
- math.big: allow bitwise ops on negative signum (#18912)
- math.big: make is_odd public and add test cases (#18916)
- math.big: add checked division methods (#18924)
- math.big: add `isqrt_checked` and standardize error format (#18939)
- sokol: use GLCORE33 on linux
- os,term.termios: add termios.set_state/2, state.disable_echo/0, use them in os.input_password, to fix `v -os wasm32_emscripten examples/2048/`
- gg: implement Android specific APK asset loading for the `create_image` function (#19015)
- sync: make sync.Direction public (#19047)
- time: store time with nanosecond resolution in time.Time, deprecate Time.microsecond, add utility methods and tests (#19062)
- time: add a format_rfc3339_nano() method to time.Time
- time: add 'i', 'ii' in custom_format() for 12-hours clock(0-12-1-11) (#19083)
- gg: expand the `-d show_fps` background, so fps>100 will not overflow it
- Math.big: restore gdc_euclid, use it for smaller numbers, fix bench_euclid.v .
- Add new generic `arrays.uniq, arrays.uniq_only, arrays.uniq_only_repeated, arrays.uniq_all_repeated, arrays.distinct`
- builtin: add support for `-d builtin_writeln_should_write_at_once` and `-d builtin_write_buf_to_fd_should_use_c_write` (#19243)
- builtin: always show the assert message, if it was defined in the source, in non test programs too (fix #19240)
- time: check if a day is a valid day of its month (#19232)
- toml: Add generic automatic decoding and encoding of simple structs, when they don't implement custom methods (#17970)

#### Web
- http: Request.host
- net.ftp: fix dir() for file names, which contain spaces (fix #18800) (#18804)
- net.http: make listener public, and add addr in Server struct (#18871)
- net.http.chunked: return `!string` on decode (#18928)
- net.conv: rename functions to match other langs, making them easier t… (#18937)
- wasm: remove dependency on thirdparty/binaryen, webassembly backend rewrite (#18120)
- wasm: add a -wasm-stack-top flag to compiler (#19157)
- net.mbedtls: add SSLListener to allow creating SSL servers (#19022)
- picoev, picohttparser: reimplement in V (#18506)
- vweb: fix parsing of form fields, send with multipart/form-data (by JS fetch)
- vweb: make vweb route paths case sensitive (#18973)
- net.mbedtls: have shutdown close accepted connections too (#19164)
- http: add support for stream connections, and custom .on_redirect, .on_progress, .on_finish callbacks to http.fetch() (#19184)
- vweb: add a user_agent utility method to the vweb context (#19204)
- vweb: avoid the controllers having to be defined in specific order (#19182)

#### ORM
- orm: fix inserting sequential values (id=0), in tables with an i64 primary field (#18791)
- Add OR in where on update and delete (#19172)

#### Database drivers
- vlib: remove deprecated `pg`, `mysql`, `sqlite`, `mssql` modules. Leave only the `db.` prefixed `db.pg`, `db.mysql` etc
- db.mysql: add the exec family of methods (#19132)
- db.sqlite: add exec_param_many and exec_param methods (#19071)
- db.sqlite: make functions return results, breaking change (#19093)

#### Native backend
- native: move functions out of amd64.v (#18857)

#### C backend
- Fix selector code to use interface method table on closure when needed (#18736)
- Fix nested or expr call (fix #18803) (#18807)
- Ensure that `<<` and `>>` has higher precedence in the generated C code, than arithmetic operations (diff between C and V precedences) (#18814)
- Fix cross assign with aliased array (#18830)
- Fix generated code for returning generic result/option to comptime var (#18834)
- Fix option map with fn type value (#18849)
- Fix returning an option tuple - `fn f() ?(int,int) { return g() }` (#18851)
- Fix printing multiple fixed array (fix #18866) (#18879)
- Fix infix expr with number overflow (fix #18905) (#18936)
- Remove \r for consistency (#18962)
- Allow dump(unsafe{nil}) and dump(voidptr(123)) in the same program
- Implement fixed array of threads wait() (#19032)
- Fix an error with ptr interpolation (fix #19048) (#19049)
- Fix spawn call fn struct field(fix #18862) (#19096)
- Fix bootstrapping on older macOS Catalina
- Fix alias of array method call(fix #19125) (#19129)
- Simplifications and clean up.
- Fix mixed fixed array and array initializing (#19246)
- Fix array sort with fn call parameter (fix #19220) (#19221)
- Fix generic struct with option fn field (#19218)
- Fix comptime assign with generic result return type (#19192)
- Fix match with comptime if expr in branch (#19189)

#### Tools
- ci: add v-analyzer builds (#18835)
- ci: cleanup more the contents of the generated v_linux.zip, v_macos.zip, and v_windows.zip, use -skip-unused
- tools: fix vcomplete for zsh (#18950)
- tools: support a toc for projects, with single exposing module, in `v doc` (#19001)
- Add support for `v should-compile-all -c examples/`, which will delete all the produced executables at the end
- vgret: add install commands for ubuntu and arch to doc string (#19247)
- fast.v: add favicon to the html produced by fast.v
- vpm: implement multithreading (#19208)
- Make performance_compare.v more robust and easier to use, by allowing `v run cmd/tools/performance_compare.v` too
- Improve oldv windows support, make it use -municode for windows builds, make it support cmd.exe
- Make repeated runs of `oldv SAME_COMMIT -c "./v file.v"`, not use the network at all
- Help: add link to the TESTS.md at the bottom of `v help test`, run CI checks on help markdown files as well
- v.builder: show the number of files, types, modules, when a program is compiled with -stats
- Improve the output of parser_speed.v and scanner_speed.v


## V 0.4
*1 July 2023*

This release has a combined changelog from 0.3.1 to 0.3.5.

You can read it here:

https://github.com/vlang/v/blob/master/changelogs0.x/0.4.md

## V 0.3.5
*29 June 2023*

#### Improvements in the language
- **Coroutines with a scheduler**. Only Linux/macOS for now, requires `-use-coroutines` and
  `coroutines.sleep()` instead of `time.sleep()`. They work with IO and net, but not with GC
  for now.
- `spawn` now spawns system threads, `go` spawns coroutines.
- Static type methods: `Foo.new()` to replace factory functions like `new_foo()`.
- Smartcasting with complex conditions:`if sum_type is Foo && !sum_type.is_info && get_name(sum_type.info.name) == 'foo' `.
- Functions can now return fixed size arrays.
- Enum values now can have attributes.
- Generic functions as function parameters are now supported: `fn f[T](x T, i int, f_ Fn[T]) T { `.
- Anonymous structs can no longer have attributes.

#### Breaking changes
- `byte` deprecated in favor of `u8` (`byte` is automatically converted to `u8` by vfmt).

#### Checker improvements/fixes
- Disallow `Result` type aliases (`type Foo = !Bar`) and `Result` in maps (`map[key]!Type`).
- Add a missing check for taking address of literal value member.
- Fixed map initialization for maps with option values.
- Allow `a << none`, where `a` is `[]?&int`.
- Disallow multiple `else` branches in a match.
- Fix index expression with sumtype of array types.
- Remove hardcoded check for function calls for `C.stat`, `C.sigaction`, etc.
- Fix multiple embedded external module interface.
- Fix missing check for diff type on map value declaration.
- Simplify error handling in the checker (#18507).
- Option alias fixes.
- Fix alias to struct ptr on struct init.
- Sumtypes can no longer hold references.
- Fix a bug checking generic closures.
- A hard to reach limit of 1 million iterations for resolving all generics.
- Fix missing check for unwrapped shift operation.
- Fix enum max value validation.
- Add a missing mutability check for `array.delete` calls.
- Disallow `_ = <- quit`.
- Disallow type matching with primitive vars.
- Warning instead of error for unnecessary brackets in if/match.
- Include import aliases when checking for import duplicates.
- Fix inferring generic array type in nested call.
- Allow casted `enum val` and `const` as fixed array size.
- Disallow multiple return values in const declarations.
- Fix contains() with array of interfaces.
- Disallow mut for blank idents.

#### Standard library
- json: Enum value string serialization supports `[json:'alias']` to change its string values.
- Struct fields can now be skipped during JSON/ORM serialization via `[json:'-']` and `[sql:'-']`,
  in addition to `[skip]`. This allows having custom behavior for different serialization methods.
- builtin: heap usage API (gc_memory_use() and gc_heap_usage())
- math.big: refactoring, gcd fixes/improvements, overflow fixes, `mod_inverse`.
- flag: fix finalize with multiple shortargs.
- runtime: add new functions total_memory/0 and free_memory/0.
- time: small cleanup of parse_iso8601 comments, make the C.strftime declaration forwards compatible
- stbi: allow customization of number of channels in `stbi.load()`.
- stbi: add a `resize_uint8` function for resizing images in memory.
- time, x.json2: improve iso8601 time decoding.
- builtin: zero out internal map/array pointers on m.free(), to reduce the work for the GC
  mark phase for non escaping maps/arrays, used in hot loops.
- time: add more detailed error descriptions, add custom format parsing with time.parse_format.
- sync: add Mutex.destroy and RwMutex.destroy methods.
- datatypes: add Bloom filter.
- rand: add missing rand.u16(), update doc comments, add test.
- builtin: speed up string methods with vmemcpy instead of `for` loop for copying data.

#### Web
- The builtin websocket library is now thread safe.
- Enhanced builtin csrf protection in vweb.
- vweb: ability to set and get values on vweb.Context.
- vweb: support for host specific static files.
- vweb: host option added to controller, and a new host attribute.
- vweb: middleware docs improved; same with docs for `[vweb_global]` and `shared`.
- vweb: return 404 on file not found.
- net.http: copy IANA's list of methods to the http.Method enum.
- net.conv: use a pure V implementation instead of C.hton etc.
- net.html: `get_tag()` methods to find first tag occurrence.
- net.html: fixed text parsing for inline tags.
- net.html: fix parsing of nested quoted strings in code tags.
- picoev: FreeBSD support.

#### ORM
- Fixed a foreign key bug that could result in an extra insert.
- Comptime bug with `[skip]` and `[sql:'-']` fixed.
- Checker error for unsupported field types.
- Allow structs without the id field, more flexible primary keys.
- Improved docs and examples.
- Uninitialized structs are no longer inserted into related tables.

#### Database drivers
- mysql: TIMESTAMP support.
- mysql: allocate memory for each string and blob dynamically depending on its value length.
- mysql: add the ability to commit transactions.

#### Native backend
- Refactoring, splitting large files into multiple.

#### C backend
- Fix code generation for generic unions.
- Fix `[N]chan` (fixed arrays of channels).
- Fix nested fixed array instantiation.
- Fix fixed array of map.
- Fix stringification of usize struct fields (before, they were treated as 32 bit *signed* numbers).

#### Comptime
- A new `$res` comptime function to get returned value in defer block (#18382).
- Fix comptimeselector option propagation.
- A mutability check for comptime assignments.
- Fix comptime assigning to sumtype or indexexpr.
- Make comptime calls work with or-block.

#### Tools
- A new VPM site: vpm.vlang.io. A better design, discoverability of packages, descriptions, most downloaded packages etc.
- vpm: installation of mixed modules.
- `v ls --install -p D:\path\vls.exe` to install a local vls executable.
- vdoc: highlight comments with gray color.
- vet: allow vetting files with global variables.
- Make util.launch_tool/3 more robust, by recompiling V tools always in a known current working folder.



## V 0.3.4

*30 Apr 2023*

#### Breaking Changes

The following changes may break compilation of existing code or change behavior at runtime:

- `json`: enums are serialized as strings by default, `[json_as_number]` attribute can be used for
  the old behavior.

  If you are serializing enums to JSON in your application, then you will need to add the
  `[json_as_number]` attribute to keep the old behavior!

- Variable shadowing has been completely banned (previously variable names could conflict with
  module names).

#### Web

- vweb now supports live page reloading.
  The web app is instantly updated in the browser (no need to refresh the page)
  every time a **.v** or a **.html** file is changed.
- vweb is now significantly faster and more stable under load, due to a new multithreaded worker
  pool, which is much more efficient at spreading the workload among all threads equally.
- vweb now supports middleware.
- vweb now supports controllers.
  It's now possible to have multiple app structs to better separate logic.
- vweb now supports overridable `.not_found()` method for custom 404 pages in vweb.
- vweb now uses database pool.
- Fixed multipart form parsing in vweb.

#### Backends

- A new pure WASM backend, based on binaryen, a WASM `builtin` module, and a pure V WASM serialization library.
- Lots of fixes and new features in the native backend, including making codegen logic platform independent.
- Now code generated by the С backend, can be compiled by a C++20 compiler.
- C backend does not generate unused interface functions now.

#### Compiler CLI

- `v share file.v` for sharing code via the playground.
- `v up` speed up for when it hasn't been run for a long time (**vc/** bootstrapping has been
  optimized).
- `v init` no longer overwrites existing `src/main.v`.
- `v self` now uses a faster TCC backend on macOS (Intel/Apple Silicon), just like on Windows/Linux.
- A new command line flag `-e` for running short V programs on command line: `v -e "println(2+5)"` (
  works just like in Perl).
- A new `-ldflags` option, in addition to `-cflags`. Works just like LDFLAGS in C.

#### ORM

- All ORM queries now return `![]` (`Result` of an array).
  This allows handling/propagating DB errors and simplifies working with ORM (one way).
- Many ORM improvements: type checks for `limit/offset/order by/where`; support of reference objects
  in `insert`; struct fields can be used with `limit/offset`; `Connection` interface.
- ORM now supports the `like` operator:
  ```v
  users := sql db {
      select from User where name like 'Bob%'
  }
  ```
- A new `-d trace_orm` option to see all SQL queries generated and used by V ORM and
  `-d trace_pg_error` to trace PG errors.

#### Standard Library

- Added new `termios` module.
- `net.ssl`: types using ssl contexts can now be converted to strings via `.str()`/printed
  via `println()`.
- `v.reflection`: added type symbol info metadata.
- `crypto` and `math` modules have been updated to use `Result` instead of `Option`.
- `datatypes.LinkedList[map]` now works correctly.
- `urllib.Values.get()` now returns an Option.
- `strconv`: `v_printf()` was made private, `v_sprintf()` was deprecated. String interpolation
  should be used instead.
- `net.http`: mime types have been updated to include all official types.
- `gg`: `create_image()` now returns `!Image` instead of `Image`, allowing to handle errors.
- `sokol`: errors during image creation no longer result in a panic, but can be handled by the
  programmer.
- `sokol`: macOS apps can now be quit using **Cmd+Q**.
- `os.hostname()` and `os.loginname()` now return `Result`.
- `strconv.atoi` optimizations.
- `println()` now supports arrays with recursive references.
- `termux`: support for cross-compilation from termux to other platforms.
- `readline` module now works much better on macOS: key navigation, history, etc (now on par with
  Linux).
- `os`: fixed a memleak in `getline()`.
- `os.Process` now has a `create_no_window` option (Windows only).
- `os.Process` now has a `set_work_folder()` method to set the initial working folder of the new
  child process.

#### Option as a first class type

Final steps in making the Option type a first class type:

- If guards now work with struct fields which are `Option` functions.
  Such fields can now also be assigned to other fields/variables.
- Option receivers can no longer have methods.
- `none` can now be cast to all `Option` types, including aliases.
- Option references are now supported: `?&Type`.
- Arrays of `Option`s are now allowed.
- Allow `foo := Foo{}`, when `Foo` has an Option field, that is a struct, that has a `[required]`
  tag on its fields.

#### Compile-time Reflection

- Compile-time interface fields evaluation.
- Compile-time enum evaluation:
  ```v
  $for item in MyEnum.fields {
      println(item.value)
      println(item.name)
  }
  ```
- Added `$option` as a compile-time reflection type representing an any Option type.
- All special compile-time reflection types are now lowercase (`$int`, `$enum`, `$option`, etc).

#### Checker Improvements/Fixes

- Enums can no longer be initialized like structs.
- Capture variables can no longer shadow anonymous function params.
- Mixing multi-return results with other types in return statements is no longer allowed.
- Assigning anonymous structs to named structs is no longer allowed.
- `[required]` fields are now checked for embedded structs.
- Fixed a bug with closures with fixed array variables.
- Builtin methods `first/last/repeat` can now be used in custom user types (previously they only
  worked in builtin arrays).
- Generic struct initialization no longer needs explicit types to be provided:
  ```v
  struct Foo[T, U] {
  	a T
  	b U
  }

  foo := Foo{
  	a: 2
  	b: 'x'
  }

  println(foo)
  ```
- unsafe: dereferencing nil references is no longer allowed in the following case:
  ```v
  a := unsafe { nil }
  println(*a)
  ```

#### OSes

- Added basic QNX support.

#### Other changes

- Lots of documentation/readme improvements.
- Lots of playground improvements: [play.vlang.io](https://play.vlang.io), including a really cool
  feature: "Show generated C code".
- A new `[spawn_stack: 131072]` function attribute for controlling the max size of the stack of the
  spawned threads.
- Channel pop now works with an `or` block: `ch := <-self.item or { return none }`
- `it` has been renamed to `index` in array inits.
- "Is V still fast?" web-page has been sped up by splitting the result table into multiple years.

#### Development

- GitHub Copilot summaries in PRs.

## V 0.3.3
*30 Jan 2023*
#### Improvements in the language
- String interpolation simplified to just '${name}', enforced by vfmt, and updated in the entire code base.
- `go foo()` has been replaced with `spawn foo()` (launches an OS thread, `go` will be used for
  upcoming coroutines instead).
- vfmt now supports `// vfmt off` and `// vfmt on` for turning off the formatting locally for short snippets of code.
  Useful for keeping your carefully arranged matrices intact.
- Match branch range expressions with consts: `match x { const1...const2 {} }`
- Hot code reloading via `[live]` is now supported in imported modules, not just the main module.
- Syntax sugar for map inits without needing explicit casts for interfaces: `all.children := { "abc": rect, "def": ui.rectangle()}`.
- `$embed_file()` fixes, including variable args support.
- `none` fixes: no longer allowed to be used as a separate type, `dump()` support, not allowed inside `unsafe`.
- Const functions: `const y = term.yellow`, then `println(y('abc'))`.
- Builtin type names can no longer be used as identifiers.
- Generic `typeof[T]()`, `sizeof[T]()`, `isreftype[T]()` functions.
- Deprecated `-error-limit` in favour of the documented `-message-limit` option.
- Maps now support aliased keys.
- Operator overloading now works with reference types.
- Generic struct inits with nested generic structs and generic optional types are now allowed.
- During array creation, `len:` is required when using default values for the array.
- Optimized one byte `[]u8` arrays creation.
- Recursive aliasing is no longer allowed (e.g. `type Alias = map[string]Alias`).

#### Breaking changes
- `[]` is now used for generics instead of `<>`.
- Accessing a pointer map value requires an `or {}` block outside `unsafe`.

#### Checker improvements/fixes
- Lots of fixes in the type checker.
- Int signedness mismatch is now checked: `cannot use literal signed integer as u8`.

#### Standard library
- `math.vec` module for generic vector math including 2D, 3D, and 4D vector operations.
- Builtin stb_image.h used by gg has been updated to the latest v2.28.
- All of vlib has been updated to use separate Option/Result types.
- To avoid confusion, all references in the code and documentation to `Optional` have been replaced with `Option`.
- `gg.Context` pipeline has more effects, including the `additive` effect.
- Much cleaner eof checks in `os`: refactor `err == IError(os.Eof{})` to `err is os.Eof`.
- Lots of work on `x.json2`, the pure V json encoder, soon to become official.
- New `v.reflection` module for runtime reflection.
- Improved `os.mv()`, which now works consistently even across different windows drives/mount points.
- `string.trim_indent()`, useful with multi line strings, that start/end with new lines and indentation.
- Reduced memory consumption in the `crypto` modules.
- Official V UI library is now licensed under MIT.
- Deprecated `math.util` and `math.mathutil` have been removed.
- New time format support: `time.format_rfc3339()`.
- `encoding.html.escape()`.
- All public functions in the `hash` and `encoding.base32` modules have been documented.
- New `crypto.pem` module.
- New `map.reserve()` method.

#### Web
- Improved vweb stability under load.

#### ORM
- Various ORM fixes and improvements, including string interpolation support, type checks, fn calls in `where`.

#### Database drivers
- VFS support in the builtin `sqlite` module; `sqlite.get_affected_rows_count()`.
- Improved `pg` compatibility with older PostgreSQL versions before 2014.
- `sqlite`, `pg`, `mysql` have been moved to `db.sqlite`, `db.pg`, `db.mysql`.

#### Native backend
- Operator support for floats, multi return.

#### Comptime
- Improved compile time checks, like `$if x is Type {`; `$if T in [$Array, $Struct, $Alias, $Function] {`.
- `$for in` works with alias types.
- New comptime features for fields: `field.is_<field>`, `field.is_alias`, `field.is_enum`.

#### OS support
- Installation instructions for using V on NixOS.
- Better `make` support for OpenBSD.
- Much improved experience for `v install pcre` on Windows (it now bundles its own .c files, so it compiles cleanly, even if the platform does not have another pcre package installed).
- V can now be compiled with tcc on latest macOS and Apple Silicon.

#### Tools
- fast.vlang.io fixes & improvements, new server.
- New official IntelliJ plugin: https://intellij-v.github.io.
- Lots of new language documentation, a nicer table of contents.
- Improved documentation for most of the vlib modules
- `make.bat` & `v up` improvements on Windows.
- TeamCity test runner support via `v -test-runner teamcity foo_test.v`.
- CI optimizations for faster runs.
- New official AdventOfCode repo with AOC solutions, also added to CI.
- More detailed timings in `v -show-timings`.
- `v new <name> web` for quickly scaffolding new web projects.


## V 0.3.2
*31 Oct 2022*

#### Improvements in the language
- New simplified string interpolation: `println("Hello, {name}!")`. It will be the only way, old syntax (`${name}` and `$name`)
  will be deprecated.
- Easier custom error creation: `return MyCustomErr{}` instead of `return IError(MyCustomErr)`.
- All floats outputs now have `.0` conditionally appended to them to improve clarity.
- Custom integer enum types: `enum Xyz as u64 {`.
- AST transformer fixes and optimizations.
- Stylistic improvements and bug fixes in vfmt.
- Casting integers to enums now requires `unsafe{}`.
- Improved error and warning messages.
- Parallel compilation now uses `sync.Pool`.
- `-skip-unused` fixes, soon to be made the default.

#### Breaking changes
*No breaking changes*

#### Checker improvements/fixes
- Improved type checker: lots of new type checks and fixed checker bugs.
- Unused last expression in `if` is now checked.
- Anonymous structs visibility issues fixed.

#### Standard library
- `net.ssl` has been migrated from a dynamically linked OpenSSL to a statically linked Mbed TLS. This means that V binaries will no
  longer have an OpenSSL dependency. OpenSSL can still be enabled via `-d use_openssl`.
- msgpack module for decoding/encoding msgpack. (`v install msgpack`)
- Most of vlib has been updated to use the new Option/Result types.
- net, net.http, vweb bugs and fixes.
- QuadTree and RingBuffer types in `datatypes`.
- Forward iterator for `datatypes.LinkedList<T>`, forward and backward iterators for `datatypes.DoublyLinkedList<T>`.
- A new `maps` module, similar to existing `arrays`. It has generic `filter`, `flatten`, `invert`, `to_map`, `to_array`, `from_array`
  functions.
- `utf8.is_number()`, `utf8.is_space()` functions.
- New `encoding.base32` module.
- `gg.TouchPoint` to differentiate between different types of touch input.
- `str.int()` conversion speedup (without -prod).

#### Web
- `vweb.csrf` module.

#### ORM
- Support parenthesized expressions like `select from User where (name == 'Sam' && is_customer == true) || id == 1`.

#### Native backend
- Lots of native backend improvements, including library calls, comptime conditionals, enums, method definitions/calls, structs.

#### V interpreter
- Some further interpreter work.

#### C backend
- cgen cleanups.

#### OS support
- Removed the need for the `[console]` attribute in Windows GUI apps.
- More precise WINAPI declarations for easier integration on Windows.
- More CI tests on FreeBSD.

#### Tools
- New stunning playground with an improved look and feel, a much better and more responsive editor,
  code sharing by link, more convenient keyboard control, reusability for potential embedding:
  https://play.vlang.io.
- Improved call tracing via `-trace-calls`.
- Lots of documentation improvements, including a better documentation of the recent Option/Result split.
- V REPL: Home/End keys support. Lots of clean up.




## V 0.3.1
*31 Aug 2022*

#### Improvements in the language
- Anonymous structs.
- Lots of bug fixes: 90% of all bugs ever submitted are closed.
- New keyword/type: `nil`. Only to be used inside `unsafe`. Replaces `voidptr(0)`.
- V can now find code in the `src/` directory. This allows making V repos much cleaner.
- Support `assert condition, extra_message`, where the `extra_message` will be evaluated and shown if the assertion fails.
- Operator overloading now works with aliases and generics.
- Scanner optimizations.
- Using C's #define is no longer allowed in normal V code, only in `.c.v` files.

#### Breaking changes
- Anonymous sumtypes have been removed (deprecated for now) due to complicating the language and the compiler too much.

#### Checker improvements/fixes
- More type checks.
- Lots of fixes in `shared` types.

#### Standard library
- `os.mkdir()` now has an optional `mode` parameter.
- `encoding.csv` is now generic, supports bools, accepts a custom delimiter, and is compatible with io.Reader/io.Writer.
- `datatypes` module now uses operator overloading.
- All `datatypes` types can be converted to V arrays.
- `smtp` improvements including multiple recipients and base64/utf8 support.
- `arrays.carray_to_varray<T>()` for converting C arrays to V arrays.
- `strconv.v_sprintf()` has been deprecated in favor of string interpolation.
- TOML module now supports `[toml:...]` attributes, just like the JSON module.
- `os.walk()` is no longer recursive (still works the same).
- `io` has been migrated to `Result`.
- Third party window control in Sokol.
- `string.replace_char()`, `math.round_sig()`.
- Improved multiplication performance in `math.big`.

#### Web
- `net.urllib` ipv6 support.
- `net.Http.Response.text` renamed to `body`.
- `net.websocket` timeout is now configurable.

#### ORM
- ORM functions now return `Result`, so the errors can be handled.

#### Database drivers

#### Native backend
- Major improvements to the fast native backend including linking support on Linux. The goal is to be able to self host V soon.

#### V interpreter
- V interpreter improvements.

#### C backend
- Parallelized cc step. Speeds up -prod and clang/gcc compilation by 300-500% (depending on
  the number of cores). Experimental and hidden behind a -parallel-cc flag, soon to be the default.
- Intel C compiler support.
- Go backend fixes.
- `#preinclude` for low level C interop.

#### OS support
- Full termux support via `$if termux {`, more predictable logging on Android.
- Older macOS support (<10.12).
- Windows code has been removed from `v.c` distributed on non-Windows systems. (`v_windows.c` is used on Windows.)

#### Tools
- DOOM is now translated/compiled and launched on CI servers. A screenshot of the running game
  is made via `vgret` and is compared to the expected result.
- VLS performance improvements, especially on Windows.
- `v ls` tool for installing, for updating, and for launching VLS (V Language Server).
- `v doc` now has syntax highlighting.




## V 0.3
*30 Jun 2022*
- C to V translation via C2V: `v translate file.c`. (Demo video: [Translating DOOM from C to V, building it in under a second and running it!](https://www.youtube.com/watch?v=6oXrz3oRoEg))
- Lots of bug fixes in V, cgen, and C interop to allow running translated DOOM.v.
- Programs built with the V compiler no longer leak memory by default.
- Closures. All operating systems are supported. ([Demo](https://x.com/v_language/status/1528710491882852352))
- `Option` and `Result` are now separate types: `?Foo` and `!Foo` respectively. Old code will continue working for 1 year and will result in a warning/hint.
- Hundreds of new checks in the type checker.
- All V's backends have been split up into separate processes.  As the result, building V got 26% faster.
- Maps and arrays can now return options: `m[bad_key] or { ... }`, `if x := arr[key] { ... }`.
- `ustring` has been replaced with `[]rune` (works just like in Go).
- Maps can now have non-string keys.
- A new compiler pass for transforming the AST (doesn't slow the compiler too much, adds about 25ms to `v self`). It eliminates unreachable branches and performs other simple optimizations and transformations.
- C backend is now parallel (just the cgen part for now).
- Lots of compiler source code clean up and minor optimizations. The compiler got ~30% faster according to fast.vlang.io.
- Better compiler source code organization (absolutely necessary as it's surpassed 100k loc).
- The naming of V's integer types is now more consistent: `byte` has been renamed to `u8`.  Old code will continue working for 1 year and will result in a warning/hint.
- The typo detector now highlights the suggested name so that it's more visible.
- `datatypes` module now has `Heap, Queue, Stack, BSTree, LinkedList`.
- Interfaces can now be embedded (like structs).
- vlib now has a TOML parser, fully compatible with TOML 1.0.
- Lots of work done on the V.js backend, including the graphics library, which has been ported to V.js.
- JS promises, await (V.js).
- It's now possible to do more complex array initialization by using each individual element of the array (`[]int{init: it}`).
- Unsigned right shift operators `>>>` and `>>>=` have been added to V. (They work exactly like in Java.)
- `-nofloat` option, which is useful for writing kernels and for embedded systems without an FPU (used in Vinix).
- Generic interfaces.
- TCC is now bundled with the language, this allows building V programs without an external C compiler dependency.
- Null can be used in `unsafe` only (for example, for C interop).
- Pointer arithmetic and comparing pointers to numbers is now also only allowed in `unsafe`.
- Inline sumtypes.
- New module `compress.gzip`.
- Lots of `net`/`net.http`/`vweb` fixes (also used for the upcoming Gitly launch).
- IPv6 support.
- `net.http` headers are now enum fields instead of strings. This allows to avoid typos and offers autocomplete.
- Struct field deprecation.
- Static GC (no longer a dynamic lib dependency).
- New various algorithms for random number generation: MT19937RNG, etc  (module `rand`).
- Fix immutability bugs that allowed to bypass compiler immutability checks and modify const/immutable values.
- Lots of fixes in the JSON serializer.
- Heap allocated only structs marked with `[heap]`.
- Significantly improve lots of error messages, make them more clear, suggest hints.
- Bug fixes and new features in the pure V `regex` module.
- Lots of new drawing functions in the graphics module (like `gg.draw_polygon_filled(), gg.draw_arc_empty()` etc)
- Builtin FPS display in `gg`.
- Latest Sokol backend in `gg`.
- Advanced CI tests for the graphics module. Graphical apps are run on GitHub Actions instances, their output is saved to an image, uploaded, and compared to the expected result.
- More bug fixes in generics.
- Bug fixes in aliases. They can now fully replace the types they alias.
- `[minify]` struct attribute for struct minification.
- `for in` now works with fixed arrays.
- The parser was made a bit faster by skipping `vfmt` code when not in `vfmt` mode (by using `-d vfmt`).
- Lots of vfmt improvements, especially with comments.
- Experimental `#[index]` syntax for negative indexing (like in Python, but needs special syntax instead of being used by default).
- Visibility bug fixes in modules (`pub`).
- Error propagation in complex expressions (e.g. `foo(bar()?)`).
- Optionals can now by used in consts (`const x := opt() or {}`).
- Lots of new documentation, including vlib modules documentation and the official V Documentation.
- vpm improvements (including a new vpm mirror).
- `sync` improvements including `sync.thread_id()`, `sync.Once`..
- V can now be used to generate object files (`foo.o`) that can be used in existing C projects.
- `-usecache` and `-skip-unused` fixes, they are close to being on by default.
- Lots of Windows issues fixed.
- Amazon Linux support.
- Fixes in shared maps and arrays.
- `term.ui` improvements, including multi byte/UTF-8 events.
- New `crypto` modules, including `crypto.des, crypto.cipher, crypto.blowfish`.
- Comptime fixes.
- 4 byte bool option (`-d 4bytebool`) for compatibility with some C software.
- `strconv` (pure V formatting module used in string interpolation) fixes and performance improvements.
- ORM fixes (pg, mysql, sqlite). Tables are now created automatically based on the V structs, no more need in sql files to create tables for apps.
- `volatile` keyword.
- `"stringliteral".len` optimization (replaced by the actual number by the new `transform` pass).
- Lots of inline assembler improvements (it's used a lot in Vinix).
- Many new functions in the `math` module.
- Separators in number literals: `1_000_000`.
- `strings.Builder` optimizations and new methods.
- Autofree fixes (still not production ready, hidden behind the `-autofree` flag).
- Lots of Android fixes in V and in vab.
- Lots of commits to the native backend (amd64/arm64).
- V interpreter fixes. (Still at an early stage.)
- Go2V translator has been started by the community, and can already translate simple programs.
- An early version of the Go backend (`v -b go -o file.go file.v`).

## V 0.2.4
*30 Aug 2021*
- Introduce `isize` and `usize` types, deprecate `size_t` in favor of `usize`.
- Add `datatypes` and `datatypes.fsm` modules.
- Add `compile_error` and `compile_warn` comptime functions.
- Bare metal support. Vinix OS kernel is now being developed in V.
- Builtin web framework vweb is now multithreaded, all CPU cores are used.
- String interpolation and struct stringers are now implemented in pure V
with a much cleaner and faster implementation. Previously libc's `sprintf`
was used.
- Improved `unused variable` warning. Assigning to a variable no longer marks it as used.
*... lots of missing changelog for this version, sorry (will update a bit later)*

## V 0.2.2 - 0.2.3
*22 Jan 2021*
- Allow interfaces to define fields, not just methods.
- `vweb` now uses struct embedding: `app.vweb.text('hello') => app.text('hello')`.
- Consts can now be declared outside of `const()` blocks: `const x = 0`.
- Overloading of  `>`, `<`, `!=`, `==`, `<=` and `>=` operators.
- New struct updating syntax: `User{ ...u, name: 'new' }` to replace `{ u | name: 'new' }`.
- `byte.str()` has been fixed and works like all other numbers. `byte.ascii_str()` has been added.
- Smart cast in for loops: `for mut x is string {}`.
- `[noinit]` struct attribute to disallow direct struct initialization with `Foo{}`.
- Array decompose: `[1, 2, 3]...` is now `...[1, 2, 3]`
- Treating `enum` as `int` and operations on `enum` except `==` and `!=` are removed for strict type checking.
- Support `[manualfree] fn f1(){}` and `[manualfree] module m1`, for functions doing their own memory management.
- Allow usage of `<` and `>` operators for struct in `.sort` method for arrays, i.e. `arr.sort(a < b)`.
- Auto generate assignment operators like `+=`, `-=`, `*=`, `/=` and `%=` if the operators are defined.
- Colorize and improve failing tests output.
- Fix `go` with a generic function: `go test<string>(c, 'abcd')`.
- Add comptime `x := $embed_file('v.png') println(x.len) println(ptr_str(x.data()))`, for embedding files into binaries.
- Advanced vdoc search on mobile layout.
- string's `left()`/`right` were removed in favor of slicing syntax: `str[..pos]`.
- gg: native graphics mode on macOS/iOS (using Cocoa Drawing API).
- Full path to consts must be specified everywhere. This makes it easy to distinguish them
from local variables.
- `__offsetof` for low level needs (works like `offsetof` in C).
- vfmt now preserves empty lines, like gofmt.
- Support for compile time environment variables via `$env('ENV_VAR')`.
- Allow method declaration of `==` and `<` operators and auto generate `!=`, `>`, `<=` and `>=`.
- support `dump(expr)`, i.e. tracing of both the location, name and value of an expression
- deprecate os.exec in favour of os.executable() which does *NOT* return an option, when the command was not found

## V 0.2.1
*30 Dec 2020*
- Hashmap bootstrapping fixes.
- Array decomposition to varargs: `fn sum(i ...int) int` => `a := [2,3,4] println(sum(a...))`
- HTML module docs generated by vdoc now have global search.

## V 0.2
*22 Dec 2020*
- Compile-time memory management via `-autofree` (not production ready yet). [Video demonstration](https://www.youtube.com/watch?v=gmB8ea8uLsM).
- Channels and locks.
- Thread safe typed arrays via keyword `shared`.
- Struct embedding.
- IO streams (`io.Reader`, `io.Writer` etc).
- A powerful websocket module that conforms to RFC 6455 and passes the Autobahn test suite (498 client tests and 249 server tests).
- The `net` module is now non blocking and is more feature complete providing similar API to Go.
- V's graphics module now uses Metal/DirectX/OpenGL instead of just OpenGL.
- V can now run in the browser via WASM and execute V code by translating it to JavaScript:
https://v-wasm.now.sh
- V binaries for Linux/Windows/macOS are now built and deployed automatically via GitHub Actions.
- Smart casting for sumtypes and interfaces, including complex expressions: `if x.expr is int { println(x.expr + 1) }`.
- Clean and easy way to sort arrays: `users.sort(a.name > b.name)`.
- A huge amount of `vfmt` fixes and improvements. It has now reached a point where it can be safely used on any V source file.
- A new CI job that runs `v fmt -verify` on the entire code base, a new command that makes sure the file/directory
has been vfmt'ed. This ensures that all code submitted to the V project is formatted.
- A new tool `v vet` for analyzing the project and finding potential bugs and errors.
- A new `term.ui` module for building dynamic terminal UIs with an example editor written in it.
- Early iOS and Android support.
- All missing ORM features from the old backend were brought back.
- Magic `it` variable has been replaced with smart casts (the change is completely handled by vfmt).
- Cross-compiling to Windows and Linux brought back.
- C2V can now generate wrappers. Example: https://github.com/medvednikov/libsodium. (C2V will be released by 0.3)
- C++ compiler support: code, generated by the C backend can now by compiled by C++ compilers.
- Short generics syntax: `foo(5)` instead of `foo<int>(5)`.
- Cached modules via `-usecache`. Faster compilation due to not needing to rebuild the entire vlib for
each program. Will be enabled by default in 0.2.1.
- New improved sum types implementation.
- Lots of errors that happen often during the development cycle were turned into warnings to increase
  development speed. They are still errors in production builds.
- Labeled `break` and `continue`.
- Lots of documentation. The official language documentation grew 3 times in size.
- `modules.vlang.io` is now generated automatically on every commit.
- Builtin compile-time JSON serializer now supports `time.Time`.
- Fixes in type aliases, to make them behave just like the types they alias.
- `array.contains(element)` is now generic.
- Lots of improvements in the JS backend and its type system.
- Simpler and more constinent function arg syntax: `foo(a int, b int, c string)` instead of `foo(a, b int, c string)`
- Lots of fixes and optimizations in the hashmap.
- Lots of missing checks in the type checker were added (for example, checking the correct usage of public struct fields).
- Mutability bug fixes
- Taking the address of a map value is no longer allowed, like in Go.
- Matrix multiplication.
- A new `#pkgconfig` flag to provide platform independent way to get compilation flags for C libraries/packages.
- Explicit parentheses requirement in complex boolean expressions.
- `println` was made even smarter, and can now handle complex types.
- Precompiled text templates can now be used outside of vweb via `$tmpl()`.
- Gitly, a big web application written in vweb has been released: https://github.com/vlang/gitly
- `['/:arg1/:arg2/action']` vweb action attribute for easily getting query parameters assigned to method arguments.
- Improved performance of text rendering, `gg.text_width()`.
- Webview module in V UI.
- Binary enum flags.
- `[export]` attribute to change exported function name (for example for calling from a C library).
- `unsafe` fixes and improvements.
- Improvements to rand: `rand.ulid()`, `rand.uuid()`, a unified customizable PRNG API.
- Hundreds of other fixes, features, and tests (from now on the changelog will be updated
right away as the feature/bug fix lands).


## V 0.1.27
*5 May 2020*

- vfmt has been re-written from scratch using the new AST parser.
    It's much faster, cleaner, and can format
files with compilation errors.
- `strconv`, `sprintf`, and `printf` in native V, without any libc calls.
- Interfaces are now a lot more stable and have all expected features.
- Lots of x64 backend improvements: function calls, if expressions, for loops, local variables.
- `map()` and `filter()` methods can now be chained.
- New `[]int{cap:cap, len:len}` syntax for initializing array length and capacity.
- New `is` keyword for checking the type of sum types and interfaces.
- `as` can now be used to cast interfaces and sum types.
- Profiling with `-profile`. Prints a nice table with details about every single function call:
    number of calls, average time per call, total time per function
- `import(xxx)` syntax has been removed in favor of `import xxx` for simplicity and greppability.
- Lots of fixes and improvements in the type checker.
- `time.StopWatch`
- `dl` module for dynamic loading.
- Automatic `str()` method generation for every single type, including all arrays.
- Short struct initialization syntax for imitating named function args: `foo(bar:0, baz:1)`.
- New operator `!in`.
- Performance improvements in critical parts of the builtin data structures (array, map).
- High order functions improvements (functions can now be returned etc).
- Anonymous functions that can be defined inside other functions.
- Built-in JSON module is back.
- Lots and lots of new tests added, including output tests that test error messages.
- Multiple errors are now printed, the compiler no longer stops after the first error.
- The new JS backend using the AST parser (almost complete).
- Variadic functions.
- `net.websocket` module (early stage).
- `vlib` is now memory leak free, lots of `autofree` improvements.
- Simplified and cleaned up `cmd/v`, `v.builder`.
- V UI was updated to work with the new backend.


## V 0.1.25
*1 Apr 2020*

- The entire compiler has been re-written with an AST parser.
    The code is now a lot cleaner and more maintainable.
    ~15k lines of old compiler code were removed.

## V 0.1.24
*31 Dec 2019*

- A new parser/generator built on top of an AST that simplifies code greatly
    and allows to implement new backends much faster.
- Sum types (`type Expr = IfExpr | MatchExpr | IntegerLiteral`).
- B-tree map (sped up the V compiler by ~10%).
- `v fmt -w`.
- The entire code base has been formatted with vfmt.
- Generic structs.
- SDL module.
- Arrays of pointers.
- os: `is_link()`, `is_dir()`, `exists()`.
- Ranging through fixed size arrays.
- Lots of fixes in ORM and vweb.
- The first tutorial: [building a simple web application with vweb](https://github.com/vlang/v/blob/master/tutorials/building_a_simple_web_blog_with_vweb/README.md)
- Match expressions now must be exhaustive.
- freestanding: `malloc()`/`free()`.
- `++` is now required instead of `+= 1` for consistency.
- Interpolated strings now allow function calls: `println('val = $get_val()')`.
- `string.replace_each([])` for an efficient replacement of multiple values.
- More utf8 helper functions.
- `-prealloc` option for block allocations.
- `type` aliases.
- Running `v` with an unknown command will result in an error.
- `atof` implementation in pure V.
- Enums can now have negative values.
- New `filepath` module.
- `math.factorial`.
- `ftp` module.
- New syntax for casting: `val as Type`.
- Fewer libc functions used (soon V will have no dependency on libc).


## V 0.1.23
*30 Nov 2019*

- [Direct x64 machine code generation](https://github.com/vlang/v/issues/2849).
    Hello world being built in 3 milliseconds.
- Bare metal support via the `-freestanding` flag, to build programs without linking to libc.
- Prebuilt V packages for Linux, macOS, and Windows.
- `string.index()` now returns `?int` instead of `int/-1`.
- Lots of fixes in Generics.
- vweb framework for developing web applications is back.
- Vorum, the forum/blogging software written in vweb, can now be compiled and has been added to CI.
- REPL, `v up` have been split up into separate applications to keep the core V compiler small.
- V now enforces short enum syntax (`.green` instead of `Color.green`) when it's enough.
- V UI for macOS.
- Interfaces have been rewritten. `[]interface` support.
- `os.cp()` for copying files and directories.
- Additional compile-time flags: `$if clang, msvc, mingw, x32, x64, big_endian, little_endian {`.
- All C functions now have to be declared, all missing C functions have been defined.
- Global variables (only with the `-enable-globals` flag)
    for low level applications like kernels and drivers.
- Nothing can be cast to bool (previously code like `if bool(1) {` worked).
- `<<` and `>>` now work with all integer types.
- V detects Cygwin and shows an error (V supports Windows natively).
- Improved type checking of some operators (`%, |, &` etc).
- Windows 7 support.
- `println(true)` now prints `true` instead of `1`.
- `os.exec()` now uses `CreateProcess` on Windows.
- fast.vlang.io website for monitoring the performance of V after every commit.
- On Windows Visual Studio is now used automatically if GCC is not installed.
- vfmt!
- Lots of cleaning up in the compiler code.
- Multi-level pointers in unsafe code (`****int`).
- MSVC backtrace.
- `$if os {` blocks are now skipped on a different OS.
- C string literals (`c'hello'`).
- AlpineLinux/musl fixes + added to CI.
- Inline assembly.
- Clipboard module (Windows, macOS, X).
- `foo()?` syntax for error propagation.
- Docs have been migrated from HTML to `doc/docs.md`.
- `eventbus` module.
- Haiku OS support.
- `malloc/free` on bare metal.
- `utf8` helper functions (`to_lower()`, `to_upper()`, etc).
- Optimization of `for c in str {`.
- `string/array.left/right/slice/substr` were removed (use `[a..b]` slicing syntax instead).



## V 0.1.22
*28 Oct 2019*

- Generic functions (`fn foo<T>(bar T) T {`) with varargs support.
- `array[start..end]` and `string[start..end]` slicing syntax.
- Optimized `array.filter()` and `array.map()`.
- `sqlite` module.
- Cached modules for faster compilation.
- Dramatic compilation optimizations: [V now compiles itself in 0.10 - 0.30 seconds](https://github.com/vlang/v/wiki/The-V-language-now-compiles-itself-in-0.09-seconds)
- V scripts (simpler and cross-platform alternative to Bash).
- Infinite multi-dimensional arrays (`[][][]int`).
- `unsafe`.
- `[deprecated]` attribute.
- `[if]` function attributes for compile time function exclusion for performance.
- `switch` has been completely removed from the language and replaced by
`match` everywhere.
- `pub struct` and `pub const`, previously all structs and consts were public
by default.
- `musl` support (V can now run on, for example, Alpine Linux).
- Module header generation. V now supports closed source modules, which are still
used in some industries.
- Constants were added to typo suggestions.
- `color in [.green, .red, .blue]` now works without specifying `Color.green`.
- V compiler is now a module that can be used by other programs.
- Backtraces now have source lines on Linux.
- `runtime.nr_cpus()`.
- `fn init()` for module initialization.
- `a in [1, 2, 3]` optimization: no array gets allocated.
- Raw strings: `s := r'hello\nworld'`.
- `if a := func() { }` syntax for handling options.
- f32/f64 comparison now uses machine epsilon by default.


## V 0.1.21
*30 Sep 2019*

- `none` keyword for options.
- Solaris support.
- All table lookup functions now use `none`.
- varargs: `fn foo(bar int, params ...string) {`.
- Double quotes (`"`) can now also be used to denote strings.
- GitHub Actions CI in addition to Travis.
- `-compress` option. The V binary built with `-compress` is only ~90 KB!
- More memory management.
- Unused modules result in an error.
- "Unused variable/module" errors are now warnings in non-production builds.
- Duplicate methods with the same name can no longer be defined.
- Struct names must be capitalized, variable/function names must use snake_case.
- Error messages are now even nicer!
- Lots of fixes in automatic `.str()` method generation for structs and arrays.
- ~30% faster parser (files are no longer parsed separately for each pass).
- `_` is no longer a variable, but an actual syntax construct to skip unused values, like in Go.
- Multiple returns (`fn foo() (int, string) {`).
- `!` can now only be used with booleans.


## V 0.1.20
*17 Sep 2019*

- JavaScript backend!
- Hundreds of C warnings were fixed. `gcc v.c` now builds without
any warnings.
- The mutability check now applies to function args (mutable
receivers that are not modified result in a compilation error).
- V tests now show how long each test took.
- Official Android support (only console applications via Termux for now).
- Typo check. If a variable/function/module etc is misspelled,
V will suggest the correct name.
- Lots of Microsoft C fixes, and a separate Travis instance for
this backend.
- Bitwise operators `|`, `^`, `&` no longer work with booleans.


## V 0.1.19
*12 Sep 2019*

- Lots of refactoring, simplifications, and optimizations in the compiler.
- Experimental memory management at compilation (only for the V compiler itself for now).
- Lots of ORM fixes.
- Functions can now be inlined via the `[inline]` attribute.
- New `mysql` module.
- Better error format that is supported by all major editors (go to error).
- Error messages now point to the actual place where the error happened.
- Custom json field names: `struct User { last_name string [json:lastName] }`.
- Raw json fields via the `[raw]` attribute.
- All C code was removed from the `freetype` module.
- `gg` module can now render all Unicode characters.
- `[typedef]` attribute for imported C struct typedefs.
- Support of Objective C interfaces (primarily for using Cocoa).
- REPL: clear command and custom functions.
- REPL tests (which are also used for testing certain compiler errors).
- Syntax bug fixed: `foo[0] += 10` is now possible.
- http: support plain HTTP protocol and follow redirects.
- http: header data is now processed correctly.
- net: basic UDP support.
- `import const` was removed from the language.
- `array.contains()` was removed from the language (`in` should be used instead).
- `[0; len]` syntax was removed (replaced with a simpler `[0].repeat(len)`)
- Primitive aliases were removed to simplify the language.
- GitHub supports V now!
- Backtraces are now printed on panics.
- A new awesome `readline` module.
- V.c is now regenerated automatically after every commit.
- A bug with struct ordering was fixed, now structs can be declared in any order.
- V modules can now be built with `v build module`.
- `@FILE, @LINE, @FN, @COLUMN` for debugging.


## V 0.1.18
*16 Aug 2019*

- Built-in ORM (`uk_customers = db.select from Customer where country == 'uk' && nr_orders > 0`).
- Map initialization syntax: `m := { ‘foo’: ‘bar’, ‘baz’: ‘foo’ }`.
- `map.delete(key)`.
- `libcurl` dependency was removed from the `http` module.
- All function arguments are now immutable by default (previously they could be
  modified inside the function).
- `http` functions now return options.
- `sync.WaitGroup`.
- `vweb` static files serving.
- `crypto.rand` module.
- `v up` to update V.
- SChannel support on Windows.
- `net.urllib` module.
- vpm package manager, `v install`.
- `()` are now required in complex bool expressions: `(a && b) || c` instead of `a && b || c`.
- All arrays now have a default `.str()` method.
- Bootstrapping V with MSVC.
- Experimental `≠` etc support.
- `encoding.csv` module.
- `$if debug {` for running code in debug mode only.
- Map struct fields are now initialized automatically, just like arrays.
- Maps now support array values.
- `json` functions can no longer be used if the `json` module is not imported.


## V 0.1.17
*29 Jul 2019*
- `vweb` module for developing web apps in V.
- vtalk, open source V forum software.
- Generics (very limited right now, but they will be gradually improved).
- Comptime codegen (`foo.$method()` where `method` is a string).
- `@` for escaping keywords (e.g. `struct Foo { @type string }`).
- Windows Unicode fixes (V can now work with non-ASCII paths etc on Windows).
- Fix mutable args bugs + don't allow primitive arguments to be modified.
- Declaring a mutable variable and never modifying it results in a compilation error.
- Interactive debugging support.
- `sync` module for Windows.
- `#!` support on Unix systems (V scripts).
- Lots of Visual Studio fixes.
- `crypto.aes` and `crypto.rc4` modules.
- Internal modules.


## V 0.1.16
*23 Jul 2019*
- V can now be used with Visual Studio!
- Hot code reloading now works with graphical applications (e.g. graph.v, bounce.v).
- Compile time memory management for arrays.
- High order functions.
- `match` expression (replacing `switch`).
- Import cycle detection.
- `crypto/md5`, `crypto/sha256`, and `crypro/sha512` modules.
- `os.executable()` - a cross platform function that returns full path to current executable.
- `~/.vlang` and `VROOT` were removed entirely. The installation is a lot cleaner now.
- V can now be packaged for all Linux distros.
- Arch Linux package.
- `string(bytes_buffer, len)`, `string(bytes_array)` casts.
- Multiple `defer`s.
- `key in map` syntax (replacing `map.exists(key)`).


## V 0.1.15
*15 Jul 2019*
- FreeBSD, OpenBSD, NetBSD, DragonFly support.
- Hot reloading now works with graphical applications: [bounce.v](examples/hot_reload/bounce.v)
- VROOT was removed, the installation process is now much simpler.
- `defer` statement.
- map.v was re-written. It's now much faster.
- `for key, val in map` syntax.
- `flag` module for parsing command line arguments.
- `zip` module.
- `crypto/sha1` module.
- Submodules and module aliases (`import encoding.base64 as b64`).


## V 0.1.14
*12 Jul 2019*
- `gg` module Windows support, V Tetris runs on Windows.
- Compile `glad` and `cJSON` only once. Programs using `gg` or `json` compile a bit faster.
- `v.c` has been cleaned up and minimized (~16k => ~10k lines of code).
- `type` aliases can now have methods.
- Const overflow check during compilation (`byte(1000)` will no longer compile).


## V 0.1.13
*10 Jul 2019*
- New enum syntax (`token == .name`), enum values are no longer global consts.
- Submodules (`import encoding.base64`).
- Hot code reloading.
- Special `err` variable for getting error values.
- Complex numbers.
- `<<` can now append arrays (`numbers << [1, 2, 3]`).
- Lots of Windows fixes (Windows still needs some work).
- Lots of REPL improvements (e.g. `>> 2 + 3` works now, no `println` required).
- The website was made easily translatable, it's now partially available in several languages.


## V 0.1.12
*4 Jul 2019*
- V can finally compile itself on Windows (https://github.com/vlang/v#mingw-w64).
- `os` module now uses options in all functions that return `File`.
- Lots of bugs with options were fixed.
- `println` was optimized. It no longer results in allocations.
    Now it also works correctly with all integer types.
- Lots of `vfmt` fixes, it will be enabled tomorrow.
- New `strings` module.
- Lots of other fixes and improvements, thanks to all the contributors.


## V 0.1.11
*1 Jul 2019*
- Cross compilation for Windows!
- Lots of Windows fixes.
- socket.v.
- maps fixed.


## V 0.1.9 - 0.1.10
*29 Jun 2019*
- Windows support via MinGW-w64. Pre-built Windows binary.
- File structure has been simplified: all vlib modules were moved to the vlib/ directory,
  makefile was moved to the root.
- One single archive with pre-built binaries for all operating systems.
- `mut var := val` was fixed (previously `mut var = val` was allowed as well).


## V 0.1.8
*28 Jun 2019*
- Single file programs without `fn main` now work as expected.
- REPL has been fixed: it now supports imports, consts, function definitions, etc.


## V 0.1.7
*27 Jun 2019*
- All C code in the compiler and vlib has been replaced with V.
- `#` syntax for embedding C code has been removed.
- Exported functions now need to be marked with `pub`, all public vlib functions have been updated.
- CI has been set up (Travis + Azure). On every commit and PR it is made sure that V
  can compile itself, all tests pass, and all examples compile.
- More tests have been uploaded.
- Cleaner bytes to string conversion: `tos2(bytes)` => `string(bytes)`.
- The home page has 3 more examples next to 'hello world' that show the features of the language.
- Lots of bugs and issues fixed.
