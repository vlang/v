# fast — the "Is V still fast?" dashboard

The engine behind [fast.vlang.io](https://fast.vlang.io). It measures how long the
V compiler takes to compile itself (and a couple of tiny programs) for a given
commit, stores every measurement in a **local SQLite database via V's ORM**, and
serves a small **[veb](https://modules.vlang.io/veb.html)** web app that
visualises the history.

```
cmd/tools/fast/
  fast.v      main dispatcher, shared config, `seed`
  models.v    Benchmark ORM model + SQLite helpers + view/diff logic
  bench.v     measurement engine + `bench`
  runner.v    "every Nth commit of a year" sampler + `run`
  server.v    the veb app (index page, JSON API, health)
  templates/
    index.html   the dashboard UI (chart + table)
  fast.db     SQLite database (git-ignored, created on first run)
```

## Commands

Run from inside `cmd/tools/fast/`:

```sh
v run . serve [-port 8080]          # start the web app (default command)
v run . bench [-clang] [-noprod]    # benchmark the current HEAD commit
v run . run [-year 2026] [-step 50] [-branch <ref>] [-dry-run]
                                    # benchmark every <step>th commit of a year
v run . seed                        # insert demo rows, to preview the UI
v run . help
```

Or compile once and reuse the binary:

```sh
v -o fast .
./fast serve
```

## Running the 2026 sampler locally

The request that this tool was built for: benchmark **every 50th commit of 2026**
on the local machine.

```sh
./fast run                 # == ./fast run -year 2026 -step 50
```

For 2026 that samples ~56 commits out of the ~2770 on the repo's default branch
(resolved from `origin/HEAD`, falling back to `master`/`main`/`HEAD`; override with
`-branch <ref>`). Preview the exact set first without building anything:

```sh
./fast run -dry-run
```

For each sampled commit the runner:

1. builds V exactly as it was at that commit with the **`oldv`** tool
   (`cmd/tools/oldv.v`) — it finds the matching `vc` bootstrap commit, prepares
   tcc and self-builds a working `./v` under `~/.cache/oldv/v_at_<commit>/`, so
   your main checkout is never touched;
2. builds an optimized `vprod` in that historic checkout;
3. runs the measurements and stores one `Benchmark` row in `fast.db`.

The first run also clones `vlang/v` and `vlang/vc` into `~/.cache/oldv/`; historic
builds are cached there, so re-running only benchmarks commits that are missing.

It is **idempotent** — commits already present in the database are skipped, so you
can stop and resume. Building + benchmarking ~56 historical commits is heavy
(a full bootstrap build plus 20 samples per measurement each), so expect it to run
for a while; the log is written to `fast.log`.

### Keeping it running

To keep the local dashboard fresh, run the sampler on a schedule and keep the web
app up, e.g. with cron:

```cron
# Update the checkout, rebuild the tool, and sample any new commits every hour.
# `fast run` enumerates commits from THIS checkout (it resolves e.g. origin/master),
# so the checkout MUST be updated here (git pull) — the oldv cache sync only
# refreshes ~/.cache/oldv. Without the pull the dashboard stops advancing.
0 * * * * cd /path/to/v && git pull --ff-only && cd cmd/tools/fast && v -o fast . && ./fast run >> fast.log 2>&1
```

and start the server once (e.g. in a `tmux`/`screen` session or a user service):

```sh
cd /path/to/v/cmd/tools/fast && ./fast serve -port 8080
```

## What is measured

| Column      | Command                          | Meaning                          |
|-------------|----------------------------------|----------------------------------|
| `v -o v.c`  | `vprod -o v.c cmd/v`             | self-compile to C                |
| `v -o v`    | `vprod -o v cmd/v`               | self-compile to a binary         |
| `v hello.v` | `vprod examples/hello_world.v`   | compile a tiny program           |
| `v.c size`  | –                                | size of the generated `v.c`      |
| scan/parse/check/cgen | `-show-timings`        | per-stage compiler times (min)   |
| V lines     | `-stats`                         | number of V source lines         |
| lines/s     | derived                          | V lines per second (`v -o v.c`)  |

Wall-clock timings take `max_samples` measurements after a couple of warmups and
discard the slowest ones to cut noise (see the constants in `fast.v`).

## Database

Everything lives in `fast.db` (SQLite). The schema is defined by the `Benchmark`
struct in `models.v` and created automatically. Inspect it directly if you like:

```sh
sqlite3 fast.db 'select commit_hash, commit_date, v_c_ms, v_self_ms from benchmarks order by commit_date;'
```
