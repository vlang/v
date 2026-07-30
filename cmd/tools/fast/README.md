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
v run . run [-year 2026] [-step 50] [-latest N] [-branch <ref>] [-dry-run]
                                    # benchmark every <step>th commit of a year,
                                    # or the N most recent commits with -latest
v run . remeasure                   # re-measure every stored commit (backfill new
                                    # metrics, e.g. RSS, onto existing rows)
v run . export [-o <dir>]           # render the static site (index.html + json)
v run . seed                        # insert demo rows, to preview the UI
v run . import [--since YYYY-MM-DD] [--ref <ref>] <table.html> [...]
                                    # migrate old fast.vlang.io history into fast.db
v run . help
```

Or compile once and reuse the binary:

```sh
v -o fast .
./fast serve
```

## Migrating history from the old static site

The previous tool accumulated its history in `table.html` (mirrored into the
gh-pages `index.html`). Before making `fast.db` authoritative, import that history
so the dashboard does not start empty:

```sh
# from a checkout of the old gh-pages output (github.com/vlang/website)
./fast import path/to/table.html            # or the old index.html
./fast import 2024.html 2023.html 2022.html # older per-year archives too

# only import rows from a given date onward:
./fast import --since 2026-01-01 index.html
```

`import` parses the old 14-column rows (timestamp, commit, message, v.c, v, …) and
inserts them. It is idempotent — commits already in the database are skipped — so
you can re-run it or point it at several files.

Each database tracks a single git history; imported rows are tagged with it and
mixing branches is rejected. By default the history is the repository's default
branch (what the old fast.vlang.io tracked). Pass **`--ref <ref>`** if you are
migrating a different history (e.g. `--ref origin/v3`) so later `run`/`bench`
commands on that branch are accepted and unrelated branches are rejected.

**Backfill the RSS charts.** The old data has no memory numbers, so imported rows
have zeroes for every RSS field and the "RSS: self-compile"/"RSS: hello.v" charts
would be empty. After importing (and whenever you add a new metric to an existing
deployment), run `remeasure` to re-measure the stored commits and populate them:

```sh
./fast remeasure   # reuses the cached oldv builds; safe to stop/resume
```

## Running the 2026 sampler locally

The request that this tool was built for: benchmark **every 50th commit of 2026**
on the local machine.

```sh
./fast run                 # == ./fast run -year 2026 -step 50
```

For 2026 that samples ~56 commits out of the ~2770 on the repo's default branch
(resolved from `origin/HEAD`, falling back to a local `master`/`main`). Each database
tracks one git history, so in a detached or shallow checkout where none of those
resolve you must pass an explicit **`-branch <ref>`** — an unresolved `HEAD` is
rejected, not tracked. Preview the exact set first without building anything:

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

**Local dynamic dashboard.** Sample on a schedule and keep the veb server up:

```cron
# Update the checkout, rebuild the tool, and sample any new commits every hour.
# `fast run` enumerates commits from THIS checkout (it resolves e.g. origin/master),
# so the checkout MUST be updated here (git pull) — the oldv cache sync only
# refreshes ~/.cache/oldv. Without the pull the dashboard stops advancing.
0 * * * * cd /path/to/v && git pull --ff-only \
  && cd cmd/tools/fast && v -o fast . && ./fast run >> fast.log 2>&1
```

```sh
cd /path/to/v/cmd/tools/fast && ./fast serve -port 8080   # in tmux/screen or a user service
```

**Publishing fast.vlang.io.** The public site is served statically from GitHub
Pages, so `fast run` alone (which only writes `fast.db`) does **not** update it —
you must also `export` the static site and push it. Point `$SITE` at a checkout of
the generated-site branch (`github.com/vlang/website`, branch `gh-pages`). The
`fast_pages.yml` workflow in `vlang/v` fetches that branch and deploys it from the
self-hosted macOS runner. It also runs hourly as a fallback:

```cron
# hourly: sample new commits, regenerate the static site, and publish it.
# The `{ diff || commit; }` group keeps `||` bound to the diff check only (skip the
# commit when nothing changed); the whole chain is `&&`-guarded and wrapped in a
# `{ ...; }` group so ANY earlier failure (pull/build/run/export) aborts before the
# commit/push and is captured in the log — nothing stale is ever published.
0 * * * * { cd /path/to/v && git pull --ff-only && cd cmd/tools/fast && v -o fast . \
  && ./fast run \
  && ./fast export -o "$SITE" \
  && git -C "$SITE" add -A \
  && { git -C "$SITE" diff --cached --quiet || git -C "$SITE" commit -m "update fast.vlang.io" ; } \
  && git -C "$SITE" push \
  && gh workflow run fast_pages.yml -R vlang/v ; } >> /path/to/v/cmd/tools/fast/fast.log 2>&1
```

(The old `fast_job.v` daemon and its `-upload` step have been removed; this
export-and-push is their replacement.)

## What is measured

| Column      | Command                          | Meaning                          |
|-------------|----------------------------------|----------------------------------|
| `v -o v.c`  | `vprod -o v.c cmd/v` / v3 self  | self-compile to C                |
| `v -o v`    | `vprod -o v cmd/v` / v3 self    | self-compile to a binary         |
| `V lines / s` | derived                        | V lines per second (`v -o v.c`)  |
| `V lines`   | `-stats`                          | number of V source lines         |
| `v hello.v` | `vprod examples/hello_world.v`   | compile a tiny program           |
| `v.c size`  | –                                | size of the generated `v.c`      |
| scan/parse/check/cgen | v3 self `-show-timings` | per-stage time and RSS (min) |

Wall-clock timings take `max_samples` measurements after a couple of warmups and
discard the slowest ones to cut noise (see the constants in `fast.v`).
Phase RSS is available for commits measured from 2026-07-30 onward; older rows
remain timing-only. Starting on that date, self-compile measurements compile
`vlib/v3/v3.v`; older measurements compile `cmd/v`.

## Database

Everything lives in `fast.db` (SQLite). The schema is defined by the `Benchmark`
struct in `models.v` and created automatically. Inspect it directly if you like:

```sh
sqlite3 fast.db \
  'select commit_hash, commit_date, v_c_ms, v_self_ms from benchmarks order by commit_date;'
```
