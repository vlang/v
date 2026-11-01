# vcgen.vlang.io

vcgen.vlang.io regenerates V's bootstrap C snapshots, `v.c` and `v_win.c` in
[vlang/vc](https://github.com/vlang/vc), from vlang/v master. It is `cmd/tools/gen_vc` running as
`gen_vc serve` on the `vpm` server, and it takes over publishing from the `deploy` job of
`.github/workflows/gen_vc_ci.yml`. The workflow keeps a `validate` job that builds every pull
request's snapshot without any secret.

It starts in dry-run mode, where it never pushes. Publishing moves to the service only when the
maintainer switches it to push mode and sets the repository variable `VCGEN_SERVICE` to `live`.

## What a run does

The service queues a run at startup, every `--poll-minutes` (5), and on each signed GitHub push
webhook for master. Requests that arrive while a run is pending coalesce into it. A run:

1. Fetches master of vlang/vc, then of vlang/v. A snapshot names a vlang/v commit that was on
   master before the snapshot was pushed, so the later vlang/v fetch contains it.
2. Stops early when nothing has to be built:
   - neither master moved since the last run (`unchanged`);
   - vlang/vc already holds the snapshot of vlang/v master (`up_to_date`, push mode);
   - every path that changed since the snapshot's source commit is ignored (`ignored`): `*.md`,
     `*.vv`, `*.out`, `*.yml`, `cmd/tools/**` except `cmd/tools/builders/*.v`, `vc.lock`, and the
     vc sha in `workflow_contract_test.v`. So a vc.lock update never causes a new snapshot;
   - the last run for the same commits failed less than `--retry-failed-minutes` (60) ago.
     Polling during that delay keeps the retry pending until the delay expires.
3. Refuses to build while MemAvailable plus SwapFree is below `--min-available-mib` (2048).
4. Checks out vlang/v master at `--v-dir`, cleans it, and runs `make`.
5. Generates `v.c` (`-cross`) and `v_win.c` (`-cross -os windows -cc msvc`) at the checkout root,
   prepends `#define V_COMMIT_HASH "<sha>"`, and runs the snapshot checks of
   `cmd/tools/gen_vc/snapshot.v`, which `gen_vc_ci.yml` uses too.
6. Commits the snapshot on top of vlang/vc master as `[v:master] <sha> - <subject>` by
   `vlang-bot`, in a bare blobless clone (no checkout of the old snapshot).
7. In push mode, checks that vlang/v master still is `<sha>`; otherwise the snapshot is `stale`
   and the next run builds the newer master. Then it pushes without force (`published`).

In dry-run mode, step 7 is skipped (`would_publish`). When vlang/vc already holds a snapshot of
the same commit, a dry run builds anyway and compares the trees: `verified` means its snapshot is
byte-identical to the published one, `differs` names the file that is not.

The checkout path is embedded in the snapshot. GitHub's runners build at `/home/runner/work/v/v`,
so the service uses the same `--v-dir` to produce the same bytes.

During bootstrapping, the POSIX and Windows compatibility helpers relocate V-owned include
directives to the current checkout. They preserve the downloaded snapshot and leave header
processing to the C compiler.

## Endpoints

| Path | Answer |
| --- | --- |
| `GET /` | mode, state, and the last run |
| `GET /status.json` | the same as JSON |
| `GET /log` | the log of the last run |
| `POST /genhook` | GitHub push webhook; `401` unless signed with `VCGEN_WEBHOOK_SECRET` |

The webhook payload only triggers a run. What is built is always the master that the run
fetches itself, so pull request code never runs on `vpm`.

## Setup on vpm

`vpm` runs Debian 12 on 2 vCPUs with 2 GB of RAM and no swap, next to other services. Generating
`v_win.c` alone peaks at about 1.5 GB, so add swap first. Run as root:

```sh
fallocate -l 4G /swapfile && chmod 600 /swapfile && mkswap /swapfile && swapon /swapfile
echo '/swapfile none swap sw 0 0' >> /etc/fstab

useradd --system --home-dir /var/lib/vcgen --shell /usr/sbin/nologin vcgen
install -d -o vcgen -g vcgen /var/lib/vcgen /home/runner/work/v
install -d /opt/vcgen

git clone --depth=1 https://github.com/vlang/v /opt/vcgen/src
make -C /opt/vcgen/src
/opt/vcgen/src/v -prod -o /opt/vcgen/gen_vc /opt/vcgen/src/cmd/tools/gen_vc
```

Create `/etc/vcgen.env`, owned by root with mode `0600`:

```ini
# openssl rand -hex 32; the same value goes into the GitHub webhook.
VCGEN_WEBHOOK_SECRET=
# Only for push mode: a fine-grained token of vlang-bot, Contents read and write on vlang/vc only.
VCGEN_PUSH_TOKEN=
```

The token reaches git through a credential helper that reads it from the environment, so it is
never part of an argument, a URL, the log, or a `.git/config`.

Create `/etc/systemd/system/vcgen.service`:

```ini
[Unit]
Description=vcgen.vlang.io: regenerate vlang/vc from vlang/v master
Wants=network-online.target
After=network-online.target

[Service]
Type=simple
User=vcgen
Group=vcgen
Environment=HOME=/var/lib/vcgen
EnvironmentFile=/etc/vcgen.env
ExecStart=/opt/vcgen/gen_vc serve --mode dry-run --work-dir /var/lib/vcgen \
    --v-dir /home/runner/work/v/v
Restart=on-failure
RestartSec=10
StateDirectory=vcgen
PrivateTmp=true
NoNewPrivileges=true
ProtectSystem=strict
ReadWritePaths=/home/runner/work/v
Nice=10
CPUWeight=20
MemoryHigh=1G
MemoryMax=2G
MemorySwapMax=4G

[Install]
WantedBy=multi-user.target
```

The memory limits make a build swap instead of pushing the other services on `vpm` out.

Point `vcgen.vlang.io` at `vpm`, get a certificate as for the other vlang.io sites, and add
`/etc/nginx/sites-available/vcgen.vlang.io`, linked from `sites-enabled`:

```nginx
server {
    listen 80;
    listen [::]:80;
    listen 443 ssl;
    listen [::]:443 ssl;
    server_name vcgen.vlang.io;
    ssl_certificate /etc/letsencrypt/live/vcgen.vlang.io/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/vcgen.vlang.io/privkey.pem;
    client_max_body_size 10m;
    location ^~ /.well-known/acme-challenge/ {
        root /var/www/letsencrypt;
    }
    location / {
        proxy_pass http://127.0.0.1:7171;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_read_timeout 15s;
    }
}
```

Then `systemctl daemon-reload && systemctl enable --now vcgen && nginx -t && systemctl reload
nginx`. Follow the runs with `journalctl -u vcgen -f` or at https://vcgen.vlang.io/.

The webhook is optional; without it, polling picks up master within `--poll-minutes`. To add it:
vlang/v, Settings, Webhooks, Add webhook. Payload URL `https://vcgen.vlang.io/genhook`, content
type `application/json`, the secret from `/etc/vcgen.env`, "Just the push event".

## Switching publishing to the service

1. Leave the service in dry-run mode until several runs report `verified`: each time the
   Actions `deploy` job publishes a snapshot, the next dry run rebuilds it and compares.
2. Change `--mode dry-run` to `--mode push` in `ExecStart`, fill in `VCGEN_PUSH_TOKEN`, and run
   `systemctl daemon-reload && systemctl restart vcgen`.
3. In vlang/v, Settings, Secrets and variables, Actions, Variables, set `VCGEN_SERVICE` to
   `live`. From then on `gen_vc_ci.yml` runs only `validate`.

Both publishers may overlap between steps 2 and 3. That is harmless: whichever pushes second
finds the same snapshot already published.

To pause the service, run `systemctl stop vcgen`. To hand publishing back to Actions, also delete
the `VCGEN_SERVICE` variable or set it to anything other than `live`.

## Running it by hand

```sh
./v -o gen_vc cmd/tools/gen_vc
./gen_vc snapshot . /tmp/vc_snapshot
./gen_vc run-once --mode dry-run --work-dir /tmp/vcgen
```

`snapshot` only generates and checks the snapshot of an already built checkout. `run-once`
makes one complete run, which is a dry run unless `--mode push` is given, and exits with 1 when
the run failed. `gen_vc serve --help` lists every option.
