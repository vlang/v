<div align="center">
<p>
    <img width="80" src="https://raw.githubusercontent.com/vlang/v-logo/master/dist/v-logo.svg?sanitize=true">
</p>
<h1>Bahasa Pemrograman V</h1>

[vlang.io](https://vlang.io) | [Docs](https://github.com/vlang/v/blob/master/doc/docs.md) | [Changelog](https://github.com/vlang/v/blob/master/CHANGELOG.md) | [Kecepatan](https://fast.vlang.io/) | [Kontribusi & desain kompiler](https://github.com/vlang/v/blob/master/CONTRIBUTING.md)

</div>
<div align="center">

<!--
[![Build Status][WorkflowBadge]][WorkflowUrl]
-->
[![Sponsor][SponsorBadge]][SponsorUrl]
[![Patreon][PatreonBadge]][PatreonUrl]
[![Discord][DiscordBadge]][DiscordUrl]
[![Twitter][TwitterUrl]][TwitterBadge]

</div>

## Fitur Utama dari V

- Kesederhanaan: bahasa dapat dipelajari di akhir pekan
- Kompilasi cepat: 110k loc/s dengan backend Clang,
     500rb loc/s dengan backend asli dan tcc *(Intel i5-7500, SSD, tanpa pengoptimalan)* ([video demo](https://www.youtube.com/watch?v=pvP6wmcl_Sc))
- Mudah dikembangkan: V mengkompilasi dirinya sendiri dalam waktu kurang dari satu detik
- Kinerja: secepat C (backend utama V dikompilasi ke C yang dapat dibaca manusia)
- Keamanan: tidak ada nol, tidak ada global, tidak ada perilaku yang tidak ditentukan, kekekalan secara default
- Terjemahan C ke V ([Menerjemahkan video demo DOOM](https://www.youtube.com/watch?v=6oXrz3oRoEg))
- Memuat ulang kode panas
- [Manajemen memori yang inovatif](https://vlang.io/#memory) ([video demo](https://www.youtube.com/watch?v=gmB8ea8uLsM))
- [Pustaka UI lintas platform](https://github.com/vlang/ui)
- Pustaka grafis bawaan
- Kompilasi silang yang mudah
- REPL
- [ORM bawaan](https://github.com/vlang/v/blob/master/doc/docs.md#orm)
- [Kerangka kerja web bawaan](https://github.com/vlang/v/blob/master/vlib/vweb/README.md)
- C dan JavaScript backend
- Bagus untuk menulis perangkat lunak tingkat rendah ([Vinix OS](https://github.com/vlang/vinix))

## Jaminan stabilitas dan perubahan di masa mendatang

Meskipun berada pada tahap pengembangan awal, bahasa V relatif stabil dan memiliki
jaminan kompatibilitas mundur, artinya kode yang Anda tulis hari ini dijamin
untuk bekerja sebulan, setahun, atau lima tahun dari sekarang.

Mungkin masih ada perubahan sintaks kecil sebelum rilis 1.0, tetapi akan ditangani
secara otomatis melalui `vfmt`, seperti yang telah dilakukan sebelumnya.

API inti V (terutama modul `os`) masih akan mengalami perubahan kecil hingga stabil 
di V 1.0. Tentu saja API akan bertambah setelah itu, tetapi tanpa merusak kode yang ada.

Tidak seperti banyak bahasa lain, V tidak akan selalu berubah, dengan fitur baru 
yang diperkenalkan dan fitur lama yang dimodifikasi. Itu akan selalu menjadi bahasa 
yang kecil dan sederhana, sangat mirip dengan yang ada sekarang.

## Menginstal V - dari sumber *(metode yang diusulkan)*

### Linux, macOS, Windows, *BSD, Solaris, WSL, Android, dll.

Biasanya menginstal V cukup sederhana jika Anda memiliki lingkungan yang sudah 
memiliki instalasi `git` fungsional.

* *(* ***HARAP DICATAT:*** *Jika Anda mengalami masalah atau Anda memiliki sistem operasi yang berbeda atau 
distribusi Linux yang tidak segera diinstal atau berfungsi, silakan lihat 
[Masalah Instalasi](https:// github.com/vlang/v/discussions/categories/installation-issues)
dan cari OS dan masalah Anda. Jika Anda tidak dapat menemukan masalah Anda, tambahkan ke diskusi yang ada
jika ada untuk OS Anda, atau buat yang baru jika diskusi utama belum ada untuk OS Anda.)*


Untuk memulai, cukup coba jalankan yang berikut di terminal/Shell Anda:
```bash
git clone https://github.com/vlang/v
cd v
make
# HINT: Using Windows?: run make.bat in the cmd.exe shell
```

Itu seharusnya dan Anda harus menemukan V Anda dapat dieksekusi di `[jalan ke V repo]/v`.
`[jalan ke V repo]` bisa dimana saja.

(Seperti pada petunjuk di atas, pada Windows `make` berarti menjalankan `make.bat`, 
jadi pastikan Anda menggunakan terminal `cmd.exe`.)

Sekarang Anda bisa mencoba `./v run examples/hello_world.v` (`v.exe` pada Windows).

* *Masalah? Silakan lihat catatan di atas dan tautan ke
[Masalah Instalasi](https://github.com/vlang/v/discussions/categories/installation-issues) untuk bantuan.*

V terus diperbarui. Untuk memperbarui V, cukup jalankan:

```bash
v up
```

### Kompiler C

Disarankan untuk menggunakan Clang, GCC, atau Visual Studio.
Jika Anda melakukan pengembangan, kemungkinan besar Anda sudah menginstal salah satunya.

Jika tidak, ikuti petunjuk berikut:

- [Menginstal kompiler C di Linux dan macOS](https://github.com/vlang/v/wiki/Installing-a-C-compiler-on-Linux-and-macOS)

- [Menginstal kompiler C di Windows](https://github.com/vlang/v/wiki/Installing-a-C-compiler-on-Windows)

Namun, jika tidak ada yang ditemukan saat menjalankan `make` di Linux atau Windows,
TCC diunduh sebagai backend C default.
Ini sangat ringan (beberapa MB) jadi ini tidak akan memakan waktu terlalu lama.

### Menghubungkan

NB: *sangat disarankan*, Anda meletakkan V di PATH Anda. Itu menghemat upaya Anda 
untuk mengetikkan path lengkap ke V Anda yang dapat dieksekusi setiap saat.
V memberikan kemudahan perintah `v symlink` untuk melakukannya dengan lebih mudah.

Pada sistem Unix, ini membuat symlink `/usr/local/bin/v` ke file executable Anda. 
Untuk melakukan itu, jalankan:

```bash
sudo ./v symlink
```

Di Windows, mulai shell baru dengan hak administratif, untuk
misalnya dengan <kbd>Windows Key</kbd>, lalu ketik `cmd.exe`, 
klik kanan pada entri menunya, dan pilih `Run as administrator`. Di shell administratif baru, 
cd ke jalur, di mana Anda telah mengkompilasi v.exe, lalu ketik:

```bat
.\v.exe symlink
```

Itu akan membuat V tersedia di mana-mana, dengan menambahkannya ke PATH Anda.
Silakan restart shell/editor Anda setelah itu, sehingga dapat memilih
variabel PATH baru.

NB: tidak perlu menjalankan `v symlink` lebih dari sekali - v akan terus tersedia, 
bahkan setelah `v up`, restart, dan seterusnya.
Anda hanya perlu menjalankannya kembali, jika Anda memutuskan untuk memindahkan 
folder repo V ke tempat lain.

### Docker

<details><summary>Perluas instruksi Docker</summary>

```bash
git clone https://github.com/vlang/v
cd v
docker build -t vlang .
docker run --rm -it vlang:latest
```

### Docker dengan Alpine/musl

```bash
git clone https://github.com/vlang/v
cd v
docker build -t vlang --file=Dockerfile.alpine .
docker run --rm -it vlang:latest
```

</details>

## Menguji dan menjalankan contoh

Pastikan V dapat mengkompilasi sendiri:

```bash
v self
```

```bash
$ v
V 0.3.x
Use Ctrl-C or `exit` to exit

>>> println('hello world')
hello world
>>>
```

```bash
cd examples
v hello_world.v && ./hello_world    # or simply
v run hello_world.v                 # this builds the program and runs it right away

v run word_counter/word_counter.v word_counter/cinderella.txt
v run news_fetcher.v
v run tetris/tetris.v
```

<img src='https://raw.githubusercontent.com/vlang/v/master/examples/tetris/screenshot.png' width=300>

NB: Untuk membangun Tetris atau 2048 (atau apa pun menggunakan modul grafis `sokol` atau `gg`)
pada beberapa sistem Linux, Anda perlu menginstal `libxi-dev` dan `libxcursor-dev` .

## V net.http, net.websocket, `v install`
Jika Anda berencana untuk menggunakan modul net.http, atau modul net.websocket, Anda juga perlu menginstal
OpenSSL pada sistem non-Windows:

```bash
macOS:
brew install openssl

Debian/Ubuntu:
sudo apt install libssl-dev

Arch/Manjaro:
openssl is installed by default

Fedora:
sudo dnf install openssl-devel
```

## Sinkronisasi V
Modul `sync` V dan implementasi saluran menggunakan libatomic.
Kemungkinan besar sudah diinstal pada sistem Anda, tetapi jika tidak, Anda dapat menginstalnya, 
dengan melakukan hal berikut:
```bash
MacOS: already installed

Debian/Ubuntu:
sudo apt install libatomic1

Fedora/CentOS/RH:
sudo dnf install libatomic-static
```

## V UI

<a href="https://github.com/vlang/ui">
<img src='https://raw.githubusercontent.com/vlang/ui/master/examples/screenshot.png' width=712>
</a>

https://github.com/vlang/ui

<!---
## JavaScript backend

[examples/hello_v_js.v](examples/hello_v_js.v):

```v
fn main() {
	for i in 0 .. 3 {
		println('Hello from V.js')
	}
}
```

```bash
v -o hi.js examples/hello_v_js.v && node hi.js
Hello from V.js
Hello from V.js
Hello from V.js
```
-->

## Aplikasi grafis Android

Dengan alat `vab` V, membangun V UI dan aplikasi grafis untuk Android dapat menjadi semudah:

```
./vab /path/to/v/examples/2048
```

[https://github.com/vlang/vab](https://github.com/vlang/vab).

<img src="https://user-images.githubusercontent.com/768942/107622846-c13f3900-6c58-11eb-8a66-55db12979b73.png">

## Mengembangkan aplikasi web

Lihat [Membangun blog web sederhana](https://github.com/vlang/v/blob/master/tutorials/building_a_simple_web_blog_with_vweb/README.md)
tutorial dan Gitly, alternatif yang ringan dan cepat untuk GitHub/GitLab:

https://github.com/vlang/gitly

<img src="https://user-images.githubusercontent.com/687996/85933714-b195fe80-b8da-11ea-9ddd-09cadc2103e4.png">

## Vinix, sebuah OS/kernel yang ditulis dalam V

V sangat bagus untuk menulis perangkat lunak tingkat rendah seperti driver dan kernel.
Vinix adalah OS/kernel yang sudah menjalankan bash, GCC, V, dan nano.

https://github.com/vlang/vinix

<img src="https://github.com/vlang/vinix/blob/main/screenshot0.png?raw=true">
<img src="https://github.com/vlang/vinix/blob/main/screenshot1.png?raw=true">

## Pengakuan

V berterima kasih kepada Fabrice Bellard atas karya aslinya di [TCC - Tiny C Compiler](https://bellard.org/tcc/). 
Perhatikan situs web TCC sudah tua; repositori TCC saat ini dapat ditemukan [di sini](https://repo.or.cz/w/tinycc.git). 
V menggunakan binari TCC pra-bangun yang terletak di [https://github.com/vlang/tccbin/](https://github.com/vlang/tccbin/).

## Penyelesaian masalah

Silakan lihat [Pemecahan Masalah](https://github.com/vlang/v/wiki/Troubleshooting) bagian di [halaman wiki](https://github.com/vlang/v/wiki) kami.

[WorkflowBadge]: https://github.com/vlang/v/workflows/CI/badge.svg
[DiscordBadge]: https://img.shields.io/discord/592103645835821068?label=Discord&logo=discord&logoColor=white
[PatreonBadge]: https://img.shields.io/endpoint.svg?url=https%3A%2F%2Fshieldsio-patreon.vercel.app%2Fapi%3Fusername%3Dvlang%26type%3Dpledges
[SponsorBadge]: https://camo.githubusercontent.com/da8bc40db5ed31e4b12660245535b5db67aa03ce/68747470733a2f2f696d672e736869656c64732e696f2f7374617469632f76313f6c6162656c3d53706f6e736f72266d6573736167653d254532253944254134266c6f676f3d476974487562
[TwitterBadge]: https://twitter.com/v_language

[WorkflowUrl]: https://github.com/vlang/v/commits/master
[DiscordUrl]: https://discord.gg/vlang
[PatreonUrl]: https://patreon.com/vlang
[SponsorUrl]: https://github.com/sponsors/medvednikov
[TwitterUrl]: https://img.shields.io/twitter/follow/v_language.svg?style=flatl&label=Follow&logo=twitter&logoColor=white&color=1da1f2
