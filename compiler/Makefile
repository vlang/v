outdir := .

all: ${outdir}/v

${outdir}/v: v.c
	cc -std=gnu11 -w -o v v.c
	./v -o ${outdir}/v .

v.c:
	wget https://vlang.io/v.c

clean:
	-rm -f ${outdir}/v ./v vc v.c
