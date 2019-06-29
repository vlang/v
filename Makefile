all: clean v
	$(info V has been successfully built)

v: v.c
	cc -std=gnu11 -w -o v v.c
	./v -o vnew compiler
	rm v.c
	mv vnew v

v.c:
	cp compiler/v019.c v.c
	#curl -Os https://raw.githubusercontent.com/vlang/vc/master/v.c

test: v
	./v -prod -o vprod compiler
	echo "Running V tests..."
	find . \( -name '*_test.v' ! -iname "glm_test*" \) -print0  | xargs -0 -n1 ./v
	echo "Building V examples..."
	find examples \( -name '*.v' ! -iname "tetris*" ! -iname "news_fetch*" ! -iname "links_scraper*" \) -print0 | xargs -0 -n1 ./v

clean:
	-rm -f v.c v vprod
