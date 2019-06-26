all: v

v: vc
	./vc -o v .

vc: v.c
	cc -std=gnu11 -w -o vc v.c

v.c:
	curl -Os https://raw.githubusercontent.com/vlang/vc/master/v.c

buildtwice: v
	@echo "Checking if V can build itself.." && \
		./v -o v . && ./v -o v .         && \
		echo "Passed" || echo "Failed"

test: buildtwice
	@echo "Running tests using final build"   && \
		find .. -name '*_test.v' -print0 | xargs -0 -n1 ./v

	@echo "Running examples using final build" && \
		find ../examples -name '*.v' -print0 | xargs -0 -n1 ./v

clean:
	-rm -f vc v v.c
