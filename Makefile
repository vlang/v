all: clean v
	$(info V has been successfully built)

v: v.c
	cc -std=gnu11 -w -o v v.c
	./v -o v compiler
	rm v.c

v.c:
	curl -Os https://raw.githubusercontent.com/vlang/vc/master/v.c

test: v
	./v -prod -o vprod compiler # Test prod build
	echo "Running V tests..."
	find . -name '*_test.v' -print0 | xargs -0 -n1 ./v
	echo "Building V examples..."
	find examples -name '*.v' -print0 | xargs -0 -n1 ./v

thirdparty/freetype-windows/win32/freetype.dll:
    git clone https://github.com/ubawurinna/freetype-windows-binaries.git thirdparty/freetype-windows
	cp thirdparty/freetype-windows/win32/freetype.dll vlib/glm/

crossbuild: thirdparty/freetype-windows/win32/freetype.dll
	#wine-stable
	./v -os windows -o v.exe compiler
	./v.exe -h
	#wget https://raw.githubusercontent.com/Winetricks/winetricks/master/src/winetricks
	#this hangs
	#WINEPREFIX=${HOME}/.wine-test WINEARCH=win32 bash winetricks -q vcrun2015

	find . \( -name '*_test.v' ! -iname 'glm*' ! -iname 'socket*' \) -print0 | xargs -0 -n1 ./v -os windows

clean:
	-rm -f v.c v vprod
