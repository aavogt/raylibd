CC       := gcc
INCLUDES := -I/usr/local/include -I/usr/include -Ivendor/raylib/build/raylib -Ivendor -Ivendor/raygui/src
CFLAGS   := -fPIC -O2 -Wl,--export-dynamic $(INCLUDES)
RAYLIB_A := $(firstword $(wildcard /usr/local/lib/libraylib.a /usr/lib/libraylib.a) vendor/raylib/build/raylib/libraylib.a)
RAYGUI_H := $(firstword $(wildcard /usr/local/include/raygui.h /usr/include/raygui.h) vendor/raygui.h)

watch: main_hot dll.so
	ulimit -v 1000000
	LD_LIBRARY_PATH=. ./main_hot &
	raylibd &
	ls dll.c | entr make dll.so

main_hot: $(RAYLIB_A) main_hot.c dll.c
	$(CC) $(CFLAGS) -Wl,--whole-archive $(RAYLIB_A) -Wl,--no-whole-archive -lm -o $@ main_hot.c

dll.so: dll.c
	$(CC) $(CFLAGS) -DRL_SO_IMPL -shared -o dll.so.2 $^
	mv dll.so.2 $@

dll.c: main.c
	raylibd --once

compile_commands.json: $(RAYGUI_H)
	bear -- $(CC) $(CFLAGS) main.c -fsyntax-only

clean:
	rm -rf main_hot dll.so cabal.project.local cabal.project.local~ \
				dist-newstyle dll.c dll.so main_hot raylibd.cabal

$(RAYGUI_H):
	$(MAKE) -Cvendor raygui.h

$(RAYLIB_A):
	$(MAKE) -Cvendor raylib/build/raylib/libraylib.a

.ONESHELL: true
.SHELL: bash
