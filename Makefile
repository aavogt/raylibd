CC       := gcc
INCLUDES := -I/usr/local/include -I/usr/include -Ivendor/raylib/build/raylib -Ivendor -Ivendor/raygui/src
CFLAGS   := -fPIC -O2 -Wl,--export-dynamic $(INCLUDES)
RAYLIB_A := $(firstword $(wildcard /usr/local/lib/libraylib.a /usr/lib/libraylib.a) vendor/raylib/build/raylib/libraylib.a)
RAYGUI_H := $(firstword $(wildcard /usr/local/include/raygui.h /usr/include/raygui.h) vendor/raygui.h)
RAYLIBD := raylibd --quiet
ISPC     := ispc
ISPC_SRCS := $(wildcard *.ispc)
ISPC_OBJS := $(ISPC_SRCS:.ispc=.ispc.o)
ISPC_HEADERS := $(ISPC_SRCS:.ispc=.h)
ISPC_SOS := $(ISPC_SRCS:.ispc=.so)

watch: main_hot dll.so
	ulimit -v 10000000
	trap 'pkill -P $$$$' EXIT INT TERM
	LD_LIBRARY_PATH=. ./main_hot &
	$(RAYLIBD) &
	find . -maxdepth 1 -type f \( -name '*.c' -o -name '*.h' -o -name '*.ispc' \) | entr -d make dll.so

main_hot: $(RAYLIB_A) main_hot.c dll.c
	$(CC) $(CFLAGS) -Wl,--whole-archive $(RAYLIB_A) -Wl,--no-whole-archive -lm -o $@ main_hot.c

main: main.c
	$(CC) $(CFLAGS) main.c $(RAYLIB_A) -lm -o main

dll.so: dll.c $(ISPC_SOS)
	$(CC) $(CFLAGS) -DRL_SO_IMPL -shared -Wl,-rpath,. -o dll.so.2 dll.c $(ISPC_SOS)
	mv dll.so.2 $@

dll.c: main.c $(ISPC_HEADERS)
	[ -e main.hs ] && ghcid --command 'cabal repl' -Tmain || $(RAYLIBD) --once

%.ispc.o: %.ispc
	$(ISPC) --pic $< -h $*.h -o $@

%.h: %.ispc.o
	@true

%.so: %.ispc.o
	$(CC) -shared -o $@ $<

compile_commands.json: $(RAYGUI_H)
	bear -- $(CC) $(CFLAGS) main.c -fsyntax-only

clean:
	rm -rf main_hot dll.so cabal.project.local cabal.project.local~ \
				dist-newstyle dll.c dll.so main_hot raylibd.cabal \
				main compile_commands.json $(ISPC_HEADERS) $(ISPC_OBJS) $(ISPC_SOS)

$(RAYGUI_H):
	$(MAKE) -Cvendor raygui.h

$(RAYLIB_A):
	$(MAKE) -Cvendor raylib/build/raylib/libraylib.a

.ONESHELL: true
.SHELL: bash
