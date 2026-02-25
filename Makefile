CC       := gcc
CFLAGS   := -fPIC -O2 -Wl,--export-dynamic 
RAYLIB_A   := vendor/raylib/build/raylib/libraylib.a

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

clean:
	rm -rf main_hot dll.so cabal.project.local cabal.project.local~ \
				dist-newstyle dll.c dll.so main_hot raylibd.cabal

$(RAYLIB_A):
	make -Cvendor

.ONESHELL: true
.SHELL: bash
