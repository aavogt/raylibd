raylib/build/raylib/libraylib.a: raylib
	cd $<
	cmake -DBUILD_EXAMPLES=OFF -Bbuild
	make -Cbuild

raygui.h:
	curl --follow https://raw.githubusercontent.com/raysan5/raygui/refs/heads/master/src/raygui.h --output $@

raygui.tar.gz:
	curl --follow https://github.com/raysan5/raygui/archive/refs/heads/master.tar.gz --output $@

raylib.tar.gz:  
	curl --follow https://github.com/raysan5/raylib/archive/refs/heads/master.tar.gz --output $@

%: %.tar.gz
	tar xf $<
	x=$$(tar tzf $< | head -n1)
	mv $$x $${x%%[-.]*}

.ONESHELL:
.SHELL: /bin/bash
