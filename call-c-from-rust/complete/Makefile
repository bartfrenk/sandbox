all: build run

build: library/libawesome_math.so

run:
	cd executable; \
	LD_LIBRARY_PATH=../library cargo run


library/libawesome_math.so:
	gcc -g -shared library/awesome_math.c -o library/libawesome_math.so

clean:
	rm -f library/libawesome_math.so

linker:
	cd executable; \
	cargo rustc -- -C link-args="-Wl,-rpath,$$ORIGIN/../library/"
