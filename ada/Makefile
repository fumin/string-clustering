GNAT = "/usr/local/gnat/bin/gnat"

all:
	$(GNAT) make -Pmain
test:
	$(GNAT) make -Pharness
	./obj/test_string_clustering
clean:
	rm obj/*
