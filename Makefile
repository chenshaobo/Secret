

all:
	erl -pa "\." -make
	cp -rf src/*.app  ebin
	rm -rf  /data/Secret
	mkdir -p /data/Secret
	cp -rf ./* /data/Secret
