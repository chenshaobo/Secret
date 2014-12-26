

all:
	erl -pa "\." -make
	rm -rf  /data/Secret
	mkdir -p /data/Secret
	cp -rf ./* /data/Secret