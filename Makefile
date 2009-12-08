SOURCE_FILES := $(wildcard src/beanstalk*.erl) src/reloader.erl


all: $(SOURCE_FILES:src/%.erl=ebin/%.beam) ebin/beanstalk.app

ebin/%.beam: src/%.erl
	@test -d ebin || mkdir ebin
	erlc -pa ebin -W +debug_info -o ebin $<

ebin/beanstalk.app: src/beanstalk.app
	cp src/beanstalk.app ebin/beanstalk.app

clean:
	rm -rf ebin
