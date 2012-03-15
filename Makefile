compile: deps
	./rebar compile

quick:
	./rebar compile skip_deps=true

deps:
	./rebar get-deps

refresh-deps:
	./rebar delete-deps
	./rebar get-deps

tests=""

.PHONY: test
test: compile
ifeq ($(tests), "")
	./rebar -j1 eunit
else
	./rebar -j1 eunit suite=$(tests)
endif

.PHONY: doc
doc:
	./rebar doc

clean:
	./rebar clean

start=""
shellopts=""

shell:
ifeq ($(start), "")
	 erl -pa ebin $(wildcard deps/*/ebin) $(shellopts)
else
	 erl -pa ebin $(wildcard deps/*/ebin) -s $(start) $(shellopts)
endif

appid=""

var-appid:
ifeq ($(appid), "")
	@echo "ERROR: appid is required"
	@exit 1
endif

appdir=""

var-appdir:
ifeq ($(appdir), "")
	@echo "ERROR: appdir is required"
	@exit 1
endif

module=""
var-module:
ifeq ($(module), "")
	@echo "ERROR: module is required"
	@exit 1
endif

new-project: var-appid var-appdir
	rebar create template=e2app appid=$(appid) dest="$(appdir)"

new-service: var-module
ifeq ($(appdir), "")
	rebar create template=e2service module=$(module) dest="."
else
	rebar create template=e2service module=$(module) dest="$(appdir)"
endif
	@echo "TODO: Add $(module) to a supervisor hierarchy (e.g. *_app file)"
