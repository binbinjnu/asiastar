APP_NAME = asiastar
all: compile test
APP_VER=$(shell awk '/release_vsn/{ print $1 }' rebar.config | tr -d \")
###===================================================================
### build
###===================================================================
.PHONY: get-deps co compile run rel_tar
get-deps:
	rebar3 get-deps
co:compile
compile: get-deps
	rebar3 compile
### clean
.PHONY: clean distclean
clean:
	rebar3 clean
distclean: test_clean
	rebar3 clean -a
rel_tar:
	rebar3 tar
###===================================================================
### test
###===================================================================
.PHONY: test eunit ct test_shell test_run test_clean
test: epmd
	rebar3 do eunit -v, ct -v, cover -v
eunit: epmd
	rebar3 do eunit -v, cover
ct: epmd
	rebar3 do ct -v, cover
test_shell:
	rebar3 as test compile
	erl -pa _build/test/lib/*/ebin -pa _build/test/lib/$(APP_NAME)/test
test_run: epmd
	rebar3 as test shell
test_clean:
	@rm -rf _build/test/lib/$(APP_NAME)/test _build/test/logs _build/test/cover
###===================================================================
### relup
###===================================================================
.PHONY: tag up_vsn up_app_vsn up_relx_vsn clean_appup build_old_vsn relup check_appup
tag:
	git tag $(APP_VER)
up_vsn: up_app_vsn up_relx_vsn
up_app_vsn:
	@exec script/up_app_vsn.sh
up_relx_vsn:
	@exec script/up_relx_vsn.sh
clean_appup:
	rebar3 appup clean
	rm -f _build/default/lib/*/ebin/*.appup
build_old_vsn:
	@exec script/build_old_vsn.sh
relup:
	@exec script/relup.sh
check_appup:
	@exec script/check_appup.sh
###===================================================================
### other
###===================================================================
.PHONY: help tree logtail epmd
help:
	rebar3 help
tree:
	rebar3 tree
epmd:
	@pgrep epmd 2> /dev/null > /dev/null || epmd -daemon || true
