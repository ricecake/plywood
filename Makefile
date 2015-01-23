REPO		?= plywood

PKG_REVISION    ?= $(shell git describe --tags)
PKG_VERSION	?= $(shell git describe --tags | tr - .)
PKG_ID           = plywood-$(PKG_VERSION)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=

.PHONY: rel deps test

all: deps compile

compile: deps
	@(./rebar compile)

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps
	@rm -rf $(PKG_ID).tar.gz

##
## Release targets
##
rel: deps compile
	@./rebar compile
	@./rebar skip_deps=true generate $(OVERLAY_VARS)

relclean:
	rm -rf rel/plywood

##
## Developer targets
##
stage : rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/plywood/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/plywood/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/plywood/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/plywood/lib;)

devrel: all
	mkdir -p dev
	(cd rel && ../rebar generate target_dir=../dev/$(REPO) overlay_vars=vars.config)

devclean: clean
	rm -rf dev

##
## Packaging targets
##
.PHONY: package
export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE

package.src: deps
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION)| (cd package && tar -xf -)
	${MAKE} -C package/$(PKG_ID) deps
	mkdir -p package/$(PKG_ID)/priv
	git --git-dir=.git describe --tags >package/$(PKG_ID)/priv/vsn.git
	for dep in package/$(PKG_ID)/deps/*; do \
             echo "Processing dep: $${dep}"; \
             mkdir -p $${dep}/priv; \
             echo `git --git-dir=$${dep}/.git describe --tags || echo '0.01'` >$${dep}/priv/vsn.git; \
        done
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

dist: package.src
	cp package/$(PKG_ID).tar.gz .

package: package.src
	${MAKE} -C package -f $(PKG_ID)/deps/node_package/Makefile

pkgclean: distclean
	rm -rf package

include tools.mk
