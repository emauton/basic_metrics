REBAR = EXOMETER_PACKAGES="(basic)" rebar
.PHONY: deps docs

all: deps compile

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean: docsclean
	$(REBAR) clean

distclean: clean
	$(REBAR) delete-deps

lint:
	elvis rock

docs: docsclean
	$(REBAR) skip_deps=true doc

docsclean:
	rm -Rf edoc
