.PHONY: default deps test repl deploy

default: deps
	mix compile

deps:
	mix deps.get
	mix deps.compile

test:
	mix test

repl:
	iex -S mix

deploy:
	mix docs
	mix coveralls.html -u
	ansible-playbook deploy/deploy.yml
