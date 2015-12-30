PROJECT = arrets
PROJECT_DESCRIPTION = A silly non-performant array implementation on top of ets
PROJECT_VERSION = 0.0.1

.DEFAULT_GOAL := app

eqc-ci: app
	erlc -o ebin test/*_SUITE.erl

include erlang.mk
