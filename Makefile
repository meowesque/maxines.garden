UNAME := $(shell uname)
SHELL := /usr/bin/env bash

ifeq ($(UNAME), Darwin)
	PROCS := $(shell sysctl -n hw.logicalcpu)
else
	PROCS := $(shell nproc)
endif

format:
	@cabal-fmt -i garden.cabal
	@find src -name '*.hs' | xargs -P $(PROCS) -I {} fourmolu -q -i {}
