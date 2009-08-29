#!/bin/bash

DIR_EBIN=$(PWD)/ebin
DIR_SOURCE=$(PWD)/src
DIR_INCLUDE=$(PWD)/include

ERLC_FLAGS=-W0 -Ddebug +debug_info
ERLC=erlc -I $(DIR_INCLUDE) -o $(DIR_EBIN) $(ERL_FLAGS) $(DIR_SOURCE)
ERL=erl -I -pa $(DIR_EBIN) -noshell -eval

LEXER=arith_lexer
PARSER=arith_parser

DIR_PARSER=$(DIR_SOURCE)/parser

compile:
	mkdir -p $(DIR_EBIN)
	# Compile the lexer generator, because it's not a part of OTP
	$(ERLC)/../leex/*.erl
	# Generate the lexer
	cd $(DIR_PARSER); $(ERL) 'leex:file("$(LEXER).xrl", [{outdir, "./"}]), halt().'
	# Generate the parser
	$(ERL) 'yecc:file("$(DIR_PARSER)/$(PARSER).yrl"), halt().'
	# Compile everything
	$(ERLC)/parser/*.erl
	$(ERLC)/compiler/*.erl

all: compile

test:
	$(ERLC)/tests/*.erl
	$(ERL) 'arith_test:simple_expr(), halt().'

clean:
	rm $(DIR_PARSER)/*.erl
	rm $(DIR_EBIN)/*.beam
