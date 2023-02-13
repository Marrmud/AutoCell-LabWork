/*
 * autocell - AutoCell compiler and viewer
 * Copyright (C) 2021  University of Toulouse, France <casse@irit.fr>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

%{

open Common
open Ast
open Printf
open Symbols

(** Raise a syntax error with the given message.
	@param msg	Message of the error. *)
let error msg =
	raise (SyntaxError msg)


(** Restructure the when assignment into selections.
	@param f	Function to build the assignment.
	@param v	Initial values.
	@param ws	Sequence of (condition, expression).
	@return		Built statement. *)
let rec make_when f v ws =
	match ws with
	| [] ->	f v
	| (c, nv)::t ->
		IF_THEN(c, f v, make_when f nv t)

%}

%token EOF

/* keywords */
%token DIMENSIONS

%token END
%token OF

%token IF
%token THEN
%token ELSE
%token ELSIF

/* symbols */
%token ASSIGN
%token COMMA
%token LBRACKET RBRACKET
%token DOT_DOT
%token DOT
%token ADD
%token SUB
%token MULT
%token LPAR RPAR
%token DIV
%token MOD
%token EQ
%token NEQ
%token LT
%token LE
%token HT
%token HE


/* values */
%token <string> ID
%token<int> INT

%start program
%type<Ast.prog> program

%%

program: INT DIMENSIONS OF config END opt_statements EOF
	{
		if $1 != 2 then error "only 2 dimension accepted";
		($4, $6)
	}
;

config:
	INT DOT_DOT INT
		{
			if $1 >= $3 then error "illegal field values";
			[("", (0, ($1, $3)))]
		}
|	fields
		{ set_fields $1 }
;

fields:
	field
		{ [$1] }
|	fields COMMA field
		{$3 :: $1 }
;

field:
	ID OF INT DOT_DOT INT
		{
			if $3 >= $5 then error "illegal field values";
			($1, ($3, $5))
		}
;
opt_statements:
	/* empty */
    	{ NOP }
|	rec_statements
		{
			$1
		}
;

rec_statements:
	statement rec_statements
    	{
			SEQ ($1, $2)
		}
|	statement
    	{
			$1
		}
;

comparison:
	expression NEQ expression2
	{COMP(COMP_NE, $1, $3)}
|	expression HE expression2
	{COMP(COMP_GE, $1, $3)}
|	expression LE expression2
	{COMP(COMP_LE, $1, $3)}
|	expression EQ expression2
	{COMP(COMP_EQ, $1, $3)}
|	expression HT expression2
	{COMP(COMP_GT, $1, $3)}
|	expression LT expression2
	{COMP(COMP_LT, $1, $3)}

statement:
	cell ASSIGN expression
		{
			if (fst $1) != 0 then error "assigned x must be 0";
			if (snd $1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, $3)
		}
|	ID ASSIGN expression
    {
    SET_VAR (declare_var $1,$3);
    }
|	IF comparison THEN statement ELSIF comparison THEN statement END
	{
		IF_THEN($2,$4,IF_THEN($6,$8,NOP))
	}
|	IF comparison THEN statement ELSE statement END
	{
		IF_THEN($2,$4,$6)
	}
|	IF comparison THEN statement END
	{
		IF_THEN($2,$4,NOP)
	}
;


cell:
	LBRACKET INT COMMA INT RBRACKET
		{
			if ($2 < -1) || ($2 > 1) then error "x out of range";
			if ($4 < -1) || ($4 > 1) then error "x out of range";
			($2, $4)
		}
;

valeurs:
	cell
		{
			printf "[%d, %d]\n" (fst $1) (snd $1);
			CELL (0, fst $1, snd $1) 
		}
|	INT
		{ 
			printf "%d\n"$1;
			CST $1
		}
| 	ID
    	{
			printf "%s\n"$1;
			if get_var $1 != -1 then VAR(get_var $1)
			else error "Register not assigned"
		}
|	LPAR expression RPAR
		{$2}
|	SUB valeurs
		{
			BINOP (OP_SUB, CST(0), $2)
		}
|	ADD valeurs
		{
			$2
		}
;

expression2:
	valeurs
		{
			$1
		}
|	expression2 MULT valeurs
		{
			printf "*\n";
			BINOP(OP_MUL,$1,$3);
		}
|	expression2 DIV valeurs
		{
			printf "/\n";
			BINOP(OP_DIV,$1,$3);
		}
|	expression2 MOD valeurs
		{
			printf "mod\n";
			BINOP(OP_MOD,$1,$3);
		}
; 

expression:
	expression2
		{$1}
|	expression ADD expression2
		{
			printf "+\n";
			BINOP(OP_ADD,$1,$3);
		}
|	expression SUB expression2
		{
			printf "-\n";
			BINOP(OP_SUB,$1,$3);
		}
;

