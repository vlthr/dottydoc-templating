parser grammar LiquidParser;

options {tokenVocab = LiquidLexer;}

template : block EOF;

block : node*;

node : tag
     | output
     | TEXT;

tag : ifTag
    | forTag
    | assignTag
    | includeTag
    | rawTag;

rawTag : TAGSTART RAWSTART TAGEND non_tag_start TAGSTART RAWEND TAGEND;

non_tag_start : ~TAGSTART*;

assignTag : TAGSTART ASSIGN id EQUALS expr TAGEND;

ifStart : TAGSTART IFSTART expr TAGEND;
ifTag : ifStart block elsif* els? ifEnd;
ifEnd : TAGSTART IFEND TAGEND;

elsif: TAGSTART ELSIF expr TAGEND block;
els: TAGSTART ELSE TAGEND block;

includeTag : TAGSTART INCLUDE expr TAGEND;

forStart : TAGSTART FORSTART id IN output_expr TAGEND;
forTag : forStart block forEnd;
forEnd : TAGSTART FOREND TAGEND;

output : OUTPUTSTART output_expr OUTPUTEND;

output_expr : output_expr FILTER id args?
            | expr;

expr : expr DOTINDEX id
| expr STARTINDEX expr ENDINDEX
| expr EQ expr
| expr NEQ expr
| expr LT expr
| expr GT expr
| expr LEQ expr
| expr GEQ expr
| expr AND expr
| expr OR expr
| term;

term : INT
     | STRDOUBLE
     | STRSINGLE
     | TRUE
     | FALSE
     | id;

id : ID;

args : COLON arglist;
arglist : expr (COMMA expr)*;