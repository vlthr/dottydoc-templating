parser grammar LiquidParser;

options {tokenVocab = LiquidLexer;}

block : node*;

node : tag
     | output
     | TEXT;

tag : ifTag
    | forTag
    | assignTag;

assignTag : TAGSTART ASSIGN id EQUALS expr TAGEND;

ifStart : TAGSTART IFSTART expr TAGEND;
ifTag : ifStart block elsif* els? ifEnd;
ifEnd : TAGSTART IFEND TAGEND;

elsif: TAGSTART ELSIF expr TAGEND block;
els: TAGSTART ELSE TAGEND block;

forStart : TAGSTART FORSTART id IN expr TAGEND;
forTag : forStart block forEnd;
forEnd : TAGSTART FOREND TAGEND;

output : OUTPUTSTART expr OUTPUTEND;

expr : expr FILTER id args?
| expr DOTINDEX id
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