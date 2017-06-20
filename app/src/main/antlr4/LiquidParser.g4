parser grammar LiquidParser;

options {tokenVocab = LiquidLexer;}

block : node*;

node : tag
     | output;

tag : ifTag
    | forTag
    | assignTag;

assignTag : TAGSTART ASSIGN id EQUALS expr TAGEND;

ifStart : TAGSTART IFSTART expr TAGEND;
ifTag : ifStart block ifEnd;
ifEnd : TAGSTART IFEND TAGEND;

forStart : TAGSTART FORSTART id IN expr TAGEND;
forTag : forStart node* block forEnd;
forEnd : TAGSTART FOREND TAGEND;

output : OUTPUTSTART expr OUTPUTEND;

expr : expr FILTER id args?
| expr DOTINDEX id
| expr STARTINDEX expr ENDINDEX
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