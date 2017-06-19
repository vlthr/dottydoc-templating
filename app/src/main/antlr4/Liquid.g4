grammar Liquid;

block : node*;

node : tag
     | output;

tag : ifTag
    | TAGSTART FORSTART id IN expr FOREND TAGEND
    | TAGSTART id TAGEND;

ifStart : TAGSTART IFSTART expr TAGEND;
ifTag : ifStart node* block ifEnd;
ifEnd : TAGSTART IFEND TAGEND;

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

args : ':' arglist;
arglist : expr (',' expr)*;

FILTER     : '|';
DOTINDEX   : '.';
STARTINDEX : '[';
ENDINDEX   : ']';
AND        : 'And';
ASSIGN     : 'assign';
ELSE       : 'else';
ELSIF      : 'elsif';
FOREND     : 'endfor';
FORSTART   : 'for';
IFEND      : 'endif';
IFSTART    : 'if';
TRUE       : 'true';
FALSE      : 'false';
IN         : 'in';

OUTPUTSTART : '{{';
OUTPUTEND   : '}}' ;
TAGSTART    : '{%';
TAGEND      : '%}';

// parser rules start with lowercase letters, lexer rules with uppercase
ID          : IDSTARTCHAR IDBODYCHAR*;
INT         : '0'..'9'+;
IDSTARTCHAR : (LETTER | '_');
IDBODYCHAR  : (LETTER | '_' | '-' | DIGIT);

fragment LETTER : 'a'..'z' | 'A'..'Z';
fragment DIGIT  : '0'..'9';
STRSINGLE   : '\'' ~'\''* '\'';
STRDOUBLE   : '"' ~'"'* '"';
WS  :   [ \t\r\n]+ -> skip; // Define whitespace rule, toss it out
