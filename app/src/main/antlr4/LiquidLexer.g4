lexer grammar LiquidLexer;

OUTPUTSTART : '{{' -> pushMode(Object);
TAGSTART    : '{%' -> pushMode(Object);
fragment NO_OUTPUT: ( ~'{' | ( '{' ~[/{]) );
fragment NO_TAG: ( ~'{' | ( '{' ~[/%]) );
TEXT : (NO_OUTPUT | NO_TAG)+;

mode Object;
FILTER     : '|';
DOTINDEX   : '.';
STARTINDEX : '[';
ENDINDEX   : ']';
ASSIGN     : 'assign';
AND        : 'and';
ELSE       : 'else';
ELSIF      : 'elsif';
FOREND     : 'endfor';
FORSTART   : 'for';
IFEND      : 'endif';
IFSTART    : 'if';
TRUE       : 'true';
FALSE      : 'false';
IN         : 'in';
EQUALS     : '=';
COLON      : ':';
COMMA      : ',';

OUTPUTEND   : '}}' -> popMode;
TAGEND      : '%}' -> popMode;

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