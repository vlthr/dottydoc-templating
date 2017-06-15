grammar Liquid;

node : tag
     | output;

tag : TAGSTART id TAGEND;

output : OUTPUTSTART expr OUTPUTEND;

expr : term;

term : INT
     | STRDOUBLE
     | STRSINGLE
     | TRUE
     | FALSE;

id : ID;

keyword : AND
        | ASSIGN
        | BREAK
        | CAPTUREEND
        | CAPTURESTART
        | CASEEND
        | CASESTART
        | COMMENTEND
        | COMMENTSTART
        | CONTAINS
        | CONTINUE
        | CYCLE
        | ELSE
        | ELSIF
        | EMPTY
        | ENDID
        | FALSE
        | FOREND
        | FORSTART
        | IFEND
        | IFSTART
        | IN
        | INCLUDE
        | NIL
        | OR
        | RAWEND
        | RAWSTART
        | TABLEEND
        | TABLESTART
        | TRUE
        | UNLESSEND
        | UNLESSSTART
        | WHEN
        | WITH;


AND : 'And';
ASSIGN : 'Assign';
BREAK : 'Break';
CAPTUREEND : 'CaptureEnd';
CAPTURESTART : 'CaptureStart';
CASEEND : 'CaseEnd';
CASESTART : 'CaseStart';
COMMENTEND : 'CommentEnd';
COMMENTSTART : 'CommentStart';
CONTAINS : 'contains';
CONTINUE : 'Continue';
CYCLE : 'Cycle';
ELSE : 'Else';
ELSIF : 'Elsif';
EMPTY : 'Empty';
ENDID : 'EndId';
FOREND : 'ForEnd';
FORSTART : 'ForStart';
IFEND : 'IfEnd';
IFSTART : 'IfStart';
IN : 'In';
INCLUDE : 'Include';
NIL : 'Nil';
OR : 'Or';
RAWEND : 'RawEnd';
RAWSTART : 'RawStart';
TABLEEND : 'TableEnd';
TABLESTART : 'TableStart';
TRUE : 'true';
FALSE : 'false';
UNLESSEND : 'UnlessEnd';
UNLESSSTART : 'UnlessStart';
WHEN : 'When';
WITH : 'With';

OUTPUTSTART : '{{';
OUTPUTEND   : '}}' ;
TAGSTART : '{%';
TAGEND   : '%}';

// parser rules start with lowercase letters, lexer rules with uppercase
ID : IDSTARTCHAR IDBODYCHAR*;
INT  : '0'..'9'+;
IDSTARTCHAR : (LETTER | '_');
IDBODYCHAR : (LETTER | '_' | '-' | DIGIT);

fragment LETTER : 'a'..'z' | 'A'..'Z';
fragment DIGIT  : '0'..'9';
STRSINGLE   : '\'' ~'\''* '\'';
STRDOUBLE   : '"' ~'"'* '"';
WS  :   [ \t\r\n]+ -> skip; // Define whitespace rule, toss it out
