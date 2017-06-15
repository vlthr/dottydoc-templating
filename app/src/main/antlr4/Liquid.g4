/** Grammars always start with a grammar header. This grammar is called
 *  ArrayInit and must match the filename: ArrayInit.g4
 */
grammar Liquid;

/** A rule called init that matches comma-separated values between {...}. */
init  : '{' value (',' value)* '}' ;  // must match at least one value

/** A value can be either a nested array/struct or a simple integer (INT) */
value : init
      | INT
      ;

expr : term;

term : INT;

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


// parser rules start with lowercase letters, lexer rules with uppercase
ID : IDSTARTCHAR IDBODYCHAR*;
INT  : '0'..'9'+;
IDSTARTCHAR : (LETTER | '_');
IDBODYCHAR : (LETTER | '_' | '-' | DIGIT);

fragment LETTER : 'a'..'z' | 'A'..'Z';
fragment DIGIT  : '0'..'9';
STRSINGLE   : '\'' ~'\''* '\'';
STRDOUBLE   : '"' ~'"'* '"';
WS  :   [ \t\r\n]+ -> skip ; // Define whitespace rule, toss it out

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
FALSE : 'False';
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
TRUE : 'True';
UNLESSEND : 'UnlessEnd';
UNLESSSTART : 'UnlessStart';
WHEN : 'When';
WITH : 'With';