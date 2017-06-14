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

// parser rules start with lowercase letters, lexer rules with uppercase
WS  :   [ \t\r\n]+ -> skip ; // Define whitespace rule, toss it out
INT  : '0'..'9'+;
