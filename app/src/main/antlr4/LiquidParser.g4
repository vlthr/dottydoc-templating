parser grammar LiquidParser;

options {tokenVocab = LiquidLexer;}

template : block EOF;

block : node*;

node : tag
     | output
     | TEXT;

tag : ifTag
    | caseTag
    | forTag
    | assignTag
    | includeTag
    | captureTag
    | commentTag
    | rawTag
    | customTag;

customTag : TAGSTART id arglist? TAGEND;

caseTag : TAGSTART CASESTART expr TAGEND any whenBlock+ els? TAGSTART CASEEND TAGEND;

whenBlock : TAGSTART WHEN expr TAGEND block;

rawTag : TAGSTART RAWSTART TAGEND any TAGSTART RAWEND TAGEND;

commentTag : TAGSTART COMMENTSTART TAGEND block TAGSTART COMMENTEND TAGEND;

captureTag : TAGSTART CAPTURESTART id TAGEND block TAGSTART CAPTUREEND TAGEND;

any : .*?;

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

output_expr : output_expr FILTER id (COLON arglist? kwargs?)?
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

kwarg: id COLON expr;

kwargs: kwarg+;

term : INT
     | STRDOUBLE
     | STRSINGLE
     | TRUE
     | FALSE
     | id;

id : ID;

arglist : expr (COMMA expr)*;