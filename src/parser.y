%start E

%epp ADD "+"
%epp SUB "-"
%epp MULT "*"
%epp DIV "/"
%epp NUM "Integer"

%token '('
%token ')'
%left 'ADD' 'SUB'
%left 'MULT' 'DIV'

%%
E -> Result<Tnode,Box<dyn Error>>:
      E 'ADD' E { Ok( Tnode::Operator{op: Op::Add, span: $span, left: Box::new($1?), right: Box::new($3?),} ) }
    | E 'SUB' E { Ok( Tnode::Operator{op: Op::Sub, span: $span, left: Box::new($1?), right: Box::new($3?),} ) }
    | E 'MULT' E { Ok( Tnode::Operator{op: Op::Mult, span: $span, left: Box::new($1?), right: Box::new($3?),} ) }
    | E 'DIV' E { Ok( Tnode::Operator{op: Op::Div, span: $span, left: Box::new($1?), right: Box::new($3?),} ) }
    | '(' E ')' { $2 }
    | 'NUM' { Ok(Tnode::Constant{span: $span}) }
    ;

Unmatched -> ():
  "UNMATCHED" { }
  ;

%%

// Any functions here are in scope for all the grammar actions above.

use myexpl::*;
use std::error::Error;
