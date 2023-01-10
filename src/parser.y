%start Program

%epp ADD "+"
%epp SUB "-"
%epp MULT "*"
%epp DIV "/"
%epp EQ "="
%epp BEGIN "begin"
%epp END "end"
%epp READ "read"
%epp WRITE "write"
%epp NUM "integer"
%epp VAR "variable"

%token "("
%token ")"
%token ";"
%left "ADD" "SUB"
%left "MULT" "DIV"

%%
Program -> Result<Tnode,Box<dyn Error>>:
      "BEGIN" Slist "END" { $2 }
    | "BEGIN" "END" { Ok(Tnode::Empty) }
    ;

Slist -> Result<Tnode,Box<dyn Error>>:
      Slist Stmt { Ok(Tnode::Connector{span: $span, left: Box::new($1?), right: Box::new($2?),}) }
    | Stmt { $1 }
    ;

Stmt -> Result<Tnode,Box<dyn Error>>:
      InputStmt { $1 }
    | OutputStmt { $1 }
    | AsgStmt { $1 }
    ;

InputStmt -> Result<Tnode,Box<dyn Error>>:
      "READ" "(" V ")" ";" { Ok(Tnode::Read{span: $span, var: Box::new($3?), }) }
    ;

OutputStmt -> Result<Tnode,Box<dyn Error>>:
      "WRITE" "(" E ")" ";" { Ok(Tnode::Write{span: $span, expression: Box::new($3?), }) }
    ;

AsgStmt -> Result<Tnode,Box<dyn Error>>:
      V "EQ" E ";" { Ok( make_operator_node(Op::Eq, $span, $1?, $3?) ) }
    ;

E -> Result<Tnode,Box<dyn Error>>:
      E "ADD" E { Ok( make_operator_node(Op::Add, $span, $1?, $3?) ) }
    | E "SUB" E { Ok( make_operator_node(Op::Sub, $span, $1?, $3?) ) }
    | E "MULT" E { Ok( make_operator_node(Op::Mult, $span, $1?, $3?) ) }
    | E "DIV" E { Ok( make_operator_node(Op::Div, $span, $1?, $3?) ) }
    | "(" E ")" { $2 }
    | V { $1 }
    | "NUM" { Ok(Tnode::Constant{span: $span,}) }
    ;

V -> Result<Tnode,Box<dyn Error>>:
    "VAR" { Ok(Tnode::Var{span: $span, name: $lexer.span_str($1.as_ref().unwrap().span()).to_string()}) }
    ;

Unmatched -> ():
  "UNMATCHED" { }
  ;

%%

// Any functions here are in scope for all the grammar actions above.

use myexpl::*;
use std::error::Error;
