%start Program

%epp ADD      "+"
%epp SUB      "-"
%epp MULT     "*"
%epp DIV      "/"
%epp MOD      "%"
%epp ASGN     "="
%epp EQ       "=="
%epp NE       "!="
%epp GT       ">"
%epp LT       "<"
%epp GE       ">="
%epp LE       "<="
%epp BEGIN    "begin"
%epp END      "end"
%epp READ     "read"
%epp WRITE    "write"
%epp NUM      "integer"
%epp VAR      "variable"
%epp IF       "if"
%epp THEN     "then"
%epp ELSE     "else"
%epp ENDIF    "endif"
%epp DO       "do"
%epp WHILE    "while"
%epp ENDWHILE "endwhile"
%epp REPEAT   "repeat"
%epp UNTIL    "until"
%epp CONTINUE "continue"
%epp BREAK    "break"
%epp DECL     "decl"
%epp ENDDECL  "enddecl"
%epp INT_T    "int"
%epp STRING_T "string"

%token "(" ")" "," ";"
%nonassoc "EQ" "NE" "LT" "GT" "LE" "GE"
%left "ADD" "SUB"
%left "MULT" "DIV" "MOD"

%%
Program -> Result<Tnode, (Option<Span>, &'static str)>:
      Declaration Body   { $1?; $2 }
    | Body               { $1 }
    ;

Declaration -> Result<(), (Option<Span>, &'static str)>:
      "DECL" DeclList "ENDDECL"  { $2 }
    | "DECL" "ENDDECL"           { Ok(()) }
    ;

DeclList -> Result<(), (Option<Span>, &'static str)>:
      DeclList Decl { $1?; $2 }
    | Decl          { $1 }
    ;

Decl -> Result<(), (Option<Span>, &'static str)>:
      Type VarList ";"    { insert_variables($lexer, $1?, $2?) }
    ;

VarList -> Result<LinkedList<Span>, (Option<Span>, &'static str)>:
      Id "," VarList   { let mut $3 = $3?; $3.push_front($1?.span()); Ok($3) }
    | Id               { Ok(LinkedList::from([$span])) }
    ;

Type -> Result<Type, (Option<Span>, &'static str)>:
      "INT_T"         { Ok(Type::Int) }
    | "STRING_T"      { Ok(Type::String) }
    ;

Body -> Result<Tnode, (Option<Span>, &'static str)>:
      "BEGIN" Slist "END" { $2 }
    | "BEGIN" "END"       { Ok(Tnode::Empty) }
    ;

Slist -> Result<Tnode, (Option<Span>, &'static str)>:
      Slist Stmt  { Ok(Tnode::Connector{left: Box::new($1?), right: Box::new($2?),}) }
    | Stmt        { $1 }
    ;

Stmt -> Result<Tnode, (Option<Span>, &'static str)>:
      InputStmt     { $1 }
    | OutputStmt    { $1 }
    | AsgStmt       { $1 }
    | BreakStmt     { $1 }
    | ContinueStmt  { $1 }
    | IfStmt        { $1 }
    | WhileStmt     { $1 }
    | RepeatStmt    { $1 }
    ;

IfStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "IF" "(" E ")" "THEN" Slist "ENDIF" ";"               { create_if_node($span, $3?, $6?, None) }
    | "IF" "(" E ")" "THEN" Slist "ELSE" Slist "ENDIF" ";"  { create_if_node($span, $3?, $6?, Some($8?)) }
    ;

WhileStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "WHILE" "(" E ")" "DO" Slist "ENDWHILE" ";"      { create_while_node($span, $3?, $6?) }
    ;

RepeatStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "REPEAT" "DO" Slist "UNTIL" "(" E ")" ";"        { create_repeat_node($span, $3?, $6?) }
    ;

InputStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "READ" "(" Id ")" ";"   { create_read_node($lexer, &$3?) }
    ;

OutputStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "WRITE" "(" E ")" ";"   { create_write_node($span, $3?) }
    ;

BreakStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "BREAK" ";"       { Ok(Tnode::Break) }
    ;

ContinueStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "CONTINUE" ";"    { Ok(Tnode::Continue) }
    ;

AsgStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      Id "ASGN" E ";"   {
          create_asg_node (
            $span,
            get_variable($lexer, &$1?)?,
            $3?
          )
      }
    ;

E -> Result<Tnode, (Option<Span>, &'static str)>:
      E "ADD" E     { create_int_node(Op::Add, $span, $1?, $3?) }
    | E "SUB" E     { create_int_node(Op::Sub, $span, $1?, $3?) }
    | E "MULT" E    { create_int_node(Op::Mult, $span, $1?, $3?) }
    | E "DIV" E     { create_int_node(Op::Div, $span, $1?, $3?) }
    | E "MOD" E     { create_int_node(Op::Mod, $span, $1?, $3?) }
    | E "EQ" E      { create_bool_node(Op::EQ, $span, $1?, $3?) }
    | E "NE" E      { create_bool_node(Op::NE, $span, $1?, $3?) }
    | E "GE" E      { create_bool_node(Op::GE, $span, $1?, $3?) }
    | E "GT" E      { create_bool_node(Op::GT, $span, $1?, $3?) }
    | E "LE" E      { create_bool_node(Op::LE, $span, $1?, $3?) }
    | E "LT" E      { create_bool_node(Op::LT, $span, $1?, $3?) }
    | "(" E ")"     { $2 }
    | "NUM"         { create_constant_node($lexer, $1.as_ref().map_err(|e| (Some(e.span()), "Faulty lexeme"))?) }
    | Id            { get_variable($lexer, &$1?) }
    ;

Id -> Result<DefaultLexeme<u32>, (Option<Span>, &'static str)>:
    "VAR"     { $1.map_err(|e| (Some(e.span()), "Faulty lexeme")) }
    ;

Unmatched -> ():
  "UNMATCHED" { }
  ;

%%

// Any functions here are in scope for all the grammar actions above.

use lrlex::DefaultLexeme;
use lrpar::Span;
use myexpl::utils::node::*;
use myexpl::ast::*;
use std::collections::LinkedList;
