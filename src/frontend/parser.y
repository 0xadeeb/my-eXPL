%start Program

%avoid_insert "NUM"
%avoid_insert "STRING_C"
%avoid_insert "VAR"
%avoid_insert "STRING_T"
%avoid_insert "INT_T"

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
%epp STRING_C "string_const"

%token "(" ")" "," ";" "[" "]" "&"
%nonassoc "==" "!=" "<" ">" "<=" ">="
%left "+" "-"
%left "*" "/" "%"

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
      Type VarList ";"    { insert_varlist($2?, $1?, $lexer) }
    ;

VarList -> Result<LinkedList<SymbolBuilder>, (Option<Span>, &'static str)>:
      VarDef "," VarList   { let mut $3 = $3?; $3.push_front($1?); Ok($3) }
    | VarDef               { Ok(LinkedList::from([$1?])) }
    ;

VarDef -> Result<SymbolBuilder, (Option<Span>, &'static str)>:
      Id                  { let s = SymbolBuilder::new($1?.span()); Ok(s) }
    | "*" Id              { let mut s = SymbolBuilder::new($2?.span()); s.ptr(true); Ok(s) }
    | Id SizeDef          { let mut s = SymbolBuilder::new($1?.span()); s.dim($2?); Ok(s) }
    ;

SizeDef -> Result<Vec<u16>, (Option<Span>, &'static str)>:
      SizeDef "[" Num "]"     { let mut $1 = $1?; $1.push(parse_int($lexer, &$3?)? as u16); Ok($1) }
    | "[" Num "]"             { Ok(Vec::from([parse_int($lexer, &$2?)? as u16])) }
    ;

Type -> Result<Type, (Option<Span>, &'static str)>:
      "INT_T"         { Ok(Type::Int) }
    | "STRING_T"      { Ok(Type::Str) }
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
      "READ" "(" Var ")" ";"   { create_read_node($span, $3?) }
    ;

OutputStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "WRITE" "(" E ")" ";"        { create_write_node($span, $3?) }
    ;

BreakStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "BREAK" ";"       { Ok(Tnode::Break) }
    ;

ContinueStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "CONTINUE" ";"    { Ok(Tnode::Continue) }
    ;

AsgStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      Var "=" E ";"               { create_asg_node($span, $1?, $3?) }
    ;

E -> Result<Tnode, (Option<Span>, &'static str)>:
      E "+" E         { create_int_node(BinaryOpType::Add, $span, $1?, $3?) }
    | E "-" E         { create_int_node(BinaryOpType::Sub, $span, $1?, $3?) }
    | E "*" E         { create_int_node(BinaryOpType::Mul, $span, $1?, $3?) }
    | E "/" E         { create_int_node(BinaryOpType::Div, $span, $1?, $3?) }
    | E "%" E         { create_int_node(BinaryOpType::Mod, $span, $1?, $3?) }
    | E "==" E        { create_bool_node(BinaryOpType::EQ, $span, $1?, $3?) }
    | E "!=" E        { create_bool_node(BinaryOpType::NE, $span, $1?, $3?) }
    | E ">=" E        { create_bool_node(BinaryOpType::GE, $span, $1?, $3?) }
    | E ">" E         { create_bool_node(BinaryOpType::GT, $span, $1?, $3?) }
    | E "<=" E        { create_bool_node(BinaryOpType::LE, $span, $1?, $3?) }
    | E "<" E         { create_bool_node(BinaryOpType::LT, $span, $1?, $3?) }
    | "(" E ")"       { $2 }
    | Var             { $1 }
    | "&" VarAccess   { create_ref($span, $2?) }
    | Num             { create_constant_node($lexer, &$1?, Type::Int) }
    | String          { create_constant_node($lexer, &$1?, Type::Str) }
    ;

Var -> Result<Tnode, (Option<Span>, &'static str)>:
      VarAccess       { $1 }
    | "*" VarAccess   { create_deref($span, $2?) }
    ;

VarAccess ->  Result<Tnode, (Option<Span>, &'static str)>:
      Id                         { get_variable($lexer, &$1?, Vec::new(), RefType::RHS) }
    | Id ArrayAccess             { get_variable($lexer, &$1?, check_access_vec($2?)?, RefType::RHS) }
    ;

ArrayAccess -> Result<Vec<Box<Tnode>>, (Option<Span>, &'static str)>:
      ArrayAccess "[" E "]"     { let mut $1 = $1?; $1.push(Box::new($3?)); Ok($1) }
    | "[" E "]"                 { Ok(vec![Box::new($2?)]) }
    ;

Id -> Result<DefaultLexeme<u32>, (Option<Span>, &'static str)>:
    "VAR"          { $1.map_err(|e| (Some(e.span()), "Faulty lexeme")) }
    ;

Num -> Result<DefaultLexeme<u32>, (Option<Span>, &'static str)>:
    "NUM"          { $1.map_err(|e| (Some(e.span()), "Faulty lexeme")) }
    ;

String -> Result<DefaultLexeme<u32>, (Option<Span>, &'static str)>:
    "STRING_C"     { $1.map_err(|e| (Some(e.span()), "Faulty lexeme")) }
    ;

Unmatched -> ():
  "UNMATCHED" { }
  ;

%%

// Any functions here are in scope for all the grammar actions above.

use lrlex::DefaultLexeme;
use lrpar::Span;
use myexpl::ast::*;
use myexpl::symbol_table::*;
use myexpl::utils::node::*;
use std::collections::LinkedList;
