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
%epp MAIN     "main"
%epp RETURN   "return"
%epp DECL     "decl"
%epp ENDDECL  "enddecl"
%epp INT_T    "int"
%epp STRING_T "string"
%epp STRING_C "string_const"

%token "(" ")" "," ";" "[" "]" "&" "{" "}"
%nonassoc "==" "!=" "<" ">" "<=" ">="
%left "+" "-"
%left "*" "/" "%"

%%
Program -> Result<(LinkedList<FnAst>, i16), (Option<Span>, &'static str)>:
      GDeclaration FDefBlock MainFn    { $1?; let mut $2 = $2?; $2.push_back($3?); Ok(($2, PARSER.lock().unwrap().gst().get_size())) }
    | GDeclaration MainFn              { $1?; Ok((LinkedList::from([$2?]), PARSER.lock().unwrap().gst().get_size())) }
    ;

// GLOBAL DECLARATION GRAMMAR
GDeclaration -> Result<(), (Option<Span>, &'static str)>:
      "DECL" GDeclList "ENDDECL"  { $2 }
    | "DECL" "ENDDECL"            { Ok(()) }
    | /* Empty */                 { Ok(()) }
    ;

GDeclList -> Result<(), (Option<Span>, &'static str)>:
      GDeclList GDecl   { $1?; $2 }
    | GDecl             { $1 }
    ;

GDecl -> Result<(), (Option<Span>, &'static str)>:
      Type GSymbolList ";"    { insert_gst($2?, $1?, $lexer) }
    ;

GSymbolList -> Result<LinkedList<SymbolBuilder>, (Option<Span>, &'static str)>:
      GSymbolDef "," GSymbolList   { let mut $3 = $3?; $3.push_front($1?); Ok($3) }
    | GSymbolDef                   { Ok(LinkedList::from([$1?])) }
    ;

GSymbolDef -> Result<SymbolBuilder, (Option<Span>, &'static str)>:
      Id                               { let s = SymbolBuilder::new($1?.span(), true); Ok(s) }
    | Id SizeDef                       { let mut s = SymbolBuilder::new($1?.span(), true); s.dim($2?); Ok(s) }
    | "*" Id                           { let mut s = SymbolBuilder::new($2?.span(), true); s.ptr(true); Ok(s) }
    | Id "(" ParamList ")"             { let mut s = SymbolBuilder::new($1?.span(), true); s.params($3?); Ok(s) }
    | "*" Id  "(" ParamList ")"        { let mut s = SymbolBuilder::new($2?.span(), true); s.ptr(true).params($4?); Ok(s) }
    ;

SizeDef -> Result<Vec<i16>, (Option<Span>, &'static str)>:
      SizeDef "[" Num "]"     { let mut $1 = $1?; $1.push(parse_int($lexer, &$3?)? as i16); Ok($1) }
    | "[" Num "]"             { Ok(Vec::from([parse_int($lexer, &$2?)? as i16])) }
    ;

Type -> Result<Type, (Option<Span>, &'static str)>:
      "INT_T"         { Ok(Type::Int) }
    | "STRING_T"      { Ok(Type::Str) }
    ;

// FUNCTION DEFINITION GRAMMAR
FDefBlock -> Result<LinkedList<FnAst>, (Option<Span>, &'static str)>:
      FDefBlock FDef        { let mut $1 = $1?; $1.push_back($2?); Ok($1) }
    | FDef                  { Ok(LinkedList::from([$1?])) }
    ;

FDef -> Result<FnAst, (Option<Span>, &'static str)>:
      FType FName "(" Params ")" "{" LDeclaration Body ReturnStmt "}"    {
        $2?; $4?; $7?;
        Tnode::create_fn(
          $1?, $8?, $9?,
          Span::new($span.start(), $5.unwrap().span().end())
        )
      }
    ;

FType -> Result<Type, (Option<Span>, &'static str)>:
      Type         { $1 }
    | "*" Type     { Ok($2?.rref().unwrap()) }
    ;

FName -> Result<&'input str, (Option<Span>, &'static str)>:
      Id           {
        Ok(
          PARSER
            .lock()
            .unwrap()
            .update_state(
              $lexer.span_str($1?.span())
            )
            .map_err(
              |msg| (Some($1.unwrap().span()), msg)
            )?
        )
      }
    ;

LDeclaration -> Result<(), (Option<Span>, &'static str)>:
      "DECL" LDeclList "ENDDECL"  { $2 }
    | "DECL" "ENDDECL"            { Ok(()) }
    | /* Empty */                { Ok(()) }
    ;

LDeclList -> Result<(), (Option<Span>, &'static str)>:
      LDeclList LDecl   { $1?; $2 }
    | LDecl             { $1 }
    ;

LDecl -> Result<(), (Option<Span>, &'static str)>:
      Type LSymbolList ";"    { insert_lst($2?, $1?, $lexer) }
    ;

LSymbolList -> Result<LinkedList<SymbolBuilder>, (Option<Span>, &'static str)>:
      LSymbolDef "," LSymbolList   { let mut $3 = $3?; $3.push_front($1?); Ok($3) }
    | LSymbolDef                   { Ok(LinkedList::from([$1?])) }
    ;

LSymbolDef -> Result<SymbolBuilder, (Option<Span>, &'static str)>:
      Id              { let s = SymbolBuilder::new($1?.span(), false); Ok(s) }
    | "*" Id          { let mut s = SymbolBuilder::new($2?.span(), false); s.ptr(true); Ok(s) }
    ;

Params -> Result<(), (Option<Span>, &'static str)>:
      ParamList       { insert_args($1?, $span) }
    ;

ParamList -> Result<LinkedList<(Type, String)>, (Option<Span>, &'static str)>:
      Param "," ParamList      { let mut $3 = $3?; $3.push_front($1?); Ok($3) }
    | Param                    { Ok(LinkedList::from([$1?])) }
    ;

Param -> Result<(Type, String), (Option<Span>, &'static str)>:
      Type Id        { Ok(($1?, $lexer.span_str($2?.span()).to_string())) }
    | Type "*" Id    { Ok(($1?.rref().unwrap(), $lexer.span_str($3?.span()).to_string())) }
    ;

MainFn -> Result<FnAst, (Option<Span>, &'static str)>:
      FType Main "(" ")" "{" LDeclaration Body ReturnStmt "}"   {
        $2?; $6?;
        Tnode::create_main_block(
          $1?, $7?, $8?,
          Span::new($span.start(), $4.unwrap().span().end())
        )
      }
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
      "IF" "(" E ")" "THEN" Slist "ENDIF" ";"               { Tnode::create_if($span, $3?, $6?, None) }
    | "IF" "(" E ")" "THEN" Slist "ELSE" Slist "ENDIF" ";"  { Tnode::create_if($span, $3?, $6?, Some($8?)) }
    ;

WhileStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "WHILE" "(" E ")" "DO" Slist "ENDWHILE" ";"      { Tnode::create_while($span, $3?, $6?) }
    ;

RepeatStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "REPEAT" "DO" Slist "UNTIL" "(" E ")" ";"        { Tnode::create_repeat($span, $3?, $6?) }
    ;

InputStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "READ" "(" Var ")" ";"   { Tnode::create_read($span, $3?) }
    ;

OutputStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "WRITE" "(" E ")" ";"    { Tnode::create_write($span, $3?) }
    ;

BreakStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "BREAK" ";"       { Ok(Tnode::Break) }
    ;

ContinueStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "CONTINUE" ";"    { Ok(Tnode::Continue) }
    ;

ReturnStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "RETURN" E ";"    { Ok(Tnode::Return{exp: Box::new($2?), span: $span}) }
    ;

AsgStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      Var "=" E ";"     { Tnode::create_asg($span, $1?, $3?) }
    ;

E -> Result<Tnode, (Option<Span>, &'static str)>:
      E "+" E                 { Tnode::create_int(BinaryOpType::Add, $span, $1?, $3?) }
    | E "-" E                 { Tnode::create_int(BinaryOpType::Sub, $span, $1?, $3?) }
    | E "*" E                 { Tnode::create_int(BinaryOpType::Mul, $span, $1?, $3?) }
    | E "/" E                 { Tnode::create_int(BinaryOpType::Div, $span, $1?, $3?) }
    | E "%" E                 { Tnode::create_int(BinaryOpType::Mod, $span, $1?, $3?) }
    | E "==" E                { Tnode::create_bool(BinaryOpType::EQ, $span, $1?, $3?) }
    | E "!=" E                { Tnode::create_bool(BinaryOpType::NE, $span, $1?, $3?) }
    | E ">=" E                { Tnode::create_bool(BinaryOpType::GE, $span, $1?, $3?) }
    | E ">" E                 { Tnode::create_bool(BinaryOpType::GT, $span, $1?, $3?) }
    | E "<=" E                { Tnode::create_bool(BinaryOpType::LE, $span, $1?, $3?) }
    | E "<" E                 { Tnode::create_bool(BinaryOpType::LT, $span, $1?, $3?) }
    | "(" E ")"               { $2 }
    | Var                     { $1 }
    | "&" VarAccess           { Tnode::create_ref($span, $2?) }
    | Num                     { Tnode::create_constant($lexer, &$1?, Type::Int) }
    | String                  { Tnode::create_constant($lexer, &$1?, Type::Str) }
    | Id "(" ArgList ")"      { Tnode::create_fncall($lexer.span_str($1?.span()), $3?, $span) }
    ;

ArgList -> Result<LinkedList<Tnode>, (Option<Span>, &'static str)>:
      ArgList "," E       { let mut $1 = $1?; $1.push_back($3?); Ok($1) }
    | E                   { Ok(LinkedList::from([$1?])) }
    | /* Empty */         { Ok(LinkedList::new()) }
    ;

Var -> Result<Tnode, (Option<Span>, &'static str)>:
      VarAccess       { $1 }
    | "*" VarAccess   { Tnode::create_deref($span, $2?) }
    ;

VarAccess ->  Result<Tnode, (Option<Span>, &'static str)>:
      Id                         { get_variable($lexer, &$1?, Vec::new(), RefType::RHS) }
    | Id ArrayAccess             { get_variable($lexer, &$1?, check_access_vec($2?)?, RefType::RHS) }
    ;

ArrayAccess -> Result<Vec<Box<Tnode>>, (Option<Span>, &'static str)>:
      ArrayAccess "[" E "]"     { let mut $1 = $1?; $1.push(Box::new($3?)); Ok($1) }
    | "[" E "]"                 { Ok(vec![Box::new($2?)]) }
    ;

Main -> Result<DefaultLexeme<u32>, (Option<Span>, &'static str)>:
    "MAIN"     {
      PARSER.lock().unwrap().update_state("").unwrap();
      $1.map_err(|e| (Some(e.span()), "Faulty lexeme"))
    }
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
use myexpl::frontend::methods::*;
use myexpl::frontend::PARSER;
use myexpl::type_table::*;
use myexpl::symbol::*;
use std::collections::LinkedList;
