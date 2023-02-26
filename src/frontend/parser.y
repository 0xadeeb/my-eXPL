%start Program

%avoid_insert   "NUM"
%avoid_insert   "STRING_C"
%avoid_insert   "VAR"
%avoid_insert   "STRING_T"
%avoid_insert   "INT_T"

%epp BEGIN      "begin"
%epp END        "end"
%epp READ       "read"
%epp WRITE      "write"
%epp NUM        "integer"
%epp VAR        "variable"
%epp IF         "if"
%epp THEN       "then"
%epp ELSE       "else"
%epp ENDIF      "endif"
%epp DO         "do"
%epp WHILE      "while"
%epp ENDWHILE   "endwhile"
%epp REPEAT     "repeat"
%epp UNTIL      "until"
%epp CONTINUE   "continue"
%epp BREAK      "break"
%epp MAIN       "main"
%epp RETURN     "return"
%epp DECL       "decl"
%epp ENDDECL    "enddecl"
%epp TYPE       "type"
%epp ENDTYPE    "endtype"
%epp CLASS      "type"
%epp ENDCLASS   "endtype"
%epp EXTENDS    "extends"
%epp NEW        "new"
%epp DELETE     "delete"
%epp SELF       "self"
%epp INT_T      "int"
%epp STRING_T   "string"
%epp STRING_C   "string_const"

%token "(" ")" "," ";" "[" "]" "&" "{" "}" "." "NULL"
%left "&&" "||"
%left "==" "!="
%left "<" ">" "<=" ">="
%left "+" "-"
%left "*" "/" "%"

%%
// START
Program -> Result<(LinkedList<FnAst>, LinkedList<Vec<u8>>, i16), (Option<Span>, &'static str)>:
      TypeDefBlock ClassDefBlock GDeclaration Functions    { $1?; Ok(($4?, $2?, $3?)) }
    ;

// TYPE DECLARATION GRAMMAR
TypeDefBlock -> Result<(), (Option<Span>, &'static str)>:
      "TYPE" TypeDefList "ENDTYPE"    { $2 }
    | "TYPE" "ENDTYPE"                { Ok(()) }
    | /* Empty */                     { Ok(()) }
    ;

TypeDefList -> Result<(), (Option<Span>, &'static str)>:
      TypeDefList TypeDef   { $1?; $2 }
    | TypeDef               { $1 }
    ;

TypeDef -> Result<(), (Option<Span>, &'static str)>:
      Id "{" FieldDeclList "}"      { PARSER.lock().unwrap().tt().insert_struct($lexer, $1?.span(), $3?) }
    ;

// CLASS DECLARATION GRAMMAR
ClassDefBlock -> Result<LinkedList<Vec<u8>>,  (Option<Span>, &'static str)>:
       "CLASS" ClassDefList "ENDCLASS"   { $2 }
     | "CLASS" "ENDCLASS"                { Ok(LinkedList::new()) }
     | /* Empty */                       { Ok(LinkedList::new()) }
     ;

ClassDefList -> Result<LinkedList<Vec<u8>>,  (Option<Span>, &'static str)>:
       ClassDefList ClassDef       { let mut $1 = $1?; $1.push_back($2?); Ok($1) }
     | ClassDef                    { Ok(LinkedList::from([$1?])) }
     ;

ClassDef -> Result<Vec<u8>, (Option<Span>, &'static str)>:
       Cname "{" CDecl MethodDefns "}"     { $1?; $4?; $3 }
     ;

Cname -> Result<(), (Option<Span>, &'static str)>:
       Id                  { PARSER.lock().unwrap().set_class($lexer, $1?.span(), None) }
     | Id "EXTENDS" Id     { PARSER.lock().unwrap().set_class($lexer, $1?.span(), Some($3?.span())) }
     ;

CDecl -> Result<Vec<u8>, (Option<Span>, &'static str)>:
      "DECL" MethodDeclList "ENDDECL"                   { PARSER.lock().unwrap().insert_cst($2?, LinkedList::new()) }
    | "DECL" FieldDeclList MethodDeclList "ENDDECL"     { PARSER.lock().unwrap().insert_cst($3?, $2?) }
    ;

FieldDeclList -> Result<LinkedList<(Span, Span)>, (Option<Span>, &'static str)>:
      FieldDeclList FieldDecl       { let mut $1 = $1?; $1.push_back($2?); Ok($1) }
    | FieldDecl                     { Ok(LinkedList::from([$1?])) }
    ;

FieldDecl -> Result<(Span, Span), (Option<Span>, &'static str)>:
      TypeDefName Id ";"            { Ok(($1?, $2?.span())) }
    ;

MethodDeclList -> Result<LinkedList<(Type, Span, LinkedList<(Type, String)>)>,  (Option<Span>, &'static str)>:
      MethodDeclList MethodDecl    { let mut $1 = $1?; $1.push_back($2?); Ok($1) }
    | MethodDecl                   { Ok(LinkedList::from([$1?])) }
    ;

MethodDecl -> Result<(Type, Span, LinkedList<(Type, String)>),  (Option<Span>, &'static str)>:
      TypeDefName Id "(" ParamList ")" ";"  { Ok(($1?, $2?.span(), $4?)) }
    ;

TypeDefName -> Result<Span,  (Option<Span>, &'static str)>:
      "INT_T"               { Ok($1.as_ref().unwrap().span()) }
    | "STRING_T"            { Ok($1.as_ref().unwrap().span()) }
    | Id                    { Ok($1?.span()) }
    ;

MethodDefns -> Result<(),  (Option<Span>, &'static str)>:
      MethodDefns FDef   { PARSER.lock().unwrap().insert_fns($2?) }
    | FDef               { PARSER.lock().unwrap().insert_fns($1?) }
    ;

// GLOBAL DECLARATION GRAMMAR
GDeclaration -> Result<i16, (Option<Span>, &'static str)>:
      "DECL" GDeclList "ENDDECL"  { $2?; Ok(PARSER.lock().unwrap().gst().get_size()) }
    | "DECL" "ENDDECL"            { Ok(0) }
    | /* Empty */                 { Ok(0) }
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
    | "*" Id                           { let mut s = SymbolBuilder::new($2?.span(), true); s.ptr(); Ok(s) }
    | Id "(" ParamList ")"             { let mut s = SymbolBuilder::new($1?.span(), true); s.params($3?); Ok(s) }
    | "*" Id  "(" ParamList ")"        { let mut s = SymbolBuilder::new($2?.span(), true); s.ptr().params($4?); Ok(s) }
    ;

SizeDef -> Result<Vec<i16>, (Option<Span>, &'static str)>:
      SizeDef "[" Num "]"     { let mut $1 = $1?; $1.push(parse_int($lexer, &$3?)? as i16); Ok($1) }
    | "[" Num "]"             { Ok(Vec::from([parse_int($lexer, &$2?)? as i16])) }
    ;

Type -> Result<Type, (Option<Span>, &'static str)>:
      "INT_T"         { Ok(Type::Primitive(PrimitiveType::Int)) }
    | "STRING_T"      { Ok(Type::Primitive(PrimitiveType::Str)) }
    | Id              {
      let span = $1?.span();
      PARSER
        .lock()
        .unwrap()
        .tt()
        .get($lexer.span_str(span))
        .ok_or((Some(span), "Type not defined"))
    }
    ;

// FUNCTION DEFINITION GRAMMAR
Functions ->  Result<LinkedList<FnAst>, (Option<Span>, &'static str)>:
      FDefBlock MainFn    { let mut $1 = $1?; $1.push_back($2?); Ok(PARSER.lock().unwrap().insert_fns($1)) }
    | MainFn              { Ok(PARSER.lock().unwrap().insert_fns($1?)) }
    ;

FDefBlock -> Result<LinkedList<FnAst>, (Option<Span>, &'static str)>:
      FDefBlock FDef        { let mut $1 = $1?; $1.push_back($2?); Ok($1) }
    | FDef                  { Ok(LinkedList::from([$1?])) }
    ;

FDef -> Result<FnAst, (Option<Span>, &'static str)>:
      FType FName "(" Params ")" "{" LDeclaration Body ReturnStmt "}"    {
        $2?; $4?; $7?;
        create_fn(
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
    | "*" Id          { let mut s = SymbolBuilder::new($2?.span(), false); s.ptr(); Ok(s) }
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
        create_main_block(
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

// STATEMENTS GRAMMAR
Stmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "IF" "(" E ")" "THEN" Slist "ENDIF" ";"                 { create_if($span, $3?, $6?, None) }
    | "IF" "(" E ")" "THEN" Slist "ELSE" Slist "ENDIF" ";"    { create_if($span, $3?, $6?, Some($8?)) }
    | "WHILE" "(" E ")" "DO" Slist "ENDWHILE" ";"             { create_while($span, $3?, $6?) }
    | "REPEAT" "DO" Slist "UNTIL" "(" E ")" ";"               { create_repeat($span, $3?, $6?) }
    | "READ" "(" Var ")" ";"                                  { create_read($span, $3?) }
    | "WRITE" "(" E ")" ";"                                   { create_write($span, $3?) }
    | "BREAK" ";"                                             { Ok(Tnode::Break) }
    | "CONTINUE" ";"                                          { Ok(Tnode::Continue) }
    | Var "=" E ";"                                           { create_asg($span, $1?, $3?) }
    | "INIT" "(" ")" ";"                                      { Ok(Tnode::Initialize) }
    | Var "=" "ALLOC" "(" ")" ";"                             { create_alloc($span, $1?) }
    | "FREE" "(" Var ")" ";"                                  { create_free($span, $3?) }
    | Var "=" "NEW" "(" Id ")" ";"                            { Err((None, "nothing")) }
    | "DELETE" "(" Var ")" ";"                                { Err((None, "nothing")) }
    ;

ReturnStmt -> Result<Tnode, (Option<Span>, &'static str)>:
      "RETURN" E ";"    { create_return($span, $2?) }
    ;

// EXPRESSION GRAMMAR
E -> Result<Tnode, (Option<Span>, &'static str)>:
      E "+" E                 { create_int(BinaryOpType::Add, $span, $1?, $3?) }
    | E "-" E                 { create_int(BinaryOpType::Sub, $span, $1?, $3?) }
    | E "*" E                 { create_int(BinaryOpType::Mul, $span, $1?, $3?) }
    | E "/" E                 { create_int(BinaryOpType::Div, $span, $1?, $3?) }
    | E "%" E                 { create_int(BinaryOpType::Mod, $span, $1?, $3?) }
    | E "==" E                { create_bool(BinaryOpType::EQ, $span, $1?, $3?) }
    | E "!=" E                { create_bool(BinaryOpType::NE, $span, $1?, $3?) }
    | E ">=" E                { create_bool(BinaryOpType::GE, $span, $1?, $3?) }
    | E ">" E                 { create_bool(BinaryOpType::GT, $span, $1?, $3?) }
    | E "<=" E                { create_bool(BinaryOpType::LE, $span, $1?, $3?) }
    | E "<" E                 { create_bool(BinaryOpType::LT, $span, $1?, $3?) }
    | E "&&" E                { create_logical_op(BinaryOpType::AND, $span, $1?, $3?) }
    | E "||" E                { create_logical_op(BinaryOpType::OR, $span, $1?, $3?) }
    | "(" E ")"               { $2 }
    | Var                     { $1 }
    | "&" VarAccess           { create_ref($span, $2?) }
    | Num                     { create_constant($lexer, &$1?, Type::Primitive(PrimitiveType::Int)) }
    | String                  { create_constant($lexer, &$1?, Type::Primitive(PrimitiveType::Str)) }
    | FnCall                  { $1 }
    | "NULL"                  { Ok(Tnode::Null) }
    ;

ArgList -> Result<LinkedList<Tnode>, (Option<Span>, &'static str)>:
      ArgList "," E       { let mut $1 = $1?; $1.push_back($3?); Ok($1) }
    | E                   { Ok(LinkedList::from([$1?])) }
    | /* Empty */         { Ok(LinkedList::new()) }
    ;

Var -> Result<Tnode, (Option<Span>, &'static str)>:
      VarAccess       { $1 }
    | "*" VarAccess   { create_deref($span, $2?) }
    ;

VarAccess ->  Result<Tnode, (Option<Span>, &'static str)>:
      Id                                { get_variable($lexer, &$1?, Vec::new(), LinkedList::new(), RefType::RHS) }
    | Id ArrayAccess                    { get_variable($lexer, &$1?, check_access_vec($2?)?, LinkedList::new(), RefType::RHS) }
    | Inst "." DotField                 { get_variable($lexer, &$1?, Vec::new(), $3?, RefType::RHS) }
    | Id ArrayAccess "." DotField       { get_variable($lexer, &$1?, check_access_vec($2?)?, $4?, RefType::RHS) }
    ;

FnCall -> Result<Tnode, (Option<Span>, &'static str)>:
      Id "(" ArgList ")"                              { create_fncall($lexer.span_str($1?.span()), $3?, $span) }
    | Id ArrayAccess "." DotField "(" ArgList ")"     { Err((None, "nothing")) }
    | Inst "." DotField "(" ArgList ")"               { Err((None, "nothing")) }
    ;

Inst -> Result<DefaultLexeme<u32>, (Option<Span>, &'static str)>:
      Id       { $1 }
    | "SELF"   { PARSER.lock().unwrap().check_self($1)  }
    ;

DotField ->  Result<LinkedList<Span>, (Option<Span>, &'static str)>:
      Id "." DotField         { let mut $3 = $3?; $3.push_front($1?.span()); Ok($3) }
    | Id                      { Ok(LinkedList::from([$1?.span()])) }
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

// LEXEME RESOLVE
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
use myexpl::frontend::semantics::*;
use myexpl::frontend::PARSER;
use myexpl::type_table::*;
use myexpl::symbol::*;
use std::collections::LinkedList;
