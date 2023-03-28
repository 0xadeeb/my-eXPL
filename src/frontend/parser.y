%start Program
%parse-param p: &RefCell<ParserState>

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
%epp EXPOS      "exposcall"
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
Program -> Result<(), SemanticError>:
      TypeDefBlock ClassDefBlock GDeclaration Functions    { $1?; $2?; $3?; $4 }
    ;

// TYPE DECLARATION GRAMMAR
TypeDefBlock -> Result<(), SemanticError>:
      "TYPE" TypeDefList "ENDTYPE"    { $2 }
    | "TYPE" "ENDTYPE"                { Ok(()) }
    | /* Empty */                     { Ok(()) }
    ;

TypeDefList -> Result<(), SemanticError>:
      TypeDefList TypeDef   { $1?; $2 }
    | TypeDef               { $1 }
    ;

TypeDef -> Result<(), SemanticError>:
      Tname "{" FieldDeclList "}"      { p.borrow_mut().type_table.insert_struct($lexer, $span, $1?, $3?) }
    ;

Tname -> Result<Type, SemanticError>:
       Id                  { p.borrow_mut().type_table.new_struct($lexer, $1?.span()) }
    ;

// CLASS DECLARATION GRAMMAR
ClassDefBlock -> Result<(),  SemanticError>:
       "CLASS" ClassDefList "ENDCLASS"   { $2 }
     | "CLASS" "ENDCLASS"                { Ok(()) }
     | /* Empty */                       { Ok(()) }
     ;

ClassDefList -> Result<(),  SemanticError>:
       ClassDefList ClassDef       { $1?; Ok(class_list_join($2?, &mut p.borrow_mut())) }
     | ClassDef                    { Ok(class_list_join($1?, &mut p.borrow_mut())) }
     ;

ClassDef -> Result<(&'input str, Vec<u8>), SemanticError>:
       Cname "{" CDecl MethodDefns "}"     { let $1 = $1?; let $3 = $3?; $4?; p.borrow_mut().end_class(); Ok(($1, $3)) }
     ;

Cname -> Result<&'input str, SemanticError>:
       Id                  { p.borrow_mut().set_class($lexer, $1?.span(), None) }
     | Id "EXTENDS" Id     { p.borrow_mut().set_class($lexer, $1?.span(), Some($3?.span())) }
     ;

CDecl -> Result<Vec<u8>, SemanticError>:
      "DECL" MethodDeclList "ENDDECL"                   { p.borrow_mut().insert_cst($span, $lexer, LinkedList::new(), $2?) }
    | "DECL" FieldDeclList MethodDeclList "ENDDECL"     { p.borrow_mut().insert_cst($span, $lexer, $2?, $3?) }
    ;

FieldDeclList -> Result<LinkedList<(Type, Span)>, SemanticError>:
      FieldDeclList FieldDecl       { insert_back($1, $2) }
    | FieldDecl                     { Ok(LinkedList::from([$1?])) }
    ;

FieldDecl -> Result<(Type, Span), SemanticError>:
      Type Id ";"            { Ok(($1?, $2?.span())) }
    ;

MethodDeclList -> Result<LinkedList<(Type, Span, LinkedList<(Type, Span)>)>,  SemanticError>:
      MethodDeclList MethodDecl    { insert_back($1, $2) }
    | MethodDecl                   { Ok(LinkedList::from([$1?])) }
    ;

MethodDecl -> Result<(Type, Span, LinkedList<(Type, Span)>),  SemanticError>:
      Type Id "(" ParamList ")" ";"  { Ok(($1?, $2?.span(), $4?)) }
    ;

MethodDefns -> Result<(),  SemanticError>:
      MethodDefns FDef   { p.borrow_mut().fn_list.push_back($2?); Ok(()) }
    | FDef               { p.borrow_mut().fn_list.push_back($1?); Ok(()) }
    ;

// GLOBAL DECLARATION GRAMMAR
GDeclaration -> Result<(), SemanticError>:
      "DECL" GDeclList "ENDDECL"  { $2 }
    | "DECL" "ENDDECL"            { Ok(()) }
    | /* Empty */                 { Ok(()) }
    ;

GDeclList -> Result<(), SemanticError>:
      GDeclList GDecl   { $1?; $2 }
    | GDecl             { $1 }
    ;

GDecl -> Result<(), SemanticError>:
      Type GSymbolList ";"    {
          let parser = &mut *p.borrow_mut();
          insert($2?, $1?, &mut parser.gst, 4096 + 8 * parser.virt_fn_list.len() as i16, $lexer)
      }
    ;

GSymbolList -> Result<LinkedList<SymbolBuilder>, SemanticError>:
      GSymbolDef "," GSymbolList   { insert_front($3, $1) }
    | GSymbolDef                   { Ok(LinkedList::from([$1?])) }
    ;

GSymbolDef -> Result<SymbolBuilder, SemanticError>:
      Id                               { let s = SymbolBuilder::new($1?.span(), true); Ok(s) }
    | Id SizeDef                       { let mut s = SymbolBuilder::new($1?.span(), true); s.dim($2?); Ok(s) }
    | "*" Id                           { let mut s = SymbolBuilder::new($2?.span(), true); s.ptr(); Ok(s) }
    | Id "(" ParamList ")"             { let mut s = SymbolBuilder::new($1?.span(), true); s.params($3?, &mut p.borrow_mut().flabel, $lexer); Ok(s) }
    | "*" Id  "(" ParamList ")"        { let mut s = SymbolBuilder::new($2?.span(), true); s.ptr().params($4?, &mut p.borrow_mut().flabel, $lexer); Ok(s) }
    ;

SizeDef -> Result<Vec<u8>, SemanticError>:
      SizeDef "[" Num "]"     { let mut $1 = $1?; $1.push(parse_int($lexer, &$3?)? as u8); Ok($1) }
    | "[" Num "]"             { Ok(Vec::from([parse_int($lexer, &$2?)? as u8])) }
    ;

Type -> Result<Type, SemanticError>:
      "INT_T"         { Ok(Type::Int) }
    | "STRING_T"      { Ok(Type::Str) }
    | Id              {
      let span = $1?.span();
      Ok(
        p.borrow()
         .type_table
         .get($lexer.span_str(span))
         .ok_or(SemanticError::new(Some(span), "Type not defined"))?
         .to_type()
      )
    }
    ;

// FUNCTION DEFINITION GRAMMAR
Functions ->  Result<(), SemanticError>:
      FDefBlock MainFn    { p.borrow_mut().fn_list.push_back($2?); $1 }
    | MainFn              { p.borrow_mut().fn_list.push_back($1?); Ok(()) }
    ;

FDefBlock -> Result<(), SemanticError>:
      FDefBlock FDef        { p.borrow_mut().fn_list.push_back($2?); $1 }
    | FDef                  { p.borrow_mut().fn_list.push_back($1?); Ok(()) }
    ;

FDef -> Result<FnAst, SemanticError>:
      FType FName "(" Params ")" "{" LDeclaration Body ReturnStmt "}"    {
        $2?; $4?; $7?;
        create_fn(
          $1?, $8?, $9?,
          Span::new($span.start(), $5.unwrap().span().end()),
          & p.borrow().lst, p.borrow().current_fn.as_ref().unwrap(),
        )
      }
    ;

FType -> Result<Type, SemanticError>:
      Type         { $1 }
    | "*" Type     { Ok($2?.rref().unwrap()) }
    ;

FName -> Result<&'input str, SemanticError>:
      Id           {
        let $1 = $1?;
        Ok(
          p.borrow_mut()
            .update_state(
              $lexer.span_str($1.span())
            )
            .map_err(
              |msg| SemanticError::new(Some($1.span()), &msg)
            )?
        )
      }
    ;

LDeclaration -> Result<(), SemanticError>:
      "DECL" LDeclList "ENDDECL"  { $2 }
    | "DECL" "ENDDECL"            { Ok(()) }
    | /* Empty */                { Ok(()) }
    ;

LDeclList -> Result<(), SemanticError>:
      LDeclList LDecl   { $1?; $2 }
    | LDecl             { $1 }
    ;

LDecl -> Result<(), SemanticError>:
      Type LSymbolList ";"    { insert($2?, $1?, &mut p.borrow_mut().lst, 1, $lexer) }
    ;

LSymbolList -> Result<LinkedList<SymbolBuilder>, SemanticError>:
      LSymbolDef "," LSymbolList   { insert_front($3, $1) }
    | LSymbolDef                   { Ok(LinkedList::from([$1?])) }
    ;

LSymbolDef -> Result<SymbolBuilder, SemanticError>:
      Id              { let s = SymbolBuilder::new($1?.span(), false); Ok(s) }
    | "*" Id          { let mut s = SymbolBuilder::new($2?.span(), false); s.ptr(); Ok(s) }
    ;

Params -> Result<(), SemanticError>:
      ParamList       { insert_args($lexer, $1?, $span, &mut p.borrow_mut()) }
    ;

ParamList -> Result<LinkedList<(Type, Span)>, SemanticError>:
      Param "," ParamList      { insert_front($3, $1) }
    | Param                    { Ok(LinkedList::from([$1?])) }
    | /* Empty */              { Ok(LinkedList::new()) }
    ;

Param -> Result<(Type, Span), SemanticError>:
      Type Id        { Ok(($1?, $2?.span())) }
    | Type "*" Id    { Ok(($1?.rref().unwrap(), $3?.span())) }
    ;

MainFn -> Result<FnAst, SemanticError>:
      FType Main "(" ")" "{" LDeclaration Body ReturnStmt "}"   {
        $2?; $6?;
        create_main_block(
          $1?, $7?, $8?,
          Span::new($span.start(), $4.unwrap().span().end()),
          &p.borrow().lst,
        )
      }
    ;

Body -> Result<Tnode, SemanticError>:
      "BEGIN" Slist "END" { $2 }
    | "BEGIN" "END"       { Ok(Tnode::Empty) }
    ;

Slist -> Result<Tnode, SemanticError>:
      Slist Stmt  { Ok(Tnode::Connector{left: Box::new($1?), right: Box::new($2?),}) }
    | Stmt        { $1 }
    ;

// STATEMENTS GRAMMAR
// TODO: complete
Stmt -> Result<Tnode, SemanticError>:
      "IF" "(" E ")" "THEN" Slist "ENDIF" ";"                 { create_if($span, $3?, $6?, None) }
    | "IF" "(" E ")" "THEN" Slist "ELSE" Slist "ENDIF" ";"    { create_if($span, $3?, $6?, Some($8?)) }
    | "WHILE" "(" E ")" "DO" Slist "ENDWHILE" ";"             { create_while($span, $3?, $6?) }
    | "REPEAT" "DO" Slist "UNTIL" "(" E ")" ";"               { create_repeat($span, $3?, $6?) }
    | "READ" "(" Var ")" ";"                                  { create_read($span, $3?) }
    | "WRITE" "(" E ")" ";"                                   { create_write($span, $3?) }
    | "BREAK" ";"                                             { Ok(Tnode::Break) }
    | "CONTINUE" ";"                                          { Ok(Tnode::Continue) }
    | Var "=" E ";"                                           { create_asg($span, $1?, $3?, &p.borrow().type_table) }
    | "INIT" "(" ")" ";"                                      { Ok(Tnode::Initialize) }
    | Var "=" "ALLOC" "(" ")" ";"                             { create_alloc($span, $1?) }
    | "FREE" "(" Var ")" ";"                                  { free_memory($span, $3?, false) }
    | Var "=" "NEW" "(" Id ")" ";"                            { create_new($lexer, $span, $1?, $5?.span(), &p.borrow().type_table) }
    | "DELETE" "(" Var ")" ";"                                { free_memory($span, $3?, true) }
    ;

ReturnStmt -> Result<Tnode, SemanticError>:
      "RETURN" E ";"    { create_return($span, $2?, &p.borrow()) }
    ;

ExposCall ->  Result<Tnode, SemanticError>:
      "EXPOS" "(" E ")"                        { create_exposcall($span, Box::new($3?), Vec::new()) }
    | "EXPOS" "(" E "," E ")"                  { create_exposcall($span, Box::new($3?), Vec::from([$5?])) }
    | "EXPOS" "(" E "," E "," E ")"            { create_exposcall($span, Box::new($3?), Vec::from([$5?, $7?])) }
    | "EXPOS" "(" E "," E "," E "," E ")"      { create_exposcall($span, Box::new($3?), Vec::from([$5?, $7?, $9?])) }
    ;

// EXPRESSION GRAMMAR
E -> Result<Tnode, SemanticError>:
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
    | Num                     { create_constant($lexer, $span, &$1?, Type::Int, false) }
    | "-" Num                 { create_constant($lexer, $span, &$2?, Type::Int, true) }
    | String                  { create_constant($lexer, $span, &$1?, Type::Str, false) }
    | FnCall                  { $1 }
    | ExposCall               { $1 }
    | "NULL"                  { Ok(Tnode::Null) }
    ;

ArgList -> Result<LinkedList<Tnode>, SemanticError>:
      ArgList "," E       { insert_back($1, $3) }
    | E                   { Ok(LinkedList::from([$1?])) }
    | /* Empty */         { Ok(LinkedList::new()) }
    ;

Var -> Result<Tnode, SemanticError>:
      VarAccess       { $1 }
    | "*" VarAccess   { create_deref($span, $2?) }
    ;

VarAccess ->  Result<Tnode, SemanticError>:
      Id                                { get_variable($lexer, &$1?, Vec::new(), LinkedList::new(), &p.borrow()) }
    | Id ArrayAccess                    { get_variable($lexer, &$1?, check_access_vec($2?)?, LinkedList::new(), &p.borrow()) }
    | Inst "." DotField                 { get_variable($lexer, &$1?, Vec::new(), $3?, &p.borrow()) }
    | Id ArrayAccess "." DotField       { get_variable($lexer, &$1?, check_access_vec($2?)?, $4?, &p.borrow()) }
    ;

FnCall -> Result<Tnode, SemanticError>:
      Id "(" ArgList ")"                              { create_fncall($lexer.span_str($1?.span()), $3?, $span, &p.borrow().gst) }
    | Id ArrayAccess "." DotField "(" ArgList ")"     { create_method_call($lexer, $1?.span(), check_access_vec($2?)?, $4?, $6?, $span, &p.borrow()) }
    | Inst "." DotField "(" ArgList ")"               { create_method_call($lexer, $1?.span(), Vec::new(), $3?, $5?, $span, &p.borrow()) }
    ;

Inst -> Result<DefaultLexeme<u32>, SemanticError>:
      Id       { $1 }
    | "SELF"   { p.borrow().check_self($1.unwrap()) }
    ;

DotField ->  Result<LinkedList<Span>, SemanticError>:
      Id "." DotField         { let mut $3 = $3?; $3.push_front($1?.span()); Ok($3) }
    | Id                      { Ok(LinkedList::from([$1?.span()])) }
    ;

ArrayAccess -> Result<Vec<Tnode>, SemanticError>:
      ArrayAccess "[" E "]"     { let mut $1 = $1?; $1.push($3?); Ok($1) }
    | "[" E "]"                 { Ok(vec![$2?]) }
    ;

Main -> Result<DefaultLexeme<u32>, SemanticError>:
    "MAIN"     {
      p.borrow_mut().update_state("").unwrap();
      $1.map_err(|e| SemanticError::new(Some(e.span()), "Faulty lexeme"))
    }
    ;

// LEXEME RESOLVE
Id -> Result<DefaultLexeme<u32>, SemanticError>:
    "VAR"          { $1.map_err(|e| SemanticError::new(Some(e.span()), "Faulty lexeme")) }
    ;

Num -> Result<DefaultLexeme<u32>, SemanticError>:
    "NUM"          { $1.map_err(|e| SemanticError::new(Some(e.span()), "Faulty lexeme")) }
    ;

String -> Result<DefaultLexeme<u32>, SemanticError>:
    "STRING_C"     { $1.map_err(|e| SemanticError::new(Some(e.span()), "Faulty lexeme")) }
    ;

Unmatched -> ():
  "UNMATCHED" { }
  ;

%%

// Any functions here are in scope for all the grammar actions above.

use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Span};
use myexpl::{
    ast::*,
    error::SemanticError,
    frontend::{parser_state::*, semantics::*},
    symbol::*,
    type_table::*,
};
use std::{cell::RefCell, collections::LinkedList};

fn parse_int(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    token: &DefaultLexeme,
) -> Result<u32, SemanticError> {
    match lexer.span_str(token.span()).parse::<u32>() {
        Ok(val) => Ok(val),
        Err(_) => Err(SemanticError::new(Some(token.span()), "Can't parse to u32")),
    }
}

fn class_list_join(
    arg: (&str, Vec<u8>),
    parser: &mut ParserState,
) {
    let list = &mut parser.virt_fn_list;
    parser.type_table.set_cidx(arg.0, list.len() as u8);
    list.push_back(arg.1);
}

fn insert_front<T>(
    rhs: Result<LinkedList<T>, SemanticError>,
    lhs: Result<T, SemanticError>,
) -> Result<LinkedList<T>, SemanticError> {
    let mut flt = rhs?;
    flt.push_front(lhs?);
    Ok(flt)
}

fn insert_back<T>(
    lhs: Result<LinkedList<T>, SemanticError>,
    rhs: Result<T, SemanticError>,
) -> Result<LinkedList<T>, SemanticError> {
    let mut flt = lhs?;
    flt.push_back(rhs?);
    Ok(flt)
}
