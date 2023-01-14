use lrlex::lrlex_mod;
use lrpar::{lrpar_mod, NonStreamingLexer};
use myexpl::backend::{code_gen, linker};
use std::{env, error::Error, ffi::OsStr, fs::File, io::Read, path::PathBuf};

// Using `lrlex_mod!` brings the lexer for `lexer.l` into scope. By default the
// module name will be `lexer_l` (i.e. the file name, minus any extensions,
// with a suffix of `_l`).
lrlex_mod!("lexer.l");
lrlex_mod!("linker.l");

// Using `lrpar_mod!` brings the parser for `parser.y` into scope. By default the
// module name will be `parser_y` (i.e. the file name, minus any extensions,
// with a suffix of `_y`).
lrpar_mod!("parser.y");

fn get_input(path: &PathBuf) -> Result<String, Box<dyn Error>> {
    let mut fd = File::open(path)?;
    let mut s = String::new();
    fd.read_to_string(&mut s)?;
    Ok(s)
}

fn main() -> Result<(), Box<dyn Error>> {
    let input_file = match env::args().nth(1) {
        Some(arg) => PathBuf::from(arg),
        None => PathBuf::from("./test_progs/prg.expl".to_owned()),
    };
    match input_file.extension().and_then(OsStr::to_str) {
        Some("expl") => {}
        _ => {
            eprintln!("Expl file wasn\'t provided!");
            std::process::exit(1);
        }
    }
    let input = get_input(&input_file)?;

    let lexerdef = lexer_l::lexerdef();
    let lexer = lexerdef.lexer(&input);
    // Pass the lexer to the parser and lex and parse the input.
    let (res, errs) = parser_y::parse(&lexer);
    if errs.is_empty() == false {
        for e in errs {
            println!("{}", e.pp(&lexer, &parser_y::token_epp));
        }
        eprintln!("Unable to evaluate expression!");
        std::process::exit(1);
    }
    let obj_file = input_file.with_extension("obj");
    match res {
        Some(Ok(root)) => match code_gen::generate_code(&root, &obj_file) {
            Ok(_) => {
                let output_file = input_file.with_extension("xsm");
                let input = get_input(&obj_file)?;
                let linker_def = linker_l::lexerdef();
                let linker_lex = linker_def.lexer(&input);
                match linker::translate_label(&linker_lex, &linker_def, &output_file) {
                    Ok(_) => println!("Comipled successfully"),
                    e @ Err(_) => return e,
                }
            }
            e @ Err(_) => return e,
        },
        Some(Err((s, msg))) => match s {
            Some(span) => {
                let ((line, col), _) = lexer.line_col(span);
                eprintln!(
                    "Evaluation error at line {} column {}\n'{}'\n{}.",
                    line,
                    col,
                    lexer.span_str(span),
                    msg
                )
            }
            None => eprint!("{msg},\nEvaluation error!"),
        },
        None => eprintln!("Error Parsing"),
    }
    Ok(())
}
