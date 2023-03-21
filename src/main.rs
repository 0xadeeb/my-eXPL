use lrlex::lrlex_mod;
use lrpar::lrpar_mod;
use myexpl::{
    backend::{code_gen::CodeGen, linker},
    frontend::parser_state::ParserState,
};
use std::{cell::RefCell, env, error::Error, ffi::OsStr, fs::File, io::Read, path::PathBuf};

// Using `lrlex_mod!` brings the lexer for `lexer.l` into scope. By default the
// module name will be `lexer_l` (i.e. the file name, minus any extensions,
// with a suffix of `_l`).
lrlex_mod!("frontend/lexer.l");
lrlex_mod!("backend/linker.l");

// Using `lrpar_mod!` brings the parser for `parser.y` into scope. By default the
// module name will be `parser_y` (i.e. the file name, minus any extensions,
// with a suffix of `_y`).
lrpar_mod!("frontend/parser.y");

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

    let p = RefCell::new(ParserState::default());
    let (res, errs) = parser_y::parse(&lexer, &p);

    if errs.is_empty() == false {
        for e in errs {
            println!("{}", e.pp(&lexer, &parser_y::token_epp));
        }
        eprintln!("Unable to evaluate program!");
        std::process::exit(1);
    }
    let obj_file = input_file.with_extension("obj");
    match res {
        Some(Ok(_virt_fn_list)) => {
            let mut code_generator = CodeGen::new(&obj_file)?;
            let gst_size = p.borrow().gst.get_size().to_owned();
            match code_generator.emit_code(&p.borrow().fn_list, gst_size) {
                Ok(()) => {
                    let output_file = input_file.with_extension("xsm");
                    let input = get_input(&obj_file)?;
                    let linker_def = linker_l::lexerdef();
                    let linker_lex = linker_def.lexer(&input);
                    match linker::translate_label(&linker_lex, &linker_def, &output_file) {
                        Ok(_) => println!("Compiled successfully"),
                        e @ Err(_) => return e,
                    }
                }
                e @ Err(_) => return e,
            }
        }
        Some(Err(e)) => e.display(&lexer),
        None => eprintln!("Error Parsing"),
    }
    Ok(())
}
