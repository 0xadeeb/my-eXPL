use lrlex::{DefaultLexeme, LRNonStreamingLexerDef, LexerDef};
use lrpar::{Lexeme, NonStreamingLexer};
use std::{collections::HashMap, error::Error, fs::OpenOptions, io::Write, path::PathBuf};

pub fn translate_label(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    lexer_def: &LRNonStreamingLexerDef<DefaultLexeme, u32>,
    file_name: &PathBuf,
) -> Result<(), Box<dyn Error>> {
    let mut fd = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(file_name)?;

    let mut map: HashMap<String, i16> = HashMap::new();
    let mut line_number: i16 = 1;

    for pass in 1..=2 {
        for l in lexer.iter() {
            match l {
                Ok(token) => {
                    let rule = lexer_def.get_rule_by_id(token.tok_id());
                    match rule.name.as_ref().unwrap().as_str() {
                        "LABEL_DEF" => {
                            if pass == 1 {
                                let mut label = lexer.span_str(token.span()).to_string();
                                label.pop();
                                map.insert(label, 2056 + 2 * (line_number - 9));
                            }
                        }
                        "LABEL" => {
                            if pass == 2 {
                                let mut label = lexer.span_str(token.span()).to_string();
                                label.remove(0);
                                label.pop();
                                match map.get(&label) {
                                    Some(no) => write!(fd, "{}", no)?,
                                    None => {
                                        return Err(Box::<dyn Error>::from(
                                            "Failed to get adrress from label",
                                        ))
                                    }
                                }
                            }
                        }
                        "NEWLINE" => {
                            if pass == 1 {
                                line_number += 1;
                            } else {
                                let s = lexer.span_str(token.span());
                                write!(fd, "{}", s)?;
                            }
                        }
                        "ANY" => {
                            if pass == 2 {
                                let s = lexer.span_str(token.span());
                                write!(fd, "{}", s)?;
                            }
                        }
                        _ => return Err(Box::<dyn Error>::from("Undefined token found")),
                    }
                }
                Err(e) => return Err(Box::new(e)),
            }
        }
    }
    Ok(())
}
