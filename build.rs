use cfgrammar::yacc::YaccKind;
use lrlex::CTLexerBuilder;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    CTLexerBuilder::new()
        .lrpar_config(|ctp| {
            ctp.yacckind(YaccKind::Grmtools)
                .grammar_in_src_dir("parser.y")
                .unwrap()
        })
        .lexer_in_src_dir("lexer.l")?
        .build()?;

    CTLexerBuilder::new()
        .lexer_in_src_dir("linker.l")?
        .build()?;

    Ok(())
}
