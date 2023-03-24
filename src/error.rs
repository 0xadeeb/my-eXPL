use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Span};

#[derive(Debug, Clone)]
pub struct SemanticError {
    span: Option<Span>,
    msg: String,
}

impl SemanticError {
    pub fn new(span: Option<Span>, msg: &str) -> Self {
        Self {
            span,
            msg: msg.to_owned(),
        }
    }

    pub fn display(&self, lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>) {
        match self.span {
            Some(s) => {
                let ((line, col), _) = lexer.line_col(s);
                eprintln!(
                    "Evaluation error at line {} column {}\n'{}'\n{}.",
                    line,
                    col,
                    lexer.span_str(s),
                    self.msg
                )
            }
            None => eprint!("{}\nEvaluation error!", self.msg),
        }
    }
}
