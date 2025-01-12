use std::cmp::{max, min};
use std::ops::Range;
use anyhow::Error;
use crate::common::LineCol;
use ariadne::{Color, Fmt, Label, Report, ReportBuilder, ReportKind, Source};
use peg::error::ParseError;
use crate::frontend::error::TypecheckerError;

macro_rules! fail_with {
    ($block:block) => {{
        eprintln!("ERROR");
        $block
        std::process::exit(1);
    }};
}

pub(crate) use fail_with;
pub struct ExtractedError {
    pub msg: String,
    pub pos: Option<LineCol>,
    pub prev_pos: Option<LineCol>,
}

pub trait ErrorWithPosition {
    fn get_position(&self) -> Option<LineCol>;

    // Used for errors that point to previous declaration etc.
    fn get_prev_position(&self) -> Option<LineCol>;
}

pub fn report_error(filename: String, src: &str, error: Error) {
    let extracted_error = extract_error(&error);
    let source = Source::from(src);

    let mut report = Report::build(ReportKind::Error, filename.clone(), 0);
    if extracted_error.pos.is_some() {
        let pos = extracted_error.pos.clone().unwrap();
        let error_span = get_error_span(&source, &pos);
        report = report.with_label(
            Label::new((filename.clone(), error_span))
                .with_message(format!("{}", "Error occurred here".fg(Color::Yellow)))
                .with_color(Color::Red)
        ).with_message(format!("[{},{}]: {}", pos.line, pos.col, extracted_error.msg));
    } else {
        report = report.with_message(format!("{}", extracted_error.msg));
    }

    add_details(report, &source, &filename, extracted_error).finish().eprint((filename, source)).unwrap();
}

fn extract_error(error: &Error) -> ExtractedError {
    if let Some(err) = error.downcast_ref::<TypecheckerError>() {
        ExtractedError {
            msg: err.to_string(),
            pos: err.get_position(),
            prev_pos: err.get_prev_position(),
        }
    } else if let Some(err) = error.downcast_ref::<ParseError<peg::str::LineCol>>() {
        ExtractedError {
            msg: "Parsing haskell output failed".into(),
            pos: Some(LineCol {
                line: err.location.line as i64,
                col: err.location.column as i64,
            }),
            prev_pos: None
        }
    } else {
        panic!("Unknown error: {:?}", error);
    }
}

fn get_error_span(source: &Source<&str>, pos: &LineCol) -> Range<usize> {
    assert!(pos.col >= 0 && pos.line >= 0);

    let line = source.line(max(0, pos.line - 1) as usize);
    let line_span = line.map_or(0..1, |l| (l.span().start as i64)..(l.span().end as i64));
    let error_start = line_span.start + pos.col;
    (max(error_start - 1, line_span.start) as usize)..(min(error_start + 2, line_span.end) as usize)
}

fn add_details<'a>(report: ReportBuilder<'a, (String, Range<usize>)>,
               source: &Source<&str>,
               filename: &str,
               error: ExtractedError) -> ReportBuilder<'a, (String, Range<usize>)> {
    if error.prev_pos.is_some() {
        let prev_pos = error.prev_pos.unwrap();
        let error_span = get_error_span(&source, &prev_pos);

        report.with_label(
            Label::new((filename.into(), error_span))
                .with_message(format!("{}", "Cause of error".fg(Color::Yellow)))
                .with_color(Color::Red)
        )
    } else {
        report
    }
}