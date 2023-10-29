use std::fs;

use chumsky::text::ident;
use chumsky::Parser;
use chumsky::{prelude::*, stream::Stream};
use core::fmt;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tower_lsp::lsp_types::SemanticTokenType;

use crate::semantic_token::LEGEND_TYPE;

use lazy_static::lazy_static;
use std::collections::BTreeSet;

/// This is the parser and interpreter for the 'Foo' language. See `tutorial.md` in the repository's root to learn
/// about it.
pub type Span = std::ops::Range<usize>;
#[derive(Debug)]
pub struct ImCompleteSemanticToken {
    pub start: usize,
    pub length: usize,
    pub token_type: usize,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    IntLiteral(String),
    RealLiteral(String),
    Identifier(String),
    Keyword(&'static str),
    Op(&'static str),
    Punct(&'static str),
}

impl Token {
    pub fn is_dec_digits(&self) -> bool {
        if let Token::IntLiteral(x) = self {
            return x.chars().all(|c| c.is_digit(10));
        }
        return false;
    }
}

lazy_static! {
    static ref KEYWORDS_MAP: BTreeSet<String> = {
        let keywords = [
            "Action",
            "ActionValue",
            "BVI",
            "C",
            "CF",
            "E",
            "SB",
            "SBR",
            "action",
            "endaction",
            "actionvalue",
            "endactionvalue",
            "ancestor",
            "begin",
            "bit",
            "case",
            "endcase",
            "clocked_by",
            "default",
            "default_clock",
            "default_reset",
            "dependencies",
            "deriving",
            "determines",
            "e",
            "else",
            "enable",
            "end",
            "enum",
            "export",
            "for",
            "function",
            "endfunction",
            "if",
            "ifc_inout",
            "import",
            "inout",
            "input_clock",
            "input_reset",
            "instance",
            "endinstance",
            "interface",
            "endinterface",
            "let",
            "match",
            "matches",
            "method",
            "endmethod",
            "module",
            "endmodule",
            "numeric",
            "output_clock",
            "output_reset",
            "package",
            "endpackage",
            "parameter",
            "path",
            "port",
            "provisos",
            "reset_by",
            "return",
            "rule",
            "endrule",
            "rules",
            "endrules",
            "same_family",
            "schedule",
            "struct",
            "tagged",
            "type",
            "typeclass",
            "endtypeclass",
            "typedef",
            "union",
            "valueOf",
            "valueof",
            "void",
            "while",
        ];

        keywords
            .iter()
            .map(|x| x.to_string())
            .collect::<BTreeSet<_>>()
    };
}

fn parser_identifier_or_keyword<'a>(
    first_letter_upper: bool,
) -> BoxedParser<'a, char, (Token, Span), Simple<char>> {
    let s = filter(move |c: &char| {
        (c.is_ascii_alphabetic() || c == &'_' || c == &'$')
            && (!(first_letter_upper ^ c.is_ascii_uppercase()))
    })
    .map(Some)
    .chain::<char, Vec<_>, _>(filter(|c: &char| c.is_ascii_alphanumeric() || c == &'_').repeated())
    .collect::<String>()
    .map_with_span(move |tok, span| {
        (
            if KEYWORDS_MAP.contains(&tok) {
                Token::Keyword(KEYWORDS_MAP.get(&tok).unwrap())
            } else if first_letter_upper {
                Token::Identifier(tok)
            } else {
                Token::Identifier(tok)
            },
            span,
        )
    });
    s.boxed()
}

fn parser_dec_digits<'a>() -> BoxedParser<'a, char, String, Simple<char>> {
    text::int(10).boxed()
}

fn parser_op<'a>() -> impl Parser<char, (Token, Span), Error = Simple<char>> {
    choice((
        just("=="),
        just("!="),
        just("&&"),
        just("||"),
        just("~&"),
        just("~|"),
        just("^~"),
        just("~^"),
        just("<<"),
        just(">>"),
        just("<="),
        just(">="),
        just("^"),
        just("+"),
        just("-"),
        just("!"),
        just("~"),
        just("&"),
        just("|"),
        just("*"),
        just("/"),
        just("%"),
        just("<"),
        just(">"),
        just("?"),
    ))
    .map_with_span(|tok, span| (Token::Op(tok), span))
}

fn parser_punct<'a>() -> impl Parser<char, (Token, Span), Error = Simple<char>> {
    choice((
        just("#"),
        just(":"),
        just(","),
        just("."),
        just(";"),
        just("["),
        just("]"),
        just("{"),
        just("}"),
        just("("),
        just(")"),
    ))
    .map_with_span(|tok, span| (Token::Punct(tok), span))
}

fn parser_dec_digits_underscore<'a>() -> BoxedParser<'a, char, String, Simple<char>> {
    filter(move |c: &char| c.is_digit(10) || c == &'_')
        .repeated()
        .at_least(1)
        .collect::<String>()
        .boxed()
}

fn parser_dec_num<'a>() -> BoxedParser<'a, char, String, Simple<char>> {
    let dec_digits = parser_dec_digits();
    let dec_digits_underscore = parser_dec_digits_underscore();
    dec_digits
        .chain::<String, _, _>(dec_digits_underscore.clone().or_not())
        .collect::<String>()
        .boxed()
}

fn parser_sign<'a>() -> BoxedParser<'a, char, String, Simple<char>> {
    just("+").or(just("-")).map(|a| a.to_owned()).boxed()
}

fn parser_int_literal() -> impl Parser<char, (Token, Span), Error = Simple<char>> {
    let dec_digits = parser_dec_digits();
    let dec_digits_underscore = parser_dec_digits_underscore();
    let hex_digits_underscore = filter(move |c: &char| c.is_digit(16) || c == &'_')
        .repeated()
        .at_least(1)
        .collect::<String>()
        .boxed();
    let oct_digits_underscore = filter(move |c: &char| c.is_digit(8) || c == &'_')
        .repeated()
        .at_least(1)
        .collect::<String>()
        .boxed();
    let bin_digits_underscore = filter(move |c: &char| c.is_digit(2) || c == &'_')
        .repeated()
        .at_least(1)
        .collect::<String>()
        .boxed();

    let sign = parser_sign();

    let bit_width = dec_digits.clone();

    let dec_num = parser_dec_num();

    #[rustfmt::skip]
    let base_literal = just("'d".to_owned()).or(just("'D".to_owned())).chain::<String, _, _>(dec_digits_underscore).or(
        just("'h".to_owned()).or(just("'H".to_owned())).chain(hex_digits_underscore).or(
            just("'o".to_owned()).or(just("'O".to_owned())).chain(oct_digits_underscore).or(
                just("'b".to_owned()).or(just("'B".to_owned())).chain(bin_digits_underscore)
            )
        )
    ).collect::<String>().boxed();

    let unsized_int_literal = sign
        .clone()
        .or_not()
        .chain::<String, _, _>(base_literal.clone())
        .or(sign.or_not().chain(dec_num))
        .collect()
        .boxed();

    let sized_int_literal = bit_width
        .chain::<String, _, _>(base_literal)
        .collect()
        .boxed();

    let int_literal = just("'0".to_owned())
        .or(just("'1".to_owned()))
        .or(sized_int_literal)
        .or(unsized_int_literal);

    int_literal
        .padded()
        .map_with_span(|tok, span| (Token::IntLiteral(tok), span))
}

fn parser_real_literal() -> impl Parser<char, (Token, Span), Error = Simple<char>> {
    let dec_digits_underscore = parser_dec_digits_underscore();
    let dec_num = parser_dec_num();

    let sign = parser_sign();
    let exp = just("e").or(just("E")).map(|a| a.to_owned()).boxed();

    let real_literal = dec_num
        .clone()
        .chain::<String, _, _>(
            just('.')
                .chain(dec_digits_underscore.clone())
                .collect::<String>()
                .or_not(),
        )
        .chain::<String, _, _>(exp)
        .chain::<String, _, _>(sign.or_not())
        .chain::<String, _, _>(dec_digits_underscore.clone())
        .or(dec_num
            .chain(just('.'))
            .collect::<String>()
            .chain::<String, _, _>(dec_digits_underscore))
        .collect()
        .boxed();
    real_literal
        .padded()
        .map_with_span(|tok, span| (Token::RealLiteral(tok), span))
}

fn parser_parse_stream_to_token_list() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>>
{
    let token = parser_int_literal()
        .or(parser_real_literal())
        .or(parser_identifier_or_keyword(true))
        .or(parser_identifier_or_keyword(false))
        .or(parser_op())
        .or(parser_punct())
        .padded()
        .recover_with(skip_then_retry_until([]));

    let ret = token
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .repeated()
        .collect::<Vec<_>>()
        .boxed();
    ret
}

#[derive(Debug)]
pub enum AST {
    AType(AstType),
}
#[derive(Debug)]
pub struct AstTypePrimary {
    ident_upper: String,
    types: Vec<AstType>,
    type_nat_1: String,
    type_nat_2: String,
}

#[derive(Debug)]
pub enum AstType {
    Type(AstTypePrimary),
    FuncType(AstTypePrimary, Vec<AstType>),
}

#[derive(Debug)]
pub struct AstTypeFormal {
    is_numeric: bool,
    type_ident: String,
}

pub type AstTypeFormals = Vec<AstTypeFormal>;

#[derive(Debug)]
pub struct AstTypeDefType {
    type_ident: String,
    type_formals: Option<AstTypeFormals>,
}
#[derive(Debug)]

pub struct AstTypedefSynonym {
    origin_type: AstType,
    new_type_def: AstTypeDefType,
}

pub type Spanned<T> = (T, Span);

fn parser_match_type_ide() -> impl Parser<Token, String, Error = Simple<Token>> {
    select! {Token::Identifier(s) => s}
}

fn parser_type() -> impl Parser<Token, Spanned<AstType>, Error = Simple<Token>> {
    let mut type_primary = Recursive::<Token, AstTypePrimary, _>::declare();
    let mut type_ = Recursive::<Token, AstType, _>::declare();

    let type_nat = filter_map(|span, tok| {
        if let Token::IntLiteral(ref lit) = tok {
            if tok.is_dec_digits() {
                return Ok(lit.clone());
            }
        }
        return Err(Simple::custom(span, "error1"));
    });

    let t1 = parser_match_type_ide()
        .then(
            just(Token::Punct("#"))
                .ignore_then(
                    type_
                        .clone()
                        .separated_by(just(Token::Punct(",")))
                        .delimited_by(just(Token::Punct("(")), just(Token::Punct(")"))),
                )
                .or_not(),
        )
        .map(|(ident_upper, types)| AstTypePrimary {
            ident_upper,
            types: types.unwrap_or_default(),
            type_nat_1: "".to_string(),
            type_nat_2: "".to_string(),
        });

    let t2 = type_nat
        .clone()
        .map(|type_nat_1| AstTypePrimary {
            ident_upper: "".to_string(),
            types: Vec::new(),
            type_nat_1,
            type_nat_2: "".to_string(),
        })
        .recover_with(skip_then_retry_until([]));

    let t3 = just(Token::Keyword("bit"))
        .ignore_then(
            type_nat
                .then_ignore(select! {Token::Punct(":") => ()})
                .then(type_nat)
                .delimited_by(just(Token::Punct("[")), just(Token::Punct("]"))),
        )
        .map(|(type_nat_1, type_nat_2)| AstTypePrimary {
            ident_upper: "".to_string(),
            types: Vec::new(),
            type_nat_1,
            type_nat_2,
        })
        .recover_with(skip_then_retry_until([]));

    type_primary.define(t1.or(t2).or(t3));

    type_.define(
        type_primary
            .clone()
            .map(|t| AstType::Type(t))
            .or(type_primary
                .then(
                    type_
                        .clone()
                        .separated_by(just(Token::Punct(",")))
                        .delimited_by(just(Token::Punct("(")), just(Token::Punct(")"))),
                )
                .map(|(a, b)| AstType::FuncType(a, b))),
    );
    let x = type_
        .clone()
        .map_with_span(|t, span| (t, span))
        .recover_with(skip_then_retry_until([]));
    x
}

fn parser_typedef_synonym() -> impl Parser<Token, Spanned<AstTypedefSynonym>, Error = Simple<Token>>
{
    let type_ide = parser_match_type_ide();
    let type_formal = just(Token::Keyword("numeric"))
        .or_not()
        .then_ignore(just(Token::Keyword("type")))
        .then(type_ide)
        .map(|(numeric, ident)| AstTypeFormal {
            is_numeric: numeric.is_some(),
            type_ident: ident,
        });

    let type_ide = parser_match_type_ide();
    let type_formals = just(Token::Punct("#")).ignore_then(
        type_formal
            .separated_by(just(Token::Punct(",")))
            .delimited_by(just(Token::Punct("(")), just(Token::Punct(")"))),
    );

    let type_def_type = type_ide
        .then(type_formals.or_not())
        .map(|(type_ident, type_formals)| AstTypeDefType {
            type_ident,
            type_formals,
        });
    let type_ = parser_type();
    let typedef_synonym = just(Token::Keyword("typedef"))
        .ignore_then(type_)
        .then(type_def_type)
        .then_ignore(just(Token::Punct(";")));
    typedef_synonym
        .map_with_span(|((origin_type, _), new_type_def), span| {
            (
                AstTypedefSynonym {
                    origin_type,
                    new_type_def,
                },
                span,
            )
        })
        .recover_with(skip_then_retry_until([]))
}

#[test]
fn main() {
    // let filename = "src/bsv_parser/test copy.bsv";
    let filename = "src/bsv_parser/test.bsv";

    let src = fs::read_to_string(&filename).expect("Failed to read file");
    let (tokens, parse_errs) = parser_parse_stream_to_token_list().parse_recovery(src.as_str());
    let tokens = tokens.unwrap();
    println!("{:?}", tokens);
    println!("{:?}", parse_errs);
    let len = src.chars().count();
    let (ast, parse_errs) = parser_typedef_synonym()
        .repeated()
        .collect::<Vec<_>>()
        .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));
    println!("{:#?}", ast);
    println!("{:#?}", parse_errs);
}

#[test]
fn test_int_literal() {
    if let (Some(tokens), _) = parser_int_literal()
        .repeated()
        .parse_recovery("125 -16 'h48454a 32'h48454a 8'o255 12'b101010 32'h_FF_FF_FF_FF")
    {
        assert_eq!(tokens[0].0, Token::IntLiteral("125".to_string()));
        assert_eq!(tokens[1].0, Token::IntLiteral("-16".to_string()));
        assert_eq!(tokens[2].0, Token::IntLiteral("'h48454a".to_string()));
        assert_eq!(tokens[3].0, Token::IntLiteral("32'h48454a".to_string()));
        assert_eq!(tokens[4].0, Token::IntLiteral("8'o255".to_string()));
        assert_eq!(tokens[5].0, Token::IntLiteral("12'b101010".to_string()));
        assert_eq!(
            tokens[6].0,
            Token::IntLiteral("32'h_FF_FF_FF_FF".to_string())
        );
    }
}

#[test]
fn test_real_literal() {
    if let (Some(tokens), _) = parser_real_literal()
        .repeated()
        .parse_recovery("1.2 0.6 2.4E10 5e-3 325.761_452_e-10 9.2e+4")
    {
        assert_eq!(tokens[0].0, Token::RealLiteral("1.2".to_string()));
        assert_eq!(tokens[1].0, Token::RealLiteral("0.6".to_string()));
        assert_eq!(tokens[2].0, Token::RealLiteral("2.4E10".to_string()));
        assert_eq!(tokens[3].0, Token::RealLiteral("5e-3".to_string()));
        assert_eq!(
            tokens[4].0,
            Token::RealLiteral("325.761_452_e-10".to_string())
        );
        assert_eq!(tokens[5].0, Token::RealLiteral("9.2e+4".to_string()));
    }
}
