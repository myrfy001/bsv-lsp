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
    StringLiteral(String),
    Identifier(String),
    Keyword(&'static str),
    Op(&'static str),
    Punct(&'static str),
    Comment(String),
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

fn parser_identifier_or_keyword<'a>() -> BoxedParser<'a, char, (Token, Span), Simple<char>> {
    let s = filter(move |c: &char| c.is_ascii_alphabetic() || c == &'_' || c == &'$')
        .map(Some)
        .chain::<char, Vec<_>, _>(
            filter(|c: &char| c.is_ascii_alphanumeric() || c == &'_').repeated(),
        )
        .collect::<String>()
        .map_with_span(move |tok, span| {
            (
                if KEYWORDS_MAP.contains(&tok) {
                    Token::Keyword(KEYWORDS_MAP.get(&tok).unwrap())
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
        just("="),
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

fn parser_string_literal() -> impl Parser<char, (Token, Span), Error = Simple<char>> {
    let escape = just('\\')
        .ignore_then(choice((
            just('n').to('\n'),
            just('t').to('\t'),
            just('\\'),
            just('"'),
            just('v').to('\x0B'),
            just('f').to('\x0C'),
            just('a').to('\x07'),
            just('x').ignore_then(
                filter(move |c: &char| c.is_digit(16))
                    .repeated()
                    .exactly(2)
                    .map(|a| {
                        char::from_u32(
                            u32::from_str_radix(&(a.iter().collect::<String>()), 8).unwrap(),
                        )
                        .unwrap()
                    }),
            ),
            filter(move |c: &char| c.is_digit(8))
                .repeated()
                .exactly(3)
                .map(|a| {
                    char::from_u32(u32::from_str_radix(&(a.iter().collect::<String>()), 8).unwrap())
                        .unwrap()
                }),
        )))
        .boxed();

    let string = none_of("\\\"")
        .or(escape)
        .repeated()
        .delimited_by(just('"'), just('"'))
        .map_with_span(|s, span| (Token::StringLiteral(s.iter().collect()), span))
        .boxed();
    string
}

fn parser_comment() -> impl Parser<char, (Token, Span), Error = Simple<char>> {
    let t1 = just("//")
        .ignore_then(take_until(just('\n').ignored()))
        .padded()
        .map_with_span(|(c, _), span| {
            let s = c.iter().collect();
            (Token::Comment(s), span)
        });

    let t2 = just("/*")
        .ignore_then(take_until(just("*/").ignored()))
        .padded()
        .map_with_span(|(c, _), span| {
            let s = c.iter().collect();
            (Token::Comment(s), span)
        });
    t1.or(t2)
}

fn parser_parse_stream_to_token_list() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>>
{
    let token = parser_int_literal()
        .or(parser_real_literal())
        .or(parser_identifier_or_keyword())
        .or(parser_string_literal())
        .or(parser_comment())
        .or(parser_op())
        .or(parser_punct())
        .padded();

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
pub struct AstTypeNat {
    nat: String,
}

#[derive(Debug)]
pub struct AstTypeIde {
    ident: String,
}

#[derive(Debug)]
pub struct AstTypeClassIde {
    ident: String,
}

#[derive(Debug)]
pub enum AstTypePrimary {
    Type {
        ident: Spanned<AstTypeIde>,
        types: Option<Vec<Spanned<AstType>>>,
    },
    Nat {
        nat: Spanned<AstTypeNat>,
    },
    Bit {
        start_nat: Spanned<AstTypeNat>,
        end_nat: Spanned<AstTypeNat>,
    },
}

#[derive(Debug)]
pub enum AstType {
    Type {
        type_: Spanned<AstTypePrimary>,
    },
    FuncType {
        ret_type: Spanned<AstTypePrimary>,
        arg_types: Vec<Spanned<AstType>>,
    },
}

#[derive(Debug)]
pub struct AstTypeFormal {
    is_numeric: bool,
    type_ident: Spanned<AstTypeIde>,
}

#[derive(Debug)]
pub struct AstTypeFormals {
    formals: Vec<Spanned<AstTypeFormal>>,
}

#[derive(Debug)]
pub struct AstTypeDefType {
    type_ident: Spanned<AstTypeIde>,
    type_formals: Option<Spanned<AstTypeFormals>>,
}

#[derive(Debug)]
pub struct AstTypedefSynonym {
    origin_type: Spanned<AstType>,
    new_type_def: Spanned<AstTypeDefType>,
}

#[derive(Debug)]
pub struct AstIdent {
    ident: String,
}

#[derive(Debug)]
pub struct AstIntLiteral {
    int_lit: String,
}

#[derive(Debug)]
pub struct AstRealLiteral {
    real_lit: String,
}

#[derive(Debug)]
pub struct AstStringLiteral {
    str_lit: String,
}
#[derive(Debug)]
pub struct AstDerives {
    derives: Vec<Spanned<AstTypeClassIde>>,
}

#[derive(Debug)]
pub enum AstTypedefEnumElement {
    NoTag {
        ident: Spanned<AstIdent>,
        value: Option<Spanned<AstIntLiteral>>,
    },
    OneTag {
        ident: Spanned<AstIdent>,
        value: Option<Spanned<AstIntLiteral>>,
        tag: Spanned<AstIntLiteral>,
    },
    TwoTag {
        ident: Spanned<AstIdent>,
        value: Option<Spanned<AstIntLiteral>>,
        tag_start: Spanned<AstIntLiteral>,
        tag_end: Spanned<AstIntLiteral>,
    },
}

#[derive(Debug)]
pub struct AstTypedefEnumElements {
    elements: Vec<Spanned<AstTypedefEnumElement>>,
}

#[derive(Debug)]
pub struct AstTypedefEnum {
    ident: Spanned<AstIdent>,
    elements: Spanned<AstTypedefEnumElements>,
    derives: Option<Spanned<AstDerives>>,
}

pub type Spanned<T> = (T, Span);

fn parser_match_type_ide() -> impl Parser<Token, Spanned<AstTypeIde>, Error = Simple<Token>> {
    select! {
        Token::Identifier(s) => s,
        Token::Keyword("Action")  => "Action".to_string(),
        Token::Keyword("ActionValue")  => "ActionValue".to_string(),
    }
    .map_with_span(|ident, span| (AstTypeIde { ident }, span))
}

fn parser_match_typeclass_ide(
) -> impl Parser<Token, Spanned<AstTypeClassIde>, Error = Simple<Token>> {
    select! {Token::Identifier(s) => s}
        .map_with_span(|ident, span| (AstTypeClassIde { ident }, span))
}

fn parser_match_identifier() -> impl Parser<Token, Spanned<AstIdent>, Error = Simple<Token>> {
    select! {Token::Identifier(s) => s}.map_with_span(|ident, span| (AstIdent { ident }, span))
}

fn parser_match_int_literal() -> impl Parser<Token, Spanned<AstIntLiteral>, Error = Simple<Token>> {
    select! {Token::IntLiteral(s) => s}
        .map_with_span(|int_lit, span| (AstIntLiteral { int_lit }, span))
}

fn parser_type() -> impl Parser<Token, Spanned<AstType>, Error = Simple<Token>> {
    let mut type_primary = Recursive::<Token, Spanned<AstTypePrimary>, _>::declare();
    let mut type_ = Recursive::<Token, Spanned<AstType>, _>::declare();

    let type_nat = filter_map(|span, tok| {
        if let Token::IntLiteral(ref lit) = tok {
            if tok.is_dec_digits() {
                return Ok(lit.clone());
            }
        }
        return Err(Simple::custom(span, "error1"));
    })
    .map_with_span(|nat, span| (AstTypeNat { nat }, span));

    let t1 = parser_match_type_ide()
        .then(
            just(Token::Punct("#"))
                .ignore_then(
                    type_
                        .clone()
                        .separated_by(just(Token::Punct(",")))
                        .at_least(1)
                        .delimited_by(just(Token::Punct("(")), just(Token::Punct(")"))),
                )
                .or_not(),
        )
        .map_with_span(|(ident, types), span| (AstTypePrimary::Type { ident, types }, span));

    let t2 = type_nat
        .clone()
        .map_with_span(|nat, span| (AstTypePrimary::Nat { nat }, span));

    let t3 = just(Token::Keyword("bit"))
        .ignore_then(
            type_nat
                .then_ignore(select! {Token::Punct(":") => ()})
                .then(type_nat)
                .delimited_by(just(Token::Punct("[")), just(Token::Punct("]"))),
        )
        .map_with_span(|(start_nat, end_nat), span| {
            (AstTypePrimary::Bit { start_nat, end_nat }, span)
        });

    type_primary.define(t1.or(t2).or(t3));

    let tt1 = type_primary
        .clone()
        .map_with_span(|type_, span| (AstType::Type { type_ }, span));
    let tt2 = type_primary
        .then(
            type_
                .clone()
                .separated_by(just(Token::Punct(",")))
                .at_least(1)
                .delimited_by(just(Token::Punct("(")), just(Token::Punct(")"))),
        )
        .map_with_span(|(ret_type, arg_types), span| {
            (
                AstType::FuncType {
                    ret_type,
                    arg_types,
                },
                span,
            )
        });

    type_.define(tt1.or(tt2));
    type_
}

fn parser_typedef_type() -> impl Parser<Token, Spanned<AstTypeDefType>, Error = Simple<Token>> {
    let type_ide = parser_match_type_ide();
    let type_formals = parser_type_formals();
    let type_def_type =
        type_ide
            .then(type_formals.or_not())
            .map_with_span(|(type_ident, type_formals), span| {
                (
                    AstTypeDefType {
                        type_ident,
                        type_formals,
                    },
                    span,
                )
            });
    type_def_type
}

fn parser_type_formals() -> impl Parser<Token, Spanned<AstTypeFormals>, Error = Simple<Token>> {
    let type_ide = parser_match_type_ide();
    let type_formal = just(Token::Keyword("numeric"))
        .or_not()
        // TODO： check the definition of the keyword type here. Should we write it as or_not()?
        // if we remove or_not(), the subinterface parser will not work.
        // so, does the keyword "type" must be here?
        .then_ignore(just(Token::Keyword("type")).or_not())
        .then(type_ide)
        .map_with_span(|(numeric, ident), span| {
            (
                AstTypeFormal {
                    is_numeric: numeric.is_some(),
                    type_ident: ident.into(),
                },
                span,
            )
        });
    let type_formals = just(Token::Punct("#"))
        .ignore_then(
            type_formal
                .separated_by(just(Token::Punct(",")))
                .at_least(1)
                .delimited_by(just(Token::Punct("(")), just(Token::Punct(")"))),
        )
        .map_with_span(|formals, span| (AstTypeFormals { formals }, span));
    type_formals
}

fn parser_typedef_synonym() -> impl Parser<Token, Spanned<AstTypedefSynonym>, Error = Simple<Token>>
{
    let type_def_type = parser_typedef_type();
    let type_ = parser_type();
    let typedef_synonym = just(Token::Keyword("typedef"))
        .ignore_then(type_)
        .then(type_def_type)
        .then_ignore(just(Token::Punct(";")))
        .map_with_span(|(origin_type, new_type_def), span| {
            (
                AstTypedefSynonym {
                    origin_type,
                    new_type_def,
                },
                span,
            )
        });
    typedef_synonym
}

fn parser_derives() -> impl Parser<Token, Spanned<AstDerives>, Error = Simple<Token>> {
    let typeclass_ide = parser_match_typeclass_ide();
    just(Token::Keyword("deriving"))
        .ignore_then(
            typeclass_ide
                .separated_by(just(Token::Punct(",")))
                .at_least(1)
                .delimited_by(just(Token::Punct("(")), just(Token::Punct(")"))),
        )
        .map_with_span(|derives, span| (AstDerives { derives }, span))
}

fn parser_typedef_enum() -> impl Parser<Token, Spanned<AstTypedefEnum>, Error = Simple<Token>> {
    let optional_int_literal = just(Token::Punct("="))
        .ignore_then(parser_match_int_literal())
        .or_not()
        .boxed();
    let t1 = parser_match_identifier()
        .then(optional_int_literal.clone())
        .map_with_span(|(ident, value), span| {
            (AstTypedefEnumElement::NoTag { ident, value }, span)
        });

    let t2 = parser_match_identifier()
        .then(
            parser_match_int_literal()
                .delimited_by(just(Token::Punct("[")), just(Token::Punct("]"))),
        )
        .then(optional_int_literal.clone())
        .map_with_span(|((ident, tag), value), span| {
            (AstTypedefEnumElement::OneTag { ident, value, tag }, span)
        });

    let t3 = parser_match_identifier()
        .then(
            parser_match_int_literal()
                .then_ignore(just(Token::Punct(":")))
                .then(parser_match_int_literal())
                .delimited_by(just(Token::Punct("[")), just(Token::Punct("]"))),
        )
        .then(optional_int_literal.clone())
        .map_with_span(|((ident, (tag_start, tag_end)), value), span| {
            (
                AstTypedefEnumElement::TwoTag {
                    ident,
                    value,
                    tag_start,
                    tag_end,
                },
                span,
            )
        });

    let typedef_enum_element = t3.or(t2).or(t1);

    let typedef_enum_elements = typedef_enum_element
        .separated_by(just(Token::Punct(",")))
        .at_least(1)
        .map_with_span(|elements, span| (AstTypedefEnumElements { elements }, span));

    let typedef_enum = just(Token::Keyword("typedef"))
        .ignore_then(just(Token::Keyword("enum")))
        .ignore_then(
            typedef_enum_elements.delimited_by(just(Token::Punct("{")), just(Token::Punct("}"))),
        )
        .then(parser_match_identifier())
        .then(parser_derives().or_not())
        .then_ignore(just(Token::Punct(";")))
        .map_with_span(|((elements, ident), derives), span| {
            (
                AstTypedefEnum {
                    ident,
                    elements,
                    derives,
                },
                span,
            )
        });

    typedef_enum
}

#[derive(Debug)]
pub enum AstUnionMember {
    Type {
        ident: Spanned<AstIdent>,
        typ: Spanned<AstType>,
    },
    SubStruct {
        ident: Spanned<AstIdent>,
        struct_: Spanned<AstSubStruct>,
    },
    SubUnion {
        ident: Spanned<AstIdent>,
        union_: Spanned<AstSubUnion>,
    },
    Void {
        ident: Spanned<AstIdent>,
    },
}

#[derive(Debug)]
pub enum AstStructMember {
    Type {
        ident: Spanned<AstIdent>,
        typ: Spanned<AstType>,
    },
    SubUnion {
        ident: Spanned<AstIdent>,
        union_: Spanned<AstSubUnion>,
    },
}

#[derive(Debug)]
pub struct AstSubStruct {
    members: Vec<Spanned<AstStructMember>>,
}

#[derive(Debug)]
pub struct AstSubUnion {
    members: Vec<Spanned<AstUnionMember>>,
}

#[derive(Debug)]
pub struct AstTypedefTaggedUnion {
    members: Vec<Spanned<AstUnionMember>>,
    typedef_type: Spanned<AstTypeDefType>,
    derives: Option<Spanned<AstDerives>>,
}

#[derive(Debug)]
pub struct AstTypedefStruct {
    members: Vec<Spanned<AstStructMember>>,
    typedef_type: Spanned<AstTypeDefType>,
    derives: Option<Spanned<AstDerives>>,
}
#[derive(Debug)]
pub enum AstTypedef {
    Synonym {
        synonym: Spanned<AstTypedefSynonym>,
    },
    Enum {
        enum_: Spanned<AstTypedefEnum>,
    },
    Struct {
        struct_: Spanned<AstTypedefStruct>,
    },
    TaggedUnion {
        union_: Spanned<AstTypedefTaggedUnion>,
    },
}

fn parser_sub_struct<'a>(
    sub_struct: Recursive<'a, Token, Spanned<AstSubStruct>, Simple<Token>>,
    sub_union: Recursive<'a, Token, Spanned<AstSubUnion>, Simple<Token>>,
    struct_member: Recursive<'a, Token, Spanned<AstStructMember>, Simple<Token>>,
    union_member: Recursive<'a, Token, Spanned<AstUnionMember>, Simple<Token>>,
) -> impl Parser<Token, Spanned<AstSubStruct>, Error = Simple<Token>> + 'a {
    just(Token::Keyword("struct"))
        .ignore_then(
            struct_member
                .repeated()
                .delimited_by(just(Token::Punct("{")), just(Token::Punct("}"))),
        )
        .map_with_span(|members, span| (AstSubStruct { members }, span))
}

fn parser_sub_union<'a>(
    sub_struct: Recursive<'a, Token, Spanned<AstSubStruct>, Simple<Token>>,
    sub_union: Recursive<'a, Token, Spanned<AstSubUnion>, Simple<Token>>,
    struct_member: Recursive<'a, Token, Spanned<AstStructMember>, Simple<Token>>,
    union_member: Recursive<'a, Token, Spanned<AstUnionMember>, Simple<Token>>,
) -> impl Parser<Token, Spanned<AstSubUnion>, Error = Simple<Token>> + 'a {
    just(Token::Keyword("union"))
        .ignore_then(just(Token::Keyword("tagged")))
        .ignore_then(
            union_member
                .repeated()
                .delimited_by(just(Token::Punct("{")), just(Token::Punct("}"))),
        )
        .map_with_span(|members, span| (AstSubUnion { members }, span))
}

fn parser_union_member<'a>(
    sub_struct: Recursive<'a, Token, Spanned<AstSubStruct>, Simple<Token>>,
    sub_union: Recursive<'a, Token, Spanned<AstSubUnion>, Simple<Token>>,
    struct_member: Recursive<'a, Token, Spanned<AstStructMember>, Simple<Token>>,
    union_member: Recursive<'a, Token, Spanned<AstUnionMember>, Simple<Token>>,
) -> impl Parser<Token, Spanned<AstUnionMember>, Error = Simple<Token>> + 'a {
    let t1 = parser_type()
        .then(parser_match_identifier())
        .then_ignore(just(Token::Punct(";")))
        .map_with_span(|(typ, ident), span| (AstUnionMember::Type { ident, typ }, span));
    let t2 = sub_struct
        .then(parser_match_identifier())
        .then_ignore(just(Token::Punct(";")))
        .map_with_span(|(struct_, ident), span| {
            (AstUnionMember::SubStruct { ident, struct_ }, span)
        });
    let t3 = sub_union
        .then(parser_match_identifier())
        .then_ignore(just(Token::Punct(";")))
        .map_with_span(|(union_, ident), span| (AstUnionMember::SubUnion { ident, union_ }, span));
    let t4 = just(Token::Keyword("void"))
        .ignore_then(parser_match_identifier())
        .then_ignore(just(Token::Punct(";")))
        .map_with_span(|ident, span| (AstUnionMember::Void { ident }, span));

    t1.or(t2.or(t3.or(t4)))
}

fn parser_struct_member<'a>(
    sub_struct: Recursive<'a, Token, Spanned<AstSubStruct>, Simple<Token>>,
    sub_union: Recursive<'a, Token, Spanned<AstSubUnion>, Simple<Token>>,
    struct_member: Recursive<'a, Token, Spanned<AstStructMember>, Simple<Token>>,
    union_member: Recursive<'a, Token, Spanned<AstUnionMember>, Simple<Token>>,
) -> impl Parser<Token, Spanned<AstStructMember>, Error = Simple<Token>> + 'a {
    let t1 = parser_type()
        .then(parser_match_identifier())
        .then_ignore(just(Token::Punct(";")))
        .map_with_span(|(typ, ident), span| (AstStructMember::Type { ident, typ }, span));
    let t2 = sub_union
        .then(parser_match_identifier())
        .then_ignore(just(Token::Punct(";")))
        .map_with_span(|(union_, ident), span| (AstStructMember::SubUnion { ident, union_ }, span));

    t1.or(t2)
}

fn generate_recursive_struct_and_union_parser() -> (
    impl Parser<Token, Spanned<AstUnionMember>, Error = Simple<Token>>,
    impl Parser<Token, Spanned<AstStructMember>, Error = Simple<Token>>,
) {
    let mut sub_struct = Recursive::<Token, Spanned<AstSubStruct>, Simple<Token>>::declare();
    let mut sub_union = Recursive::<Token, Spanned<AstSubUnion>, Simple<Token>>::declare();
    let mut struct_member = Recursive::<Token, Spanned<AstStructMember>, Simple<Token>>::declare();
    let mut union_member = Recursive::<Token, Spanned<AstUnionMember>, Simple<Token>>::declare();

    union_member.define(parser_union_member(
        sub_struct.clone(),
        sub_union.clone(),
        struct_member.clone(),
        union_member.clone(),
    ));

    struct_member.define(parser_struct_member(
        sub_struct.clone(),
        sub_union.clone(),
        struct_member.clone(),
        union_member.clone(),
    ));

    sub_struct.define(parser_sub_struct(
        sub_struct.clone(),
        sub_union.clone(),
        struct_member.clone(),
        union_member.clone(),
    ));

    sub_union.define(parser_sub_union(
        sub_struct.clone(),
        sub_union.clone(),
        struct_member.clone(),
        union_member.clone(),
    ));

    (union_member, struct_member)
}

fn parser_typedef_tagged_union(
) -> impl Parser<Token, Spanned<AstTypedefTaggedUnion>, Error = Simple<Token>> {
    let (union_member, _) = generate_recursive_struct_and_union_parser();
    just(Token::Keyword("typedef"))
        .ignore_then(just(Token::Keyword("union")))
        .ignore_then(just(Token::Keyword("tagged")))
        .ignore_then(
            union_member
                .repeated()
                .delimited_by(just(Token::Punct("{")), just(Token::Punct("}"))),
        )
        .then(parser_typedef_type())
        .then(parser_derives().or_not())
        .then_ignore(just(Token::Punct(";")))
        .map_with_span(|((members, typedef_type), derives), span| {
            (
                AstTypedefTaggedUnion {
                    members,
                    typedef_type,
                    derives,
                },
                span,
            )
        })
}

fn parser_typedef_struct() -> impl Parser<Token, Spanned<AstTypedefStruct>, Error = Simple<Token>> {
    let (_, struct_member) = generate_recursive_struct_and_union_parser();
    just(Token::Keyword("typedef"))
        .ignore_then(just(Token::Keyword("struct")))
        .ignore_then(
            struct_member
                .repeated()
                .delimited_by(just(Token::Punct("{")), just(Token::Punct("}"))),
        )
        .then(parser_typedef_type())
        .then(parser_derives().or_not())
        .then_ignore(just(Token::Punct(";")))
        .map_with_span(|((members, typedef_type), derives), span| {
            (
                AstTypedefStruct {
                    members,
                    typedef_type,
                    derives,
                },
                span,
            )
        })
}

fn parser_typedef() -> impl Parser<Token, Spanned<AstTypedef>, Error = Simple<Token>> {
    let t1 = parser_typedef_synonym()
        .map_with_span(|synonym, span| (AstTypedef::Synonym { synonym }, span));
    let t2 = parser_typedef_enum().map_with_span(|enum_, span| (AstTypedef::Enum { enum_ }, span));
    let t3 = parser_typedef_struct()
        .map_with_span(|struct_, span| (AstTypedef::Struct { struct_ }, span));
    let t4 = parser_typedef_tagged_union().map_with_span(|union_, span: std::ops::Range<usize>| {
        (AstTypedef::TaggedUnion { union_ }, span)
    });
    t1.or(t2.or(t3).or(t4))
}

#[derive(Debug)]
pub enum AstExpression {
    Cond,
    Op,
    ExprPrimary(Spanned<AstExprPrimary>),
}

#[derive(Debug)]
pub enum AstExprPrimary {
    Ident(Spanned<AstIdent>),
    IntLiteral(Spanned<AstIntLiteral>),
    RealLiteral(Spanned<AstRealLiteral>),
    StringLiteral(Spanned<AstStringLiteral>),
    Expr(Box<Spanned<AstExpression>>),
    ValueOf(Spanned<AstType>),
    DontCare(Spanned<()>),
}

fn parser_expr() -> impl Parser<Token, Spanned<AstExpression>, Error = Simple<Token>> {
    let mut expression = Recursive::<Token, Spanned<AstExpression>, Simple<Token>>::declare();
    parser_expr_primary(expression).map_with_span(|e, span| (AstExpression::ExprPrimary(e), span))
}

fn parser_expr_primary<'a>(
    expression: Recursive<'a, Token, Spanned<AstExpression>, Simple<Token>>,
) -> impl Parser<Token, Spanned<AstExprPrimary>, Error = Simple<Token>> + 'a {
    choice((
        parser_match_identifier().map_with_span(|e, span| (AstExprPrimary::Ident(e), span)),
        parser_expr_pri_valueof(),
    ))
}

fn parser_expr_pri_valueof<'a>(
) -> impl Parser<Token, Spanned<AstExprPrimary>, Error = Simple<Token>> + 'a {
    just(Token::Keyword("valueof"))
        .or(just(Token::Keyword("valueOf")))
        .ignore_then(parser_type().delimited_by(just(Token::Punct("(")), just(Token::Punct(")"))))
        .map_with_span(|typ, span| (AstExprPrimary::ValueOf(typ), span))
}

#[derive(Debug)]
pub struct AstAttributeInstances {
    insts: Vec<Spanned<AstAttributeInstance>>,
}

#[derive(Debug)]
pub struct AstAttributeInstance {
    attrs: Vec<Spanned<AstAttrSpec>>,
}

#[derive(Debug)]
pub struct AstAttrSpec {
    name: Spanned<AstIdent>,
    expr: Option<Spanned<AstExpression>>,
}

fn parser_attribute_instances(
) -> impl Parser<Token, Spanned<AstAttributeInstances>, Error = Simple<Token>> {
    let attr_spec = parser_match_identifier()
        .then(just(Token::Punct("=")).ignore_then(parser_expr()).or_not())
        .map_with_span(|(name, expr), span| (AstAttrSpec { name, expr }, span));
    let attribute_instance = attr_spec
        .separated_by(just(Token::Punct(",")))
        .at_least(1)
        .delimited_by(
            just(Token::Punct("(")).then(just(Token::Op("*"))),
            just(Token::Op("*")).then(just(Token::Punct(")"))),
        )
        .map_with_span(|attrs, span| (AstAttributeInstance { attrs }, span));
    attribute_instance
        .repeated()
        .at_least(1)
        .map_with_span(|insts, span| (AstAttributeInstances { insts }, span))
}

#[derive(Debug)]
pub struct AstMethodProtoFormal {
    attrs: Option<Spanned<AstAttributeInstances>>,
    type_: Spanned<AstType>,
    ident: Spanned<AstIdent>,
}

#[derive(Debug)]
pub struct AstMethodProtoFormals {
    formals: Vec<Spanned<AstMethodProtoFormal>>,
}

#[derive(Debug)]
pub struct AstMethodProto {
    attrs: Option<Spanned<AstAttributeInstances>>,
    type_: Spanned<AstType>,
    ident: Spanned<AstIdent>,
    formals: Option<Spanned<AstMethodProtoFormals>>,
}

#[derive(Debug)]
pub struct AstSubinterfaceDecl {
    attrs: Option<Spanned<AstAttributeInstances>>,
    typedef: Spanned<AstTypeDefType>,
    ident: Spanned<AstIdent>,
}

#[derive(Debug)]
pub enum AstInterfaceMemberDecl {
    Method(Spanned<AstMethodProto>),
    SubInterface(Spanned<AstSubinterfaceDecl>),
}

#[derive(Debug)]
pub struct AstInterfaceDecl {
    attrs: Option<Spanned<AstAttributeInstances>>,
    typedef: Spanned<AstTypeDefType>,
    members: Vec<Spanned<AstInterfaceMemberDecl>>,
    type_ident: Option<Spanned<AstTypeIde>>,
}

fn parser_interface_decl() -> impl Parser<Token, Spanned<AstInterfaceDecl>, Error = Simple<Token>> {
    let method_proto_formal = parser_attribute_instances()
        .or_not()
        .then(parser_type())
        .then(parser_match_identifier())
        .map_with_span(|((attrs, type_), ident), span| {
            (
                AstMethodProtoFormal {
                    attrs,
                    type_,
                    ident,
                },
                span,
            )
        });

    let method_proto_formals = method_proto_formal
        .separated_by(just(Token::Punct(",")))
        .at_least(1)
        .map_with_span(|formals, span| (AstMethodProtoFormals { formals }, span));

    let method_proto = parser_attribute_instances()
        .or_not()
        .then_ignore(just(Token::Keyword("method")))
        .then(parser_type())
        .then(parser_match_identifier())
        .then(
            method_proto_formals
                .or_not()
                .delimited_by(just(Token::Punct("(")), just(Token::Punct(")")))
                .or_not(),
        )
        .then_ignore(just(Token::Punct(";")))
        .map_with_span(|(((attrs, type_), ident), formals), span| {
            AstInterfaceMemberDecl::Method((
                AstMethodProto {
                    attrs,
                    type_,
                    ident,
                    formals: formals.unwrap_or_default(),
                },
                span,
            ))
        });

    let subinterface_decl = parser_attribute_instances()
        .or_not()
        .then_ignore(just(Token::Keyword("interface")))
        .then(parser_typedef_type())
        .then(parser_match_identifier())
        .then_ignore(just(Token::Punct(";")))
        .map_with_span(|((attrs, typedef), ident), span| {
            AstInterfaceMemberDecl::SubInterface((
                AstSubinterfaceDecl {
                    attrs,
                    typedef,
                    ident,
                },
                span,
            ))
        });

    let interface_member_decl = method_proto
        .or(subinterface_decl)
        .map_with_span(|i, span| (i, span));
    parser_attribute_instances()
        .or_not()
        .then(
            parser_typedef_type()
                .then_ignore(just(Token::Punct(";")))
                .then(interface_member_decl.repeated())
                .delimited_by(
                    just(Token::Keyword("interface")),
                    just(Token::Keyword("endinterface")),
                ),
        )
        .then(
            just(Token::Punct(":"))
                .ignore_then(parser_match_type_ide())
                .or_not(),
        )
        .map_with_span(|((attrs, (typedef, members)), type_ident), span| {
            (
                AstInterfaceDecl {
                    attrs,
                    typedef,
                    members,
                    type_ident,
                },
                span,
            )
        })
}

#[derive(Debug)]
pub enum AstModuleActualParamArg {
    Expr(Spanned<AstExpression>),
    ClockedByExpr(Spanned<AstExpression>),
    ResetByExpr(Spanned<AstExpression>),
}

#[derive(Debug)]
pub struct AstModuleApp {
    ident: Spanned<AstIdent>,
    param_args: Vec<Spanned<AstModuleActualParamArg>>,
}

#[derive(Debug)]
pub struct AstModuleInst {
    attrs: Option<Spanned<AstAttributeInstances>>,
    type_: Spanned<AstType>,
    ident: Spanned<AstIdent>,
    module_app: Spanned<AstModuleApp>,
}

fn parser_module_inst() -> impl Parser<Token, Spanned<AstModuleInst>, Error = Simple<Token>> {
    let t1 = parser_expr().map_with_span(|e, span| (AstModuleActualParamArg::Expr(e), span));
    let t2 = just(Token::Keyword("clocked_by"))
        .ignore_then(parser_expr())
        .map_with_span(|e, span| (AstModuleActualParamArg::ClockedByExpr(e), span));
    let t3 = just(Token::Keyword("reset_by"))
        .ignore_then(parser_expr())
        .map_with_span(|e, span| (AstModuleActualParamArg::ClockedByExpr(e), span));
    let module_actual_param_arg = t1.or(t2).or(t3);
    let module_app = parser_match_identifier()
        .then(
            module_actual_param_arg
                .separated_by(just(Token::Punct(",")))
                .at_least(1)
                .or_not()
                .delimited_by(just(Token::Punct("(")), just(Token::Punct(")")))
                .or_not(),
        )
        .map_with_span(|(ident, param_args), span| {
            (
                AstModuleApp {
                    ident,
                    param_args: param_args.unwrap_or_default().unwrap_or_default(),
                },
                span,
            )
        });
    parser_attribute_instances()
        .or_not()
        .then(parser_type())
        .then(parser_match_identifier())
        .then_ignore(just(Token::Op("<")).then(just(Token::Op("-"))))
        .then(module_app)
        .then_ignore(just(Token::Punct(";")))
        .map_with_span(|(((attrs, type_), ident), module_app), span| {
            (
                AstModuleInst {
                    attrs,
                    type_,
                    ident,
                    module_app,
                },
                span,
            )
        })
}

#[derive(Debug)]
pub struct AstReturnStmt {
    expr: Spanned<AstExpression>,
}
fn parser_return_stmt() -> impl Parser<Token, Spanned<AstReturnStmt>, Error = Simple<Token>> {
    just(Token::Keyword("return"))
        .ignore_then(parser_expr())
        .then_ignore(just(Token::Punct(";")))
        .map_with_span(|expr, span| (AstReturnStmt { expr }, span))
}

pub struct AstFunctionBodyBeginEndStmt {}
pub enum AstBeginEndStmtInner {
    FunctionBody(AstFunctionBodyBeginEndStmt),
}
pub struct AstBeginEndStmt {
    ident1: Option<Spanned<AstIdent>>,
    stmt: Spanned<AstBeginEndStmtInner>,
    ident2: Option<Spanned<AstIdent>>,
}

fn parser_function_body_begin_end_stmt(
    inner_parser: impl Parser<Token, Spanned<AstBeginEndStmtInner>, Error = Simple<Token>>,
) -> impl Parser<Token, Spanned<AstBeginEndStmt>, Error = Simple<Token>> {
    just(Token::Keyword("begin"))
        .ignore_then(just(Token::Punct(":")).ignore_then(parser_match_identifier().or_not()))
        .then(inner_parser.delimited_by(just(Token::Punct("(")), just(Token::Punct(")"))))
        .then_ignore(just(Token::Keyword("end")))
        .then(just(Token::Punct(":")).ignore_then(parser_match_identifier().or_not()))
        .map_with_span(|((ident1, stmt), ident2), span| {
            (
                AstBeginEndStmt {
                    ident1,
                    stmt,
                    ident2,
                },
                span,
            )
        })
}

// fn parser_function_body_stmt(

// ) -> impl Parser<Token, Spanned<AstBeginEndStmt>, Error = Simple<Token>> {}

#[derive(Debug)]
pub struct AstFunctionFormal {
    type_: Spanned<AstType>,
    ident: Spanned<AstIdent>,
}

#[derive(Debug)]
pub struct AstFunctionFormals {
    formals: Vec<Spanned<AstFunctionFormal>>,
}

#[derive(Debug)]
pub struct AstFunctionProto {
    type_: Spanned<AstType>,
    ident: Spanned<AstIdent>,
    formals: Option<Spanned<AstFunctionFormals>>,
    provisos: Option<Spanned<AstProvisos>>,
}

fn parser_function_def() -> impl Parser<Token, Spanned<AstBeginEndStmt>, Error = Simple<Token>> {
    let function_formal = parser_type()
        .then(parser_match_identifier())
        .map_with_span(|(type_, ident), span| (AstFunctionFormal { type_, ident }, span));
    let function_formals = function_formal
        .separated_by(just(Token::Punct(",")))
        .at_least(1)
        .map_with_span(|formals, span| (AstFunctionFormals { formals }, span));
    let function_proto = just(Token::Keyword("function"))
        .ignore_then(parser_type())
        .then(parser_match_identifier())
        .then(
            function_formals
                .or_not()
                .delimited_by(just(Token::Punct("(")), just(Token::Punct(")"))),
        )
        .then(parser_provisos().or_not())
        .map_with_span(|(((type_, ident), formals), provisos), span| {
            (
                AstFunctionProto {
                    type_,
                    ident,
                    formals,
                    provisos,
                },
                span,
            )
        });
    parser_attribute_instances()
        .or_not()
        .then(function_proto)
        .then(parser_function_body())
        .then_ignore(just(Token::Keyword("endfunction")))
        .then(just(Token::Punct(":")).ignore_then(parser_match_identifier()))
}

#[derive(Debug)]
pub struct AstProviso {
    ident: Spanned<AstIdent>,
    types: Vec<Spanned<AstType>>,
}

#[derive(Debug)]
pub struct AstProvisos {
    provisos: Vec<Spanned<AstProviso>>,
}

fn parser_provisos() -> impl Parser<Token, Spanned<AstProvisos>, Error = Simple<Token>> {
    let proviso = parser_match_identifier()
        .then_ignore(just(Token::Punct("#")))
        .then(
            parser_type()
                .separated_by(just(Token::Punct(",")))
                .at_least(1)
                .delimited_by(just(Token::Punct("(")), just(Token::Punct(")"))),
        )
        .map_with_span(|(ident, types), span| (AstProviso { ident, types }, span));
    just(Token::Keyword("provisos"))
        .ignore_then(
            proviso
                .separated_by(just(Token::Punct(",")))
                .at_least(1)
                .delimited_by(just(Token::Punct("(")), just(Token::Punct(")"))),
        )
        .map_with_span(|provisos, span| (AstProvisos { provisos }, span))
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

    // let ret = parser_interface_decl()
    //     // .recover_with(skip_then_retry_until([]))
    //     .repeated()
    //     .collect::<Vec<_>>()
    //     .parse(Stream::from_iter(len..len + 1, tokens.into_iter()));
    // println!("{:#?}", ret);

    let (ast, parse_errs) = parser_provisos()
        .recover_with(skip_then_retry_until([]))
        .repeated()
        .collect::<Vec<_>>()
        .parse_recovery_verbose(Stream::from_iter(len..len + 1, tokens.into_iter()));
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
