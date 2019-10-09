#include "share/atspre_staload_libats_ML.hats"

#include "atstd/lib.hats"

#include "./fixity.ats"

#define ATS_PACKNAME "patsec"

staload "./error.sats"
staload "./reader.sats"

fun debug(msg: string): void
fun debug_reader(reader: Reader): void
fun failed(msg: string): void
fun inc_indent(): void
fun dec_indent(): void

typedef Parser(a: t@ype) =
  '{ parser = (Reader) -<cloref1> Result(a, Error)
   }

fun pdebug(msg: string): Parser(@())
fun pinc_indent(): Parser(@())
fun pdec_indent(): Parser(@())
fun {a: t@ype} with_indent(msg: string, parser: Parser(a)): Parser(a)
fun {a: t@ype} without_indent(msg: string, parser: Parser(a)): Parser(a)

fun alpha(): Parser(char)

fun alpha_num(): Parser(char)

fun {a: t@ype} {b: t@ype} {c: t@ype} between(start: Parser(a), end_: Parser(b), middle: Parser(c)): Parser(c)

fun char(character: char): Parser char

fun {a: t@ype} choice(parsers: list0(Parser(a))): Parser(a)

fun digit(): Parser(char)

fun eof(): Parser(@())

fun {a: t@ype} {b: t@ype} end_by(parser1: Parser(a), parser2: Parser(b)): Parser(list0(a))

fun {a: t@ype} look_ahead(parser: Parser(a)): Parser(a)

fun {a: t@ype} many(parser: Parser(a)): Parser(list0(a))

fun {a: t@ype} many1(parser: Parser(a)): Parser(list0(a))

fun {a: t@ype} {b: t@ype} many_till(parser: Parser(a), end_parser: Parser(b)): Parser(list0(a))

fun natural(): Parser(int)

fun newline(): Parser(char)

fun none_of(chars: string): Parser(char)

fun {a: t@ype} not_followed_by(parser: Parser(a)): Parser(@())

fun one_of(chars: string): Parser(char)

fun {a: t@ype} option(default: a, parser: Parser(a)): Parser(a)

fun {a: t@ype} or(parser1: Parser(a), parser2: Parser(a)): Parser(a)

macdef <|> (a, b) = or(,(a), ,(b))

fun {a: t@ype} return(value: a): Parser(a)

fun satisfy(predicate: (char) -<cloref1> bool): Parser(char)

fun {a: t@ype} skip_many(parser: Parser(a)): Parser(@())

fun space(): Parser(char)

fun spaces(): Parser(@())

fun string(str: string): Parser string

fun {a: t@ype} tryp(parser: Parser(a)): Parser(a)

fun {a: t@ype}{b: t@ype} app_left(parser1: Parser(a), parser2: Parser(b)): Parser(a)
fun {a: t@ype}{b: t@ype} app_right(parser1: Parser(a), parser2: Parser(b)): Parser(b)

overload <* with app_left
overload *> with app_right

fun {a: t@ype}{b: t@ype} fmap_left(value: a, parser: Parser(b)): Parser(a)
fun {a: t@ype}{b: t@ype} fmap_right(parser: Parser(a), value: b): Parser(b)
fun {a: t@ype}{b: t@ype} fmap(func: (a) -<cloref1> b, parser: Parser(a)): Parser(b)

overload <% with fmap_left
overload %> with fmap_right
overload <%> with fmap

fun {a: t@ype} label(parser: Parser(a), label: string): Parser(a)

overload <?> with label

fun {a: t@ype}{b: t@ype} bind(parser1: Parser(a), (a) -<cloref1> Parser(b)): Parser(b)

fun {a: t@ype}{b: t@ype} bind_(parser1: Parser(a), parser2: Parser(b)): Parser(b)

macdef >>= (a, b) = bind(,(a), ,(b))

(*overload >> with bind*)
overload >> with bind_

(*infixr >>=
fun {a: t@ype}{b: t@ype} >>=(parser1: Parser(a), (a) -<cloref1> Parser(b)): Parser(b)*)

(*infixr >>
fun {a: t@ype}{b: t@ype} >>(parser1: Parser(a), parser2: Parser(b)): Parser(b)*)

fun get_position(): Parser(Position)

fun {a: t@ype} parse(parser: Parser(a), input: string): Result(a, Error)

fun error_string(error: Error): string

fun prerr_error(error: Error): void

overload prerr with prerr_error
