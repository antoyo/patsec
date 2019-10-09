(*
 * FIXME: column in position seems to be off by one.
 * TODO: improve debugging trace by taking inspiration from:
 * https://github.com/jwiegley/parsec-free
 *)

#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

#include "atstd/lib.hats"

#include "./fixity.ats"

#define ATS_DYNLOADFLAG 0

staload UN = "prelude/SATS/unsafe.sats"

#define ATS_PACKNAME "patsec"

staload "./error.sats"
staload "./lib.sats"
staload "./reader.sats"
staload "./string.sats"

var debug_indent: int = 0
val indent = ref_make_viewptr(view@debug_indent | addr@debug_indent)

#ifdef PATSEC_DEBUG
#then
implement debug(msg) =
    prerrln!(ref_get_elt(indent) * " ", "> ", msg)
#else
implement debug(msg) = ()
#endif

#ifdef PATSEC_DEBUG
#then
implement debug_reader(reader) =
    prerrln!(ref_get_elt(indent) * " ", "> Input: ", reader.debug())
#else
implement debug_reader(reader) = ()
#endif

#ifdef PATSEC_DEBUG
#then
implement failed(msg) =
    prerrln!(ref_get_elt(indent) * " ", "> ", msg, " FAILED")
#else
implement failed(msg) = ()
#endif

#ifdef PATSEC_DEBUG
#then
implement inc_indent() =
    let val current_indent: int = $UN.cast{int}(ref_get_elt(indent))
    in
    ref_set_elt(indent, current_indent + 4)
    end
#else
implement inc_indent() = ()
#endif

#ifdef PATSEC_DEBUG
#then
implement dec_indent() =
    let val current_indent: int = $UN.cast{int}(ref_get_elt(indent))
    in
    ref_set_elt(indent, current_indent - 4)
    end
#else
implement dec_indent() = ()
#endif

implement {a} return(value) =
   '{ parser = lam(reader) =>
        Ok(value)
    }

implement pdebug(msg) =
   '{ parser = lam(reader) => (
        debug(msg);
        Ok(@())
        )
    }

implement pdec_indent() =
   '{ parser = lam(reader) => (
        dec_indent();
        Ok(@())
        )
    }

implement pinc_indent() =
   '{ parser = lam(reader) => (
        inc_indent();
        Ok(@())
        )
    }

implement {a} with_indent(msg, parser) =
   '{ parser = lam(reader) => (
        debug(msg);
        debug_reader(reader);
        inc_indent();
        let val result = parser.parser(reader) in (
            dec_indent();
            case+ result of
            | Ok(_) => result
            | Err(_) => (
                failed(msg);
                result
            )
        )
        end
    )
    }

implement {a} without_indent(msg, parser) =
   '{ parser = lam(reader) => (
        debug(msg);
        debug_reader(reader);
        let val result = parser.parser(reader) in (
            case+ result of
            | Ok(_) => result
            | Err(_) => (
                failed(msg);
                result
            )
        )
        end
    )
    }

implement {a}{b} app_left(parser1, parser2) =
   '{ parser = lam(reader) =>
        case+ parser1.parser(reader) of
        | Ok(result) =>
            (case+ parser2.parser(reader) of
            | Ok(_) => Ok(result)
            | Err(error) => Err(error)
            )
        | Err(error) => Err(error)
    }

implement {a}{b} app_right(parser1, parser2) =
   '{ parser = lam(reader) =>
        case+ parser1.parser(reader) of
        | Ok(_) =>
            (case+ parser2.parser(reader) of
            | Ok(result) => Ok(result)
            | Err(error) => Err(error)
            )
        | Err(error) => Err(error)
    }

implement {a}{b} fmap_left(value, parser) =
   '{ parser = lam(reader) =>
        case+ parser.parser(reader) of
        | Ok(_) => Ok(value)
        | Err(error) => Err(error)
    }

implement {a}{b} fmap(func, parser) =
   '{ parser = lam(reader) =>
        case+ parser.parser(reader) of
        | Ok(value) => Ok(func(value))
        | Err(error) => Err(error)
    }

implement {a}{b} fmap_right(parser, value) =
   '{ parser = lam(reader) =>
        case+ parser.parser(reader) of
        | Ok(result) => Ok(value)
        | Err(error) => Err(error)
    }

implement {a}{b} bind_(parser1, parser2) =
   '{ parser = lam(reader) =>
        case+ parser1.parser(reader) of
        | Ok(_) => parser2.parser(reader)
        | Err(error) => Err(error)
    }

implement {a}{b} bind(parser, callback) =
   '{ parser = lam(reader) =>
        case+ parser.parser(reader) of
        | Ok(a) =>
            let val parser2 = callback(a)
            in
                parser2.parser(reader)
            end
        | Err(error) => Err(error)
    }

implement {a} label(parser, label) =
   '{ parser = lam(reader) =>
        case+ parser.parser(reader) of
        | Ok(a) => Ok(a)
        | Err(error) => Err(
           '{ kind = UnexpectedExpecting(
               '{ actual = escape_chars(reader.get_char_repr())
                , expected = label
                }
            )
            , position = error.position
            }
        )
    }

implement alpha() =
    with_indent("alpha", one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

implement digit() =
    satisfy(lam(char) => isdigit_char(char)) <?> "a digit"

implement {a} or(parser1, parser2) =
    with_indent("or",
   '{ parser = lam(reader) =>
        let val start = reader.get_index()
        in
            case+ parser1.parser(reader) of
            | Ok(value) => Ok(value)
            | Err(error) =>
                if reader.get_index() = start then
                    let val start = reader.get_index()
                    in
                        (* We try the second parser only if the first one did not consumed any input. *)
                        case+ parser2.parser(reader) of
                        | Ok(value) => Ok(value)
                        | Err(error2) =>
                            if reader.get_index() = start then
                                (* We only combine the errors if the second parser did not consumed any input. *)
                                Err(combine_errors(error, error2))
                            else
                                Err(error2)
                    end
                else
                    Err(error)
        end
    })

implement alpha_num() =
    with_indent("alpha_num", alpha() <|> digit())

implement {a} {b} {c} between(start, end_, middle) =
    with_indent("between",
    start >>
    middle >>= (lam (result) =>
    end_ >>
    return result)
    )

implement char(character) =
   without_indent("char " + escape_chars(char2string(character)), '{ parser = lam(reader) =>
        case+ reader.get_char() of
        | None0() =>
            Err(reader.error(UnexpectedExpecting(
               '{ actual = "end of file"
                , expected = escape_chars(char2string(character))
                }
            )))
        | Some0(first) => (
            if character = first then (
                reader.advance();
                Ok(first)
            )
            else
                Err(reader.error(UnexpectedExpecting(
                    (* TODO: only escape when showing the error? *)
                   '{ actual = escape_chars(char2string(first))
                    , expected = escape_chars(char2string(character))
                    }
                )))
        )
    })

implement {a} choice(parsers) =
    with_indent("choice", '{ parser = lam(reader) =>
        let fun {a: t@ype} call(parsers: list0(Parser(a)), actual_error: option0(Error)): Result(a, Error) =
            case+ parsers of
            | list0_nil() =>
                (case+ actual_error of
                | Some0(error) => Err(error)
                | None0() => Err(reader.error(EmptyList))
                )
            | list0_cons(parser, rest) =>
                let val start = reader.get_index()
                in
                    case+ parser.parser(reader) of
                    | Ok(value) => Ok(value)
                    | Err(error) =>
                        if reader.get_index() = start then
                            (* We try the rest of the  parsers only if the current one did not consumed any input. *)
                            call(rest, Some0(maybe_combine_errors(actual_error, error)))
                        else
                            Err(error)
                end
        in
            call(parsers, None0())
        end
    })

implement eof() =
   without_indent("eof", '{ parser = lam(reader) => (
        case+ reader.get_char() of
        | Some0(_) => Err(reader.error(UnexpectedExpecting(
               '{ actual = escape_chars(reader.get_char_repr())
                , expected = "end of input"
                }
            )))
        | None0() => Ok(@())
    )})

implement {a} {b} end_by(parser1, parser2) =
    with_indent("end_by", '{ parser = lam(reader) => (
        let fun parse(acc: list0(a)) =
            let val start = reader.get_index()
            in
                case+ parser1.parser(reader) of
                | Ok(result) =>
                    let val start = reader.get_index()
                    in
                        case+ parser2.parser(reader) of
                        | Ok(_) => parse(cons0(result, acc))
                        | Err(error) => Err(error)
                    end
                | Err(error) =>
                    if reader.get_index() = start then
                        (* Succeed only if the first parser did not consumed any input. *)
                        (* TODO: parse parser2 here as well. *)
                        Ok(list0_reverse(acc))
                    else
                        Err(error)
            end
        in
            parse(nil0())
        end
    )})

implement {a} look_ahead(parser) =
    with_indent("look_ahead", '{ parser = lam(reader) =>
        let val start = reader.get_index()
            val position = reader.get_position()
        in
            case+ parser.parser(reader) of
            | Ok(value) => (
                reader.set_index(start); (* Look ahead does not consume input. *)
                reader.set_position(position);
                Ok(value)
            )
            | Err(error) => Err(error)
        end
    })

implement {a} not_followed_by(parser) =
    with_indent("not_followed_by", '{ parser = lam(reader) =>
        let val start = reader.get_index()
            val position = reader.get_position()
        in
            case+ parser.parser(reader) of
            | Ok(value) => Err(reader.error(Unexpected('{ actual = escape_chars(reader.get_char_repr()) })))
            | Err(error) => (
                reader.set_index(start); (* Not followed by does not consume input. *)
                reader.set_position(position);
                Ok(@())
            )
        end
    })

implement {a} many(parser) =
    with_indent("many", '{ parser = lam(reader) =>
        let fun parse(acc: list0(a)) =
            let val start = reader.get_index()
            in
                case+ parser.parser(reader) of
                | Ok(result) =>
                    parse(cons0(result, acc))
                | Err(error) =>
                    if reader.get_index() = start then
                        (* We return the result only if the parser did not consumed any input. *)
                        Ok(list0_reverse(acc))
                    else
                        Err(error)
            end
        in
            parse(nil0())
        end
    })

implement {a} many1(parser) =
    (* TODO: try previous implementation again. *)
    with_indent("many1", '{ parser = lam(reader) =>
        let fun parse(acc: list0(a)) =
            let val start = reader.get_index()
            in
                case+ parser.parser(reader) of
                | Ok(result) =>
                    parse(cons0(result, acc))
                | Err(error) =>
                    if reader.get_index() = start then
                        (* We return the result only if the parser did not consumed any input. *)
                        if list0_is_empty(acc) then
                            Err(error)
                        else
                            Ok(list0_reverse(acc))
                    else
                        Err(error)
            end
        in
            parse(nil0())
        end
    })

implement {a}{b} many_till(parser, end_parser) =
    with_indent("many_till", '{ parser = lam(reader) =>
        let fun parse(acc: list0(a)) =
            let val start = reader.get_index()
            in
                case+ parser.parser(reader) of
                | Ok(result) =>
                    let val new_acc = cons0(result, acc)
                    in
                        case+ end_parser.parser(reader) of
                        | Ok(_) => Ok(list0_reverse(new_acc))
                        | Err(_) => parse(new_acc)
                    end
                | Err(error) =>
                    if reader.get_index() = start then
                        case+ end_parser.parser(reader) of
                        | Ok(_) => Ok(list0_reverse(acc))
                        | Err(error) => Err(error)
                    else
                        Err(error)
            end
        in
            parse(nil0())
        end
    })

implement natural() =
    many1(digit()) >>= (lam(digits) =>
    return(g0string2int(string_make_list0(digits)))
    )

implement newline() =
    with_indent("newline", char '\n')

implement none_of(chars) =
    with_indent("none_of \"" + escape_chars(chars) + "\"", '{ parser = lam(reader) =>
        case+ reader.get_char() of
        | None0() =>
            Err(reader.error(UnexpectedExpecting(
               '{ actual = "end of file"
                , expected = "none of " + escape_chars(chars)
                }
            )))
        | Some0(first) =>
            if first <> 0 && string_find_index(chars, $UN.cast{charNZ}(first)) <> i2ssz(~1) then
                Err(reader.error(UnexpectedExpecting(
                   '{ actual = escape_chars(char2string(first))
                    , expected = "none of " + escape_chars(chars)
                    }
                )))
            else (
                reader.advance();
                Ok(first)
            )
    })

implement one_of(chars) =
    with_indent("one_of \"" + escape_chars(chars) + "\"", '{ parser = lam(reader) =>
        case+ reader.get_char() of
        | None0() =>
            Err(reader.error(UnexpectedExpecting(
               '{ actual = "end of file"
                , expected = "one of " + escape_chars(chars)
                }
            )))
        | Some0(first) =>
            if first <> 0 && string_find_index(chars, $UN.cast{charNZ}(first)) <> i2ssz(~1) then (
                reader.advance();
                Ok(first)
            )
            else
                Err(reader.error(UnexpectedExpecting(
                   '{ actual = escape_chars(char2string(first))
                    , expected = "one of " + escape_chars(chars)
                    }
                )))
    })

implement {a} option(default, parser) =
    with_indent("option",
   '{ parser = lam(reader) =>
        let val start = reader.get_index()
        in
            case+ parser.parser(reader) of
            | Ok(value) => Ok(value)
            | Err(error) =>
                if reader.get_index() = start then
                    Ok(default)
                else
                    Err(error)
        end
    })

implement satisfy(predicate) =
   without_indent("satisfy", '{ parser = lam(reader) =>
        case+ reader.get_char() of
        | None0() =>
            Err(reader.error(Unexpected(
               '{ actual = "end of file"
                }
            )))
        | Some0(first) => (
            if predicate(first) then (
                reader.advance();
                Ok(first)
            )
            else
                Err(reader.error(Unexpected(
                   '{ actual = escape_chars(char2string(first))
                    }
                )))
        )
    })

implement {a} skip_many(parser) =
    with_indent("skip_many",
    many(parser) *>
    return(@())
    )

implement space() =
    (* FIXME: other characters are spaces. *)
    char ' '

implement spaces() =
    skip_many(space())

implement string(initial_string) =
    without_indent("string " + initial_string, '{ parser = lam(reader) => (
        let fun process(chars: list0(char)): Result(string, Error) =
            case+ chars of
            | list0_nil() => Ok(initial_string)
            | list0_cons(character, chars) =>
                (case+ reader.get_char() of
                | None0() =>
                    Err(reader.error(UnexpectedExpecting(
                       '{ actual = "end of file"
                        , expected = escape_chars(char2string(character))
                        }
                    )))
                | Some0(first) =>
                    if character = first then (
                        reader.advance();
                        process(chars)
                    )
                    else
                        Err(reader.error(UnexpectedExpecting(
                           '{ actual = escape_chars(char2string(first))
                            , expected = escape_chars(char2string(character))
                            }
                        )))
                )
        in
            process(string_explode(initial_string))
        end
    )
    }) <?> initial_string

implement {a} tryp(parser) =
    without_indent("try", '{ parser = lam(reader) =>
    let val start = reader.get_index()
        val position = reader.get_position()
        val result = parser.parser(reader)
    in
        case+ result of
        | Ok(_) => result
        | Err(_) => (
            reader.set_index(start); (* Try does not consume input if it fails. *)
            reader.set_position(position);
            result
        )
    end
    })

implement get_position() =
   '{ parser = lam(reader) => Ok(reader.get_position()) }

implement {a} parse(parser, input) =
    parser.parser(reader_new(input))

implement error_string(error) =
    let val msg =
        case+ error.kind of
        | EmptyList() => "empty list in choice"
        | Unexpected(error) =>  "unexpected " + error.actual
        | UnexpectedExpecting(error) =>  "unexpected " + error.actual + "\nexpecting " + error.expected
    in
        error.position.to_string() + ":\n" + msg
    end

implement prerr_error(error) =
    prerr!(error_string(error))
