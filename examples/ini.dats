(*
 * Compilation with debug traces:
 * patscc -IATS ../.. -DATS PATSEC_DEBUG -DATS_MEMALLOC_LIBC -o ini ini.dats ../src/lib.dats ../src/std.dats ../src/reader.dats ../src/string.dats ../src/error.dats -latslib
 *)

#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

#include "atstd/lib.hats"
#include "patsec/lib.hats"

datatype Value =
    | Bool of bool
    | Comment of string
    | Num of double
    | String of string

fun print_value(value: Value) =
    case+ value of
    | Bool(bool) => print!("Bool(", bool, ")")
    | Comment(comment) => print!("Comment(", comment, ")")
    | Num(num) => print!("Num(", num, ")")
    | String(str) => print!("String(", str, ")")

overload print with print_value

typedef IniMap = list0(@(string, list0(@(string, Value))))

fun print_values(values: list0(@(string, Value))) = (
    print!("[");
    list0_foreach(values, lam(@(key, value)) =>
        print!("(", key, ", ", value, ")")
    );
    print!("]")
)

fun print_ini_map(map: IniMap) = (
    print!("[");
    list0_foreach(map, lam(@(group, values)) => (
        print!("(", group, ", ");
        print_values(values);
        print!("), ")
    ));
    println!("]")
)

fun boolean(): Parser(bool) =
    (true <% string "true" <|> false <% string "false") <*
    look_ahead(newline())

fun comment(): Parser(@(string, Value)) =
    (*char '#' >> (many(none_of "\n")) >>= (lam(comment) =>*)
    char '#' *> (many(none_of "\n")) >>= (lam(comment) =>
    return(("", Comment(string_make_list0(comment)))))

fun group_name(): Parser(string) =
    (*char '\[' >>
    many(none_of "[]\127") >>= (lam(name) =>
    char ']' >>
    return(string_make_list0(name)))*)
    between(char '\[', char ']', many(none_of "[]\127\n") >>= (lam(name) =>
        return(string_make_list0(name))
    )) <* newline()

fun key(): Parser(string) =
    (*look_ahead(none_of "[") >>*)
    look_ahead(none_of "[") *>
    many1(alpha_num() <|> one_of "[]_-@") >>= (lam(key) =>
    return(string_make_list0(key))
    )

fun ini_string(): Parser(string) =
    many(none_of "\n") >>= (lam(value) =>
    return(string_make_list0(value))
    )

fun number(): Parser(string) =
    (*string_make_list0 <%> many1(satisfy(lam(char) => isdigit_char(char))) (* TODO: try to make this work *) *)
    (lam(list) => string_make_list0(list)) <%> many1(satisfy(lam(char) => isdigit_char(char)) <?> "a digit")
    (*many1(satisfy(lam(char) => isdigit_char(char))) >>= (lam(list) =>
    return(string_make_list0(list))
    )*)

fun num(): Parser(double) =
    (number() >>= (lam(integer) =>
    char('.') >>
    number() >>= (lam(decimals) =>
    return(g0string2float_double(integer + "." + decimals))
    ))) <*
    look_ahead(newline())

fun value(): Parser(Value) =
    choice(g0ofg1($list(
        tryp((lam(bool) => Bool(bool)) <%> boolean()),
        tryp((lam(num) => Num(num)) <%> num()),
        ((lam(string) => String(string)) <%> ini_string())
    ))) (*<?> "a valid desktop value"*)
    (*((lam(bool) => Bool(bool)) <%> boolean()) <|>
    ((lam(num) => Num(num)) <%> num()) <|>
    ((lam(string) => String(string)) <%> ini_string())*)

fun pair(): Parser(@(string, Value)) =
    key() >>= (lam(key) =>
    char '=' >>
    value() >>= (lam(value) =>
    return((key, value))
    ))

fun block(): Parser(list0(@(string, Value))) =
    end_by(comment() <|> pair(), many1(newline()))

fun group(): Parser(@(string, list0(@(string, Value)))) =
    group_name() >>= (lam(name) =>
    block() >>= (lam(values) =>
    return(@(name, values))
    ))

(*fun line(): Parser(string) =*)
    (*(comment() <|> pair() <|> group()) <*
    many(newline())*)
    (*(comment() <|> pair() <|> group()) >>= (lam(line) =>
    many(newline()) >>
    return(line)
    )*)

fun file(): Parser(IniMap) =
    many_till(group(), eof())
    (*many(line())*)
    (*end_by(line(), many1(newline()))*)

fun parse_desktop(source: string): Result(IniMap, Error) =
    parse(file(), source)

fun parse_desktop_file(filename: string): Result(IniMap, Error) =
    let val file = fileref_open_exn(filename, file_mode_r)
        val content = list0_map(fileref_get_lines_stringlst(file), lam(line) => string_append(line, "\n"))
        val source = stringlst_concat(content)
    in
        parse_desktop(source)
    end

implement main0() = {
    val result = parse(comment(), "# Test comment")
    val () =
        case+ result of
        | Ok(@(_, result)) => (
            print_value(result);
            println!()
        )
        | Err(error) => prerrln!("Error: ", error)

    val result = parse(group_name(), "[name]\n")
    val () =
        case+ result of
        | Ok(name) => println!(name)
        | Err(error) => prerrln!("Error: ", error)

    val result = parse(pair(), "key=value")
    val () =
        case+ result of
        | Ok(result) => (
            print_values(g0ofg1($list(result)));
            println!()
        )
        | Err(error) => prerrln!("Error: ", error)

    val result = parse(file(), "[test]\n# Test comment\n[name]\nkey=value\n")
    val () =
        case+ result of
        | Ok(result) => print_ini_map(result)
        | Err(error) => prerrln!("Error: ", error)

    val result = parse(file(), "[test]\n# Test comment\n[name]\nkey=true is not false\n")
    val () =
        case+ result of
        | Ok(result) => print_ini_map(result)
        | Err(error) => prerrln!("Error: ", error)

    val result = parse(file(), "[test]\n# Test comment\n[name]\nkey=10.0 is not false\n")
    val () =
        case+ result of
        | Ok(result) => print_ini_map(result)
        | Err(error) => prerrln!("Error: ", error)

    val result = parse_desktop_file("/usr/share/applications/termite.desktop")
    val () =
        case+ result of
        | Ok(result) => print_ini_map(result)
        | Err(error) => prerrln!("Error: ", error)

    val result = parse(file(), "[test]\n# Test comment\n[name]\nkey=10.05\n")
    val () =
        case+ result of
        | Ok(result) => print_ini_map(result)
        | Err(error) => prerrln!("Error: ", error)

    val result = parse(file(), "[test\n")
    val () =
        case+ result of
        | Ok(result) => print_ini_map(result)
        | Err(error) => prerrln!("Error: ", error)
}
