#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

#define ATS_DYNLOADFLAG 0

#define ATS_PACKNAME "patsec"

staload "./reader.sats"
staload "./string.sats"

implement reader_new(input: string) =
    let val input = g1ofg0(input)
    in
   @{ input = input
    , input_length = string1_length(input)
    , index = ref(i2sz(0))
    , position = ref(
       @{ column = 1
        , line = 1
        }
    )
    }
    end

implement reader_advance(reader) = (
    case+ reader.get_char() of
    | Some0('\n') =>
        let val new_position =
           @{ column = 1
            , line = !(reader.position).line + 1
            }
        in
            !(reader.position) := new_position
        end
    | Some0(_) =>
        let val new_position =
           @{ column = !(reader.position).column + 1
            , line = !(reader.position).line
            }
        in
            !(reader.position) := new_position
        end
    | None0() => ();
    !(reader.index) := !(reader.index) + 1
)

implement reader_debug(reader) =
    escape_chars(string_make_substring(reader.input, !(reader.index), i2sz(10)))

implement reader_error(reader, error) =
   '{ kind = error
    , position = !(reader.position)
    }

implement reader_get_char(reader) =
    let val index = g1ofg0(!(reader.index))
    in
        if index < reader.input_length then
            let val c: char = string_get_at_size(reader.input, index)
            in
                Some0(c)
            end
        else
            None0()
    end


implement reader_get_char_repr(reader) =
    case+ reader.get_char() of
    | Some0(char) => char2string(char)
    | None0() => "end of file"

implement reader_get_index(reader) =
    !(reader.index)

implement reader_get_position(reader) =
    !(reader.position)

implement reader_set_index(reader, index) =
    !(reader.index) := index

implement reader_set_position(reader, position) =
    !(reader.position) := position
