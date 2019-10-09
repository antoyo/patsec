#include "share/atspre_staload_libats_ML.hats"

#define ATS_PACKNAME "patsec"

staload "./error.sats"

typedef Reader =
   [n: int]
   @{ input = string(n)
    , input_length = size_t(n)
    , index = ref(size_t)
    , position = ref(Position)
    }

fun reader_new(input: string): Reader

fun reader_advance(reader: Reader): void

overload .advance with reader_advance

fun reader_debug(reader: Reader): string

overload .debug with reader_debug

fun reader_error(reader: Reader, error: ErrorKind): Error

overload .error with reader_error

fun reader_get_char(reader: Reader): option0(char)

overload .get_char with reader_get_char

fun reader_get_char_repr(reader: Reader): string

overload .get_char_repr with reader_get_char_repr

fun reader_get_index(reader: Reader): size_t

overload .get_index with reader_get_index

fun reader_get_position(reader: Reader): Position

overload .get_position with reader_get_position

fun reader_set_index(reader: Reader, index: size_t): void

overload .set_index with reader_set_index

fun reader_set_position(reader: Reader, position: Position): void

overload .set_position with reader_set_position
