#include "share/atspre_staload_libats_ML.hats"

#define ATS_PACKNAME "patsec"

typedef Position =
   @{ column = int
    , line = int
    }

typedef ExpectingError =
   '{ expected = string
    }

typedef UnexpectedError =
   '{ actual = string
    }

typedef UnexpectedExpectingError =
   '{ actual = string
    , expected = string
    }

datatype ErrorKind =
    | EmptyList
    | Unexpected of UnexpectedError
    | UnexpectedExpecting of UnexpectedExpectingError

typedef Error =
   '{ kind = ErrorKind
    , position = Position
    }

fun combine_errors(error1: Error, error2: Error): Error

fun maybe_combine_errors(error1: option0(Error), error2: Error): Error

fun position_to_string(position: Position): string

overload .to_string with position_to_string
