#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

#define ATS_DYNLOADFLAG 0

#define ATS_PACKNAME "patsec"

staload "./error.sats"

implement combine_errors(error1, error2) =
    let val kind =
        case+ (error1.kind, error2.kind) of
        | (EmptyList(), _) => EmptyList()
        | (_, EmptyList()) => EmptyList()
        | (Unexpected(_), error) => error
        | (error, Unexpected(_)) => error
        | (UnexpectedExpecting('{ actual = actual, expected = expected1 }),
           UnexpectedExpecting('{ expected = expected2, ... })) => UnexpectedExpecting(
             '{ actual = actual
              , expected = expected1 + " or " + expected2
              }
           )
    in
       '{ kind = kind
        , position = error1.position
        }
    end

implement maybe_combine_errors(error1, error2) =
    case+ error1 of
    | Some0(error) => combine_errors(error, error2)
    | None0() => error2

implement position_to_string(position) =
    "(line " + tostring_val<int>(position.line) + ", column " + tostring_val<int>(position.column) + ")"
