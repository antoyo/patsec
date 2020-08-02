#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

#include "atstd/lib.hats"
#include "patsec/lib.hats"

fun optional_test(): Parser(string) =
    option("FAIL", string "test")

implement main0(argc, argv) = {
    val result = parse(optional_test(), "test")
    val () =
        case+ result of
        | Ok(result) => assertloc(result = "test")
        | Err(error) => assertloc(false)

    val result = parse(optional_test(), "notest")
    val () =
        case+ result of
        | Ok(result) => assertloc(result = "FAIL")
        | Err(error) => assertloc(false)
}
