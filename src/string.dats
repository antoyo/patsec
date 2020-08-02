#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

#define ATS_DYNLOADFLAG 0

#define ATS_PACKNAME "patsec"

staload "./string.sats"

implement escape_chars(str) =
    if str = " " then
        "` `"
    else
        stringlst_concat(string_list0_map(str, lam(char) =>
            case+ char of
            | '\n' => "\\n"
            | '\t' => "\\t"
            | char => char2string(char)
        ))

