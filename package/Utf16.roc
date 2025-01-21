module [
    from_utf8,
    from_utf8_le,
    from_utf8_be,
    to_str,
    to_utf8,
    from_str,
    from_str_le,
    from_str_be,
    from_codepoints,
    to_codepoints,
]

import Utf8

Utf16CodeUnitProblem : [
    UnpairedHighSurrogate,
    UnpairedLowSurrogate,
    CodepointTooLarge,
]

Utf16Problem : { index : U64, problem : Utf16CodeUnitProblem }

Endian : [Big, Little]

## Convert a Str to Utf-16.
from_str : Str -> Result (List U16) [BadUtf8 Utf8.Utf8Problem]
from_str = |str| str |> Str.to_utf8 |> from_utf8

## Convert a Str to Utf-16 using big-endian byte order.
from_str_le : Str -> Result (List U16) [BadUtf8 Utf8.Utf8Problem]
from_str_le = |str| str |> Str.to_utf8 |> from_utf8_with_bom(Little)

## Convert a Str to Utf-16 using little-endian byte order.
from_str_be : Str -> Result (List U16) [BadUtf8 Utf8.Utf8Problem]
from_str_be = |str| str |> Str.to_utf8 |> from_utf8_with_bom(Big)

## Convert Utf-16 to Utf-8.
to_utf8 : List U16 -> Result (List U8) [BadUtf16 Utf16Problem]
to_utf8 = |utf16|
    try to_codepoints(utf16)
    |> Utf8.from_codepoints
    |> Ok

## Convert Utf-16 to Str.
to_str : List U16 -> Result Str [BadUtf16 Utf16Problem] # , BadUtf8 Utf8.Utf8Problem]
to_str = |utf16|
    try to_utf8(utf16)
    |> Str.from_utf8
    |> Result.on_err(|_| crash "Utf-16 was successfully converted to Utf-8, however string conversion from Utf-8 failed. This is definitely a bug. Please file an issue, with a reproduction if possible.")

expect
    utf16_le = [0xFEFF, 0x003F, 0x003F]
    utf16_be = [0xFFFE, 0x3F00, 0x3F00]
    le_res = to_str(utf16_le)
    be_res = to_str(utf16_be)
    (le_res == Ok "??") and (be_res == le_res)

expect
    to_str([0xFEFF, 0x0041, 0x005A]) == Ok "AZ"

## Convert Utf-8 to Utf-16.
from_utf8 : List U8 -> Result (List U16) [BadUtf8 Utf8.Utf8Problem]
from_utf8 = |utf8| from_utf8_with_bom(utf8, Little)

expect
    utf8 = ['A', 'Z']
    utf16 = from_utf8(utf8)
    utf16 == Ok [0xFFFE, 0x4100, 0x5A00]

## Convert Utf-8 to Utf-16 using little-endian byte order.
from_utf8_le : List U8 -> Result (List U16) [BadUtf8 Utf8.Utf8Problem]
from_utf8_le = |utf8| from_utf8_with_bom(utf8, Little)

expect
    utf8 = ['A', 'Z']
    utf16 = from_utf8_le(utf8)
    utf16 == Ok [0xFFFE, 0x4100, 0x5A00]

## Convert Utf-8 to Utf-16 using big-endian byte order.
from_utf8_be : List U8 -> Result (List U16) [BadUtf8 Utf8.Utf8Problem]
from_utf8_be = |utf8| from_utf8_with_bom(utf8, Big)

expect
    utf8 = ['A', 'Z']
    utf16 = from_utf8_be(utf8)
    utf16 == Ok [0xFEFF, 0x0041, 0x005A]

## Convert Utf-8 to Utf-16 with byte order mark.
from_utf8_with_bom : List U8, Endian -> Result (List U16) [BadUtf8 Utf8.Utf8Problem]
from_utf8_with_bom = |utf8, endian|
    codepoints =
        try Utf8.to_codepoints(utf8)
    codepoints
    |> List.map(from_codepoint)
    |> List.join
    |> List.map(|b| if endian == Little then swap_bytes(b) else b)
    |> List.prepend(if endian == Little then 0xFFFE else 0xFEFF)
    |> Ok

## Convert a list of unicode codepoints to a list of Utf-16 code units.
from_codepoints : List U32 -> List U16
from_codepoints = |codepoints| codepoints |> List.map(from_codepoint) |> List.join |> List.map(swap_bytes)

## Convert a unicode codepoint to Utf-16.
from_codepoint : U32 -> List U16
from_codepoint = |codepoint|
    if codepoint <= 0xFFFF then
        [Num.to_u16(codepoint)]
    else
        offset_codepoint = codepoint - 0x10000
        high_surrogate = Num.to_u16(0xD800 + Num.shift_right_zf_by(offset_codepoint, 10))
        low_surrogate = Num.to_u16(0xDC00 + Num.bitwise_and(offset_codepoint, 0x3FF))
        [high_surrogate, low_surrogate]

## Convert a list of Utf-16 codeunits to unicode codepoints.
to_codepoints : List U16 -> Result (List U32) [BadUtf16 Utf16Problem]
to_codepoints = |utf16|
    when utf16 is
        [0xFEFF, .. as rest] -> to_codepoints_help(rest, 1, Big)
        [0xFFFE, .. as rest] -> to_codepoints_help(rest, 1, Little)
        _ -> to_codepoints_help(utf16, 0, Little)

## Recursive helper for to_codepoints.
to_codepoints_help : List U16, U64, Endian -> Result (List U32) [BadUtf16 Utf16Problem]
to_codepoints_help = |codeunits, index, endian|
    when codeunits is
        [cu1, cu2, .. as rest] if is_high_surrogate(cu1) and is_low_surrogate(cu2) ->
            codepoint = to_codepoint(cu1, cu2)
            if codepoint > 0x10FFFF then
                Err (BadUtf16 { index, problem: CodepointTooLarge })
            else
                Ok List.join([[codepoint], try to_codepoints_help(rest, index + 2, endian)])

        [cu1, cu2, .. as rest] if is_high_surrogate(swap_bytes(cu1)) and is_low_surrogate(swap_bytes(cu2)) ->
            codepoint = to_codepoint(swap_bytes(cu1), swap_bytes(cu2))
            if codepoint > 0x10FFFF then
                Err (BadUtf16 { index, problem: CodepointTooLarge })
            else
                Ok List.join([[codepoint], try to_codepoints_help(rest, index + 2, endian)])

        [cu1, ..] if is_high_surrogate(cu1) ->
            Err (BadUtf16 { index, problem: UnpairedHighSurrogate })

        [cu1, ..] if is_low_surrogate(cu1) ->
            Err (BadUtf16 { index, problem: UnpairedLowSurrogate })

        [cu1, .. as rest] ->
            codepoint =
                when endian is
                    Big -> Num.to_u32(cu1)
                    Little -> Num.to_u32(swap_bytes(cu1))
            Ok List.join([[codepoint], try to_codepoints_help(rest, index + 1, endian)])

        [] ->
            Ok []

## Check if a codeunit is a high surrogate.
is_high_surrogate : U16 -> Bool
is_high_surrogate = |cu| cu >= 0xD800 and cu <= 0xDBFF

## Check if a codeunit is a low surrogate.
is_low_surrogate : U16 -> Bool
is_low_surrogate = |cu| cu >= 0xDC00 and cu <= 0xDFFF

## Convert a high and low surrogate pair to a unicode codepoint.
to_codepoint : U16, U16 -> U32
to_codepoint = |high_surrogate, low_surrogate|
    high_surrogate_offset = Num.to_u32(high_surrogate - 0xD800)
    low_surrogate_offset = Num.to_u32(low_surrogate - 0xDC00)
    0x10000 + Num.shift_left_by(high_surrogate_offset, 10) + low_surrogate_offset

## Swap the high and low bytes of a U16.
swap_bytes : U16 -> U16
swap_bytes = |x| Num.bitwise_or(Num.shift_left_by(Num.bitwise_and(x, 0xFF), 8), Num.shift_right_zf_by(x, 8))

expect (swap_bytes(0xFFFE) == 0xFEFF) and (swap_bytes(0xFEFF) == 0xFFFE)
