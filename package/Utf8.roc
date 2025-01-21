module [Utf8Problem, to_codepoints, from_codepoints]

Utf8ByteProblem : [
    InvalidStartByte,
    UnexpectedEndOfSequence,
    ExpectedContinuation,
    OverlongEncoding,
    CodepointTooLarge,
    EncodesSurrogateHalf,
]

Utf8Problem : { index : U64, problem : Utf8ByteProblem }

## Convert a list of Utf-8 code units to a list of codepoints.
to_codepoints : List U8 -> Result (List U32) [BadUtf8 Utf8Problem]
to_codepoints = |utf8| to_codepoints_help(utf8, 0)

## Recursive helper for to_codepoints.
to_codepoints_help : List U8, U64 -> Result (List U32) _
to_codepoints_help = |bytes, index|
    when bytes is
        [b1, b2, b3, b4, .. as rest] if has_4_byte_prefix(b1) ->
            when codeunits_to_codepoint([b1, b2, b3, b4]) is
                Ok(codepoint) ->
                    remainder = try to_codepoints_help(rest, index + 4)
                    Ok List.join([[codepoint], remainder])

                Err (BadUtf8 problem) -> Err (BadUtf8 { index, problem })

        [b1, b2, b3, .. as rest] if has_3_byte_prefix(b1) ->
            when codeunits_to_codepoint([b1, b2, b3]) is
                Ok(codepoint) ->
                    remainder = try to_codepoints_help(rest, index + 3)
                    Ok List.join([[codepoint], remainder])

                Err (BadUtf8 problem) -> Err (BadUtf8 { index, problem })

        [b1, b2, .. as rest] if has_2_byte_prefix(b1) ->
            when codeunits_to_codepoint([b1, b2]) is
                Ok(codepoint) ->
                    remainder = try to_codepoints_help(rest, index + 2)
                    Ok List.join([[codepoint], remainder])

                Err (BadUtf8 problem) -> Err (BadUtf8 { index, problem })

        [b1, .. as rest] if has_1_byte_prefix(b1) ->
            when codeunits_to_codepoint([b1]) is
                Ok(codepoint) ->
                    remainder = try to_codepoints_help(rest, index + 1)
                    Ok List.join([[codepoint], remainder])

                Err (BadUtf8 problem) -> Err (BadUtf8 { index, problem })

        [b1, ..] if is_invalid_start_byte(b1) ->
            Err (BadUtf8 { index, problem: InvalidStartByte })

        [] ->
            Ok []

        _ ->
            Err (BadUtf8 { index, problem: UnexpectedEndOfSequence })

## Convert a list of codepoints to a list of Utf-8 code units.
codeunits_to_codepoint : List U8 -> Result U32 [BadUtf8 Utf8ByteProblem]
codeunits_to_codepoint = |bytes|
    when bytes is
        [b1, b2, b3, b4] if has_4_byte_prefix(b1) ->
            if !validate_continuations([b2, b3, b4]) then
                Err (BadUtf8 ExpectedContinuation)
            else if b1 == 0xF0 and Num.bitwise_xor(b2, 0x80) < 0x10 then
                Err (BadUtf8 OverlongEncoding)
            else
                codepoint =
                    (Num.bitwise_and(b1, 0x07) |> Num.to_u32 |> Num.shift_left_by(18))
                    |> Num.bitwise_or(Num.bitwise_and(b2, 0x3F) |> Num.to_u32 |> Num.shift_left_by(12))
                    |> Num.bitwise_or(Num.bitwise_and(b3, 0x3F) |> Num.to_u32 |> Num.shift_left_by(6))
                    |> Num.bitwise_or(Num.bitwise_and(b4, 0x3F) |> Num.to_u32)
                if codepoint >= 0xD800 and codepoint <= 0xDFFF then
                    Err (BadUtf8 EncodesSurrogateHalf)
                else if codepoint > 0x10FFFF then
                    Err (BadUtf8 CodepointTooLarge)
                else
                    Ok codepoint

        [b1, b2, b3] if has_3_byte_prefix(b1) ->
            if !validate_continuations([b2, b3]) then
                Err (BadUtf8 ExpectedContinuation)
            else if b1 == 0xE0 and Num.bitwise_xor(b2, 0x80) < 0x20 then
                Err (BadUtf8 OverlongEncoding)
            else
                codepoint =
                    (Num.bitwise_and(b1, 0x0F) |> Num.to_u32 |> Num.shift_left_by(12))
                    |> Num.bitwise_or(Num.bitwise_and(b2, 0x3F) |> Num.to_u32 |> Num.shift_left_by(6))
                    |> Num.bitwise_or(Num.bitwise_and(b3, 0x3F) |> Num.to_u32)
                if codepoint >= 0xD800 and codepoint <= 0xDFFF then
                    Err (BadUtf8 EncodesSurrogateHalf)
                else
                    Ok codepoint

        [b1, b2] if has_2_byte_prefix(b1) ->
            if !has_continuation_prefix(b2) then
                Err (BadUtf8 ExpectedContinuation)
            else
                (Num.bitwise_and(b1, 0x1F) |> Num.to_u32 |> Num.shift_left_by(6))
                |> Num.bitwise_or(Num.bitwise_and(b2, 0x3F) |> Num.to_u32)
                |> Ok

        [b1] if has_1_byte_prefix(b1) ->
            Num.to_u32(b1) |> Ok

        _ ->
            Err (BadUtf8 UnexpectedEndOfSequence)

## Convert a list of codepoints to a list of Utf-8 code units.
from_codepoints : List U32 -> List U8
from_codepoints = |codepoints|
    List.join(codepoints |> List.map(codepoint_to_codeunits))

## Convert a codepoint to a list of Utf-8 code units.
codepoint_to_codeunits : U32 -> List U8
codepoint_to_codeunits = |codepoint|
    if codepoint < 0x80 then
        [Num.to_u8(codepoint)]
    else if codepoint < 0x800 then
        [
            Num.shift_right_by(codepoint, 6) |> Num.bitwise_or(0xC0) |> Num.to_u8,
            Num.bitwise_and(codepoint, 0x3F) |> Num.bitwise_or(0x80) |> Num.to_u8,
        ]
    else if codepoint < 0x10000 then
        [
            Num.shift_right_by(codepoint, 12) |> Num.bitwise_or(0xE0) |> Num.to_u8,
            Num.shift_right_by(codepoint, 6) |> Num.bitwise_and(0x3F) |> Num.bitwise_or(0x80) |> Num.to_u8,
            Num.bitwise_and(codepoint, 0x3F) |> Num.bitwise_or(0x80) |> Num.to_u8,
        ]
    else
        [
            Num.shift_right_by(codepoint, 18) |> Num.bitwise_or(0xF0) |> Num.to_u8,
            Num.shift_right_by(codepoint, 12) |> Num.bitwise_and(0x3F) |> Num.bitwise_or(0x80) |> Num.to_u8,
            Num.shift_right_by(codepoint, 6) |> Num.bitwise_and(0x3F) |> Num.bitwise_or(0x80) |> Num.to_u8,
            Num.bitwise_and(codepoint, 0x3F) |> Num.bitwise_or(0x80) |> Num.to_u8,
        ]

## Check if a code unit is the first of a 4-byte sequence.
has_4_byte_prefix : U8 -> Bool
has_4_byte_prefix = |byte| Num.bitwise_and(byte, 0xF8) == 0xF0

## Check if a code unit is the first of a 3-byte sequence.
has_3_byte_prefix : U8 -> Bool
has_3_byte_prefix = |byte| Num.bitwise_and(byte, 0xF0) == 0xE0

## Check if a code unit is the first of a 2-byte sequence.
has_2_byte_prefix : U8 -> Bool
has_2_byte_prefix = |byte| Num.bitwise_and(byte, 0xE0) == 0xC0

## Check if a code unit is a single byte.
has_1_byte_prefix : U8 -> Bool
has_1_byte_prefix = |byte| Num.bitwise_and(byte, 0x80) == 0x00

## Check if a code unit is a continuation byte.
has_continuation_prefix : U8 -> Bool
has_continuation_prefix = |byte| Num.bitwise_and(byte, 0xC0) == 0x80

## Check if a set of bytes are all continuation bytes.
validate_continuations : List U8 -> Bool
validate_continuations = |bytes| List.all(bytes, has_continuation_prefix)

## Check if a code unit is an invalid start byte.
is_invalid_start_byte : U8 -> Bool
is_invalid_start_byte = |byte| Num.bitwise_and(byte, 0xC0) == 0x80 or byte >= 0xF8
