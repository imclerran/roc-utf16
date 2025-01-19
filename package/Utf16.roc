module [utf8_to_utf16, str_to_utf16, utf16_to_utf8, utf16_to_str]

import Unsafe

Utf8 : List U8
Utf16 : List U16

Utf8ByteProblem : [
    InvalidStartByte,
    UnexpectedEndOfSequence,
    ExpectedContinuation,
    OverlongEncoding,
    CodepointTooLarge,
    EncodesSurrogateHalf,
]

Utf8Problem : { index : U64, problem : Utf8ByteProblem }

str_to_utf16 : Str -> Result Utf16 [BadUtf8 Utf8Problem]
str_to_utf16 = |str| str |> Str.to_utf8 |> utf8_to_utf16

## Convert a UTF-8 encoded byte sequence to a UTF-16 encoded code unit sequence.
utf8_to_utf16 : Utf8 -> Result Utf16 [BadUtf8 Utf8Problem]
utf8_to_utf16 = |utf8|
    max_code_units = List.len(utf8)
    get_byte = |index| List.get(utf8, index)
    valid_continuations = |bytes| List.all(bytes, |b| Num.bitwise_and(b, 0x80) == 0x80)

    converted = List.walk_with_index_until(
        utf8,
        Ok { utf16: List.with_capacity(max_code_units), skip_bytes: 0 },
        |state_result, byte, index|
            state = Unsafe.unwrap(state_result, "Unreachable: state is always Ok")
            if state.skip_bytes > 0 then
                Continue (Ok { state & skip_bytes: state.skip_bytes - 1 })
            else if Num.bitwise_and(byte, 0xF8) == 0xF0 then
                # 4 byte codedpoint
                when (get_byte(index + 1), get_byte(index + 2), get_byte(index + 3)) is
                    (Ok byte2, Ok byte3, Ok byte4) ->
                        if !valid_continuations([byte2, byte3, byte4]) then
                            Break (Err (BadUtf8({ index, problem: ExpectedContinuation })))
                        else if byte == 0xF0 and Num.bitwise_xor(byte2, 0x80) < 0x10 then
                            return Break (Err (BadUtf8({ index: index + 0, problem: OverlongEncoding })))
                            else

                        codepoint =
                            (Num.bitwise_and(byte, 0x07) |> Num.to_u32 |> Num.shift_left_by(18))
                            |> Num.bitwise_or(Num.bitwise_and(byte2, 0x3F) |> Num.to_u32 |> Num.shift_left_by(12))
                            |> Num.bitwise_or(Num.bitwise_and(byte3, 0x3F) |> Num.to_u32 |> Num.shift_left_by(6))
                            |> Num.bitwise_or(Num.bitwise_and(byte4, 0x3F) |> Num.to_u32)

                        if codepoint > 0x10FFFF then
                            Break (Err (BadUtf8({ index, problem: CodepointTooLarge })))
                        else if codepoint >= 0xD800 and codepoint <= 0xDFFF then
                            # Unreachable - eliminated by OverlongEncoding check
                            Break (Err (BadUtf8({ index, problem: EncodesSurrogateHalf })))
                        else
                            offset_codepoint = codepoint - 0x10000
                            high_surrogate = Num.to_u16(0xD800 + Num.shift_right_zf_by(offset_codepoint, 10))
                            low_surrogate = Num.to_u16(0xDC00 + Num.bitwise_and(offset_codepoint, 0x3FF))
                            Continue (Ok { state & utf16: List.join([state.utf16, [high_surrogate, low_surrogate]]), skip_bytes: 3 })

                    _ -> Break (Err (BadUtf8({ index, problem: UnexpectedEndOfSequence })))
            else if Num.bitwise_and(byte, 0xF0) == 0xE0 then
                # 3 byte codepoint
                when (get_byte(index + 1), get_byte(index + 2)) is
                    (Ok byte2, Ok byte3) ->
                        if !valid_continuations([byte2, byte3]) then
                            Break (Err (BadUtf8({ index, problem: ExpectedContinuation })))
                        else if byte == 0xE0 and Num.bitwise_xor(byte2, 0x80) < 0x20 then
                            return Break (Err (BadUtf8({ index: index + 0, problem: OverlongEncoding })))
                            else

                        code_unit =
                            (Num.bitwise_and(byte, 0x0F) |> Num.to_u16 |> Num.shift_left_by(12))
                            |> Num.bitwise_or(Num.bitwise_and(byte2, 0x3F) |> Num.to_u16 |> Num.shift_left_by(6))
                            |> Num.bitwise_or(Num.bitwise_and(byte3, 0x3F) |> Num.to_u16)
                        Continue (Ok { state & utf16: List.append(state.utf16, code_unit), skip_bytes: 2 })

                    _ -> Break (Err (BadUtf8({ index, problem: UnexpectedEndOfSequence })))
            else if Num.bitwise_and(byte, 0xE0) == 0xC0 then
                # 2 byte codepoint
                when get_byte(index + 1) is
                    Ok byte2 if !valid_continuations([byte2]) ->
                        Break (Err (BadUtf8({ index, problem: ExpectedContinuation })))

                    Ok byte2 ->
                        code_unit =
                            (Num.bitwise_and(byte, 0x1F) |> Num.to_u16 |> Num.shift_left_by(6))
                            |> Num.bitwise_or(Num.bitwise_and(byte2, 0x3F) |> Num.to_u16)
                        Continue (Ok { state & utf16: List.append(state.utf16, code_unit), skip_bytes: 1 })

                    Err _ -> Break (Err (BadUtf8({ index, problem: UnexpectedEndOfSequence })))
            else if byte < 0x80 then
                # 1 byte codepoint
                Continue (Ok ({ state & utf16: List.append(state.utf16, Num.to_u16(byte)) }))
            else
                Break (Err (BadUtf8({ index, problem: InvalidStartByte }))),
    )
    when converted is
        Ok final -> final.utf16 |> List.release_excess_capacity |> Ok
        Err err -> Err err

utf16_to_str : Utf16 -> Result Str [BadUtf16]
utf16_to_str = |utf16|
    utf16
    |> utf16_to_utf8
    |> Result.try(|utf8| Str.from_utf8(utf8))
    |> Result.map_err(|_| BadUtf16)

utf16_to_utf8 : Utf16 -> Result Utf8 [BadUtf16]
utf16_to_utf8 = |utf16|
    max_code_units = List.len(utf16)
    get_code_unit = |index| List.get(utf16, index)

    converted = List.walk_with_index_until(
        utf16,
        { utf8: List.with_capacity(max_code_units), skip_code_units: 0, err: None },
        |state, code_unit, index|
            if state.skip_code_units > 0 then
                Continue { state & skip_code_units: state.skip_code_units - 1 }
            else if code_unit >= 0xD800 and code_unit <= 0xDBFF then
                # high surrogate
                when (code_unit, get_code_unit(index + 1)) is
                    (high_surrogate, Ok(low_surrogate)) ->
                        codepoint =
                            Num.shift_left_by(Num.to_u32(high_surrogate) - 0xD800, 10)
                            |> Num.bitwise_or(Num.to_u32(low_surrogate) - 0xDC00)
                            |> Num.add(0x10000)
                        Continue({ state & utf8: List.join([state.utf8, utf8_encode_codepoint(codepoint)]), skip_code_units: 1 })

                    _ -> Break({ state & err: BadUtf16 })
            else if code_unit >= 0xDC00 and code_unit <= 0xDFFF then
                # low surrogate
                Break({ state & err: BadUtf16 })
            else
                codepoint = code_unit |> Num.to_u32
                Continue { state & utf8: List.join([state.utf8, utf8_encode_codepoint(codepoint)]) },
    )
    when converted.err is
        None -> converted.utf8 |> List.release_excess_capacity |> Ok
        _ -> Err BadUtf16

utf8_encode_codepoint : U32 -> List U8
utf8_encode_codepoint = |codepoint|
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

# =============================================================================
# TESTS: SUCCESS CASES
# =============================================================================

# -----------------------------------------------------------------------------
# Test string to UTF-16 and back

expect
    "🔥" |> str_to_utf16 |> Result.try(utf16_to_str) == Ok("🔥")

expect
    str = "The quick brown brown fox jumps over the lazy dog."
    str |> str_to_utf16 |> Result.try(utf16_to_str) == Ok(str)

# -----------------------------------------------------------------------------
# Test various size UTF-8 codepoints

expect
    # 2x codepoint (1 byte each)
    utf8 = List.repeat(0b0111_1111, 2)
    expected = utf8 |> List.map(Num.to_u16)
    utf16 = utf8_to_utf16(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> utf16_to_utf8 == Ok(utf8))

expect
    # 1x codepoint (2 bytes)
    utf8 = [0b1101_1111, 0b1011_1111]
    expected = [0b0000_0111_1111_1111]
    utf16 = utf8_to_utf16(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> utf16_to_utf8 == Ok(utf8))

expect
    # 1x codepoint (3 bytes)
    utf8 = [0b1110_1111, 0b1011_1111, 0b1011_1111]
    expected = [0b1111_1111_1111_1111]
    utf16 = utf8_to_utf16(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> utf16_to_utf8 == Ok(utf8))

expect
    # 2x codepoint (3 bytes each)
    utf8 = [0b1110_1111, 0b1011_1111, 0b1011_1111] |> List.repeat(2) |> List.join
    expected = [0b1111_1111_1111_1111] |> List.repeat(2) |> List.join
    utf16 = utf8_to_utf16(utf8) |> Result.with_default([])

    (utf16 == expected) and (utf16 |> utf16_to_utf8 == Ok(utf8))

expect
    # 1x codepoint (4 bytes) - high/low surrogate pair
    utf8 = [0b1111_0100, 0b1000_1111, 0b1011_1111, 0b1011_1111]
    expected = [0xD800 + 0x3FF, 0xDC00 + 0x3FF]
    utf16 = utf8_to_utf16(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> utf16_to_utf8 == Ok(utf8))

expect
    # 2x codepoint (4 bytes each) - high/low surrogate pairs
    utf8 = List.repeat([0b1111_0100, 0b1000_1111, 0b1011_1111, 0b1011_1111], 2) |> List.join
    expected = List.repeat([0xD800 + 0x3FF, 0xDC00 + 0x3FF], 2) |> List.join
    utf16 = utf8_to_utf16(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> utf16_to_utf8 == Ok(utf8))

# =============================================================================
# TESTS: ERROR CASES
# =============================================================================

# -----------------------------------------------------------------------------
# InvalidStartByte

expect
    # Start with continuation - validate against Str.from_utf8
    utf8 = [0b1000_0000]
    utf16_res = utf8_to_utf16(utf8) |> Result.map_ok(|_| ResultIsErr)
    str_res = Str.from_utf8(utf8) |> Result.map_ok(|_| ResultIsErr)
    expected = Err(BadUtf8({ index: 0, problem: InvalidStartByte }))

    (utf16_res == str_res) and (utf16_res == expected)

expect
    # Too many consecutive bits in prefix - validate against Str.from_utf8
    utf8 = [0b1111_1000]
    utf16_res = utf8_to_utf16(utf8) |> Result.map_ok(|_| ResultIsErr)
    str_res = Str.from_utf8(utf8) |> Result.map_ok(|_| ResultIsErr)
    expected = Err(BadUtf8({ index: 0, problem: InvalidStartByte }))

    (utf16_res == str_res) and (utf16_res == expected)

# -----------------------------------------------------------------------------
# UnexpectedEndOfSequence

expect
    # Unexpected end of sequence - validate against Str.from_utf8
    utf8_2 = [0b1101_1111]
    utf8_3 = [0b1110_1111, 0b1011_1111]
    utf8_4 = [0b1111_0111, 0b1011_1111, 0b1011_1111]
    utf16_2_res = utf8_to_utf16(utf8_2) |> Result.map_ok(|_| ResultIsErr)
    utf16_3_res = utf8_to_utf16(utf8_3) |> Result.map_ok(|_| ResultIsErr)
    utf16_4_res = utf8_to_utf16(utf8_4) |> Result.map_ok(|_| ResultIsErr)
    err_2 = Err(BadUtf8({ index: 0, problem: UnexpectedEndOfSequence }))
    err_3 = Err(BadUtf8({ index: 0, problem: UnexpectedEndOfSequence }))
    err_4 = Err(BadUtf8({ index: 0, problem: UnexpectedEndOfSequence }))
    str_2_res = Str.from_utf8(utf8_2) |> Result.map_ok(|_| ResultIsErr)
    str_3_res = Str.from_utf8(utf8_3) |> Result.map_ok(|_| ResultIsErr)
    str_4_res = Str.from_utf8(utf8_4) |> Result.map_ok(|_| ResultIsErr)

    (utf16_2_res == err_2)
    and (utf16_3_res == err_3)
    and (utf16_4_res == err_4)
    and
    (utf16_2_res == str_2_res)
    and (utf16_3_res == str_3_res)
    and (utf16_4_res == str_4_res)

# -----------------------------------------------------------------------------
# ExpectedContinuation

expect
    # Missing continuation bytes
    utf8_2 = [0b1101_1111, 0b0011_1111]
    utf8_3 = [0b1110_1111, 0b1011_1111, 0b0011_1111]
    utf8_4 = [0b1111_0111, 0b1011_1111, 0b1011_1111, 0b0011_1111]
    utf16_2_res = utf8_to_utf16(utf8_2) |> Result.map_ok(|_| ResultIsErr)
    utf16_3_res = utf8_to_utf16(utf8_3) |> Result.map_ok(|_| ResultIsErr)
    utf16_4_res = utf8_to_utf16(utf8_4) |> Result.map_ok(|_| ResultIsErr)
    err_2 = Err(BadUtf8({ index: 0, problem: ExpectedContinuation }))
    err_3 = Err(BadUtf8({ index: 0, problem: ExpectedContinuation }))
    err_4 = Err(BadUtf8({ index: 0, problem: ExpectedContinuation }))
    str_2_res = Str.from_utf8(utf8_2) |> Result.map_ok(|_| ResultIsErr)
    str_3_res = Str.from_utf8(utf8_3) |> Result.map_ok(|_| ResultIsErr)
    str_4_res = Str.from_utf8(utf8_4) |> Result.map_ok(|_| ResultIsErr)

    (utf16_2_res == err_2)
    and (utf16_3_res == err_3)
    and (utf16_4_res == err_4)
    and
    (utf16_2_res == str_2_res)
    and (utf16_3_res == str_3_res)
    and (utf16_4_res == str_4_res)

expect
    # Validate missing expected continuation byte error index against Str.from_utf8
    utf8 = [0b1111_0111, 0b1011_1111, 0b1011_1111, 0b0011_1111]
    utf16_res = utf8_to_utf16(utf8) |> Result.map_ok(|_| ResultIsErr)
    err = Err(BadUtf8({ index: 0, problem: ExpectedContinuation }))
    str_res = Str.from_utf8(utf8) |> Result.map_ok(|_| ResultIsErr)

    (utf16_res == err) and (utf16_res == str_res)

# -----------------------------------------------------------------------------
# OverlongEncoding

expect
    # only 11 bits used in 3 byte codepoint
    utf8_okay = [0b1110_0000, 0b1010_0000, 0b1000_0000]
    utf8_overlong = [0b1110_0000, 0b1001_1111, 0b1011_1111]
    expected_err = Err(BadUtf8({ index: 0, problem: OverlongEncoding }))
    utf16_okay = utf8_to_utf16(utf8_okay) |> Result.map_ok(|_| Encoded)
    utf16_err = utf8_to_utf16(utf8_overlong)

    (utf16_okay == Ok Encoded) and (utf16_err == expected_err)

expect
    # only 16 bits used in 4 byte codepoint
    utf8_okay = [0b1111_0000, 0b1001_0000, 0b1000_0000, 0b1000_0000]
    utf8_overlong = [0b1111_0000, 0b1000_1111, 0b1011_1111, 0b1011_1111]
    expected_err = Err(BadUtf8({ index: 0, problem: OverlongEncoding }))
    utf16_okay = utf8_to_utf16(utf8_okay) |> Result.map_ok(|_| Encoded)
    utf16_err = utf8_to_utf16(utf8_overlong)

    (utf16_okay == Ok Encoded) and (utf16_err == expected_err)

# -----------------------------------------------------------------------------
# CodepointTooLarge

expect
    # 0x110000 (0x10FFFF + 1) - validate against Str.from_utf8
    utf8 = [0b1111_0100, 0b1001_0000, 0b1000_0000, 0b1000_0000]
    utf16_res = utf8_to_utf16(utf8) |> Result.map_ok(|_| ResultIsErr)
    err = Err(BadUtf8({ index: 0, problem: CodepointTooLarge }))
    str_res = Str.from_utf8(utf8) |> Result.map_ok(|_| ResultIsErr)

    (utf16_res == err)
    and
    (utf16_res == str_res)

# 0b1_0000_1111_1111_1111_1111

# -----------------------------------------------------------------------------
# EncodesSurrogateHalf

expect
    # surrogate half # 0xD800 = 0b1101_1000_0000_0000
    utf8_4 = [0b1111_0000, 0b1000_1101, 0b1000_0000, 0b1000_0000]
    utf16_res = utf8_to_utf16(utf8_4) |> Result.map_ok(|_| ResultIsErr)
    str_4_res = Str.from_utf8(utf8_4) |> Result.map_ok(|_| ResultIsErr)
    # err_4 = Err(BadUtf8({ index: 0, problem: EncodesSurrogateHalf }))
    # str_4_res == err_4 && # Str.from_utf8 does not check for surrogate half
    (utf16_res == str_4_res)
