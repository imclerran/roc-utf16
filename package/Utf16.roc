module [utf8_to_utf16, str_to_utf16, utf16_to_utf8, utf16_to_str]

Utf8 : List U8
Utf16 : List U16

str_to_utf16 : Str -> Result Utf16 [BadUtf8]
str_to_utf16 = |str| str |> Str.to_utf8 |> utf8_to_utf16

utf8_to_utf16 : Utf8 -> Result Utf16 [BadUtf8]
utf8_to_utf16 = |utf8|
    max_code_units = List.len(utf8)
    get_byte = |index| List.get(utf8, index)

    converted = List.walk_with_index_until(
        utf8,
        { utf16: List.with_capacity(max_code_units), skip_bytes: 0, err: None },
        |state, byte, index|
            if state.skip_bytes > 0 then
                Continue { state & skip_bytes: state.skip_bytes - 1 }
            else if Num.bitwise_and(byte, 0xF0) == 0xF0 then
                # 4 byte codepoint
                when (get_byte(index + 1), get_byte(index + 2), get_byte(index + 3)) is
                    (Ok byte2, Ok byte3, Ok byte4) ->
                        codepoint =
                            (Num.bitwise_and(byte, 0x07) |> Num.to_u32 |> Num.shift_left_by(18))
                            |> Num.bitwise_or(Num.bitwise_and(byte2, 0x3F) |> Num.to_u32 |> Num.shift_left_by(12))
                            |> Num.bitwise_or(Num.bitwise_and(byte3, 0x3F) |> Num.to_u32 |> Num.shift_left_by(6))
                            |> Num.bitwise_or(Num.bitwise_and(byte4, 0x3F) |> Num.to_u32)

                        offset_codepoint = codepoint - 0x10000
                        high_surrogate = Num.to_u16(0xD800 + Num.shift_right_zf_by(offset_codepoint, 10))
                        low_surrogate = Num.to_u16(0xDC00 + Num.bitwise_and(offset_codepoint, 0x3FF))
                        Continue { state & utf16: List.join([state.utf16, [high_surrogate, low_surrogate]]), skip_bytes: 3 }

                    _ -> Break { state & err: BadUtf8 }
            else if Num.bitwise_and(byte, 0xE0) == 0xE0 then
                # 3 byte codepoint
                when (get_byte(index + 1), get_byte(index + 2)) is
                    (Ok byte2, Ok byte3) ->
                        code_unit =
                            (Num.bitwise_and(byte, 0x0F) |> Num.to_u16 |> Num.shift_left_by(12))
                            |> Num.bitwise_or(Num.bitwise_and(byte2, 0x3F) |> Num.to_u16 |> Num.shift_left_by(6))
                            |> Num.bitwise_or(Num.bitwise_and(byte3, 0x3F) |> Num.to_u16)
                        Continue { state & utf16: List.append(state.utf16, code_unit), skip_bytes: 2 }

                    _ -> Break { state & err: BadUtf8 }
            else if Num.bitwise_and(byte, 0xC0) == 0xC0 then
                # 2 byte codepoint
                when get_byte(index + 1) is
                    Ok byte2 ->
                        code_unit =
                            (Num.bitwise_and(byte, 0x1F) |> Num.to_u16 |> Num.shift_left_by(6))
                            |> Num.bitwise_or(Num.bitwise_and(byte2, 0x3F) |> Num.to_u16)
                        Continue { state & utf16: List.append(state.utf16, code_unit), skip_bytes: 1 }

                    Err _ -> Break { state & err: BadUtf8 }
            else if byte < 0x80 then
                # 1 byte codepoint
                Continue { state & utf16: List.append(state.utf16, Num.to_u16(byte)) }
            else
                Break { state & err: BadUtf8 },
    )
    when converted.err is
        None -> converted.utf16 |> List.release_excess_capacity |> Ok
        _ -> Err BadUtf8

utf16_to_str : Utf16 -> Result Str [BadUtf16]
utf16_to_str = |utf16|
    utf16 
    |> utf16_to_utf8 
    |> Result.try(|utf8| Str.from_utf8(utf8) |> Result.map_err(|_| BadUtf16))

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
            else if code_unit >= 0xD800 && code_unit <= 0xDBFF then
                # high surrogate
                when (code_unit, get_code_unit(index + 1)) is
                    (high_surrogate, Ok(low_surrogate)) ->
                        codepoint =
                            Num.shift_left_by(Num.to_u32(high_surrogate) - 0xD800, 10)
                            |> Num.bitwise_or(Num.to_u32(low_surrogate) - 0xDC00)
                            |> Num.add(0x10000)
                        Continue({ state & utf8: List.join([state.utf8, utf8_encode_codepoint(codepoint)]), skip_code_units: 1 })

                    _ -> Break({ state & err: BadUtf16 })
            else if code_unit >= 0xDC00 && code_unit <= 0xDFFF then
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
# TESTS:
expect
    # 2 bytes, 2 codepoints
    utf8 = List.repeat(0b0111_1111, 2)
    expected = utf8 |> List.map(Num.to_u16)
    utf16 = utf8_to_utf16(utf8) |> Result.with_default []

    (utf16 == expected) && (utf16 |> utf16_to_utf8 == Ok(utf8))

expect
    # 2 bytes, 1 codepoint`
    utf8 = [0b1101_1111, 0b1011_1111]
    expected = [0b0000_0111_1111_1111]
    utf16 = utf8_to_utf16(utf8) |> Result.with_default []

    (utf16 == expected) && (utf16 |> utf16_to_utf8 == Ok(utf8))

expect
    # 3 bytes, 1 codepoint
    utf8 = [0b1110_1111, 0b1011_1111, 0b1011_1111]
    expected = [0b1111_1111_1111_1111]
    utf16 = utf8_to_utf16(utf8) |> Result.with_default []

    (utf16 == expected) && (utf16 |> utf16_to_utf8 == Ok(utf8))

expect
    # 6 bytes, 2 codepoints
    utf8 = [0b1110_1111, 0b1011_1111, 0b1011_1111, 0b1110_1111, 0b1011_1111, 0b1011_1111]
    expected = [0b1111_1111_1111_1111, 0b1111_1111_1111_1111]
    utf16 = utf8_to_utf16(utf8) |> Result.with_default([])

    (utf16 == expected) && (utf16 |> utf16_to_utf8 == Ok(utf8))

expect
    # 4 bytes, 1 codepoint (high/low surrogate pair)
    utf8 = [0b1111_0100, 0b1000_1111, 0b1011_1111, 0b1011_1111]
    expected = [0xD800 + 0x3FF, 0xDC00 + 0x3FF]
    utf16 = utf8_to_utf16(utf8) |> Result.with_default []

    (utf16 == expected) && (utf16 |> utf16_to_utf8 == Ok(utf8))

expect
    # 8 bytes, 2 codepoints
    utf8 = List.repeat([0b1111_0100, 0b1000_1111, 0b1011_1111, 0b1011_1111], 2) |> List.join
    expected = List.repeat([0xD800 + 0x3FF, 0xDC00 + 0x3FF], 2) |> List.join
    utf16 = utf8_to_utf16(utf8) |> Result.with_default []

    (utf16 == expected) && (utf16 |> utf16_to_utf8 == Ok(utf8))

expect
    "🔥" |> str_to_utf16 |> Result.try(utf16_to_str) == Ok("🔥")
