module [utf8_to_utf16, str_to_utf16]

Utf8 : List U8
Utf16 : List U16

str_to_utf16 : Str -> Result Utf16 [BadUtf8]_
str_to_utf16 = |str| str |> Str.to_utf8 |> utf8_to_utf16

utf8_to_utf16 : Utf8 -> Result Utf16 [BadUtf8]_
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
    if converted.err == None then
        converted.utf16 |> List.release_excess_capacity |> Ok
    else
        Err converted.err

expect
    # 2 bytes, 2 codepoints
    utf8 = List.repeat(0b0111_1111, 2)
    expected = utf8 |> List.map(Num.to_u16)
    utf16 = utf8_to_utf16(utf8)
    utf16 == Ok(expected)

expect
    # 2 bytes, 1 codepoint`
    utf8 = [0b1101_1111, 0b1011_1111]
    expected = [0b0000_0111_1111_1111]
    utf16 = utf8_to_utf16(utf8)
    utf16 == Ok(expected)

expect
    # 3 bytes, 1 codepoint
    utf8 = [0b1110_1111, 0b1011_1111, 0b1011_1111]
    expected = [0b1111_1111_1111_1111]
    utf16 = utf8_to_utf16(utf8)
    utf16 == Ok(expected)

expect
    # 6 bytes, 2 codepoints
    utf8 = [0b1110_1111, 0b1011_1111, 0b1011_1111, 0b1110_1111, 0b1011_1111, 0b1011_1111]
    expected = [0b1111_1111_1111_1111, 0b1111_1111_1111_1111]
    utf16 = utf8_to_utf16(utf8)
    utf16 == Ok(expected)

expect
    # 4 bytes, 1 codepoint (high/low surrogate pair)
    utf8 = [0b1111_0100, 0b1000_1111, 0b1011_1111, 0b1011_1111]
    expected = [0xD800 + 0x3FF, 0xDC00 + 0x3FF]
    utf16 = utf8_to_utf16(utf8)
    utf16 == Ok(expected)

expect
    # 8 bytes, 2 codepoints
    utf8 = List.repeat([0b1111_0100, 0b1000_1111, 0b1011_1111, 0b1011_1111], 2) |> List.join
    expected = List.repeat([0xD800 + 0x3FF, 0xDC00 + 0x3FF], 2) |> List.join
    utf16 = utf8_to_utf16(utf8)
    utf16 == Ok(expected)
