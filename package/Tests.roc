module []

# import Utf8
import Utf16

# =============================================================================
# TESTS: SUCCESS CASES
# =============================================================================

# -----------------------------------------------------------------------------
# Test string to UTF-16 and back

## BE: [0xFEFF, 0xD83D, 0xDD25]
## LE: [0xFFFE, 0x3DD8, 0x25DD]

expect
    "🔥" |> Utf16.from_str_le |> Result.try(Utf16.to_str) == Ok("🔥")

expect
    str = "The quick brown brown fox jumps over the lazy dog."
    str |> Utf16.from_str |> Result.try(Utf16.to_str) == Ok(str)

# -----------------------------------------------------------------------------
# Test various size UTF-8 codepoints

expect
    # 2x codepoint (1 byte each) - default byte order
    utf8 = List.repeat(0b0111_1111, 2)
    expected = [0xFFFE, 0b0111_1111_0000_0000, 0b0111_1111_0000_0000]
    utf16 = Utf16.from_utf8(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 2x codepoint (1 byte each) - little-endian byte order
    utf8 = List.repeat(0b0111_1111, 2)
    expected = [0xFFFE, 0b0111_1111_0000_0000, 0b0111_1111_0000_0000]
    utf16 = Utf16.from_utf8_le(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 2x codepoint (1 byte each) - big-endian byte order
    utf8 = List.repeat(0b0111_1111, 2)
    expected = [0xFEFF, 0b0111_1111, 0b0111_1111]
    utf16 = Utf16.from_utf8_be(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 1x codepoint (2 bytes) - default byte order
    utf8 = [0b1101_1111, 0b1011_1111]
    expected = [0xFFFE, 0b1111_1111_0000_0111]
    utf16 = Utf16.from_utf8(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 1x codepoint (2 bytes) - little-endian byte order
    utf8 = [0b1101_1111, 0b1011_1111]
    expected = [0xFFFE, 0b1111_1111_0000_0111]
    utf16 = Utf16.from_utf8_le(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 1x codepoint (2 bytes) - big-endian byte order
    utf8 = [0b1101_1111, 0b1011_1111]
    expected = [0xFEFF, 0b0000_0111_1111_1111]
    utf16 = Utf16.from_utf8_be(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 1x codepoint (3 bytes) - default byte order
    utf8 = [0b1110_1111, 0b1011_1111, 0b1011_1111]
    expected = [0xFFFE, 0b1111_1111_1111_1111]
    utf16 = Utf16.from_utf8(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 1x codepoint (3 bytes) - little-endian byte order
    utf8 = [0b1110_1111, 0b1011_1111, 0b1011_1111]
    expected = [0xFFFE, 0b1111_1111_1111_1111]
    utf16 = Utf16.from_utf8_le(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 1x codepoint (3 bytes) - big-endian byte order
    utf8 = [0b1110_1111, 0b1011_1111, 0b1011_1111]
    expected = [0xFEFF, 0b1111_1111_1111_1111]
    utf16 = Utf16.from_utf8_be(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 2x codepoint (3 bytes each) - default byte order
    utf8 = [0b1110_1111, 0b1011_1111, 0b1011_1111] |> List.repeat(2) |> List.join
    expected = [0b1111_1111_1111_1111] |> List.repeat(2) |> List.join |> List.prepend 0xFFFE
    utf16 = Utf16.from_utf8(utf8) |> Result.with_default([])

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 2x codepoint (3 bytes each) - little-endian byte order
    utf8 = [0b1110_1111, 0b1011_1111, 0b1011_1111] |> List.repeat(2) |> List.join
    expected = [0b1111_1111_1111_1111] |> List.repeat(2) |> List.join |> List.prepend 0xFFFE
    utf16 = Utf16.from_utf8_le(utf8) |> Result.with_default([])

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 2x codepoint (3 bytes each) - big-endian byte order
    utf8 = [0b1110_1111, 0b1011_1111, 0b1011_1111] |> List.repeat(2) |> List.join
    expected = [0b1111_1111_1111_1111] |> List.repeat(2) |> List.join |> List.prepend 0xFEFF
    utf16 = Utf16.from_utf8_be(utf8) |> Result.with_default([])

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 1x codepoint (4 bytes) - high/low surrogate pair, default byte order
    utf8 = [0b1111_0100, 0b1000_1111, 0b1011_1111, 0b1011_1111]
    expected = [0xFFFE, 0xFFDB, 0xFFDF]
    utf16 = Utf16.from_utf8(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 1x codepoint (4 bytes) - high/low surrogate pair, little-endian byte order
    utf8 = [0b1111_0100, 0b1000_1111, 0b1011_1111, 0b1011_1111]
    expected = [0xFFFE, 0xFFDB, 0xFFDF]
    utf16 = Utf16.from_utf8_le(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 1x codepoint (4 bytes) - high/low surrogate pair, big-endian byte order
    utf8 = [0b1111_0100, 0b1000_1111, 0b1011_1111, 0b1011_1111]
    expected = [0xFEFF, 0xDBFF, 0xDFFF]
    utf16 = Utf16.from_utf8_be(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 2x codepoint (4 bytes each) - high/low surrogate pairs, default byte order
    utf8 = List.repeat([0b1111_0100, 0b1000_1111, 0b1011_1111, 0b1011_1111], 2) |> List.join
    expected = List.repeat([0xFFDB, 0xFFDF], 2) |> List.join |> List.prepend 0xFFFE
    utf16 = Utf16.from_utf8(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 2x codepoint (4 bytes each) - high/low surrogate pairs, big-endian byte order
    utf8 = List.repeat([0b1111_0100, 0b1000_1111, 0b1011_1111, 0b1011_1111], 2) |> List.join
    expected = List.repeat([0xFFDB, 0xFFDF], 2) |> List.join |> List.prepend 0xFFFE
    utf16 = Utf16.from_utf8_le(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

expect
    # 2x codepoint (4 bytes each) - high/low surrogate pairs, big-endian byte order
    utf8 = List.repeat([0b1111_0100, 0b1000_1111, 0b1011_1111, 0b1011_1111], 2) |> List.join
    expected = List.repeat([0xDBFF, 0xDFFF], 2) |> List.join |> List.prepend 0xFEFF
    utf16 = Utf16.from_utf8_be(utf8) |> Result.with_default []

    (utf16 == expected) and (utf16 |> Utf16.to_utf8 == Ok(utf8))

# =============================================================================
# TESTS: ERROR CASES
# =============================================================================

# -----------------------------------------------------------------------------
# InvalidStartByte

expect
    # Start with continuation - validate against Str.from_utf8
    utf8 = [0b1000_0000]
    utf16_res = Utf16.from_utf8(utf8) |> Result.map_ok(|_| ResultIsErr)
    str_res = Str.from_utf8(utf8) |> Result.map_ok(|_| ResultIsErr)
    expected = Err(BadUtf8({ index: 0, problem: InvalidStartByte }))

    (utf16_res == str_res) and (utf16_res == expected)

expect
    # Too many consecutive bits in prefix - validate against Str.from_utf8
    utf8 = [0b1111_1000]
    utf16_res = Utf16.from_utf8(utf8) |> Result.map_ok(|_| ResultIsErr)
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
    utf16_2_res = Utf16.from_utf8(utf8_2) |> Result.map_ok(|_| ResultIsErr)
    utf16_3_res = Utf16.from_utf8(utf8_3) |> Result.map_ok(|_| ResultIsErr)
    utf16_4_res = Utf16.from_utf8(utf8_4) |> Result.map_ok(|_| ResultIsErr)
    err_2 = Err(BadUtf8({ index: 0, problem: UnexpectedEndOfSequence }))
    err_3 = Err(BadUtf8({ index: 0, problem: UnexpectedEndOfSequence }))
    err_4 = Err(BadUtf8({ index: 0, problem: UnexpectedEndOfSequence }))
    str_2_res = Str.from_utf8(utf8_2) |> Result.map_ok(|_| ResultIsErr)
    str_3_res = Str.from_utf8(utf8_3) |> Result.map_ok(|_| ResultIsErr)
    str_4_res = Str.from_utf8(utf8_4) |> Result.map_ok(|_| ResultIsErr)

    (utf16_2_res == err_2)
    and (utf16_3_res == err_3)
    and (utf16_4_res == err_4)
    and (utf16_2_res == str_2_res)
    and (utf16_3_res == str_3_res)
    and (utf16_4_res == str_4_res)

# -----------------------------------------------------------------------------
# ExpectedContinuation

expect
    # Missing continuation bytes
    utf8_2 = [0b1101_1111, 0b0011_1111]
    utf8_3 = [0b1110_1111, 0b1011_1111, 0b0011_1111]
    utf8_4 = [0b1111_0111, 0b1011_1111, 0b1011_1111, 0b0011_1111]
    utf16_2_res = Utf16.from_utf8(utf8_2) |> Result.map_ok(|_| ResultIsErr)
    utf16_3_res = Utf16.from_utf8(utf8_3) |> Result.map_ok(|_| ResultIsErr)
    utf16_4_res = Utf16.from_utf8(utf8_4) |> Result.map_ok(|_| ResultIsErr)
    err_2 = Err(BadUtf8({ index: 0, problem: ExpectedContinuation }))
    err_3 = Err(BadUtf8({ index: 0, problem: ExpectedContinuation }))
    err_4 = Err(BadUtf8({ index: 0, problem: ExpectedContinuation }))
    str_2_res = Str.from_utf8(utf8_2) |> Result.map_ok(|_| ResultIsErr)
    str_3_res = Str.from_utf8(utf8_3) |> Result.map_ok(|_| ResultIsErr)
    str_4_res = Str.from_utf8(utf8_4) |> Result.map_ok(|_| ResultIsErr)

    (utf16_2_res == err_2)
    and (utf16_3_res == err_3)
    and (utf16_4_res == err_4)
    and (utf16_2_res == str_2_res)
    and (utf16_3_res == str_3_res)
    and (utf16_4_res == str_4_res)

# -----------------------------------------------------------------------------
# OverlongEncoding

expect
    # only 11 bits used in 3 byte codepoint
    utf8_okay = [0b1110_0000, 0b1010_0000, 0b1000_0000]
    utf8_overlong = [0b1110_0000, 0b1001_1111, 0b1011_1111]
    expected_err = Err(BadUtf8({ index: 0, problem: OverlongEncoding }))
    utf16_okay = Utf16.from_utf8(utf8_okay) |> Result.map_ok(|_| Encoded)
    utf16_err = Utf16.from_utf8(utf8_overlong)

    (utf16_okay == Ok Encoded) and (utf16_err == expected_err)

expect
    # only 16 bits used in 4 byte codepoint
    utf8_okay = [0b1111_0000, 0b1001_0000, 0b1000_0000, 0b1000_0000]
    utf8_overlong = [0b1111_0000, 0b1000_1111, 0b1011_1111, 0b1011_1111]
    expected_err = Err(BadUtf8({ index: 0, problem: OverlongEncoding }))
    utf16_okay = Utf16.from_utf8(utf8_okay) |> Result.map_ok(|_| Encoded)
    utf16_err = Utf16.from_utf8(utf8_overlong)

    (utf16_okay == Ok Encoded) and (utf16_err == expected_err)

# -----------------------------------------------------------------------------
# CodepointTooLarge

expect
    # 0x110000 (0x10FFFF + 1) - validate against Str.from_utf8
    utf8 = [0b1111_0100, 0b1001_0000, 0b1000_0000, 0b1000_0000]
    utf16_res = Utf16.from_utf8(utf8) |> Result.map_ok(|_| ResultIsErr)
    err = Err(BadUtf8({ index: 0, problem: CodepointTooLarge }))
    str_res = Str.from_utf8(utf8) |> Result.map_ok(|_| ResultIsErr)

    (utf16_res == err) and (utf16_res == str_res)

# -----------------------------------------------------------------------------
# EncodesSurrogateHalf

expect
    # Lower bound of surrogate half range - 0xD800 = 0b1101_1000_0000_0000
    utf8 = [0b1110_1101, 0b1010_0000, 0b1000_0000]
    utf16_res = Utf16.from_utf8(utf8) |> Result.map_ok(|_| ResultIsErr)
    str_res = Str.from_utf8(utf8) |> Result.map_ok(|_| ResultIsErr)
    expected = Err(BadUtf8({ index: 0, problem: EncodesSurrogateHalf }))

    (utf16_res == str_res) and (utf16_res == expected)

expect
    # Upper bound of surrogate half range - 0xDBFF = 0b1101_1011_1111_1111
    utf8 = [0b1110_1101, 0b1011_1111, 0b1011_1111]
    utf16_res = Utf16.from_utf8(utf8) |> Result.map_ok(|_| ResultIsErr)
    str_res = Str.from_utf8(utf8) |> Result.map_ok(|_| ResultIsErr)
    expected = Err(BadUtf8({ index: 0, problem: EncodesSurrogateHalf }))

    (utf16_res == str_res) and (utf16_res == expected)