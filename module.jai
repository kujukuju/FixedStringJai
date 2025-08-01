
#module_parameters()(ASSERT_LENGTH: bool = true);

FixedString :: struct (size: int) {
    buffer: [size] u8;
    terminator: u8 #align 1;
    // #run validate_fixed_string(size);
}

// validate_fixed_string :: ($size: int) {
//     FixedStringMatch :: struct {
//         buffer: [size] u8;
//         terminator: u8 #align 1;
//     }
//     fixed: FixedStringMatch;

//     buffer_ptr := cast(*void) fixed.buffer.data;
//     terminator_ptr := cast(*void) *fixed.terminator;

//     assert(terminator_ptr - buffer_ptr == size, "Incorrect terminator offset for FixedString(%).\n", size);
// }

operator [] :: (buffer: FixedString($T), index: int) -> u8 {
    // TODO can I just do buffer.buffer[index]? ive had mixed results in the past
    return buffer.buffer.data[index];
}

operator == :: (a: FixedString($N), b: FixedString($S)) -> bool {
    return to_string(*a) == to_string(*b);
}

operator == :: (a: FixedString($N), b: string) -> bool #symmetric {
    return to_string(*a) == b;
}

operator == :: (a: FixedString($N), b: *u8) -> bool #symmetric {
    return to_string(*a) == to_string(b);
}

operator ! :: (buffer: FixedString($N)) -> bool {
    return !to_string(*buffer);
}

// create_fixed_string :: (buffer: FixedString($N)) -> FixedString(N) {
//     result: FixedString(N);
//     result.buffer = buffer.buffer;
//     return result;
// }

create_fixed_string :: (buffer: [$N] u8) -> FixedString(N) {
    result: FixedString(N);
    result.buffer = buffer;
    return result;
}

create_fixed_string :: (any_value: $T) -> FixedString(20) #modify {
    return T == s64 || T == s32 || T == s16 || T == s8 || T == u64 || T == u32 || T == u16 || T == u8;
} {
    buffer: FixedString(20);
    index: int;

    value := cast(s64) any_value;

    if value == 0 {
        buffer.buffer.data[0] = #char "0";
        return buffer;
    }

    negative := value < 0;
    if negative {
        value = -value;
    }

    while value != 0 {
        buffer.buffer.data[index] = cast(u8) ((value % 10) + 48);
        value /= 10;

        index += 1;
    }

    if negative {
        buffer.buffer.data[index] = #char "-";
        index += 1;
    }

    start_index: int;
    index -= 1;

    while index > start_index {
        buffer.buffer.data[start_index], buffer.buffer.data[index] = buffer.buffer.data[index], buffer.buffer.data[start_index];

        start_index += 1;
        index -= 1;
    }

    return buffer;
}

// this method isnt really good at all
create_fixed_string :: (value: float) -> FixedString(50) {
    buffer: FixedString(50);

    string_value := tprint("%", value);
    if string_value.count > 50 {
        assert(string_value.count <= 50, "I wrote this assuming a float couldn't be longer than 47 characters.");
    }

    write(*buffer, string_value);

    return buffer;
}

create_fixed_string :: (value: string, $size: int) -> FixedString(size) {
    #if ASSERT_LENGTH {
        assert(value.count <= size, "String does not fit inside fixed string buffer. % %\n", value.count, size);
    }

    buffer: FixedString(size);
    memcpy(buffer.buffer.data, value.data, min(value.count, size));

    return buffer;
}

create_fixed_string :: ($value: string) -> FixedString(value.count) {
    buffer: FixedString(value.count);
    memcpy(buffer.buffer.data, value.data, value.count);

    return buffer;
}

create_fixed_string :: (value: *u8, $N: int) -> FixedString(N) {
    return create_fixed_string(value, c_style_strlen(value), N);
}

create_fixed_string :: (value: *u8, size: int, $N: int) -> FixedString(N) {
    #if ASSERT_LENGTH {
        assert(size <= N, "String does not fit inside fixed string buffer. % %\n", size, N);
    }

    buffer: FixedString(N);
    memcpy(buffer.buffer.data, value, min(size, N));

    return buffer;
}

write :: (buffer: *FixedString($N), value: string) {
    #if ASSERT_LENGTH {
        assert(value.count <= N, "String does not fit inside fixed string buffer. % %\n", value.count, N);
    }

    if value.count >= buffer.buffer.count {
        memcpy(buffer.buffer.data, value.data, N);
    } else {
        memcpy(buffer.buffer.data, value.data, value.count);
    }
}

write :: (buffer: *FixedString($N), value: string, offset: int) {
    #if ASSERT_LENGTH {
        assert(offset + value.count <= N, "String does not fit inside fixed string buffer. % %\n", offset + value.count, N);
    }

    if value.count >= buffer.buffer.count + offset {
        memcpy(buffer.buffer.data + offset, value.data, N - offset);
    } else {
        memcpy(buffer.buffer.data + offset, value.data, value.count);
    }
}

append :: (buffer: *FixedString($N), value: string) {
    write(buffer, value, count(buffer.*));
}

write :: (buffer: *FixedString($N), value: *u8) {
    write(buffer, to_string(value));
}

write :: (buffer: *FixedString($N), value: *u8, offset: int) {
    write(buffer, to_string(value), offset);
}

append :: (buffer: *FixedString($N), value: *u8) {
    write(buffer, value, count(buffer.*));
}

write :: (buffer: *FixedString($N), value: FixedString($S)) {
    write(buffer, to_string(*value));
}

write :: (buffer: *FixedString($N), value: FixedString($S), offset: int) {
    write(buffer, to_string(*value), offset);
}

append :: (buffer: *FixedString($N), value: FixedString($S)) {
    write(buffer, to_string(*value), count(buffer.*));
}

write :: (buffer: *FixedString($N), value: int) {
    int_string := create_fixed_string(value);

    write(buffer, int_string);
}

write :: (buffer: *FixedString($N), value: int, offset: int) {
    int_string := create_fixed_string(value);

    write(buffer, int_string, offset);
}

append :: (buffer: *FixedString($N), value: int) {
    write(buffer, value, count(buffer.*));
}

write :: (buffer: *FixedString($N), value: float) {
    float_string := create_fixed_string(value);

    write(buffer, float_string);
}

write :: (buffer: *FixedString($N), value: float, offset: int) {
    float_string := create_fixed_string(value);

    write(buffer, float_string, offset);
}

append :: (buffer: *FixedString($N), value: float) {
    write(buffer, value, count(buffer.*));
}

count :: (bytes: *u8) -> int {
    index := 0;
    while bytes[index] != 0 {
        index += 1;
    }

    return index;
}

count :: (fixed: FixedString) -> int {
    return count(fixed.buffer.data);
}

to_upper_case :: (fixed: FixedString($N)) -> FixedString(N) {
    upper: FixedString(N);
    for i: 0..N - 1 {
        upper.buffer[i] = to_upper(fixed.buffer[i]);
    }
    return upper;
}

to_lower_case :: (fixed: FixedString($N)) -> FixedString(N) {
    lower: FixedString(N);
    for i: 0..N - 1 {
        lower.buffer[i] = to_lower(fixed.buffer[i]);
    }
    return lower;
}

to_c_string :: (fixed: *FixedString($N)) -> *u8 {
    return fixed.buffer.data;
}

to_string :: (fixed: *FixedString($N)) -> string {
    value: string;
    value.data = fixed.buffer.data;
    value.count = count(fixed);

    return value;
}

begins_with :: (fixed: FixedString($N), suffix: string) -> bool {
    return begins_with(to_string(*fixed), suffix);
}

begins_with :: (buffer: FixedString($N), suffix: FixedString($S)) -> bool {
    return begins_with(to_string(*buffer), to_string(*suffix));
}

starts_with :: begins_with;

ends_with :: (fixed: FixedString($N), postfix: string) -> bool {
    return ends_with(to_string(*fixed), postfix);
}

ends_with :: (buffer: FixedString($N), postfix: FixedString($S)) -> bool {
    return ends_with(to_string(*buffer), to_string(*postfix));
}

alloc_string :: (buffer: FixedString($N)) -> string {
    result := alloc_string(count(buffer));
    memcpy(result.data, buffer.buffer.data, result.count);
    return result;
}

delete_first :: (buffer: *FixedString($N), count: int) {
    remaining := N - count;

    for i: 0..remaining - 1 {
        buffer.buffer.data[i] = buffer.buffer.data[i + count];
    }

    for i: remaining..buffer.buffer.count - 1 {
        buffer.buffer.data[i] = 0;
    }
}

delete_after :: (buffer: *FixedString($N), index: int) {
    for i: index + 1..N - 1 {
        buffer.buffer.data[i] = 0;
    }
}

first_character_index :: (buffer: FixedString($N), character: u8) -> int {
    index: int;
    while index < buffer.buffer.count {
        if buffer.buffer.data[index] == character {
            return index;
        }

        index += 1;
    }

    return -1;
}

// this seems to crash the compiler but in an isolated test it does not
// last_character_index :: (buffer: FixedString($N), $character: string) -> int #modify {
//     return character.count == 1;
// } {
//     return last_character_index(buffer, character[0]);
// }

last_character_index :: (buffer: FixedString($N), character: u8) -> int {
    index: int = buffer.buffer.count - 1;
    while index >= 0 {
        if buffer.buffer.data[index] == character {
            return index;
        }

        index -= 1;
    }

    return -1;
}

string_after_last_character :: (buffer: FixedString($N), character: u8) -> FixedString(N) {
    fixed: FixedString(N);
    last_index := last_character_index(buffer, character);

    return fixed_substring(buffer, last_index, N - last_index);
}

string_after_first_character :: (buffer: FixedString($N), character: u8) -> FixedString(N) {
    fixed: FixedString(N);
    first_index := first_character_index(buffer, character);

    return fixed_substring(buffer, first_index, N - first_index);
}

contains :: (buffer: FixedString($N), part: string) -> bool {
    if part.count == 0 {
        return true;
    }

    index: int;

    while index < N && buffer.buffer.data[index] != 0 {
        // counts the part index
        part_index := 0;
        // counts the first match of buffer.data if there is one found
        // not sure if this is actually faster but it seems kinda smart
        index_offset := 0;
        while index + part_index < N && buffer.buffer.data[index + part_index] == part.data[part_index] {
            if buffer.buffer.data[index + part_index] == part.data[0] && index_offset == 0 {
                index_offset = part_index;
            }

            part_index += 1;
            if part_index == part.count {
                return true;
            }
        }

        if index_offset > 0 {
            index += index_offset;
        } else {
            index += 1;
        }
    }

    return false;
}

fixed_substring :: (buffer: FixedString($N), start: int, $length: int) -> FixedString(length) {
    substring: FixedString(length);
    for i: 0..length - 1 {
        substring[i] = buffer.buffer.data[start + i];
    }

    return substring;
}

fixed_substring :: (buffer: FixedString($N), start: int, length: int) -> FixedString(N) {
    substring: FixedString(N);
    for i: 0..length - 1 {
        substring.buffer[i] = buffer.buffer.data[start + i];
    }

    return substring;
}

fixed_substring :: (buffer: FixedString($N), start: int) -> FixedString(N) {
    length := N - start;

    substring: FixedString(N);
    for i: 0..length - 1 {
        substring.buffer[i] = buffer.buffer.data[start + i];
    }

    return substring;
}

temp_substring :: (buffer: *FixedString($N), start: int, length: int) -> string {
    value: string;
    value.data = buffer.buffer.data + start;
    value.count = length;

    return value;
}

get_hash :: (buffer: FixedString($N)) -> u32 {
    return cast,trunc(u32) fnv1a_hash(buffer.buffer.data, count(buffer));
}

clear :: (buffer: *FixedString($N)) {
    memset(buffer.buffer.data, 0, N);
}

#scope_file

#import "Basic";
#import "Hash";
#import "String";

Basic :: #import "Basic";
Hash :: #import "Hash";
Table :: #import "Hash_Table";

#poke_name Basic operator ==;

#poke_name Hash get_hash;
#poke_name Hash operator ==;

#poke_name Table operator ==;