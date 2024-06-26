FixedString :: struct (size: int) {
    buffer: [size] u8;
}

operator [] :: (buffer: FixedString($T), index: int) -> u8 {
    // TODO can I just do buffer.buffer[index]? ive had mixed results in the past
    return buffer.buffer.data[index];
}

operator == :: (a: FixedString($N), b: FixedString($S)) -> bool {
    #if N != S {
        return false;
    } else {
        return inline compare(a, b);
    }
}

operator == :: (a: FixedString($N), b: string) -> bool #symmetric {
    if N < b.count {
        return false;
    }

    for i: 0..b.count - 1 {
        if a.buffer.data[i] != b.data[i] {
            return false;
        }
    }

    return true;
}

create_fixed_string :: (buffer: FixedString($N)) -> FixedString(N) {
    result: FixedString(N);
    result.buffer = buffer.buffer;
    // memcpy(result.buffer.data, buffer.buffer.data, N);
    return result;
}

create_fixed_string :: (value: int) -> FixedString(20) {
    buffer: FixedString(20);
    index: int;

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
        buffer.buffer.data[start_index], buffer.buffer.data[index] = swap(buffer.buffer.data[start_index], buffer.buffer.data[index]);

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
    buffer: FixedString(size);
    for i: 0..value.count - 1 {
        buffer.buffer.data[i] = value.data[i];
    }

    return buffer;
}

create_fixed_string :: ($value: string) -> FixedString(value.count + 1) {
    buffer: FixedString(value.count + 1);
    for i: 0..value.count - 1 {
        buffer.buffer.data[i] = value.data[i];
    }

    return buffer;
}

create_fixed_string :: (value: *u8, $size: int) -> FixedString(size) {
    buffer: FixedString(size);
    
    index := 0;
    while value[index] != 0 {
        buffer.buffer.data[index] = value[index];
        index += 1;
    }

    return buffer;
}

write :: (buffer: *FixedString($N), value: string) {
    if value.count >= buffer.buffer.count {
        memcpy(buffer.buffer.data, value.data, buffer.buffer.count);
        buffer.buffer.data[buffer.buffer.count - 1] = 0;
    } else {
        memcpy(buffer.buffer.data, value.data, value.count);
        buffer.buffer.data[value.count] = 0;
    }
}

write :: (buffer: *FixedString($N), value: string, offset: int) {
    if value.count >= buffer.buffer.count + offset {
        memcpy(buffer.buffer.data + offset, value.data, buffer.buffer.count - offset);
        buffer.buffer.data[buffer.buffer.count - 1] = 0;
    } else {
        memcpy(buffer.buffer.data + offset, value.data, value.count);
        buffer.buffer.data[value.count + offset] = 0;
    }
}

append :: (buffer: *FixedString($N), value: string) {
    write(buffer, value, count(<< buffer));
}

write :: (buffer: *FixedString($N), value: *u8) {
    index := 0;
    while index < buffer.buffer.count && value[index] != 0 {
        buffer.buffer.data[index] = value[index];
        index += 1;
    }

    if index < buffer.buffer.count {
        buffer.buffer.data[index] = value[index];
    } else {
        buffer.buffer.data[buffer.buffer.count - 1] = 0;
    }
}

write :: (buffer: *FixedString($N), value: *u8, offset: int) {
    index := 0;
    while index + offset < buffer.buffer.count && value[index] != 0 {
        buffer.buffer.data[index + offset] = value[index];
        index += 1;
    }

    if index + offset < buffer.buffer.count {
        buffer.buffer.data[index + offset] = value[index];
    } else {
        buffer.buffer.data[buffer.buffer.count - 1] = 0;
    }
}

append :: (buffer: *FixedString($N), value: *u8) {
    write(buffer, value, count(<< buffer));
}

write :: (buffer: *FixedString($N), value: FixedString($S)) {
    min_count :: #run min(N, S);
    for i: 0..min_count - 1 {
        buffer.buffer.data[i] = value.buffer.data[i];
    }

    for i: min_count..N - 1 {
        buffer.buffer.data[i] = 0;
    }
}

write :: (buffer: *FixedString($N), value: FixedString($S), offset: int) {
    index: int;
    while index + offset < N && index < S {
        buffer.buffer.data[index + offset] = value.buffer.data[index];

        index += 1;
    }

    // TODO not all of these confirm ending buffer in \0, does that matter?
}

append :: (buffer: *FixedString($N), value: FixedString($S)) {
    write(buffer, value, count(<< buffer));
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
    write(buffer, value, count(<< buffer));
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
    write(buffer, value, count(<< buffer));
}

count :: (bytes: *u8) -> int {
    index := 0;
    while bytes[index] != 0 {
        index += 1;
    }

    return index;
}

count :: (buffer: FixedString($N)) -> int {
    index := 0;
    while index < N && buffer.buffer.data[index] != 0 {
        index += 1;
    }

    return index;
}

temp_c_string :: (fixed: *FixedString($N)) -> *u8 {
    if count(fixed) < N {
        return fixed.buffer.data;
    }

    result: *u8 = temporary_alloc(N + 1);
    memcpy(result, fixed.buffer.data, N);
    result[N] = 0;

    return result;
}

temp_string :: (buffer: *FixedString($N)) -> string {
    value: string;
    value.data = buffer.buffer.data;
    value.count = count(buffer);

    return value;
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

begins_with :: (buffer: FixedString($N), suffix: FixedString($S)) -> bool {
    offset: int;
    while offset < buffer.buffer.count && offset < suffix.buffer.count {
        if buffer.buffer.data[offset] != suffix.buffer.data[offset] {
            return false;
        }

        offset += 1;
    }

    return true;
}

begins_with :: (buffer: FixedString($N), suffix: string) -> bool {
    offset: int;
    while offset < buffer.buffer.count && offset < suffix.count {
        if buffer.buffer.data[offset] != suffix.data[offset] {
            return false;
        }

        offset += 1;
    }

    return true;
}

ends_with :: (buffer: FixedString($N), suffix: FixedString($S)) -> bool {
    buffer_count := count(buffer);
    suffix_count := count(suffix);
    offset: int;
    while offset < buffer_count && offset < suffix_count {
        buffer_index := buffer_count - offset - 1;
        suffix_index := suffix_count - offset - 1;
        if buffer.buffer.data[buffer_index] != suffix.buffer.data[suffix_index] {
            return false;
        }

        offset += 1;
    }

    return true;
}

ends_with :: (buffer: FixedString($N), suffix: string) -> bool {
    buffer_count := count(buffer);
    offset: int;
    while offset < buffer_count && offset < suffix.count {
        buffer_index := buffer_count - offset - 1;
        suffix_index := suffix.count - offset - 1;
        if buffer.buffer.data[buffer_index] != suffix.data[suffix_index] {
            return false;
        }

        offset += 1;
    }

    return true;
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
        substring[i] = buffer.buffer.data[start + i];
    }

    return substring;
}

fixed_substring :: (buffer: FixedString($N), start: int) -> FixedString(N) {
    length := N - start;

    substring: FixedString(N);
    for i: 0..length - 1 {
        substring[i] = buffer.buffer.data[start + i];
    }

    return substring;
}

temp_substring :: (buffer: *FixedString($N), start: int, length: int) -> string {
    value: string;
    value.data = buffer.buffer.data + start;
    value.count = length;

    return value;
}

hash :: (buffer: FixedString($N)) -> u32 {
    return sdbm_hash(buffer.buffer.data, N);
}

compare :: (a: FixedString($N), b: FixedString(N)) -> bool {
    return memcmp(a.buffer.data, b.buffer.data, N) == 0;

    // index: int;
    // while index < N {
    //     if a.buffer.data[index] != b.buffer.data[index] {
    //         return false;
    //     }

    //     if a.buffer.data[index] == 0 && b.buffer.data[index] == 0 {
    //         return true;
    //     }

    //     index += 1;
    // }

    // return true;
}

clear :: (buffer: *FixedString($N)) {
    for i: 0..N - 1 {
        buffer.buffer.data[i] = 0;
    }
}

#scope_file

#import "Basic";
#import "Hash";