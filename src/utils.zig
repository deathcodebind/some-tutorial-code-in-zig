const std = @import("std");
const FnParam = std.builtin.Type.Fn.Param;
pub fn comptimeMap(comptime In: type, comptime Out: type, input: []const In, mapper: fn (In) Out) []const Out {
    var result: []const Out = &[_]Out{};
    inline for (input) |item| {
        result = result ++ [_]Out{mapper(item)};
    }
    return result;
}

pub fn comptimeJoin(comptime Child: type, comptime raw: []const Child, joiner: Child) []const Child {
    var result: []const Child = &[_]Child{};
    inline for (raw) |item| {
        result = result ++ [_]Child{item};
        if (result.len < (raw.len * 2) - 1) {
            result = result ++ [_]Child{joiner};
        }
    }

    return result;
}

pub fn comptimeTupleToSlice(comptime Inner: type, value: anytype) []const Inner {
    const ti = @typeInfo(@TypeOf(value));
    if (ti != .@"struct" or !ti.@"struct".is_tuple) @compileError("expect a tuple");
    var result: []const Inner = &[_]Inner{};
    inline for (value) |item| {
        result = result ++ [_]Inner{item};
    }
    return result;
}

pub fn isTypeTuple(comptime Types: anytype) bool {
    const ti = @typeInfo(@TypeOf(Types));
    if (ti == .@"struct" and ti.@"struct".is_tuple) {
        for (ti.@"struct".fields) |field| {
            if (field.type != type) {
                return false;
            }
        }
        return true;
    } else {
        return false;
    }
}
