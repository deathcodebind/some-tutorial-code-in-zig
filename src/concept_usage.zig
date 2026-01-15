const std = @import("std");

const concept = @import("concept.zig");
const Concept2 = concept.Concept2;
const GenericTypeArg = concept.GenericTypeArg;
const GenericFunction = concept.GenericFunction;

pub const Iterable = Concept2.init().addFunction(.{ .generic = .{ "next", .{
    .gas = &[_]GenericTypeArg{.{
        .name = "Item",
    }},
    .builder = struct {
        pub fn run(comptime Self: type, comptime input: anytype) concept.Type.Fn {
            return @typeInfo(fn (*Self) ?input[0]).@"fn";
        }
    }.run,
} } });

pub inline fn fold(comptime IterableType: type, comptime Output: type, init: Output, iter: *Iterable.check(IterableType), f: fn (Output, IterableType.Item) Output) Output {
    var accum = init;
    while (iter.next()) |item| {
        accum = f(accum, item);
    }
    return accum;
}

test "iterable" {
    const Range = struct {
        pub fn range(comptime T: type) type {
            return struct {
                pub const Self = @This();
                pub const Item = T;
                min: T,
                max: T,
                step: T,
                current: T,
                pub fn next(self: *Self) ?T {
                    if (self.current > self.max) return null;
                    const result = self.current;
                    self.current += self.step;
                    return result;
                }
            };
        }
    }.range;
    const MyRange = Range(u8);
    try std.testing.expect(Iterable.check(MyRange) == MyRange);
    var range = MyRange{ .min = 0, .max = 10, .step = 2, .current = 0 };
    const result = fold(MyRange, u8, 0, &range, struct {
        pub fn add(a: u8, b: u8) u8 {
            return a + b;
        }
    }.add);
    try std.testing.expectEqual(30, result);
}

pub const CompareResult = enum(i8) {
    less = -1,
    equal = 0,
    greater = 1,
    const Self = @This();
    pub fn is_eq(self: Self) bool {
        return self == .equal;
    }
    pub fn is_neq(self: Self) bool {
        return self != .equal;
    }
    pub fn is_lt(self: Self) bool {
        return self == .less;
    }
    pub fn is_lte(self: Self) bool {
        return self == .less or self == .equal;
    }
    pub fn is_gt(self: Self) bool {
        return self == .greater;
    }
    pub fn is_gte(self: Self) bool {
        return self == .greater or self == .equal;
    }
    pub fn then(self: Self, other: Self) Self {
        if (self.is_eq()) return other;
        return self;
    }
};

pub const Comparable = Concept2.init().addFunction(.{ .generic = .{ "cmp", .{
    .gas = &[_]GenericTypeArg{},
    .builder = struct {
        pub fn run(comptime Self: type, comptime _: anytype) concept.Type.Fn {
            return @typeInfo(fn (Self, Self) CompareResult).@"fn";
        }
    }.run,
} } });

test "comparable" {
    const StringComparable = struct {
        str: []const u8,
        pub const Self = @This();
        pub fn cmp(self: Self, other: Self) CompareResult {
            return if (std.mem.eql(u8, self.str, other.str)) .equal else neq: {
                break :neq if (self.str.len < other.str.len) .less else .greater;
            };
        }
    };
    try std.testing.expect(Comparable.check(StringComparable) == StringComparable);
}

pub const Hashable = Concept2.init().addFunction(.{ .generic = .{ "hash", .{
    .gas = &[_]GenericTypeArg{},
    .builder = struct {
        pub fn run(comptime Self: type, comptime _: anytype) concept.Type.Fn {
            return @typeInfo(fn (Self) u64).@"fn";
        }
    }.run,
} } });

test "hashable" {
    const StringHashable = struct {
        str: []const u8,
        pub const Self = @This();
        pub fn hash(self: Self) u64 {
            return std.hash_map.hashString(self.str);
        }
    };
    try std.testing.expect(Hashable.check(StringHashable) == StringHashable);
}
