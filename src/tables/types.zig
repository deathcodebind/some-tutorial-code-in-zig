const BaseType = union(enum) {
    const Integer = struct {
        size: u8,
    };
    const Float = struct {
        size: u8,
    };
    const String = struct {
        const Format = enum {
            Utf8,
            Utf16,
            Utf32,
        };
        format: Format = .Utf8,
        length: u32,
        const Self = @This();
        pub fn size(self: Self) u32 {
            return switch (self.format) {
                .Utf8 => 1 * self.length,
                .Utf16 => 2 * self.length,
                .Utf32 => 4 * self.length,
            };
        }
    };
    integer: Integer,
    float: Float,
    string: String,
};
