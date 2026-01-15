//! By convention, root.zig is the root source file when making a library.
const std = @import("std");
const utils = @import("utils.zig");
pub const Type = std.builtin.Type;
pub const FnParam = Type.Fn.Param;
const comptimeMap = @import("utils.zig").comptimeMap;

pub const RequireFn = struct {
    fn_name: [:0]const u8,
    args: []const FnParam,
    return_type: ?type,
    const Self = @This();
    pub fn check(self: Self, comptime T: type) bool {
        if (@hasDecl(T, self.fn_name)) {
            const fn_decl = @typeInfo(@TypeOf(@field(T, self.fn_name)));
            if (fn_decl != .@"fn") @compileError("Type does not satisfy requirement");
            const fn_decl_fn = fn_decl.@"fn";
            if (fn_decl_fn.params.len != self.args.len) @compileError("Type does not satisfy requirement, function parameter amount not match");
            if (self.return_type) |return_type| {
                if (fn_decl_fn.return_type) |rt| {
                    if (rt != return_type) @compileError("Type does not satisfy requirement, function return type not match");
                } else @compileError("Type does not satisfy requirement, function return type not match");
            } else {
                if (fn_decl_fn.return_type != null) @compileError("Type does not satisfy requirement, function return type not match");
            }
            for (fn_decl_fn.params, 0..) |param, i| {
                if (param.is_generic != self.args[i].is_generic or
                    param.is_noalias != self.args[i].is_noalias or
                    param.type != self.args[i].type) @compileError("Type does not satisfy requirement, function parameter type not match");
            }
            return true;
        } else {
            @compileError("Type does not satisfy requirement");
        }
    }
};

pub fn RequireCheckFn(comptime Self: type) RequireFn {
    return .{
        .fn_name = "check",
        .args = &[_]FnParam{
            FnParam{
                .type = Self,
                .is_generic = false,
                .is_noalias = false,
            },
            FnParam{
                .type = type,
                .is_generic = false,
                .is_noalias = false,
            },
        },
        .return_type = bool,
    };
}

pub const RequireField = struct {
    def: Type.StructField,
    const Self = @This();
    pub fn check(self: Self, comptime T: type) bool {
        if (@hasField(T, self.def.name)) {
            // even we could use @fieldType to check the type, but const A: u8 = 1 and a: u8 should pass the same requirement
            // which is not we expected, so we still need to use @typeInfo to loop through the fields
            const fields = @typeInfo(T).@"struct".fields;
            inline for (fields) |field| {
                if (std.mem.eql(u8, field.name, self.def.name)) {
                    return field.alignment == self.def.alignment and
                        field.is_comptime == self.def.is_comptime and
                        field.type == self.def.type;
                } else {
                    continue;
                }
            }
            return false;
        } else {
            @compileError("Type does not satisfy requirement");
        }
    }
};

test "struct const defs" {
    const A = struct {
        const Self = @This();
    };
    try std.testing.expect(A.Self == @field(A, "Self"));
}

pub fn RequireDecl(comptime ValueType: type) type {
    return struct {
        name: [:0]const u8,
        value: ValueType,
        const Self = @This();
        pub fn check(self: Self, comptime T: type) bool {
            if (@hasDecl(T, self.name)) {
                const decl = @field(T, self.name);
                // fieldType only works for real fields,
                // but I wander why field works for const fields
                const decl_type = @TypeOf(decl);
                return decl == self.value and @alignOf(decl_type) == @alignOf(ValueType);
            } else {
                @compileError("Type does not satisfy requirement");
            }
        }
    };
}

pub fn Concept(comptime AnyTypeThatPassedRequireCheckFnConcept: anytype) fn (anytype) type {
    const InputType = @TypeOf(AnyTypeThatPassedRequireCheckFnConcept);
    const InputTypeTypeInfo = @typeInfo(InputType);
    if (InputTypeTypeInfo == .@"fn") {
        const InputTypeFn = InputTypeTypeInfo.@"fn";
        if (!InputTypeFn.is_generic) @compileError("if your requiment is not generic, do not use function, use Requirement helper types or provide a type that could pass RequireCheckFn");
        return struct {
            pub fn run(value: anytype) type {
                return Concept(AnyTypeThatPassedRequireCheckFnConcept(value))(value);
            }
        }.run;
    }
    const InputTypeTypeInfoStruct = InputTypeTypeInfo.@"struct";
    const InputTypeFieldLen = InputTypeTypeInfoStruct.fields.len;
    if (InputTypeTypeInfoStruct.is_tuple) {
        for (InputTypeTypeInfoStruct.fields) |field_info| {
            const fieldTypeInfo = @typeInfo(field_info.type);
            if (fieldTypeInfo == .@"fn") {
                if (!fieldTypeInfo.@"fn".is_generic) @compileError("not generic function as a requirement is not allowed");
            } else if (!RequireCheckFn(field_info.type).check(field_info.type)) unreachable;
        }
        return struct {
            pub fn check(value: anytype) type {
                const T = blk: {
                    const valueType = @TypeOf(value);
                    if (@typeInfo(valueType) == .type) {
                        break :blk value;
                    } else {
                        break :blk valueType;
                    }
                };
                comptime var i: usize = 0;
                inline while (i < InputTypeFieldLen) : (i += 1) {
                    const TypeInfoOfField = @typeInfo(@TypeOf(AnyTypeThatPassedRequireCheckFnConcept[i]));
                    if (TypeInfoOfField == .@"fn") {
                        if (Concept(AnyTypeThatPassedRequireCheckFnConcept[i](T))(T) != T) @compileError("concept check failed");
                        continue;
                    }
                    if (!AnyTypeThatPassedRequireCheckFnConcept[i].check(T)) unreachable;
                }
                return T;
            }
        }.check;
    } else {
        if (!RequireCheckFn(InputType).check(InputType)) unreachable;
        return struct {
            pub fn check(value: anytype) type {
                const T = blk: {
                    const valueType = @TypeOf(value);
                    if (@typeInfo(valueType) == .type) {
                        break :blk value;
                    } else {
                        break :blk valueType;
                    }
                };
                if (!AnyTypeThatPassedRequireCheckFnConcept.check(T)) unreachable;
                return T;
            }
        }.check;
    }
}

pub fn RequireGenericArg(comptime name: [:0]const u8) type {
    return struct {
        pub const TypeName = name;
        pub fn check(comptime T: type) bool {
            if (@hasDecl(T, name)) {
                const decl = @field(T, name);
                const decl_type = @TypeOf(decl);
                return decl_type == type;
            }
        }
    };
}

const GenericArg = struct {
    name: [:0]const u8 = "",
    is_param: bool = true,
};

/// this requirement should first check if there is a decl with the name of that Generic arg,
/// if return type is null, that does not mean that the function returns void,
/// it mean that return is a generic type, which depends on those generic args
pub fn RequireGenericFn(comptime name: [:0]const u8, comptime GenericArgs: []const GenericArg, comptime returnType: ?type) fn (type) RequireFn {
    return struct {
        pub fn run(comptime T: type) RequireFn {
            inline for (GenericArgs) |arg| {
                if (!@hasDecl(T, arg.name)) @compileError("missing Generic Arg definition of name: " ++ @typeName(arg.name));
                const decl = @field(T, arg.name);
                const decl_type = @TypeOf(decl);
                if (decl_type != type) return false;
            }
            return RequireGenericFnRequirementConstructor(name, T, GenericArgs, returnType);
        }
    }.run;
}
pub fn RequireGenericFnRequirementConstructor(name: [:0]const u8, comptime T: type, args: []const GenericArg, comptime returnType: ?type) RequireFn {
    comptime var fn_params: []const FnParam = &[_]FnParam{};
    if (returnType) |rt| {
        for (args) |arg| {
            if (arg.is_param)
                fn_params = fn_params ++ [_]FnParam{
                    .{
                        .type = @field(T, arg.name),
                        .is_generic = false,
                        .is_noalias = false,
                    },
                };
            return .{
                .fn_name = name,
                .args = fn_params,
                .return_type = rt,
            };
        }
    } else {
        comptime var return_type_index: ?usize = null;
        for (args, 0..) |arg, i| {
            if (arg.is_param) {
                fn_params = fn_params ++ [_]FnParam{
                    .{
                        .type = @field(T, arg.name),
                        .is_generic = false,
                        .is_noalias = false,
                    },
                };
            } else return_type_index = i;
        }
        return .{
            .fn_name = name,
            .args = fn_params,
            .return_type = blk: {
                if (return_type_index) |index| {
                    break :blk @field(T, args[index].name);
                } else {
                    break :blk void;
                }
            },
        };
    }
}

test "concept with sub type" {
    const Addable = struct {
        pub const Self = @This();
        pub const RHS = Self;
        pub const Output = Self;
        pub fn add(self: Self, rhs: RHS) Output {
            return Output{
                .value = self.value + rhs.value,
            };
        }
        value: i32,
    };
    const AddableConcept = Concept(.{
        RequireGenericFn("add", &[_]GenericArg{
            .{
                .name = "Self",
            },
            .{
                .name = "RHS",
            },
            .{
                .name = "Output",
                .is_param = false,
            },
        }, null),
        RequireField{
            .def = .{
                .alignment = @alignOf(i32),
                .is_comptime = false,
                .name = "value",
                .type = i32,
                .default_value_ptr = null,
            },
        },
    });
    try std.testing.expect(AddableConcept(Addable) == Addable);
}
pub fn AnytypeAllType(comptime types: anytype) bool {
    const ti = @typeInfo(@TypeOf(types));
    if (ti != .@"struct") return false;
    inline for (ti.@"struct".fields) |field| {
        if (field.type != type) return false;
    }
    return true;
}

pub const GenericTypeArg = struct {
    name: [:0]const u8,
    placeholder: ?Concept2 = null,
};

pub const GenericField = struct {
    gas: []const GenericTypeArg,
    builder: fn (comptime anytype) Type.StructField,
    pub const Self = @This();
    pub fn init(gas: []const GenericTypeArg, builder: fn (comptime anytype) Type.StructField) Self {
        const builder_params_ti = @typeInfo(@TypeOf(builder));
        if (builder_params_ti != .@"struct") @compileError("builder should be a function takes a type tuple");
        const builder_params_type = builder_params_ti.@"struct";
        if (builder_params_type.fields.len != gas.len) @compileError("builder should be a function takes a type tuple with the same length as gas");
        return .{
            .gas = gas,
            .builder = builder,
        };
    }
    pub fn build(self: Self, comptime types: []const type) Type.StructField {
        if (types.len != self.gas.len) @compileError("types should be type tuple with the same length as gas");
        return self.builder(types);
    }
};

pub const GenericFunction = struct {
    gas: []const GenericTypeArg,
    builder: fn (comptime type, comptime anytype) Type.Fn,
    pub const Self = @This();
    pub fn init(gas: []const GenericTypeArg, builder: fn (comptime anytype) Type.Fn) Self {
        const builder_params_ti = @typeInfo(@TypeOf(builder));
        if (builder_params_ti != .@"struct") @compileError("builder should be a function takes a type tuple");
        const builder_params_type = builder_params_ti.@"struct";
        if (builder_params_type.fields.len != gas.len) @compileError("builder should be a function takes a type tuple with the same length as gas");
        return .{
            .gas = gas,
            .builder = builder,
        };
    }
    pub fn build(self: Self, comptime TypeUsedAsSelf: type, comptime types: []const type) Type.Fn {
        if (types.len != self.gas.len) @compileError("types should be type tuple with the same length as gas");
        return self.builder(TypeUsedAsSelf, types);
    }
};

pub const Concept2Field = union(enum) {
    normal: Type.StructField,
    generic: struct { []const u8, GenericField },
};

pub const Concept2Function = union(enum) {
    normal: struct { []const u8, Type.Fn },
    generic: struct { []const u8, GenericFunction },
};

pub const CustomTrait = fn (comptime type) bool;

pub const Concept2 = struct {
    gas: []const GenericTypeArg = &[_]GenericTypeArg{.{
        .name = "Self",
    }},
    fields: []const Concept2Field = &[_]Concept2Field{},
    functions: []const Concept2Function = &[_]Concept2Function{},
    traits: []const *const CustomTrait = &[_]*const CustomTrait{},
    pub const Self = @This();
    pub fn init() Self {
        return .{};
    }
    pub fn addField(self: Self, comptime fieldInfo: Concept2Field) Self {
        if (fieldInfo == .normal) {
            return .{
                .fields = self.fields ++ [_]Concept2Field{fieldInfo},
            };
        }
        var gas_addtion: []const GenericTypeArg = &[_]GenericTypeArg{};
        const generic_field = fieldInfo.generic;
        comptime var found = false;
        inline for (generic_field.@"1".gas) |ga_| {
            inline for (self.gas) |ga| {
                if (std.mem.eql(u8, ga.name, ga_.name)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                gas_addtion = gas_addtion ++ [_]GenericTypeArg{ga_};
            } else {
                found = false;
            }
        }
        return .{
            .gas = self.gas ++ gas_addtion,
            .fields = self.fields ++ [_]Concept2Field{fieldInfo},
            .functions = self.functions,
            .traits = self.traits,
        };
    }
    pub fn addFunction(self: Self, comptime functionInfo: Concept2Function) Self {
        if (functionInfo == .normal) {
            return .{
                .functions = self.functions ++ [_]Concept2Function{functionInfo},
            };
        }
        var gas_addtion: []const GenericTypeArg = &[_]GenericTypeArg{};
        const generic_function = functionInfo.generic.@"1";
        comptime var found = false;
        inline for (generic_function.gas) |ga_| {
            inline for (self.gas) |ga| {
                if (std.mem.eql(u8, ga.name, ga_.name)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                gas_addtion = gas_addtion ++ [_]GenericTypeArg{ga_};
            } else {
                found = false;
            }
        }
        return .{
            .gas = self.gas ++ gas_addtion,
            .functions = self.functions ++ [_]Concept2Function{functionInfo},
            .traits = self.traits,
            .fields = self.fields,
        };
    }
    pub fn addTrait(self: Self, comptime trait: CustomTrait) Self {
        return .{
            .traits = self.traits ++ [_]CustomTrait{trait},
            .gas = self.gas,
            .fields = self.fields,
            .functions = self.functions,
        };
    }

    pub fn checkGenericsExist(self: Self, t: type) bool {
        inline for (self.gas) |ga| {
            if (!@hasDecl(t, ga.name)) return false;
            if (ga.placeholder) |c| {
                const ty = @field(t, ga.name);
                if (c.check(ty) != ty) return false;
            }
        }
        return true;
    }

    pub fn check(self: Self, comptime input: type) type {
        const input_ti = @typeInfo(input);
        if (input_ti != .@"struct") @compileError("expect struct type");
        const input_type = input_ti.@"struct";
        if (input_type.fields.len < self.fields.len) @compileError("the fields inside the input struct is less than field requirement");
        if (!self.checkGenericsExist(input)) @compileError("generic arg check failed");
        inline for (self.fields) |field| {
            var found_field = false;
            if (field == .normal) {
                inline for (input_type.fields) |field_| {
                    if (std.mem.eql(u8, field_.name, field.normal.name)) {
                        found_field = field.normal.alignment == field_.alignment and field.normal.is_comptime == field_.is_comptime and field.normal.type == field_.type;
                        break;
                    }
                }
            } else {
                inline for (input_type.fields) |field_| {
                    const field_impl = field.generic.@"1".build(comptimeMap(GenericTypeArg, type, field.generic.@"1".gas, struct {
                        pub fn run(in: GenericTypeArg) type {
                            return @field(input, in.name);
                        }
                    }.run));
                    if (std.mem.eql(u8, field_.name, field_impl.name)) {
                        found_field = field_impl.alignment == field_.alignment and field_impl.is_comptime == field_.is_comptime and field_impl.type == field_.type;
                        break;
                    }
                }
            }
            if (!found_field) @compileError("field " ++ blk: {
                if (field == .normal) {
                    break :blk field.normal.name;
                } else {
                    break :blk field.generic.@"0";
                }
            } ++ " not found or type not match with the requirement");
        }
        inline for (self.functions) |function| {
            if (function == .normal) {
                if (!@hasDecl(input, function.normal.@"0")) @compileError("function with name " ++ function.normal.@"0" ++ " not found inside input type");
                const input_func = @field(input, function.normal.@"0");
                const input_func_ti = @typeInfo(@TypeOf(input_func));
                if (input_func_ti != .@"fn") @compileError(function.normal.@"0" + " is not a function");
                const input_fn = input_func_ti.@"fn";
                if (input_fn.params.len != function.normal.@"1".params.len) @compileError("function " ++ function.normal.@"0" ++ " parameter length not match with the requirement");
                if (input_fn.return_type != function.normal.@"1".return_type) @compileError("function " ++ function.normal.@"0" ++ " return type not match with the requirement");
                if (input_fn.is_generic != function.normal.@"1".is_generic) @compileError("expect " ++ blk: {
                    if (function.normal.@"1".is_generic) {
                        break :blk "generic function";
                    } else {
                        break :blk "non-generic function";
                    }
                } ++ function.normal.@"0" ++ " got a " ++ blk1: {
                    if (input_fn.is_generic) {
                        break :blk1 "generic function";
                    } else {
                        break :blk1 "non-generic function";
                    }
                } ++ "with that name");
                if (input_fn.is_var_args) @compileError("var args function is not supported yet, since you could not create this type of function in zig yet");
                if (function.normal.@"1".calling_convention != input_fn.calling_convention) @compileError("function with different call convention is not the same");
                inline for (function.normal.@"1".params, 0..) |param, i| {
                    var params_all_match = true;
                    params_all_match = param.is_generic == input_fn.params[i].is_generic;
                    params_all_match = param.is_noalias == input_fn.params[i].is_noalias;
                    params_all_match = param.type == input_fn.params[i].type;
                    if (!params_all_match) @compileError("function " ++ function.normal.@"0" ++ " parameter " ++ @typeName(param.type) ++ " at index " ++ @typeName(i) ++ " not match with the requirement");
                }
            } else {
                if (!@hasDecl(input, function.generic.@"0")) @compileError("function with name " ++ function.generic.@"0" ++ " not found inside input type");
                const generic_function_name = function.generic.@"0";
                const generic_function = function.generic.@"1";
                const generic_function_impl = generic_function.build(self: {
                    break :self @field(input, self.gas[0].name);
                }, comptimeMap(GenericTypeArg, type, generic_function.gas, struct {
                    pub fn run(in: GenericTypeArg) type {
                        return @field(input, in.name);
                    }
                }.run));
                const input_func = @field(input, generic_function_name);
                const input_func_ti = @typeInfo(@TypeOf(input_func));
                if (input_func_ti != .@"fn") @compileError(generic_function_name ++ " is not a function");
                const input_fn = input_func_ti.@"fn";
                if (input_fn.params.len != generic_function_impl.params.len) @compileError("function " ++ generic_function_name ++ " parameter length not match with the requirement");
                if (input_fn.return_type != generic_function_impl.return_type) @compileError("function " ++ generic_function_name ++ " return type not match with the requirement");
                if (input_fn.is_generic != generic_function_impl.is_generic) @compileError("expect " ++ blk: {
                    if (generic_function_impl.is_generic) {
                        break :blk "generic function";
                    } else {
                        break :blk "non-generic function";
                    }
                } ++ generic_function_impl.name ++ " got a " ++ blk1: {
                    if (input_fn.is_generic) {
                        break :blk1 "generic function";
                    } else {
                        break :blk1 "non-generic function";
                    }
                } ++ "with that name");
                if (input_fn.is_var_args) @compileError("var args function is not supported yet, since you could not create this type of function in zig yet");
                if (@intFromEnum(generic_function_impl.calling_convention) != @intFromEnum(input_fn.calling_convention)) @compileError("function with different call convention is not the same");
                inline for (generic_function_impl.params, 0..) |param, i| {
                    var params_all_match = true;
                    params_all_match = param.is_generic == input_fn.params[i].is_generic;
                    params_all_match = param.is_noalias == input_fn.params[i].is_noalias;
                    params_all_match = param.type == input_fn.params[i].type;
                    if (!params_all_match) @compileError("function " ++ generic_function_name ++ " parameter " ++ @typeName(param.type) ++ " at index " ++ @typeName(i) ++ " not match with the requirement");
                }
            }
        }
        return input;
    }
    pub fn combine(comptime concepts: Self) Self {
        var gas_addtion: []const GenericTypeArg = &[_]GenericTypeArg{};
        var fields_addtion: []const Concept2Field = &[_]Concept2Field{};
        var functions_addtion: []const Concept2Function = &[_]Concept2Function{};
        inline for (concepts) |concept| {
            gas_addtion = gas_addtion ++ concept.gas;
            fields_addtion = fields_addtion ++ concept.fields;
            functions_addtion = functions_addtion ++ concept.functions;
        }
        return .{
            .fields = fields_addtion,
            .functions = functions_addtion,
            .gas = gas_addtion,
        };
    }
};

test "concept builder" {
    const Addable = struct {
        pub const Self = @This();
        pub const RHS = Self;
        pub const Output = Self;
        pub fn add(self: Self, rhs: RHS) Output {
            return Output{
                self.value + rhs.value,
            };
        }
        value: i32,
    };
    const AddableConcept = (Concept2.init().addField(.{
        .normal = .{
            .alignment = @alignOf(i32),
            .is_comptime = false,
            .name = "value",
            .type = i32,
            .default_value_ptr = null,
        },
    })).addFunction(.{ .generic = .{ "add", .{
        .gas = &[_]GenericTypeArg{
            .{
                .name = "Self",
            },
            .{
                .name = "RHS",
            },
            .{
                .name = "Output",
            },
        },
        .builder = struct {
            pub fn run(comptime Self: type, comptime input: anytype) Type.Fn {
                return @typeInfo(fn (Self, input[0]) input[1]).@"fn";
            }
        }.run,
    } } });
    try std.testing.expect(AddableConcept.check(Addable) == Addable);
}

fn getFunctionInfo(comptime TargetType: type, comptime fnName: [:0]const u8) Type.Fn {
    if (@hasDecl(TargetType, fnName)) {
        return @typeInfo(@TypeOf(@field(TargetType, fnName))).@"fn";
    }
    @compileError("Function " ++ fnName ++ " not found in type " ++ @typeName(TargetType));
}

pub const Concept3CheckedResult = union(enum) {
    pub const FailedResult = struct {
        reason: [:0]const u8,
    };
    passed,
    failed: FailedResult,
};

pub const Requirement = union(enum) {
    pub const FnChecker = fn (comptime FNInfo: Type.Fn) Concept3CheckedResult;
    pub const FieldChecker = fn (comptime FieldInfo: Type.StructField) Concept3CheckedResult;
    @"fn": FnChecker,
    field: FieldChecker,
};

pub const RequirementList = struct {
    requirements: []const Requirement,
    names: []const [:0]const u8,
    pub fn init() RequirementList {
        return .{
            .requirements = &[_]Requirement{},
            .names = &[_][:0]const u8{},
        };
    }
    pub fn addFn(self: RequirementList, comptime name: [:0]const u8, comptime checker: Requirement.FnChecker) RequirementList {
        return .{
            .requirements = self.requirements ++ [_]Requirement{.{ .@"fn" = checker }},
            .names = self.names ++ [_][:0]const u8{name},
        };
    }
    pub fn addField(self: RequirementList, comptime name: [:0]const u8, comptime checker: Requirement.FieldChecker) RequirementList {
        return .{
            .requirements = self.requirements ++ [_]Requirement{.{ .@"fn" = checker }},
            .names = self.names ++ [_][:0]const u8{name},
        };
    }
};
/// concept3 is a simplified version of concept2, which does not support generic within requirements
/// if you want to use generic you should write outter generic function that returns Concept3
fn Concept3(comptime requirements: RequirementList) fn (comptime type) bool {
    if (requirements.requirements.len != requirements.names.len) @compileError("requirements and names length should be the same");
    return struct {
        pub fn run(comptime T: type) bool {
            inline for (requirements.requirements, 0..) |requirement, i| {
                if (requirement == .@"fn") comptime {
                    const fn_info = getFunctionInfo(T, requirements.names[i]);
                    const result = requirement.@"fn"(fn_info);
                    if (result != .passed) {
                        @compileError("Function requirement " ++ requirements.names[i] ++ " not satisfied: " ++ result.failed.reason);
                    }
                } else {
                    const ti = @typeInfo(T);
                    if (ti != .@"struct") @compileError("Field requirement could only be applied to struct types");
                    const struct_ti = ti.@"struct";
                    var found = false;
                    inline for (struct_ti.fields) |field_info| {
                        if (std.mem.eql(u8, field_info.name, requirements.names[i])) {
                            found = true;
                            const result = requirement.field(field_info);
                            if (result != .passed) {
                                @compileError("Field requirement " ++ requirements.names[i] ++ " not satisfied: " ++ result.failed.reason);
                            }
                            break;
                        }
                    }
                    if (!found) {
                        @compileError("Field requirement " ++ requirements.names[i] ++ " not found in type " ++ @typeName(T));
                    }
                }
            }
            return true;
        }
    }.run;
}
test "concept3" {
    const Matcher = Concept3(RequirementList.init().addFn("match", struct {
        fn run(comptime FNInfo: Type.Fn) Concept3CheckedResult {
            if (FNInfo.params.len != 2) {
                return .failed{ .reason = "match function should have 2 parameters" };
            }
            if (FNInfo.return_type != bool) {
                return .failed{ .reason = "match function should return bool" };
            }
            return .passed;
        }
    }.run));
    const StringMatcher = struct {
        str: []const u8,
        pub const Self = @This();
        pub fn match(self: Self, other: []const u8) bool {
            return std.mem.eql(u8, self.str, other);
        }
    };
    try std.testing.expect(Matcher(StringMatcher));
}

pub const GenericConstructorOfRequirementList = fn (comptime Types: anytype) RequirementList;

pub fn GenericLayer(comptime Types: anytype, comptime requirements: GenericConstructorOfRequirementList) fn (comptime type) bool {
    if (!utils.isTypeTuple(Types)) @compileError("Types should be a type tuple");
    return Concept3(requirements(Types));
}

test "generic layer" {
    comptime {
        const MatcherRequirements = struct {
            pub fn MR(comptime Types: anytype) RequirementList {
                return RequirementList.init().addFn("match", struct {
                    fn _run(comptime FNInfo: Type.Fn) Concept3CheckedResult {
                        if (FNInfo.params.len != 2) {
                            return .failed{ .reason = "match function should have 2 parameters" };
                        }
                        const expected_type = utils.comptimeTupleToSlice(type, Types)[0];
                        if (FNInfo.params[1].type != expected_type) comptime {
                            return .failed{ .reason = "first parameter type not match expected type" };
                        };
                        if (FNInfo.return_type != bool) {
                            return .failed{ .reason = "match function should return bool" };
                        }
                        return .passed;
                    }
                }._run);
            }
        }.MR;
        const Matcher = GenericLayer(.{[]const u8}, MatcherRequirements);
        const StringMatcher = struct {
            str: []const u8,
            pub const Self = @This();
            pub fn match(self: Self, other: []const u8) bool {
                return std.mem.eql(u8, self.str, other);
            }
        };
        try std.testing.expect(Matcher(StringMatcher));
    }
}
