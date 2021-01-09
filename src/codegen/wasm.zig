const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const leb = std.leb;
const mem = std.mem;

const Module = @import("../Module.zig");
const Decl = Module.Decl;
const ir = @import("../ir.zig");
const Inst = ir.Inst;
const Type = @import("../type.zig").Type;
const Value = @import("../value.zig").Value;

fn genValtype(ty: Type) u8 {
    return switch (ty.tag()) {
        .u32, .i32 => 0x7F,
        .u64, .i64 => 0x7E,
        .f32 => 0x7D,
        .f64 => 0x7C,
        else => @panic("TODO: Implement more types for wasm."),
    };
}

pub fn genFunctype(buf: *ArrayList(u8), decl: *Decl) !void {
    const ty = decl.typed_value.most_recent.typed_value.ty;
    const writer = buf.writer();

    // functype magic
    try writer.writeByte(0x60);

    // param types
    try leb.writeULEB128(writer, @intCast(u32, ty.fnParamLen()));
    if (ty.fnParamLen() != 0) {
        const params = try buf.allocator.alloc(Type, ty.fnParamLen());
        defer buf.allocator.free(params);
        ty.fnParamTypes(params);
        for (params) |param_type| try writer.writeByte(genValtype(param_type));
    }

    // return type
    const return_type = ty.fnReturnType();
    switch (return_type.tag()) {
        .void, .noreturn => try leb.writeULEB128(writer, @as(u32, 0)),
        else => {
            try leb.writeULEB128(writer, @as(u32, 1));
            try writer.writeByte(genValtype(return_type));
        },
    }
}

pub fn genCode(buf: *ArrayList(u8), decl: *Decl) !void {
    assert(buf.items.len == 0);
    const writer = buf.writer();

    // Reserve space to write the size after generating the code
    try buf.resize(5);

    // Calculate the locals of the body
    // We must calculate the occurance of each value type and emit the count of it
    const tv = decl.typed_value.most_recent.typed_value;
    const mod_fn = tv.val.castTag(.function).?.data;

    var locals = std.AutoArrayHashMap(u8, u32).init(buf.allocator);
    defer locals.deinit();

    for (mod_fn.body.instructions) |inst| {
        if (inst.tag != .alloc) continue;

        const alloc = inst.castTag(.alloc).?;
        const elem_type = alloc.base.ty.elemType();
        const zig_type = elem_type.zigTypeTag();

        if (zig_type != .Int and zig_type != .Float) return error.TODOImplementMoreWasmCodegen;

        const wasm_type = genValtype(elem_type);
        const entry = try locals.getOrPut(wasm_type);
        if (entry.found_existing)
            entry.entry.value += 1
        else
            entry.entry.value = 1;

        // append the local to our list of locals so they can be set/retrieved later
        try decl.fn_link.wasm.?.locals.append(buf.allocator, inst);
    }

    // write the amount of different types were found
    try leb.writeULEB128(writer, @intCast(u32, locals.items().len));

    // emit the actual locals amount per type
    for (locals.items()) |entry| {
        try leb.writeULEB128(writer, entry.value);
        try leb.writeULEB128(writer, entry.key);
    }

    // Write instructions
    // TODO: check for and handle death of instructions
    for (mod_fn.body.instructions) |inst| {
        // skip load as they should be triggered by other instructions
        // as well as alloc as it has been handled above
        if (inst.tag != .load and inst.tag != .alloc and inst.tag != .add) {
            try genInst(buf, decl, inst);
        }
    }

    // Write 'end' opcode
    try writer.writeByte(0x0B);

    // Fill in the size of the generated code to the reserved space at the
    // beginning of the buffer.
    const size = buf.items.len - 5 + decl.fn_link.wasm.?.idx_refs.items.len * 5;
    leb.writeUnsignedFixed(5, buf.items[0..5], @intCast(u32, size));
}

fn genInst(buf: *ArrayList(u8), decl: *Decl, inst: *Inst) error{ OutOfMemory, TODOImplementMoreWasmCodegen }!void {
    return switch (inst.tag) {
        .add => genAdd(buf, decl, inst.castTag(.add).?),
        .alloc => unreachable, // already handled in 'genCode'
        .block => genBlock(buf, decl, inst.castTag(.block).?),
        .br => genBreak(buf, decl, inst.castTag(.br).?),
        .call => genCall(buf, decl, inst.castTag(.call).?),
        .cmp_lt => genCmp(buf, decl, inst.castTag(.cmp_lt).?, .lt),
        .condbr => genCondBr(buf, decl, inst.castTag(.condbr).?),
        .constant => genConstant(buf, decl, inst.castTag(.constant).?),
        .dbg_stmt => {},
        .load => genLoad(buf, decl, inst.castTag(.load).?),
        .loop => genLoop(buf, decl, inst.castTag(.loop).?),
        .ret => genRet(buf, decl, inst.castTag(.ret).?),
        .retvoid => {},
        .store => genStore(buf, decl, inst.castTag(.store).?),
        else => {
            std.debug.print("TODO: {s}", .{inst.tag});
            return error.TODOImplementMoreWasmCodegen;
        },
    };
}

fn genConstant(buf: *ArrayList(u8), decl: *Decl, inst: *Inst.Constant) !void {
    const writer = buf.writer();
    switch (inst.base.ty.tag()) {
        .u32 => {
            try writer.writeByte(0x41); // i32.const
            try leb.writeILEB128(writer, inst.val.toUnsignedInt());
        },
        .i32 => {
            try writer.writeByte(0x41); // i32.const
            try leb.writeILEB128(writer, inst.val.toSignedInt());
        },
        .u64 => {
            try writer.writeByte(0x42); // i64.const
            try leb.writeILEB128(writer, inst.val.toUnsignedInt());
        },
        .i64 => {
            try writer.writeByte(0x42); // i64.const
            try leb.writeILEB128(writer, inst.val.toSignedInt());
        },
        .f32 => {
            try writer.writeByte(0x43); // f32.const
            // TODO: enforce LE byte order
            try writer.writeAll(mem.asBytes(&inst.val.toFloat(f32)));
        },
        .f64 => {
            try writer.writeByte(0x44); // f64.const
            // TODO: enforce LE byte order
            try writer.writeAll(mem.asBytes(&inst.val.toFloat(f64)));
        },
        .void => {},
        else => return error.TODOImplementMoreWasmCodegen,
    }
}

fn genRet(buf: *ArrayList(u8), decl: *Decl, inst: *Inst.UnOp) !void {
    try genInst(buf, decl, inst.operand);
}

fn genCall(buf: *ArrayList(u8), decl: *Decl, inst: *Inst.Call) !void {
    const func_inst = inst.func.castTag(.constant).?;
    const func = func_inst.val.castTag(.function).?.data;
    const target = func.owner_decl;
    const target_ty = target.typed_value.most_recent.typed_value.ty;

    if (inst.args.len != 0) return error.TODOImplementMoreWasmCodegen;

    try buf.append(0x10); // call

    // The function index immediate argument will be filled in using this data
    // in link.Wasm.flush().
    try decl.fn_link.wasm.?.idx_refs.append(buf.allocator, .{
        .offset = @intCast(u32, buf.items.len),
        .decl = target,
    });
}

fn genStore(buf: *ArrayList(u8), decl: *Decl, inst: *Inst.BinOp) !void {
    const idx = decl.fn_link.wasm.?.getLocalidx(inst.lhs).?;

    const writer = buf.writer();

    // generate codegen for rhs before we store it
    try genInst(buf, decl, inst.rhs);

    // local.set
    try writer.writeByte(0x21);
    try leb.writeULEB128(writer, idx);
}

fn genLoad(buf: *ArrayList(u8), decl: *Decl, inst: *Inst.UnOp) !void {
    const idx = decl.fn_link.wasm.?.getLocalidx(inst.operand).?;
    const writer = buf.writer();

    // load the local at index `idx` onto the stack
    try writer.writeByte(0x20);
    try leb.writeULEB128(writer, idx);
}

fn genAdd(buf: *ArrayList(u8), decl: *Decl, inst: *Inst.BinOp) !void {
    const ty = inst.base.ty;
    // std.debug.print("{s}\n", .{inst.base.tag});
    std.debug.print("{s} + {s}\n", .{ inst.lhs.tag, inst.rhs.tag });

    try genInst(buf, decl, inst.lhs);
    try genInst(buf, decl, inst.rhs);

    const byte_code: u8 = switch (ty.tag()) {
        .u32, .i32 => 0x6A, //i32.add
        .u64, .i64 => 0x7C, //i64.add
        .f32 => 0x92, //f32.add
        .f64 => 0xA0, //f64.add
        else => @panic("TODO: Implement more WASM types"),
    };

    try buf.append(byte_code);
}

fn genBlock(buf: *ArrayList(u8), decl: *Decl, block: *Inst.Block) !void {
    try genBody(buf, decl, block.body);
}

fn genBody(buf: *ArrayList(u8), decl: *Decl, body: ir.Body) !void {
    for (body.instructions) |inst| try genInst(buf, decl, inst);
}

fn genLoop(buf: *ArrayList(u8), decl: *Decl, loop: *Inst.Loop) !void {
    const writer = buf.writer();

    // set correct scope that we are generating code for
    const scope = &decl.fn_link.wasm.?.scope;
    const prev_scope = scope.*;
    scope.* = .loop;

    // begin a block (required to jump to)
    try writer.writeByte(0x02);
    // block type (void)
    try writer.writeByte(0x40);

    // begin a loop block
    try writer.writeByte(0x03);
    try writer.writeByte(0x40);

    try genBody(buf, decl, loop.body);

    // write .end byte to end loop
    try writer.writeByte(0x0B);
    // write .end byte to end initial block
    try writer.writeByte(0x0B);

    // return to old scope
    scope.* = prev_scope;
}

fn genCmp(buf: *ArrayList(u8), decl: *Decl, cmp: *Inst.BinOp, op: std.math.CompareOperator) !void {
    std.debug.print("Comparing: {s} < {s}\n", .{ cmp.lhs.tag, cmp.rhs.tag });
    try genInst(buf, decl, cmp.lhs);
    try genInst(buf, decl, cmp.rhs);

    const ty = cmp.lhs.ty.tag();
    std.debug.print("Ty: {s}\n", .{ty});
    const byte_code: ?u8 = switch (op) {
        .lt => @as(?u8, switch (ty) {
            .i32 => 0x48, // i32.lt_s
            .u32 => 0x49, // i32.lt_u
            .i64 => 0x53, // i64.lt_s
            .u64 => 0x54, // i64.lt_u
            .f32 => 0x5D, // f32.lt
            .f64 => 0x63, // f64.lt
            else => null,
        }),
        else => null,
    };

    const byte_to_emit = byte_code orelse return error.TODOImplementMoreWasmCodegen;

    try buf.append(byte_to_emit);
}

fn genCondBr(buf: *ArrayList(u8), decl: *Decl, condbr: *Inst.CondBr) !void {
    const writer = buf.writer();
    const current_scope = decl.fn_link.wasm.?.scope;

    // if the scope is `loop` we need to break out if the condition equals to false
    if (current_scope == .loop) {
        // check if result equals '0' (i32.eqz)
        try writer.writeByte(0x45);
        // break if false (br_if)
        try writer.writeByte(0x0D);
        // break last block
        try leb.writeULEB128(writer, @as(u32, 1));
    }

    try genBody(buf, decl, condbr.then_body);

    if (current_scope == .loop) {
        // break the loop so we jump back to the start
        try writer.writeByte(0x0C);
        try leb.writeULEB128(writer, @as(u32, 0));
    }
}

fn genBreak(buf: *ArrayList(u8), decl: *Decl, br: *Inst.Br) !void {
    std.debug.print("Break: ----\n{}\n", .{br});
}
