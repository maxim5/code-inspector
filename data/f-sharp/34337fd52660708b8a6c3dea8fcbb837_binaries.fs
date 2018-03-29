namespace IronLua.Binaries

open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Linq
open IronLua
open IronLua.Runtime

type Endianness = BigEndian = 0 | LittleEndian = 1
type NumberTypes = FloatingPoint = 0 | Integral = 1
type ConstantTypes = Nil = 0 | Boolean = 1 | Number = 3 | String = 4
type VarArgFlags = HasArg = 0 | IsVarArg = 1| NeedsArg = 4

type Instruction(bytecode : uint32) = 
    struct 
    end

type Constant = 
    struct 
        val Index : uint32
        val internal Value : LuaValue

        new(index : uint32) = { Index = index; Value = LuaValue.Nil }
        new(index : uint32, value : bool) = { Index = index; Value = LuaValue.FromBool value}
        new(index : uint32, value : double) = { Index = index; Value = LuaValue.FromDouble value }
        new(index : uint32, value : string) = { Index = index; Value = LuaValue.FromString value }

        member this.Type with get() = this.Value.Type

        member this.Number with get() = if this.Value.Type = LuaValueTypes.Number 
                                        then this.Value.Number 
                                        else 0.0

        member this.Boolean with get() = if this.Value.Type = LuaValueTypes.Boolean
                                         then this.Value.Bool 
                                         else false

        member this.String with get() = if this.Value.Type = LuaValueTypes.String
                                        then this.Value.String 
                                        else null

        override this.ToString() = sprintf "const: %A (idx: %i)" this.Value this.Index

    end

type Upvalue = 
    struct 

        val InStack : bool
        val Index : uint32
        val Name : string

        new(instack : bool, index : uint32, name : string) = { InStack = instack; Index = index; Name = name; }

        override this.ToString() = sprintf "upvalue: %s (idx: %i)" this.Name this.Index

    end
type Local = 
    struct 

        val Index : uint32
        val Name : string
        val SourceLine : int
        val SourceLastLine : int
        
        new(index : uint32, name : string, sourceline : int, sourcelastline : int) = {
            Index = index;
            Name = name;
            SourceLine = sourceline;
            SourceLastLine = sourcelastline;
        }

        override this.ToString() = sprintf "local: %s (idx: %i)" this.Name this.Index

    end

type LineInfo = 
    struct 

        val Line : int

        new (line : int) = { Line = line }

        override this.ToString() = sprintf "line: %i" this.Line

    end

type Prototype = 
    class 
        val Name : string
        val Depth : uint32
        val Index : uint32
        val DefinitionFirstLine : uint32
        val DefinitionLastLine : uint32
        val Instructions : IList<Instruction>
        val Upvalues : IList<Upvalue>
        val VarArgFlags : VarArgFlags
        val MaxStack : uint32
        val Constants : IList<Constant>
        val Prototypes : IList<Prototype>
        val Locals : IList<Local>
        val LineInfo : IList<LineInfo>

        internal new(name : string, depth : uint32, index : uint32,
                     deffirstline : uint32, deflastline : uint32,
                     code : IList<Instruction>, upvalues : IList<Upvalue>,
                     vaflags : VarArgFlags, maxstack : uint32,
                     consts : IList<Constant>, protos : IList<Prototype>,
                     locals : IList<Local>, lineinfo : IList<LineInfo>) = {
                            Name = name;
                            Depth = depth;
                            Index = index;
                            DefinitionFirstLine = deffirstline;
                            DefinitionLastLine = deflastline;
                            Instructions = code;
                            Upvalues = upvalues;
                            VarArgFlags = vaflags;
                            MaxStack = maxstack;
                            Constants = consts;
                            Prototypes = protos;
                            Locals = locals;
                            LineInfo = lineinfo;                    
                        }

        override this.ToString() = sprintf "proto: %s" this.Name
    end

type Chunk = 
    class
        val Name : string
        val Signature : string
        val Endianness : Endianness
        val IntSize : uint32
        val PtrSize : uint32
        val InstrSize : uint32
        val NumberSize : uint32
        val NumberType : NumberTypes
        val TopFunction : Prototype
        val Prototypes : IList<Prototype>

        internal new(name : string, sig' : string, 
                     endianness : Endianness, 
                     intsize : uint32, ptrsize : uint32,
                     instrsize : uint32, numbersize : uint32,
                     type' : NumberTypes, top : Prototype, protos : IList<Prototype>) = {
                            Name = name;
                            Signature = sig';
                            Endianness = endianness;
                            IntSize = intsize;
                            PtrSize = ptrsize;
                            InstrSize = instrsize;
                            NumberSize = numbersize;
                            NumberType = type';
                            TopFunction = top;
                            Prototypes = protos;
                        }

        override this.ToString() = sprintf "chunk: %s" this.Name

    end

module BinaryChunkCompiler = 
    let compile (stream : Stream) : Chunk =
        LuaRuntimeException("compile not implemented") |> raise

module BinaryChunkReader =

    let luaSignature = [| 27uy; byte 'L'; byte 'u'; byte 'a'; |]
    let chunkHeaderTail = [| 0x19uy; 0x93uy; byte '\r'; byte '\n'; 0x1auy; byte '\n' |]

    let readbytes c (stream : Stream) = 
        let data = Array.zeroCreate c
        let bytesread = stream.Read(data, 0, c)
        data

    let readbyte (stream : Stream) = 
        stream.ReadByte() |> byte

    let readasuint32 (stream : Stream) = 
        stream.ReadByte() |> uint32

    let rec readluanum (stream : Stream) = 
        BitConverter.ToDouble(stream |> readbytes 8, 0);

    and readluabool (stream : Stream) = 
        match stream |> readbyte with
        | 0uy ->    true
        | 1uy ->    false
        | _ as x -> LuaRuntimeException("Bad boolean constant value.") |> raise
    
    and readluaint (stream : Stream) = 
        let bytes = stream |> readbytes 4
        BitConverter.ToInt32(bytes, 0);

    and readluauint (stream : Stream) = 
        stream |> readluaint |> uint32

    and readluastring (stream : Stream) = 
        let size = stream |> readluaint
        let str : string = stream |> readbytes size |> toAscii  
        str.Substring(0, str.Length - 1) // trim null terminator

    and readinstruction i (stream : Stream) = 
        Instruction(stream |> readluauint)

    and readk i (stream : Stream) = 
        let ktype = stream |> readbyte |> toConstantType
        match ktype with 
        | ConstantTypes.Nil ->      Constant(i)
        | ConstantTypes.Boolean ->  Constant(i, stream |> readluabool)
        | ConstantTypes.Number ->   Constant(i, stream |> readluanum)
        | ConstantTypes.String ->   Constant(i, stream |> readluastring)
        | _ ->                      LuaRuntimeException("Bad constant type.") |> raise

    and readupvaluedata i (stream : Stream) = 
        KeyValuePair(stream |> readluabool, stream |> readasuint32)

    and readlineinfo i (stream : Stream) = 
        LineInfo(stream |> readluaint)

    and readlocal i (stream : Stream) = 
        Local(i, stream |> readluastring, 
                 stream |> readluaint, 
                 stream |> readluaint)

    and readupvalues (data : KeyValuePair<bool, uint32> array) (stream : Stream) = 
        let c = stream |> readluaint in
        if c = 0 then [| for i = 0 to data.Length - 1 do yield Upvalue(data.[i].Key, data.[i].Value, "?") |]
        else          [| for i = 0 to c - 1 do yield Upvalue(data.[i].Key, data.[i].Value, stream |> readluastring) |]

    and toAscii bytes = 
        Encoding.ASCII.GetString(bytes)
    and toEndianness = function 
        | 0uy  -> Endianness.BigEndian
        | 1uy ->  Endianness.LittleEndian
        | _ ->    LuaRuntimeException("Bad byte endianness.") |> raise
    and toNumberType = function
        | 0uy ->  NumberTypes.FloatingPoint
        | 1uy ->  NumberTypes.Integral
        | _ ->    LuaRuntimeException("Bad integral flag.") |> raise
    and toVarArgFlags = function
        | 0uy ->  VarArgFlags.HasArg
        | 1uy ->  VarArgFlags.IsVarArg
        | 4uy ->  VarArgFlags.NeedsArg
        | _ ->    LuaRuntimeException("Bad vararg flag.") |> raise
    and toConstantType = function
        | 0uy -> ConstantTypes.Nil
        | 1uy -> ConstantTypes.Boolean
        | 3uy -> ConstantTypes.Number
        | 4uy -> ConstantTypes.String
        | _ ->   LuaRuntimeException("Bad constant type.") |> raise
    and verifyChunkTail (bytes : byte array) = 
        if bytes.SequenceEqual(chunkHeaderTail) then bytes 
        else LuaRuntimeException("Wrong Lua 5.2 chunk header tail.") |> raise
    and verifySig (bytes : byte array) = 
        if bytes.SequenceEqual(luaSignature) then bytes 
        else LuaRuntimeException("Wrong Lua 5.2 signature.") |> raise
    and verifyVer b = 
        b = 0x52uy
    and readproto index depth stream =
        let linedefined = stream |> readluauint
        let lastlinedefined = stream |> readluauint
        let paramcount = stream |> readasuint32
        let vaflags = stream |> readbyte |> toVarArgFlags
        let maxstack = stream |> readasuint32
        let code = let c = stream |> readluauint in 
                   if c = 0u then [| |]
                   else           [| for i = 1u to c do yield stream |> readinstruction i |]
        let consts = let c = stream |> readluauint in 
                     if c = 0u then [| |]
                     else           [| for i = 1u to c do yield stream |> readk i |]
        let protos = let c = stream |> readluauint in 
                     if c = 0u then [| |]
                     else           [| for i = 1u to c do yield stream |> readproto i (depth + 1u) |]
        let upvaluedata = let c = stream |> readluauint in 
                          if c = 0u then [| |]
                          else           [| for i = 1u to c do yield stream |> readupvaluedata i |]
        // debug data
        let name = stream |> readluastring
        let lineinfo = let c = stream |> readluauint in 
                       if c = 0u then [| |]
                       else           [| for i = 1u to c do yield stream |> readlineinfo i |]
        let locals = let c = stream |> readluauint in 
                     if c = 0u then [| |]
                     else           [| for i = 1u to c do yield stream |> readlocal i |]

        let upvalues = stream |> readupvalues upvaluedata

        Prototype(name, depth, index, 
                  linedefined, lastlinedefined, 
                  code, upvalues, vaflags, maxstack, consts, protos, locals, lineinfo)

    and readchunk (stream : Stream) =
        let sig' = stream |> readbytes 4 |> verifySig |> toAscii
        let ver = stream |> readbyte |> verifyVer
        let format = stream |> readbyte
        let endianness = stream |> readbyte |> toEndianness
        let intsize = stream |> readasuint32
        let ptrsize = stream |> readasuint32
        let instrsize = stream |> readasuint32
        let numsize = stream |> readasuint32
        let numtype = stream |> readbyte |> toNumberType
        let tail = stream |> readbytes 6 |> verifyChunkTail
        let top = stream |> readproto 0u 0u
        let protos = top.Prototypes
        Chunk(top.Name, sig', endianness, 
              intsize, ptrsize, instrsize, 
              numsize, numtype, top, protos)

module ChunkReader = 
    let readchunk (stream : Stream) = 
        BinaryChunkCompiler.compile(stream)
