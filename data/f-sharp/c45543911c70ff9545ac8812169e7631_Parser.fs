// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"


open Ast


# 12 "obj\x86\Debug\Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | ASTER
  | SLASH
  | FLOAT of (System.Double)
  | INT32 of (System.Int32)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_ASTER
    | TOKEN_SLASH
    | TOKEN_FLOAT
    | TOKEN_INT32
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_Prog
    | NONTERM_Expr
    | NONTERM_Term
    | NONTERM_Factor

// This function maps tokens to integers indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | LPAREN  -> 1 
  | RPAREN  -> 2 
  | PLUS  -> 3 
  | MINUS  -> 4 
  | ASTER  -> 5 
  | SLASH  -> 6 
  | FLOAT _ -> 7 
  | INT32 _ -> 8 

// This function maps integers indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_LPAREN 
  | 2 -> TOKEN_RPAREN 
  | 3 -> TOKEN_PLUS 
  | 4 -> TOKEN_MINUS 
  | 5 -> TOKEN_ASTER 
  | 6 -> TOKEN_SLASH 
  | 7 -> TOKEN_FLOAT 
  | 8 -> TOKEN_INT32 
  | 11 -> TOKEN_end_of_input
  | 9 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_Prog 
    | 3 -> NONTERM_Expr 
    | 4 -> NONTERM_Expr 
    | 5 -> NONTERM_Expr 
    | 6 -> NONTERM_Term 
    | 7 -> NONTERM_Term 
    | 8 -> NONTERM_Term 
    | 9 -> NONTERM_Factor 
    | 10 -> NONTERM_Factor 
    | 11 -> NONTERM_Factor 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 11 
let _fsyacc_tagOfErrorTerminal = 9

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | LPAREN  -> "LPAREN" 
  | RPAREN  -> "RPAREN" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | ASTER  -> "ASTER" 
  | SLASH  -> "SLASH" 
  | FLOAT _ -> "FLOAT" 
  | INT32 _ -> "INT32" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | LPAREN  -> (null : System.Object) 
  | RPAREN  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | ASTER  -> (null : System.Object) 
  | SLASH  -> (null : System.Object) 
  | FLOAT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | INT32 _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 1us; 65535us; 0us; 2us; 2us; 65535us; 0us; 3us; 18us; 5us; 4us; 65535us; 0us; 10us; 6us; 7us; 8us; 9us; 18us; 10us; 6us; 65535us; 0us; 15us; 6us; 15us; 8us; 15us; 11us; 12us; 13us; 14us; 18us; 15us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 5us; 8us; 13us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 3us; 2us; 3us; 4us; 1us; 2us; 3us; 3us; 4us; 11us; 1us; 3us; 3us; 3us; 6us; 7us; 1us; 4us; 3us; 4us; 6us; 7us; 3us; 5us; 6us; 7us; 1us; 6us; 1us; 6us; 1us; 7us; 1us; 7us; 1us; 8us; 1us; 9us; 1us; 10us; 1us; 11us; 1us; 11us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 10us; 12us; 16us; 18us; 22us; 24us; 28us; 32us; 34us; 36us; 38us; 40us; 42us; 44us; 46us; 48us; |]
let _fsyacc_action_rows = 20
let _fsyacc_actionTableElements = [|3us; 32768us; 1us; 18us; 7us; 16us; 8us; 17us; 0us; 49152us; 0us; 16385us; 3us; 32768us; 0us; 4us; 3us; 6us; 4us; 8us; 0us; 16386us; 3us; 32768us; 2us; 19us; 3us; 6us; 4us; 8us; 3us; 32768us; 1us; 18us; 7us; 16us; 8us; 17us; 2us; 16387us; 5us; 11us; 6us; 13us; 3us; 32768us; 1us; 18us; 7us; 16us; 8us; 17us; 2us; 16388us; 5us; 11us; 6us; 13us; 2us; 16389us; 5us; 11us; 6us; 13us; 3us; 32768us; 1us; 18us; 7us; 16us; 8us; 17us; 0us; 16390us; 3us; 32768us; 1us; 18us; 7us; 16us; 8us; 17us; 0us; 16391us; 0us; 16392us; 0us; 16393us; 0us; 16394us; 3us; 32768us; 1us; 18us; 7us; 16us; 8us; 17us; 0us; 16395us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 4us; 5us; 6us; 10us; 11us; 15us; 19us; 22us; 26us; 29us; 32us; 36us; 37us; 41us; 42us; 43us; 44us; 45us; 49us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 1us; 2us; 3us; 3us; 1us; 3us; 3us; 1us; 1us; 1us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 3us; 3us; 3us; 4us; 4us; 4us; 5us; 5us; 5us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 16385us; 65535us; 16386us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16390us; 65535us; 16391us; 16392us; 16393us; 16394us; 65535us; 16395us; |]
let _fsyacc_reductions ()  =    [| 
# 131 "obj\x86\Debug\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data :  Ast.Equation )) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 140 "obj\x86\Debug\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Prog)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 27 "Parser.fsy"
                                   Equation(_1) 
                   )
# 27 "Parser.fsy"
                 :  Ast.Equation ));
# 151 "obj\x86\Debug\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "Parser.fsy"
                                          _1 
                   )
# 30 "Parser.fsy"
                 : 'Prog));
# 162 "obj\x86\Debug\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Term)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "Parser.fsy"
                                               Plus(_1, _3)  
                   )
# 33 "Parser.fsy"
                 : 'Expr));
# 174 "obj\x86\Debug\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Term)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "Parser.fsy"
                                               Minus(_1, _3) 
                   )
# 34 "Parser.fsy"
                 : 'Expr));
# 186 "obj\x86\Debug\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Term)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "Parser.fsy"
                                       Term(_1)      
                   )
# 35 "Parser.fsy"
                 : 'Expr));
# 197 "obj\x86\Debug\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Term)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Factor)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "Parser.fsy"
                                                 Times(_1, _3)  
                   )
# 38 "Parser.fsy"
                 : 'Term));
# 209 "obj\x86\Debug\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Term)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Factor)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "Parser.fsy"
                                                 Divide(_1, _3) 
                   )
# 39 "Parser.fsy"
                 : 'Term));
# 221 "obj\x86\Debug\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Factor)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "Parser.fsy"
                                        Factor(_1)     
                   )
# 40 "Parser.fsy"
                 : 'Term));
# 232 "obj\x86\Debug\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : System.Double)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "Parser.fsy"
                                        Float(_1)  
                   )
# 43 "Parser.fsy"
                 : 'Factor));
# 243 "obj\x86\Debug\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : System.Int32)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "Parser.fsy"
                                        Integer(_1) 
                   )
# 44 "Parser.fsy"
                 : 'Factor));
# 254 "obj\x86\Debug\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "Parser.fsy"
                                                 ParenEx(_2) 
                   )
# 45 "Parser.fsy"
                 : 'Factor));
|]
# 266 "obj\x86\Debug\Parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 12;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf :  Ast.Equation  =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))