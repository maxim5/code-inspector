// Implementation file for parser generated by fsyacc
#light
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 9 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"

open Fp.FpCore;;

# 10 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | Lstr of (string)
  | Lvar of (string)
  | Lident of (string)
  | Lint of (int)
  | Lr of (int)
  | Ls of (int)
  | Laff
  | LDef
  | LUndef
  | LShow
  | LQuit
  | LLoad
  | LSave
  | LT
  | LF
  | Lapplytoall
  | Linsert
  | Lo
  | Lcond
  | Lcst
  | Lins
  | Lbu
  | Lwhile
  | Lcom
  | Lscl
  | Leol
  | Lsqu
  | Rsqu
  | Lang
  | Rang
  | Lpar
  | Rpar
  | Leval
  | Lprim of (string)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_Lstr
    | TOKEN_Lvar
    | TOKEN_Lident
    | TOKEN_Lint
    | TOKEN_Lr
    | TOKEN_Ls
    | TOKEN_Laff
    | TOKEN_LDef
    | TOKEN_LUndef
    | TOKEN_LShow
    | TOKEN_LQuit
    | TOKEN_LLoad
    | TOKEN_LSave
    | TOKEN_LT
    | TOKEN_LF
    | TOKEN_Lapplytoall
    | TOKEN_Linsert
    | TOKEN_Lo
    | TOKEN_Lcond
    | TOKEN_Lcst
    | TOKEN_Lins
    | TOKEN_Lbu
    | TOKEN_Lwhile
    | TOKEN_Lcom
    | TOKEN_Lscl
    | TOKEN_Leol
    | TOKEN_Lsqu
    | TOKEN_Rsqu
    | TOKEN_Lang
    | TOKEN_Rang
    | TOKEN_Lpar
    | TOKEN_Rpar
    | TOKEN_Leval
    | TOKEN_Lprim
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startfct
    | NONTERM__startexp
    | NONTERM__startcmd
    | NONTERM_cmd
    | NONTERM_exp
    | NONTERM_fatom
    | NONTERM_fct
    | NONTERM_comp
    | NONTERM_list
    | NONTERM_fctlist

// This function maps tokens to integers indexes
let tagOfToken (t:token) = 
  match t with
  | Lstr _ -> 0 
  | Lvar _ -> 1 
  | Lident _ -> 2 
  | Lint _ -> 3 
  | Lr _ -> 4 
  | Ls _ -> 5 
  | Laff  -> 6 
  | LDef  -> 7 
  | LUndef  -> 8 
  | LShow  -> 9 
  | LQuit  -> 10 
  | LLoad  -> 11 
  | LSave  -> 12 
  | LT  -> 13 
  | LF  -> 14 
  | Lapplytoall  -> 15 
  | Linsert  -> 16 
  | Lo  -> 17 
  | Lcond  -> 18 
  | Lcst  -> 19 
  | Lins  -> 20 
  | Lbu  -> 21 
  | Lwhile  -> 22 
  | Lcom  -> 23 
  | Lscl  -> 24 
  | Leol  -> 25 
  | Lsqu  -> 26 
  | Rsqu  -> 27 
  | Lang  -> 28 
  | Rang  -> 29 
  | Lpar  -> 30 
  | Rpar  -> 31 
  | Leval  -> 32 
  | Lprim _ -> 33 

// This function maps integers indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_Lstr 
  | 1 -> TOKEN_Lvar 
  | 2 -> TOKEN_Lident 
  | 3 -> TOKEN_Lint 
  | 4 -> TOKEN_Lr 
  | 5 -> TOKEN_Ls 
  | 6 -> TOKEN_Laff 
  | 7 -> TOKEN_LDef 
  | 8 -> TOKEN_LUndef 
  | 9 -> TOKEN_LShow 
  | 10 -> TOKEN_LQuit 
  | 11 -> TOKEN_LLoad 
  | 12 -> TOKEN_LSave 
  | 13 -> TOKEN_LT 
  | 14 -> TOKEN_LF 
  | 15 -> TOKEN_Lapplytoall 
  | 16 -> TOKEN_Linsert 
  | 17 -> TOKEN_Lo 
  | 18 -> TOKEN_Lcond 
  | 19 -> TOKEN_Lcst 
  | 20 -> TOKEN_Lins 
  | 21 -> TOKEN_Lbu 
  | 22 -> TOKEN_Lwhile 
  | 23 -> TOKEN_Lcom 
  | 24 -> TOKEN_Lscl 
  | 25 -> TOKEN_Leol 
  | 26 -> TOKEN_Lsqu 
  | 27 -> TOKEN_Rsqu 
  | 28 -> TOKEN_Lang 
  | 29 -> TOKEN_Rang 
  | 30 -> TOKEN_Lpar 
  | 31 -> TOKEN_Rpar 
  | 32 -> TOKEN_Leval 
  | 33 -> TOKEN_Lprim 
  | 36 -> TOKEN_end_of_input
  | 34 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startfct 
    | 1 -> NONTERM__startexp 
    | 2 -> NONTERM__startcmd 
    | 3 -> NONTERM_cmd 
    | 4 -> NONTERM_cmd 
    | 5 -> NONTERM_cmd 
    | 6 -> NONTERM_cmd 
    | 7 -> NONTERM_exp 
    | 8 -> NONTERM_exp 
    | 9 -> NONTERM_exp 
    | 10 -> NONTERM_exp 
    | 11 -> NONTERM_exp 
    | 12 -> NONTERM_exp 
    | 13 -> NONTERM_exp 
    | 14 -> NONTERM_exp 
    | 15 -> NONTERM_fatom 
    | 16 -> NONTERM_fatom 
    | 17 -> NONTERM_fatom 
    | 18 -> NONTERM_fatom 
    | 19 -> NONTERM_fatom 
    | 20 -> NONTERM_fatom 
    | 21 -> NONTERM_fatom 
    | 22 -> NONTERM_fatom 
    | 23 -> NONTERM_fatom 
    | 24 -> NONTERM_fatom 
    | 25 -> NONTERM_fatom 
    | 26 -> NONTERM_fct 
    | 27 -> NONTERM_fct 
    | 28 -> NONTERM_comp 
    | 29 -> NONTERM_comp 
    | 30 -> NONTERM_list 
    | 31 -> NONTERM_list 
    | 32 -> NONTERM_fctlist 
    | 33 -> NONTERM_fctlist 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 36 
let _fsyacc_tagOfErrorTerminal = 34

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | Lstr _ -> "Lstr" 
  | Lvar _ -> "Lvar" 
  | Lident _ -> "Lident" 
  | Lint _ -> "Lint" 
  | Lr _ -> "Lr" 
  | Ls _ -> "Ls" 
  | Laff  -> "Laff" 
  | LDef  -> "LDef" 
  | LUndef  -> "LUndef" 
  | LShow  -> "LShow" 
  | LQuit  -> "LQuit" 
  | LLoad  -> "LLoad" 
  | LSave  -> "LSave" 
  | LT  -> "LT" 
  | LF  -> "LF" 
  | Lapplytoall  -> "Lapplytoall" 
  | Linsert  -> "Linsert" 
  | Lo  -> "Lo" 
  | Lcond  -> "Lcond" 
  | Lcst  -> "Lcst" 
  | Lins  -> "Lins" 
  | Lbu  -> "Lbu" 
  | Lwhile  -> "Lwhile" 
  | Lcom  -> "Lcom" 
  | Lscl  -> "Lscl" 
  | Leol  -> "Leol" 
  | Lsqu  -> "Lsqu" 
  | Rsqu  -> "Rsqu" 
  | Lang  -> "Lang" 
  | Rang  -> "Rang" 
  | Lpar  -> "Lpar" 
  | Rpar  -> "Rpar" 
  | Leval  -> "Leval" 
  | Lprim _ -> "Lprim" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | Lstr _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | Lvar _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | Lident _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | Lint _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | Lr _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | Ls _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | Laff  -> (null : System.Object) 
  | LDef  -> (null : System.Object) 
  | LUndef  -> (null : System.Object) 
  | LShow  -> (null : System.Object) 
  | LQuit  -> (null : System.Object) 
  | LLoad  -> (null : System.Object) 
  | LSave  -> (null : System.Object) 
  | LT  -> (null : System.Object) 
  | LF  -> (null : System.Object) 
  | Lapplytoall  -> (null : System.Object) 
  | Linsert  -> (null : System.Object) 
  | Lo  -> (null : System.Object) 
  | Lcond  -> (null : System.Object) 
  | Lcst  -> (null : System.Object) 
  | Lins  -> (null : System.Object) 
  | Lbu  -> (null : System.Object) 
  | Lwhile  -> (null : System.Object) 
  | Lcom  -> (null : System.Object) 
  | Lscl  -> (null : System.Object) 
  | Leol  -> (null : System.Object) 
  | Lsqu  -> (null : System.Object) 
  | Rsqu  -> (null : System.Object) 
  | Lang  -> (null : System.Object) 
  | Rang  -> (null : System.Object) 
  | Lpar  -> (null : System.Object) 
  | Rpar  -> (null : System.Object) 
  | Leval  -> (null : System.Object) 
  | Lprim _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 0us; 65535us; 0us; 65535us; 1us; 65535us; 4us; 5us; 8us; 65535us; 2us; 3us; 4us; 17us; 23us; 24us; 26us; 64us; 32us; 33us; 42us; 43us; 48us; 49us; 65us; 64us; 21us; 65535us; 0us; 61us; 2us; 61us; 4us; 61us; 8us; 61us; 23us; 61us; 26us; 61us; 32us; 61us; 38us; 39us; 40us; 41us; 42us; 61us; 44us; 61us; 47us; 48us; 48us; 61us; 50us; 51us; 51us; 52us; 53us; 61us; 57us; 61us; 59us; 61us; 62us; 61us; 65us; 61us; 68us; 61us; 14us; 65535us; 0us; 1us; 2us; 30us; 4us; 30us; 8us; 9us; 23us; 31us; 26us; 30us; 32us; 30us; 42us; 30us; 44us; 67us; 48us; 30us; 53us; 54us; 59us; 60us; 65us; 30us; 68us; 67us; 16us; 65535us; 0us; 56us; 2us; 56us; 4us; 56us; 8us; 56us; 23us; 56us; 26us; 56us; 32us; 56us; 42us; 56us; 44us; 56us; 48us; 56us; 53us; 56us; 57us; 58us; 59us; 56us; 62us; 63us; 65us; 56us; 68us; 56us; 2us; 65535us; 26us; 28us; 65us; 66us; 2us; 65535us; 44us; 45us; 68us; 69us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 2us; 3us; 5us; 14us; 36us; 51us; 68us; 71us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 1us; 2us; 1us; 2us; 1us; 3us; 1us; 3us; 1us; 3us; 1us; 3us; 1us; 3us; 1us; 4us; 1us; 4us; 1us; 4us; 1us; 5us; 1us; 5us; 1us; 5us; 1us; 6us; 1us; 6us; 1us; 7us; 1us; 8us; 1us; 9us; 1us; 10us; 2us; 11us; 25us; 1us; 11us; 1us; 11us; 2us; 12us; 13us; 1us; 12us; 1us; 13us; 1us; 13us; 1us; 14us; 2us; 14us; 25us; 1us; 14us; 1us; 14us; 1us; 15us; 1us; 16us; 1us; 17us; 1us; 18us; 1us; 19us; 1us; 19us; 1us; 20us; 1us; 20us; 1us; 21us; 1us; 21us; 1us; 22us; 1us; 22us; 1us; 22us; 1us; 23us; 1us; 23us; 1us; 23us; 1us; 24us; 1us; 24us; 1us; 24us; 1us; 25us; 1us; 25us; 1us; 25us; 2us; 26us; 27us; 1us; 26us; 1us; 26us; 1us; 26us; 1us; 26us; 2us; 28us; 29us; 1us; 28us; 1us; 28us; 2us; 30us; 31us; 1us; 30us; 1us; 30us; 2us; 32us; 33us; 1us; 32us; 1us; 32us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 10us; 12us; 14us; 16us; 18us; 20us; 22us; 24us; 26us; 28us; 30us; 32us; 34us; 36us; 38us; 40us; 42us; 44us; 46us; 49us; 51us; 53us; 56us; 58us; 60us; 62us; 64us; 67us; 69us; 71us; 73us; 75us; 77us; 79us; 81us; 83us; 85us; 87us; 89us; 91us; 93us; 95us; 97us; 99us; 101us; 103us; 105us; 107us; 109us; 111us; 113us; 115us; 118us; 120us; 122us; 124us; 126us; 129us; 131us; 133us; 136us; 138us; 140us; 143us; 145us; |]
let _fsyacc_action_rows = 70
let _fsyacc_actionTableElements = [|11us; 32768us; 2us; 37us; 4us; 36us; 5us; 35us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 30us; 53us; 33us; 34us; 0us; 49152us; 16us; 32768us; 1us; 22us; 2us; 37us; 3us; 21us; 4us; 36us; 5us; 35us; 13us; 19us; 14us; 20us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 28us; 26us; 30us; 23us; 33us; 34us; 0us; 49152us; 19us; 32768us; 1us; 22us; 2us; 37us; 3us; 21us; 4us; 36us; 5us; 35us; 7us; 6us; 8us; 11us; 9us; 14us; 13us; 19us; 14us; 20us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 28us; 26us; 30us; 23us; 33us; 34us; 0us; 49152us; 1us; 32768us; 2us; 7us; 1us; 32768us; 6us; 8us; 11us; 32768us; 2us; 37us; 4us; 36us; 5us; 35us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 30us; 53us; 33us; 34us; 1us; 32768us; 25us; 10us; 0us; 16387us; 1us; 32768us; 2us; 12us; 1us; 32768us; 25us; 13us; 0us; 16388us; 1us; 32768us; 2us; 15us; 1us; 32768us; 25us; 16us; 0us; 16389us; 1us; 32768us; 25us; 18us; 0us; 16390us; 0us; 16391us; 0us; 16392us; 0us; 16393us; 0us; 16394us; 16us; 32768us; 1us; 22us; 2us; 37us; 3us; 21us; 4us; 36us; 5us; 35us; 13us; 19us; 14us; 20us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 28us; 26us; 30us; 23us; 33us; 34us; 1us; 32768us; 31us; 25us; 0us; 16395us; 17us; 32768us; 1us; 22us; 2us; 37us; 3us; 21us; 4us; 36us; 5us; 35us; 13us; 19us; 14us; 20us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 28us; 26us; 29us; 27us; 30us; 23us; 33us; 34us; 0us; 16396us; 1us; 32768us; 29us; 29us; 0us; 16397us; 1us; 32768us; 32us; 32us; 2us; 32768us; 31us; 55us; 32us; 32us; 16us; 32768us; 1us; 22us; 2us; 37us; 3us; 21us; 4us; 36us; 5us; 35us; 13us; 19us; 14us; 20us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 28us; 26us; 30us; 23us; 33us; 34us; 0us; 16398us; 0us; 16399us; 0us; 16400us; 0us; 16401us; 0us; 16402us; 11us; 32768us; 2us; 37us; 4us; 36us; 5us; 35us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 30us; 53us; 33us; 34us; 0us; 16403us; 11us; 32768us; 2us; 37us; 4us; 36us; 5us; 35us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 30us; 53us; 33us; 34us; 0us; 16404us; 16us; 32768us; 1us; 22us; 2us; 37us; 3us; 21us; 4us; 36us; 5us; 35us; 13us; 19us; 14us; 20us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 28us; 26us; 30us; 23us; 33us; 34us; 0us; 16405us; 11us; 32768us; 2us; 37us; 4us; 36us; 5us; 35us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 30us; 53us; 33us; 34us; 1us; 32768us; 27us; 46us; 0us; 16406us; 11us; 32768us; 2us; 37us; 4us; 36us; 5us; 35us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 30us; 53us; 33us; 34us; 16us; 32768us; 1us; 22us; 2us; 37us; 3us; 21us; 4us; 36us; 5us; 35us; 13us; 19us; 14us; 20us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 28us; 26us; 30us; 23us; 33us; 34us; 0us; 16407us; 11us; 32768us; 2us; 37us; 4us; 36us; 5us; 35us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 30us; 53us; 33us; 34us; 11us; 32768us; 2us; 37us; 4us; 36us; 5us; 35us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 30us; 53us; 33us; 34us; 0us; 16408us; 11us; 32768us; 2us; 37us; 4us; 36us; 5us; 35us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 30us; 53us; 33us; 34us; 1us; 32768us; 31us; 55us; 0us; 16409us; 1us; 16411us; 18us; 57us; 11us; 32768us; 2us; 37us; 4us; 36us; 5us; 35us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 30us; 53us; 33us; 34us; 1us; 32768us; 24us; 59us; 11us; 32768us; 2us; 37us; 4us; 36us; 5us; 35us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 30us; 53us; 33us; 34us; 0us; 16410us; 1us; 16413us; 17us; 62us; 11us; 32768us; 2us; 37us; 4us; 36us; 5us; 35us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 30us; 53us; 33us; 34us; 0us; 16412us; 1us; 16415us; 23us; 65us; 16us; 32768us; 1us; 22us; 2us; 37us; 3us; 21us; 4us; 36us; 5us; 35us; 13us; 19us; 14us; 20us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 28us; 26us; 30us; 23us; 33us; 34us; 0us; 16414us; 1us; 16417us; 23us; 68us; 11us; 32768us; 2us; 37us; 4us; 36us; 5us; 35us; 15us; 38us; 19us; 42us; 20us; 40us; 21us; 47us; 22us; 50us; 26us; 44us; 30us; 53us; 33us; 34us; 0us; 16416us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 12us; 13us; 30us; 31us; 51us; 52us; 54us; 56us; 68us; 70us; 71us; 73us; 75us; 76us; 78us; 80us; 81us; 83us; 84us; 85us; 86us; 87us; 88us; 105us; 107us; 108us; 126us; 127us; 129us; 130us; 132us; 135us; 152us; 153us; 154us; 155us; 156us; 157us; 169us; 170us; 182us; 183us; 200us; 201us; 213us; 215us; 216us; 228us; 245us; 246us; 258us; 270us; 271us; 283us; 285us; 286us; 288us; 300us; 302us; 314us; 315us; 317us; 329us; 330us; 332us; 349us; 350us; 352us; 364us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 1us; 1us; 5us; 3us; 3us; 2us; 1us; 1us; 1us; 1us; 3us; 2us; 3us; 3us; 1us; 1us; 1us; 1us; 2us; 2us; 2us; 3us; 3us; 3us; 3us; 5us; 1us; 3us; 1us; 3us; 1us; 3us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 3us; 3us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 6us; 6us; 7us; 7us; 8us; 8us; 9us; 9us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 49152us; 65535us; 49152us; 65535us; 65535us; 65535us; 65535us; 16387us; 65535us; 65535us; 16388us; 65535us; 65535us; 16389us; 65535us; 16390us; 16391us; 16392us; 16393us; 16394us; 65535us; 65535us; 16395us; 65535us; 16396us; 65535us; 16397us; 65535us; 65535us; 65535us; 16398us; 16399us; 16400us; 16401us; 16402us; 65535us; 16403us; 65535us; 16404us; 65535us; 16405us; 65535us; 65535us; 16406us; 65535us; 65535us; 16407us; 65535us; 65535us; 16408us; 65535us; 65535us; 16409us; 65535us; 65535us; 65535us; 65535us; 16410us; 65535us; 65535us; 16412us; 65535us; 65535us; 16414us; 65535us; 65535us; 16416us; |]
let _fsyacc_reductions ()  =    [| 
# 305 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.fct)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startfct));
# 314 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startexp));
# 323 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.cmd)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startcmd));
# 332 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.fct)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Def(_2,_4) 
                   )
# 54 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : Fp.FpCore.cmd));
# 344 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Undef _2 
                   )
# 55 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : Fp.FpCore.cmd));
# 355 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Show _2 
                   )
# 56 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : Fp.FpCore.cmd));
# 366 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Exp _1 
                   )
# 57 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : Fp.FpCore.cmd));
# 377 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      T 
                   )
# 63 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : Fp.FpCore.expr));
# 387 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      F 
                   )
# 64 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : Fp.FpCore.expr));
# 397 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Int (bignum.FromInt _1) 
                   )
# 65 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : Fp.FpCore.expr));
# 408 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Var _1 
                   )
# 66 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : Fp.FpCore.expr));
# 419 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      _2 
                   )
# 67 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : Fp.FpCore.expr));
# 430 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 68 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Seq [] 
                   )
# 68 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : Fp.FpCore.expr));
# 440 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 69 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Seq _2 
                   )
# 69 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : Fp.FpCore.expr));
# 451 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.fct)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      App(_1, _3) 
                   )
# 70 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : Fp.FpCore.expr));
# 463 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 74 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Prim _1 
                   )
# 74 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'fatom));
# 474 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 75 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Sel _1 
                   )
# 75 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'fatom));
# 485 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 76 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      RSel _1 
                   )
# 76 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'fatom));
# 496 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 78 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      User _1 
                   )
# 78 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'fatom));
# 507 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'fatom)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 80 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      ApplyToAll _2 
                   )
# 80 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'fatom));
# 518 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'fatom)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 81 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Insert _2 
                   )
# 81 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'fatom));
# 529 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 82 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Constant _2 
                   )
# 82 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'fatom));
# 540 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'fctlist)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 83 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Construction _2 
                   )
# 83 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'fatom));
# 551 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'fatom)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 84 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Bu(_2,_3) 
                   )
# 84 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'fatom));
# 563 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'fatom)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'fatom)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 85 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      While(_2,_3) 
                   )
# 85 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'fatom));
# 575 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.fct)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 86 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      _2 
                   )
# 86 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'fatom));
# 586 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'comp)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'comp)) in
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.fct)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 90 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Condition(_1,_3,_5) 
                   )
# 90 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : Fp.FpCore.fct));
# 599 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'comp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 91 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      _1 
                   )
# 91 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : Fp.FpCore.fct));
# 610 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'fatom)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'comp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 95 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      Composition(_1,_3) 
                   )
# 95 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'comp));
# 622 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'fatom)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 96 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      _1 
                   )
# 96 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'comp));
# 633 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 99 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      _1 :: _3 
                   )
# 99 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'list));
# 645 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 100 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      [ _1 ] 
                   )
# 100 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'list));
# 656 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.fct)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'fctlist)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 104 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      _1 :: _3 
                   )
# 104 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'fctlist));
# 668 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Fp.FpCore.fct)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 105 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                      [ _1 ] 
                   )
# 105 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fsy"
                 : 'fctlist));
|]
# 680 "C:\Users\Василий\Desktop\Diploma\fp\FpInterpreter\fp_parser.fs"
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
    numTerminals = 37;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let fct lexer lexbuf : Fp.FpCore.fct =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
let exp lexer lexbuf : Fp.FpCore.expr =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 2))
let cmd lexer lexbuf : Fp.FpCore.cmd =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 4))
