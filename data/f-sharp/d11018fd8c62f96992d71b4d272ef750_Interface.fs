namespace Microsoft.Research.GeneralPDP.DKAL.Engine

open Microsoft.Research.DkalEngine
open Microsoft.Research.DkalEngine.Ast
open Microsoft.Research.DkalEngine.Util
open ParsingCtxFactory

open System.Collections.Generic

module Interface =

  /// Does the proper bindings with the engine
  type Comm(pctx: ParsingCtx, eng: Engine, cb: Message -> unit) = 
    let say (s:string) = System.Console.WriteLine ("{0}: {1}", pctx.Me.Name, s)

    member this.ReceiveMessage msg = eng.Listen (this, msg)

    interface ICommunicator with
      member this.RequestFinished () = ()
      member this.ExceptionHandler e = say ("exception: " + e.ToString())
      member this.Warning e = say ("warning: " + e)
      member this.Knows inf = ()
      member this.QueryResults (inf, res) = ()
      member this.SendMessage msg = cb msg
      member this.PrincipalById id = failwith "unimplemented"
      member this.PrincipalId (p:Ast.Principal) = failwith "unimplemented"

  /// Actual DKAL Engine Interface.
  /// cb is a callback that's invoked every time the engine has something to say
  type DkalEngineInterface(ppalName:string, sql: string, cb: Message -> unit) =
    let pctx, initialAssertions = xacmlAwareParsingCtx ppalName
    let eng = Engine.Config (Options.Create())
    let comm = Comm(pctx, eng, cb)

    let instantiateAssertion (a: Assertion) = 
      let ppal = pctx.LookupOrAddPrincipal ppalName
      match a with
      | SendTo(c) -> SendTo({ai= {origin= fakePos; principal= ppal};
                             target= c.target;
                             message= c.message;
                             proviso= c.proviso;
                             certified= c.certified;
                             trigger= c.trigger})
      | Knows(k) -> Knows({ai= {origin= fakePos; principal= ppal};
                           infon= k.infon})
      | a -> a

    do
      eng.options.PrivateSql <- sql
      eng.Reset ()
      // Load initial commrules from prelude 
      List.iter eng.AddAssertion (List.map instantiateAssertion initialAssertions)
      eng.AddDefaultFilter ()


    member this.Pctx = pctx

    member this.Me = pctx.Me

    member this.PrincipalByName (n: string) = pctx.LookupOrAddPrincipal n

    member this.ClearRules () = 
      eng.communications <- []
      eng.infostrate <- []

    member this.AddRule a = eng.AddAssertion (instantiateAssertion a)

    member this.ReceiveMessage m =
      comm.ReceiveMessage(m)

    member this.Talk () = eng.Talk comm
    member this.CheckPoint () = eng.CheckPoint ()
    member this.Finish () = eng.Finish ()

    // engine options
    member this.SetDispatcher (d: string) = 
      eng.options.Dispatcher <- d
    member this.SetLearning (l: string) = 
      eng.options.Learning <- l

    member this.CommRules () =
      List.map (fun c -> SendTo c) 
        (List.filter (fun (c: Communication) -> match c.trigger with
                                                | App(f, [t]) when f.name = "asInfon" -> 
                                                    match t with
                                                    | App(g, [Const(Int(0)); Const(Int(1))]) when g.name = "==" -> false
                                                    | _ -> true
                                                | _ -> true
           ) eng.communications)

    member this.Knows () =
      List.map (fun c -> Knows c) eng.infostrate
