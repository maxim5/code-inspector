module Mtype.Data

open System

[<Measure>] type rad
[<Measure>] type deg
[<Measure>] type cm
[<Measure>] type ms
[<Measure>] type s
[<Measure>] type tick

let Pi = 1.0<rad> * Math.PI

open Newtonsoft.Json
open Newtonsoft.Json.Linq

[<AutoOpen>]
module JsonUtils =
    /// Dynamic JObject attribute access
    let inline (?) (o : #JObject) k =
        match o.[k] with
        | null -> Unchecked.defaultof<_>
        | x -> x.ToObject<_>()

    let inline (?*) (o : #JObject) k =
        match o.[k] with
        | null -> None
        | x -> Some <| x.ToObject<_>()        

    let inline fromJson (o : #JToken) : 'a = o.ToObject<'a>()
    let (!?>) = fromJson

    /// Case-insensitive string comparison
    let (^=) (x : string) (y : string) =
        if x <> null then x.Equals(y, StringComparison.InvariantCultureIgnoreCase)
        else y = null

// {"msgType": "join", "data": { "name": "Schumacher", "key": "UEWJBVNHDS" }}
//type JoinMessage = { name : string; key : string; color : string }

type SwitchDirection = | Left | Right

type ClientGameStateMessage<'a> = { Data : 'a; GameTick : int<tick> }

type BotId = { name : string; key : string }

// {"msgType": "createRace", "data": {"botId": {"name": "schumacher","key": "UEWJBVNHDS"},"trackName": "hockenheim","password": "schumi4ever","carCount": 3}}
type CreateRaceMessage = { botId : BotId; trackName : string; password : string; carCount : int }

type JoinRaceMessage = {
    botId : BotId
    trackName : string
    password : string
    carCount : int
}

type ClientMessage =
    | Ping
    | Join of BotId
    | Throttle of ClientGameStateMessage<decimal>
    | SwitchLane of ClientGameStateMessage<SwitchDirection>
    | CreateRace of CreateRaceMessage
    | JoinRace of JoinRaceMessage
with
    member message.ToJson() =
        let msg : (string * obj) [] =
            match message with
            | Ping ->
                [| "msgType", ("ping" :> _) |]
            | Join x ->
                [| "msgType", ("join" :> _); "data", (x :> _) |]
            | Throttle { Data = x; GameTick = t } ->
                [| 
                    "msgType", ("throttle" :> _)
                    "data", x :> _
                    "gameTick", (t :> _)
                |]
            | SwitchLane { Data = x; GameTick = t } ->
                [| 
                    "msgType", ("switchLane" :> _)
                    "data", (match x with | Left -> "Left" | Right -> "Right") :> _
                    "gameTick", (t :> _)
                |]
            | CreateRace x -> [| "msgType", ("createRace" :> _); "data", (x :> _) |]
            | JoinRace x -> [| "msgType", ("joinRace" :> _); "data", (x :> _) |]

        JsonConvert.SerializeObject(dict msg, Formatting.None)
        
type TrackTurnPiece = { radius : decimal<cm>; angle : decimal<deg> }

type TrackPieceGeometry =
    | Straight of decimal<cm>
    | Turn of TrackTurnPiece

// [ { "length": 100.0 }, { "length": 100.0, "switch": true }, { "radius": 200, "angle": 22.5 } ]
type TrackPiece = { CanSwitch : bool; Geometry : TrackPieceGeometry }

type TrackPieceConverter() =
    inherit JsonConverter()
    
    override x.CanConvert(t) =
        t = typedefof<TrackPiece>
 
    override x.WriteJson(writer, value, serializer) = raise <| NotImplementedException()
 
    override x.ReadJson(reader, t, existingValue, serializer) =
        let jo = JObject.Load(reader)

        let canSwitch = jo ?* "switch"
        let length : decimal<cm> option = jo ?* "length"
        let radius : decimal<cm> option = jo ?* "radius"
        let angle : decimal<deg> option = jo ?* "angle"

        let trackPiece = {
            CanSwitch = canSwitch.IsSome
            Geometry =
                match length, radius, angle with
                | Some l, None, None -> Straight l
                | None, Some r, Some a -> Turn { radius = r; angle = a }
                | _ -> failwith "wtf"
        }

        trackPiece :> _

// [{"distanceFromCenter": -20,"index": 0},{"distanceFromCenter": 0,"index": 1},{"distanceFromCenter": 20,"index": 2}]
type TrackLane = { distanceFromCenter : decimal<cm>; index : int }

// {"x": -340.0,"y": -96.0}
type Position = { x : decimal<cm>; y : decimal<cm> }

// {"position": ...,"angle": 90.0}
type TrackStartingPoint = { position : Position; angle : decimal<deg> }

// {"id": "indianapolis", "name": "Indianapolis", "pieces": [ ... ], "lanes": [ ... ], "startingPoint": ... }
type TrackData = {
    id : string
    name : string
    [<JsonProperty(ItemConverterType=typedefof<TrackPieceConverter>)>] pieces : TrackPiece[]
    lanes : TrackLane[]
    startingPoint : TrackStartingPoint
}

// {"length": 40.0,"width": 20.0,"guideFlagPosition": 10.0}
type CarDimensions = { length : decimal<cm>; width : decimal<cm>; guideFlagPosition : decimal<cm> }

// {"name": "Schumacher","color": "red"}
type CarId = { name : string; color : string }

// {"id": ...,"dimensions": ...},
type CarData =  { id : CarId; dimensions : CarDimensions }

// {"laps": 3,"maxLapTimeMs": 30000,"quickRace": true}
type RaceSession = { laps : int; maxLapTimeMs : int<ms>; quickRace : bool }

// {"track": ...,"cars": [ ... ],"raceSession": ...}
type RaceData = { track : TrackData; cars : CarData[]; raceSession : RaceSession }

// { "race": ... }
type GameInitMessage = { race : RaceData }

// {"startLaneIndex": 0,"endLaneIndex": 0}
type CarPiecePositionLane = { startLaneIndex : int; endLaneIndex : int }

// {"pieceIndex": 0,"inPieceDistance": 0.0,"lane": ...,"lap": 0}
type CarPiecePosition = { pieceIndex : int; inPieceDistance : decimal<cm>; lane : CarPiecePositionLane; lap : int }

// {"id": ...,"angle": 0.0,"piecePosition": ...}
type CarPosition = { id : CarId; angle : decimal<deg>; piecePosition : CarPiecePosition}

type CarPositionsMessage = { Positions : CarPosition[] }

// {"laps": 3,"ticks": 9999,"millis": 45245}
type RaceResult = { laps : int; ticks : int<tick>; millis : int<ms> }

// {"lap": 2,"ticks": 3333,"millis": 20000}
type LapResult = { lap : int; ticks : int<tick>; millis : int<ms> }

// {"car": ...,"result": ...}
type GameResult<'a> = { Car : CarId; Result : 'a option }

// {"results": [...],"bestLaps": [...] }
type GameEndMessage = {
    RaceResult : GameResult<RaceResult> seq
    BestLaps : GameResult<LapResult> seq
}

type CrashMessage = { CrashedCar : CarId }

type SpawnMessage = { SpawnedCar : CarId }

type GameStateMessage<'a> = { gameId : string; gameTick : int<tick>; data : 'a }

// {"overall": 1,"fastestLap": 1}
type RaceRanking = { overall : int; fastestLap : int }

// {"car": ...,"lapTime": ...,"raceTime": ...,"ranking": ... }
type LapFinishedMessage = {
    car : CarId
    lapTime : LapResult
    raceTime : RaceResult
    ranking : RaceRanking
}

type DisqualifiedMessage = { car : CarId; reason : string }

type FinishedMessage = { FinishedCar : CarId }

type ServerMessage =
    | Unknown of string
    | JoinAck of BotId
    | YourCar of CarId
    | GameInit of GameInitMessage
    | GameStart of GameStateMessage<unit>
    | CarPositions of GameStateMessage<CarPositionsMessage>
    | GameEnd of GameEndMessage
    | TournamentEnd
    | Crash of GameStateMessage<CrashMessage>
    | Spawn of GameStateMessage<SpawnMessage>
    | LapFinished of GameStateMessage<LapFinishedMessage>
    | Disqualified of GameStateMessage<DisqualifiedMessage>
    | Finished of GameStateMessage<FinishedMessage>
with
    static member FromJson(jsonString) =
        let jo = JObject.Parse(jsonString)
        match jo?msgType with
        // {"msgType": "yourCar", "data": ... }
        | "yourCar" -> YourCar jo?data
        // {"msgType": "gameInit", "data": ... }
        | "gameInit" ->
            let s = JsonSerializer.Create(new JsonSerializerSettings())
            s.Converters.Add(new TrackPieceConverter())
            GameInit <| jo.["data"].ToObject<_>(s)
        // {"msgType": "gameStart", "data": null}
        | "gameStart" -> GameStart { data = (); gameId = jo?gameId; gameTick = jo?gameTick }
        // {"msgType": "carPositions", "data": [ ... ], "gameId": "OIUHGERJWEOI", "gameTick": 0}
        | "carPositions" ->
            CarPositions {
                data = { Positions = jo?data }
                gameId = jo?gameId
                gameTick = jo?gameTick 
            }
        // {"msgType": "gameEnd", "data": ...}
        | "gameEnd" ->
            let parseResult j = {
                Car = j?car
                Result =
                    if (j?result?ticks <> null)
                    then Some (j?result)
                    else None
            }
            GameEnd {
                RaceResult = jo?data?results |> Seq.map parseResult
                BestLaps = jo?data?bestLaps |> Seq.map parseResult
            }
        // {"msgType": "tournamentEnd", "data": null}
        | "tournamentEnd" -> TournamentEnd
        // {"msgType": "crash", "data": {"name": "Rosberg","color": "blue"}, "gameId": "OIUHGERJWEOI", "gameTick": 3}
        | "crash" ->
            Crash {
                data = { CrashedCar = jo?data }
                gameId = jo?gameId; gameTick = jo?gameTick
            }
        // {"msgType": "spawn", "data": {"name": "Rosberg","color": "blue"}, "gameId": "OIUHGERJWEOI", "gameTick": 150}
        | "spawn" ->
            Spawn {
                data = { SpawnedCar = jo?data }
                gameId = jo?gameId; gameTick = jo?gameTick
            }
        // {"msgType": "lapFinished", "data": ..., "gameId": "OIUHGERJWEOI", "gameTick": 300}
        | "lapFinished" -> LapFinished <| fromJson jo
        // {"msgType": "dnf", "data": {"car": {"name": "Rosberg","color": "blue"},"reason": "disconnected"}, "gameId": "OIUHGERJWEOI", "gameTick": 650}
        | "dnf" -> Disqualified jo?data
        // {"msgType": "finish", "data": {"name": "Schumacher","color": "red"}, "gameId": "OIUHGERJWEOI", "gameTick": 2345}
        | "finish" ->
            Finished {
                data = { FinishedCar = jo?data }
                gameId = jo?gameId; gameTick = jo?gameTick
            }
        | "join" -> JoinAck jo?data
        | _ -> Unknown jsonString
