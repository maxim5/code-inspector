namespace GameOfLife.Model
  module Interface =

    module Cell =

      type State =
        | Dead  = 0x00uy
        | Alive = 0xFFuy

      let flip state =
        if state = State.Alive then State.Dead
        else State.Alive

      type Coordinate(x: int, y: int) =
        let mutable _x = x
        let mutable _y = y

        member this.X
          with get() =
            _x
          and set(x) =
            _x <- x

        member this.Y
          with get() =
            _y
          and set(y) =
            _y <- y

        static member (+) (a: Coordinate, b: Coordinate) =
          new Coordinate(a.X + b.X, a.Y + b.Y)

        static member (-) (a: Coordinate, b: Coordinate) =
          new Coordinate(a.X - b.X, a.Y - b.Y)

        static member (*) (a: Coordinate, b: int) =
          new Coordinate(a.X * b, a.Y * b)

        static member (/) (a: Coordinate, b: int) =
          new Coordinate(a.X / b, a.Y / b)

        interface System.IComparable with
          member this.CompareTo coords =
            match coords with
              | :? Coordinate as b ->
                match this.X - b.X, this.Y - b.Y with
                  | 0, dY -> dY
                  | dX, 0 -> dX
                  | _ -> 0
              | _ -> invalidArg "coords" (sprintf "Unable to compare %s with %s" (this.GetType().ToString()) (coords.GetType().ToString()))
          


    let coord x y = new Cell.Coordinate(x, y)

    let North       = coord 0 -1
    let East        = coord 1 0
    let South       = coord 0 1
    let West        = coord -1 0
    let NorthEast = North + East
    let NorthWest = North + West
    let SouthEast = South + East
    let SouthWest = South + West

    let Directions = [| North; NorthEast; East; SouthEast; South; SouthWest; West; NorthWest |]

    [<Measure>] type gen // generation
    [<Measure>] type sec // second

    type ICell =
      abstract Live:                unit -> unit
      abstract Die:                 unit -> unit
      abstract Flip:                unit -> unit
      abstract IsAlive:             bool
        with get
      abstract IsDead:              bool
        with get
      abstract NumAliveNeighbours:  int
        with get
      abstract NumDeadNeighbours:  int
        with get
      abstract Go:                  int -> Cell.Coordinate -> option<ICell>
      abstract Position:            Cell.Coordinate
        with get
      abstract State:               Cell.State
        with get, set
      abstract Neighbours:          Map<Cell.Coordinate, ICell>
        with get
      abstract Next:                unit -> option<ICell>

    type Rule = ICell -> Cell.State

    type IMatrix =
      inherit System.ICloneable
      abstract Width:     int
        with get
      abstract Height:     int
        with get
      abstract IsValid:     Cell.Coordinate -> bool
      abstract Get:         Cell.Coordinate -> option<Cell.State>
      abstract Set:         Cell.Coordinate -> Cell.State -> unit
      abstract Cell:        Cell.Coordinate -> option<ICell>
      abstract Fill :       Cell.State -> unit
      abstract Reset:       int * int * Cell.State -> unit
      abstract Reset:       unit -> unit
      abstract Resize:      int * int -> unit
      abstract Transform:   Rule -> IMatrix

    type ISimulator<'Collection, 'Item, 'Value> =
      abstract Data:            System.IObservable<'Collection>
      abstract Control:         System.IObservable<ISimulator<'Collection, 'Item, 'Value>>
      abstract Start:           unit -> unit
      abstract Stop:            unit -> unit
      abstract Reset:           unit -> unit
      abstract Reset:           'Collection -> unit
      abstract Advance:         int<gen> -> unit
      abstract InitialState:    'Collection
        with get
      abstract CurrentState:    'Collection
        with get
      abstract Rule:            ('Item -> 'Value)
        with get, set
      abstract Generation:      int<gen>
        with get, set
      abstract Speed:           float<gen/sec>
        with get, set
      abstract Running:         bool
        with get

    type IMatrixSimulator = ISimulator<IMatrix, ICell, Cell.State>

    // some syntactic sugar
    let Alive = Cell.State.Alive
    let Dead = Cell.State.Dead


    let go distance direction (cell: option<ICell>) =
      if cell = None then None
      else cell.Value.Go distance direction