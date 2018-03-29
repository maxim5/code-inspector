namespace GameOfLife.Model
  module Interface =

    module Cell =

      type State =
        | Dead  = 0x00uy
        | Alive = 0xFFuy

      let flip state =
        if state = State.Alive then State.Dead
        else State.Alive


      type Position =
        | Coordinates of int * int

        static member X (pos: Position) =
          match pos with
            | Position.Coordinates(posX, _) -> posX

        static member Y (pos: Position) =
          match pos with
            | Position.Coordinates(_, posY) -> posY

        static member (+)(pos: Position, dir: Direction) =
          match pos, dir with
            | Position.Coordinates(posX, posY), Direction.Coordinates(dirX, dirY) -> Position.Coordinates(posX + dirX, posY + dirY)
            
      and Direction =
        | Coordinates of int * int

        static member X (dir: Direction) =
          match dir with
            | Direction.Coordinates(dirX, _) -> dirX

        static member Y (dir: Direction) =
          match dir with
            | Direction.Coordinates(_, dirY) -> dirY

        static member (+)(dir1: Direction, dir2: Direction) =
          match dir1, dir2 with
            | Coordinates(dir1X, dir1Y), Coordinates(dir2X, dir2Y) -> Direction.Coordinates(dir1X + dir2X, dir1Y + dir2Y)

        static member (*)(dir: Direction, factor: int) =
          match dir with
            | Direction.Coordinates(dirX, dirY) -> Direction.Coordinates(dirX * factor, dirY * factor)


    open Cell

    let absolute x y = Position.Coordinates(x, y)    
    let relative x y = Direction.Coordinates(x, y)

    let North       = Direction.Coordinates(-1, 0)
    let East        = Direction.Coordinates(0, 1)
    let South       = Direction.Coordinates(1, 0)
    let West        = Direction.Coordinates(0, -1)
    let NorthEast = North + East
    let NorthWest = North + West
    let SouthEast = South + East
    let SouthWest = South + West

    let Directions = [| North; NorthEast; East; SouthEast; South; SouthWest; West; NorthWest |]

    [<Measure>] type gen // generation
    [<Measure>] type sec // second

    type  ICell =
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
      abstract Go:                  int -> Cell.Direction -> option<ICell>
      abstract Position:            Cell.Position
        with get
      abstract State:               Cell.State
        with get, set
      abstract Neighbours:          Map<Cell.Direction, ICell>
        with get
      abstract Next:                unit-> option<ICell>

    type Rule = ICell -> Cell.State

    type IMatrix =
      inherit System.ICloneable
      abstract NumRows:     int
        with get
      abstract NumColumns:     int
        with get
      abstract IsValid:     Cell.Position -> bool
      abstract Get:         Cell.Position -> option<Cell.State>
      abstract Set:         Cell.Position -> Cell.State -> unit
      abstract Cell:        Cell.Position -> option<ICell>
      abstract Fill :       Cell.State -> unit
      abstract Reset:       int * int * Cell.State -> unit
      abstract Reset:       unit -> unit
      abstract Resize:      int * int -> unit
      abstract Transform:   Rule -> IMatrix


    type ISimulator<'Collection, 'Item, 'Value> =
      inherit System.IObservable<'Collection>
      //inherit System.IObservable<ISimulator<'Collection, 'Item, 'Value>>
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