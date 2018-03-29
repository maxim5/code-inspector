namespace GameOfLife.UserInterface
  module Interface =

    type ISimulatorControl =
      inherit System.IObserver<GameOfLife.Model.Interface.IMatrixSimulator>
      abstract AsControl:         System.Windows.Forms.Control
        with get
      abstract OnStart:           (unit -> unit)
        with get, set
      abstract OnStop:            (unit -> unit)
        with get, set
      abstract OnReset:           (unit -> unit)
        with get, set
      abstract OnNext:            (unit -> unit)
        with get, set
      abstract OnSpeedChanged:    (float -> unit)
        with get, set

    type IMatrixView =
      inherit System.IObserver<GameOfLife.Model.Interface.IMatrix>
      abstract AsControl:   System.Windows.Forms.Control
        with get

    type IUserInterface =
      abstract AsForm:            System.Windows.Forms.Form
        with get
      abstract SimulatorControl:  ISimulatorControl
        with get
