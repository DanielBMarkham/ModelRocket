module Utils
open System
open System.Windows.Forms
open System.Drawing
open System.Timers
// Measure annotations
[<Measure>] type kg
[<Measure>] type m
[<Measure>] type km
[<Measure>] type sec
[<Measure>] type mps = m/sec
[<Measure>] type mpss = m/sec^2
// Types
type Position = { XPos:float<km>; YPos:float<km>}
type Velocity = {XSpeed:float<m/sec>;YSpeed:float<m/sec>}
type MassivePlanetaryObjectType = { Mass:float<kg>;Radius:float<km>;Position:Position}
type SpaceShipType = {Height:float<m>; Width:float<m>; Mass:float<kg>; Velocity:Velocity}
type SimulationStateType = {
    SimulationStart:DateTime    
    TickCount:int64
    WindowScale:double
    TimeScale:double
    LastTickTime:DateTime
    TotalRunTimeSoFar:TimeSpan
    SpaceShip:SpaceShipType
    }
// Program spec. Taking out the deployment-specific scaffolding, 
// the entire thing is three platform-independent functions
// Remember: the specification to your program is in your types and tests. Nowhere else
type SimulationEngine = SimulationStateType->SimulationStateType
type GetSimulationState = unit->SimulationStateType
type PutSimulationState = SimulationStateType->unit
// Initial Values
let topButtonBarHeight=25
let statusBarHeight=15
let earth = {Mass=5.972E24<kg>; Radius=43.0<km>; Position={YPos=(-43.0<km>); XPos=(-43.0<km>)}}
type DisplayPositionType = |CenterBottom
let spaceship= {
        Height=100.0<m>
        Width=10.0<m>
        Mass=100.0<kg>
        Velocity={XSpeed=0.0<mps>; YSpeed=0.0<mps>} }
let desiredRocketshipWidthInModelTerms = 0.004<km>
let computedDisplayScale = 2.0*earth.Radius/desiredRocketshipWidthInModelTerms
let desiredRocketshipWidthInPixels = 7
let desiredRocketshipHeightInPixels = (int)((2.0/spaceship.Width) * spaceship.Height)
let initialModelState = 
                        {
                        SimulationStart=DateTime.Now
                        TickCount=0L
                        WindowScale=(double)computedDisplayScale
                        TimeScale=100.0
                        LastTickTime=DateTime.Now
                        TotalRunTimeSoFar=TimeSpan.Zero
                        SpaceShip=spaceship
                        }
// UI setup. Outer onion for our toy
let simulationForm =
    let temp=new Form()
    temp.Show()
    temp.WindowState<-FormWindowState.Maximized
    temp.Text<-"Rocket Explorer"
    temp.Tag<-initialModelState
    temp
let addTopButton left caption buttonWidth =
    let temp = new Button()
    temp.SetBounds(left, 5, buttonWidth,topButtonBarHeight)
    temp.Text<-caption
    temp.Name<-caption
    temp
let addTopButtons (btns:string list) =
    let buttonWidth = simulationForm.Width/(btns.Length)
    btns |> List.iteri(fun i x->
        let left = buttonWidth * i
        let newBtn=addTopButton  left x buttonWidth
        simulationForm.Controls.Add(newBtn))
type StringArgReturningVoidFunction = delegate of string->unit 
let rec setStatusLine(text:string):unit =
    let sl=simulationForm.Controls.Item("statusLine")
    if sl.InvokeRequired then
        let d = new StringArgReturningVoidFunction(setStatusLine)
        if simulationForm.Disposing=false && simulationForm.IsDisposed=false
            then 
            try
                simulationForm.Invoke(d, text) |> ignore
            with |_ ->()
            else ()
    else
        sl.Text<-(text)
let getSimState:GetSimulationState = fun()->(simulationForm.Tag:?>SimulationStateType)
let setSimState:PutSimulationState = 
    fun(newState)->(
                       let statusText =
                            newState.TotalRunTimeSoFar.ToString(@"dd\:hh\:mm\:ss\:fffff")
                            + "    Time Scale: " + newState.TimeScale.ToString()
                            + "    Window Scale: " + newState.WindowScale.ToString()
                       setStatusLine(statusText)
                       simulationForm.Invalidate()
                       simulationForm.Tag<-newState
                    )
let setupUI (simEngine:SimulationEngine) =
    let statusLine = new Label()
    statusLine.Text<-"Simulation Time 00:00"
    statusLine.Name<-"statusLine"
    let simulationTimer = new Timers.Timer(100.0)
    simulationTimer.Elapsed.Add(fun args->getSimState() |> simEngine |> setSimState)
    statusLine.SetBounds(0,(topButtonBarHeight + 14),simulationForm.Width,statusBarHeight)
    simulationForm.Controls.Add(statusLine)
    ["Start"; "Stop"; "Reset"] |> addTopButtons
    setSimState initialModelState
    // finally, handlers
    simulationForm.Controls.Item("Start").Click.Add(fun e->
        {getSimState() with SimulationStart=DateTime.Now} |> setSimState
        simulationTimer.Enabled<-true)
    simulationForm.Controls.Item("Stop").Click.Add(fun e->simulationTimer.Enabled<-false)
    simulationForm.Controls.Item("Reset").Click.Add(fun e->simulationTimer.Enabled<-false)
    simulationForm.MouseWheel.Add(fun e->
        let scaleMove:double= 1.0 + double(e.Delta)/1000.0
        if (Control.ModifierKeys.HasFlag Keys.Control) then
            {getSimState() with TimeScale=getSimState().TimeScale*scaleMove} |> setSimState
            else ()
        if (Control.ModifierKeys.HasFlag Keys.Shift) then
            {getSimState() with WindowScale=getSimState().WindowScale*scaleMove} |> setSimState
            else ()
        )
    simulationForm.Paint.Add(fun e->
        // we want the spaceship centered. For now, zoom scale is 1
        let modelState=getSimState()
        let spaceshipBrush = System.Drawing.Brushes.DarkMagenta
        let spaceshipLeft=simulationForm.Width/2 - (int)modelState.SpaceShip.Width/2
        let spaceshipTop=simulationForm.Height/2 - (int)modelState.SpaceShip.Height
        let spaceshipDisplayRectangle = 
            Rectangle(spaceshipLeft,spaceshipTop, desiredRocketshipWidthInPixels, desiredRocketshipHeightInPixels )
        let earthBrush = Brushes.DarkBlue
        let newEarthScale=(float)earth.Radius * computedDisplayScale/2.0
        let earthLeft = (int)((float)spaceshipLeft-newEarthScale*newEarthScale)
        let earthTop =  (int)((float)spaceshipTop-newEarthScale*newEarthScale)
        let earthDisplayRectangle = 
            Rectangle(earthLeft,earthTop,(int)(newEarthScale*2.0), (int)(newEarthScale*2.0))
        // Math needs to go here to determine intersection of earth and display. Dang you, Math!
        e.Graphics.FillRectangle(spaceshipBrush, spaceshipDisplayRectangle)
        if     (earthDisplayRectangle.Left<0  || earthDisplayRectangle.Right>simulationForm.Width)
            && (earthDisplayRectangle.Top<0   ||  earthDisplayRectangle.Top>simulationForm.Height)
            && (earthDisplayRectangle.Right<0 ||earthDisplayRectangle.Right>simulationForm.Width)
            && (earthDisplayRectangle.Bottom<0||earthDisplayRectangle.Bottom>simulationForm.Height)
            then
                let bottomScreenRectangle =
                    Rectangle(0,spaceshipDisplayRectangle.Bottom,simulationForm.Width,simulationForm.Height/2)
                e.Graphics.FillRectangle(earthBrush, bottomScreenRectangle)
            else 
                e.Graphics.FillEllipse(earthBrush, earthDisplayRectangle)
        )