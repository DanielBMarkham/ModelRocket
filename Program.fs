open Utils
open System
open System.Windows.Forms
open System.Drawing
open System.Timers
open System.ComponentModel


let runSimulation:SimulationEngine = 
    fun currentState->(
                                let newState =
                                    { currentState with
                                        TickCount=System.DateTime.Now.Ticks
                                        TotalRunTimeSoFar=DateTime.Now - currentState.SimulationStart
                                    }
                                newState
                        )


[<EntryPoint>]
let main argv = 
    setupUI runSimulation
    System.Windows.Forms.Application.Run(simulationForm)
    0
