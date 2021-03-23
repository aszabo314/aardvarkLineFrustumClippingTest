
open System
open Aardvark.Base
open Aardvark.Base.Rendering
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim   
open System.Runtime.InteropServices

#nowarn "9"

let clipLine (line : Line3d) (hull : Hull3d) =
    let planes = hull.PlaneArray
    let mutable p0 = line.P0
    let mutable p1 = line.P1

    let rec contains (pi : int) (i : int) (pt : V3d) =
        if i >= planes.Length then
            true
        elif pi = i then
            contains pi (i+1) pt
        else
            planes.[i].Height pt <= 0.0 && contains pi (i+1) pt

    let mutable valid = true
    let mutable pi = 0
    while pi < planes.Length do
        let p = planes.[pi]
        let h0 = p.Height p0
        let h1 = p.Height p1

        if h0 < 0.0 && h1 > 0.0 then
            // p0 inside
            let pt = //(p0 * (h1 + p.Distance) - p1 * (h0 + p.Distance)) / (h1 - h0)
                let ray = Ray3d(p0, (p1-p0).Normalized)
                ray.Intersect p
            if contains pi 0 pt then
                p1 <- pt
        elif h1 < 0.0 && h0 > 0.0 then
            // p1 inside
            let pt = //(p0 * (h1 + p.Distance) - p1 * (h0 + p.Distance)) / (h1 - h0)
                let ray = Ray3d(p0, (p1-p0).Normalized)
                ray.Intersect p
            if contains pi 0 pt then
                p0 <- pt
        elif h0 > 0.0 && h1 > 0.0 then
            // both outside
            valid <- false
            pi <- planes.Length

        pi <- pi + 1

    if valid then Line3d(p0, p1) |> Some
    else None

let test() =
    let line = Line3d(V3d.OOO, V3d.OOI * 8.0)
    let box = Box3d.FromCenterAndSize (V3d.OOI * 4.0, V3d.III)
    let hull = Hull3d.Create box
    let clipped = clipLine line hull
    printfn "%A" clipped //Some [[0, 0, 0], [0, 0, 8]]

[<EntryPoint>]
let main argv = 
    Aardvark.Init()

    use app = new OpenGlApplication()
    let win = app.CreateGameWindow()
    let rt = app.Runtime

    let initialView = CameraView.lookAt (V3d(6,6,6)) V3d.Zero V3d.OOI
    
    let debugEnabled = AVal.init 0
    let normalProj = win.Sizes |> AVal.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))
    let debugProj = win.Sizes |> AVal.map (fun s -> Frustum.perspective 90.0 0.1 200.0 (float s.X / float s.Y)) 

    let normalView = AVal.init initialView
    let debugView  = AVal.init initialView

    let viewTrafo = 
        debugEnabled |> AVal.bind (fun d -> 
            if d=0 then normalView :> aval<_>
            else debugView :> aval<_>
        ) |> AVal.bind (DefaultCameraController.control win.Mouse win.Keyboard win.Time)  
    
    let viewTrafoTrafo = viewTrafo |> AVal.map CameraView.viewTrafo

    let proj = debugEnabled |> AVal.bind (fun d -> if d=0 then normalProj else debugProj) |> AVal.map Frustum.projTrafo

    let line = AVal.init (Line3d(V3d.OOO, V3d.OOI * 8.0))

    let frustumViewTrafo =
        adaptive {
            let! d = debugEnabled
            if d = 0 then 
                return! viewTrafo
            else 
                return! normalView
        }
    let clippedLine =
        AVal.map3 (fun v (l : Line3d) p -> 
            let vp = CameraView.viewTrafo v * Frustum.projTrafo p
            let hull = ViewProjection.toHull3d vp
            clipLine l hull
        ) frustumViewTrafo line normalProj
        
    win.Keyboard.Down.Values.Add (fun k -> 
        match k with 
        | Keys.Space -> 
            let nv = if debugEnabled.Value = 0 then Log.line "debug"; 1 else Log.line "normal"; 0
            transact (fun _ -> 
                if debugEnabled.Value = 0 then 
                    normalView.Value <- viewTrafo.GetValue()
                else 
                    debugView.Value <- viewTrafo.GetValue()
                debugEnabled.Value <- nv
            )
        | _ -> ()
    ) 

    let frustumSg = 
        Sg.wireBox' C4b.DeepPink (Box3d(V3d.NNN, V3d.III))
        |> Sg.trafo (AVal.map2 (fun v p -> (CameraView.viewTrafo v * Frustum.projTrafo p).Inverse) frustumViewTrafo normalProj)
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.thickLine
            do! DefaultSurfaces.vertexColor
        }
        |> Sg.uniform "LineWidth" (AVal.constant 1.5)

    let originalLineSg =
        line |> AVal.map (fun l -> 
            Sg.lines (AVal.constant C4b.DarkOliveGreen) (AVal.constant [|l|])
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.thickLine
                do! DefaultSurfaces.vertexColor
            }
            |> Sg.uniform "LineWidth" (AVal.constant 1.0)
        ) |> Sg.dynamic

    let clippedLineSg =
        clippedLine |> AVal.map (fun l -> 
            match l with 
            | Some l ->
                let box0 =
                    Sg.wireBox' C4b.LightGoldenRodYellow (Box3d.FromCenterAndSize(l.P0,V3d.III*0.1))
                let box1 =
                    Sg.wireBox' C4b.LightGoldenRodYellow (Box3d.FromCenterAndSize(l.P1,V3d.III*0.1))
                let line = 
                    Sg.lines (AVal.constant C4b.LightGoldenRodYellow) (AVal.constant [|l|])
                Sg.ofList [box0;box1;line]
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.thickLine
                    do! DefaultSurfaces.vertexColor
                }
                |> Sg.uniform "LineWidth" (AVal.constant 3.5)
            | None -> Sg.empty
        ) |> Sg.dynamic
    
    let sg =
        Sg.ofList [
            frustumSg
            originalLineSg
            clippedLineSg
        ]
        |> Sg.viewTrafo viewTrafoTrafo
        |> Sg.projTrafo proj

    win.RenderTask <- rt.CompileRender(win.FramebufferSignature, sg)
    win.Run()

    0
