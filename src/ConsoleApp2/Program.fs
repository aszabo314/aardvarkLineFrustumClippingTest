
open System
open Aardvark.Base
open Aardvark.Base.Rendering
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim   
open System.Runtime.InteropServices

#nowarn "9"

let intersectPlaneLine (p : Plane3d) (l : Line3d) =
    let ray = Ray3d(l.P0, (l.P1-l.P0).Normalized)
    let t0 = 0.0
    let t1 = ray.GetTOfProjectedPoint(l.P1)
    let hit = ray.Intersect p
    let tHit = ray.GetTOfProjectedPoint(hit)
    if tHit >= t0 && tHit <= t1 then Some (tHit, ray.GetPointOnRay tHit)
    else None

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

    let clippedLine =
        AVal.bind (fun p -> 
            AVal.map2 (fun v (l : Line3d) -> 
                let vp = CameraView.viewTrafo v * Frustum.projTrafo p
                let hull = ViewProjection.toHull3d vp

                let mutable ts = []
                for (p : Plane3d) in hull.PlaneArray do
                    match intersectPlaneLine p l with 
                    | Some (t,hit) ->
                        if hull.Contains(hit) then 
                            ts <- (t,hit)::ts
                    | None -> ()

                match ts with 
                | [] -> if hull.Contains l.P0 || hull.Contains l.P1 then Some l else Log.warn "empty"; None 
                | [t,hit] -> 
                    if hull.Contains l.P0 then Some (Line3d(l.P0,hit))
                    elif hull.Contains l.P1 then Some (Line3d(hit,l.P1))
                    else Log.error "very bad"; None
                | [(t0,hit0);(t1,hit1)] -> 
                    if t0 <= t1 then Some (Line3d(hit0,hit1))
                    else Some (Line3d(hit1,hit0))
                | _ -> Log.error "wtf %d" ts.Length; None

            ) normalView line
        ) normalProj
        

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

    let frustumViewTrafo =
        adaptive {
            let! d = debugEnabled
            if d = 0 then 
                return! viewTrafo
            else 
                return! normalView
        }

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
            Sg.lines (AVal.constant C4b.LightGoldenRodYellow) (AVal.constant (l |> Option.toArray))
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.thickLine
                do! DefaultSurfaces.vertexColor
            }
            |> Sg.uniform "LineWidth" (AVal.constant 3.5)
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
