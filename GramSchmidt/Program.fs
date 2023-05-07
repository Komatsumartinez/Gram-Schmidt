module GramSchmidt

open System

type Vector = double[]

let dotProduct (v1: Vector) (v2: Vector) =
    Array.map2 (*) v1 v2 |> Array.sum

let scaleVector (scalar: double) (v: Vector) =
    Array.map ((*) scalar) v

let subtractVectors (v1: Vector) (v2: Vector) =
    Array.map2 (-) v1 v2

let normalize (v: Vector) =
    let norm = sqrt (dotProduct v v)
    if norm = 0.0 then v else scaleVector (1.0 / norm) v

let rec gramSchmidtRec (inputVectors: Vector list) (outputVectors: Vector list) =
    match inputVectors with
    | [] -> outputVectors
    | x :: xs ->
        let rec projectOntoAccumulator (v: Vector) (orthogonalComponents: Vector list) =
            match orthogonalComponents with
            | [] -> v
            | y :: ys ->
                let projection = scaleVector (dotProduct v y) y
                let newV = subtractVectors v projection
                projectOntoAccumulator newV ys

        let orthogonalComponent = projectOntoAccumulator x outputVectors
        let normalizedComponent = normalize orthogonalComponent
        if normalizedComponent |> Array.exists (fun n -> not (Double.IsNaN(n) || Double.IsInfinity(n))) then
            gramSchmidtRec xs (normalizedComponent :: outputVectors)
        else
            gramSchmidtRec xs outputVectors

let gramSchmidt (inputVectors: Vector list) =
    List.rev (gramSchmidtRec inputVectors [])
    
let v1 = [| 1.0; 2.0; 3.0 |]
let v2 = [| 4.0; 5.0; 6.0 |]
let v3 = [| 7.0; 8.0; 9.0 |]
let inputVectors = [v1; v2; v3]

let outputVectors = gramSchmidt inputVectors
printfn "Output vectors:"
outputVectors |> List.iter (fun v -> printfn "%A" v)