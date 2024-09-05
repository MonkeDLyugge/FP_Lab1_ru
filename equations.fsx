open System

let eps = 0.0001

let func1 x = tan(x / 2.) - (1. - tan(x / 2.)) + x
let func2 x = 0.4 + atan(System.Math.Sqrt(x)) - x
let func3 x = 3. * sin(System.Math.Sqrt(x)) + 0.35 * x - 3.8

let func1' x = (1. / (2. * (sin(x / 2.) ** 2.))) + (1. / (2. * (cos(x / 2.) ** 2.))) + 1.
let func2' x = 1. / (System.Math.Sqrt(x) * (2. * x + 2.)) - 1.
let func3' x = 3. * cos(System.Math.Sqrt(x)) / (2. * System.Math.Sqrt(x)) + (7. / 20.)


let phi1 x = x - func1 x / func1' x
let phi2 x = x - func2 x / func2' x
let phi3 x = x - func3 x / func3' x


let a1 = 1.
let b1 = 2.

let a2 = 1.
let b2 = 2.

let a3 = 2.
let b3 = 3.

let absDiff a b =
    abs(a - b)

let rec dichotomy func l r =
  let mid = (l + r) / 2.
  if absDiff l r < eps then  
    mid
  else if func l * func mid < 0. then 
    dichotomy func l mid
  else 
    dichotomy func mid r

let iterations phi x' = 
  let rec loop x = 
    if absDiff (phi x) x < eps then 
      phi x
    else 
      loop (phi x)
  loop x'

    
let newthon func func' x' = 
    iterations (fun x -> x - ((func x) / (func' x))) x'

let main =
    printfn "a  b \t Dichotomy\t Iterations \t Newthon"
    printfn "%f %f  %10.5f  %10.5f  %10.5f" a1 b1 (dichotomy func1 a1 b1) (iterations phi1 ((a1 + b1) / 2.)) (newthon func1 func1' ((a1 + b1) / 2.))
    printfn "%f %f  %10.5f  %10.5f  %10.5f" a2 b2 (dichotomy func2 a2 b2) (iterations phi2 ((a2 + b2) / 2.)) (newthon func2 func2' ((a2 + b2) / 2.))
    printfn "%f %f  %10.5f  %10.5f  %10.5f" a3 b3 (dichotomy func3 a3 b3) (iterations phi3 ((a3 + b3) / 2.)) (newthon func3 func3' ((a3 + b3) / 2.))

main
