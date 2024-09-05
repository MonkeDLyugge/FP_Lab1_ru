//v14
let a = 0.1
let b = 0.6
let n = 10.
let eps = 0.0001

let rec loop i n func x =
  if i <= n then
    let x = func x
    loop (i + 1.) n func x
  else
    x
    
let m a b = a * b

let pow x n = loop 1. n (m x) x

let f x = (x * 2. - 3.) / ((x - 1.) ** 2.)

let rec dumbTaylor x i eps sum = 
  let cur = (i + 3.) * x ** i
  if (abs(cur) < eps) then
    (-sum, i)
  else
    let sum = sum + cur
    dumbTaylor x (i + 1.) eps sum


let rec smartTaylor x last i eps sum =
  let cur = last * x + x ** ((i - 1.) / 2. + 1.)
  if (abs(cur) < eps) then
    (-sum, (i - 1.) / 2. + 1.)
  else
    let sum = sum + cur
    smartTaylor x cur (i + 2.) eps sum

let determine i = 
  let x = a + i / n * (b - a)
  let dT, dTi = dumbTaylor x 0. eps 0.
  let sT, sTi = smartTaylor x 3. 1. eps 3.
  printfn "|%5.2f|  %10.6f|  %10.6f|   %10.0f|  %10.6f|   %10.0f|" x (f x) dT dTi sT sTi
  i + 1.


let main =
    printfn "--------------------------------------------------------------------------"
    printfn "|  x  |    f(x)    |   Naive    |    Iters    |   Smart    |    Iters    |"
    printfn "--------------------------------------------------------------------------"
    loop 0. n determine 0.
    printfn "--------------------------------------------------------------------------"

main
