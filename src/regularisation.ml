type t = float * float

let increase (delta, mu) =
  let delta = max 2. (2. *. delta) in
  let mu = max 1E-6 (mu *. delta) in
  delta, mu


let decrease (delta, mu) =
  let delta = min (1. /. 2.) (delta /. 2.) in
  let mu =
    let mu = mu *. delta in
    if mu > 1E-6 then mu else 0.
  in
  delta, mu
