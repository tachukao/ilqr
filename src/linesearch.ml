let backtrack ?(alpha = 1.) ?(alpha_min = 1e-8) ?(tau = 0.5) f0 f =
  let rec backtrack alpha =
    let fv, prms = f alpha in
    if f0 < fv
    then if alpha < alpha_min then None else backtrack (tau *. alpha)
    else Some prms
  in
  backtrack (tau *. alpha)
