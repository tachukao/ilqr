val backtrack
  :  ?alpha:float
  -> ?alpha_min:float
  -> ?tau:float
  -> 'a
  -> (float -> 'a * 'b)
  -> 'b option
