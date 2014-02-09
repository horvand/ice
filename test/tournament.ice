Y1
where
  dim t <- 4

  var Y1 = 
    if #.t <= 0 then
      X1
    else
      ( Y1 @ [x <- #.x * 2,     y <- #.y * 2]
      + Y1 @ [x <- #.x * 2 + 1, y <- #.y * 2]
      + Y1 @ [x <- #.x * 2,     y <- #.y * 2 + 1]
      + Y1 @ [x <- #.x * 2 + 1, y <- #.y * 2 + 1]
      ) @ [t <- #.t - 1]
    fi

  var X1 =
    if #.x >= 1 and #.x <= 1024 and
         #.y >= 1 and #.y <= 1024 then
      (fib.10)
    else
      1
    fi

  dim x <- 0
  dim y <- 0
end