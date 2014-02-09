fun tournament.d1.d2.n X = Y
where
  dim t <- ilog.n

  var NWOfQuad = Y @ [d1 <- #.d1 * 2,     d2 <- #.d2 * 2 + 1]
  var NEOfQuad = Y @ [d1 <- #.d1 * 2 + 1, d2 <- #.d2 * 2 + 1]
  var SEOfQuad = Y @ [d1 <- #.d1 * 2 + 1, d2 <- #.d2 * 2]
  var SWOfQuad = Y @ [d1 <- #.d1 * 2,     d2 <- #.d2 * 2]

  var Y = fby.t X (NWOfQuad + NEOfQuad + SEOfQuad + SWOfQuad)
end

dim x
dim y

var G = fib.10

tournament.x.y.16 G

