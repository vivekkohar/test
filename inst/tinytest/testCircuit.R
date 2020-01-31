
data("demoCircuit")
racipe <- RacipeSE()
sracipeCircuit(racipe) <- demoCircuit
tmp1 <- sracipeCircuit(racipe)
storage.mode(demoCircuit$Type) <- "character"
sracipeCircuit(racipe) <- demoCircuit
tmp2 <- sracipeCircuit(racipe)
tinytest::expect_equal(tmp1,tmp2)
