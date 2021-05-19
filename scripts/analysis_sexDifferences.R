e <- summarize(group_by_at(filter(allLengthsEnv, species =="EP"), vars(station, year, sex)), length = mean(length.unscaled))

t <- summarize(group_by_at(filter(allLengthsEnv, species =="TS"), vars(station, year, sex)), length = mean(length.unscaled))

n <- summarize(group_by_at(filter(allLengthsEnv, species =="ND"), vars(station, year, sex)), length = mean(length.unscaled))

e.diff <- mean(filter(e, sex == "F")$length) - mean(filter(e, sex == "M")$length)
t.diff <- mean(filter(t, sex == "F")$length) - mean(filter(t, sex == "M")$length)
n.diff <- mean(filter(n, sex == "F")$length) - mean(filter(n, sex == "M")$length)

t.test(length~sex, e)
t.test(length~sex, t)
t.test(length~sex, n)
