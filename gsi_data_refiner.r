load("./gsi.rda")
ls()
write.csv(gsi, file="./gsi.csv", row.names=FALSE)
print("done")