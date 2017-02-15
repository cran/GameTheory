AlphaMin <-
function (E, C, Names = NULL)
{
N <- length(C)
Nc <- NULL
 for (i in 1:N) {
 R <- min(C[1],E/N)
Nc <- rbind(Nc, R)
}
print(Nc)
NE <- E - sum(Nc)
CP <- C - Nc
print(NE)
print(CP)
NC <- Proportional(NE, CP)$Results
print(NC)
 print(CP)
R <- NC + Nc
Output <- list(Results = R, Claims = C, Method = "Alpha-min Rule",
                        Short = "\u03b1-min", E = E, Names = Names)
class(Output) <- "ClaimsRule"
 return(Output)
}