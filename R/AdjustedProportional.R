AdjustedProportional <-
function(E,C,Names=NULL){
	
	N <- length(C)
	Nc <- NULL
	for (i in 1:N) {
		R <- max(0, E - sum(C) + C[i])
		Nc <- rbind(Nc, R)
	}

	NE <- E - sum(Nc)
	CP <- C - Nc

	
	
	Nc2 <- NULL

	for (i in 1:N) {
		R <- min(CP[i], NE)
		Nc2 <- rbind(Nc2, R)
	}
	
	NC <- Proportional(NE, Nc2)$Results

	### End Curiel et al. (1987) 
	
	R <- NC + Nc
	Output <- list(Results = R, Claims = C, Method = "Adjusted Proportional Rule", Short = "AP", E = E, Names = Names)
	class(Output) <- "ClaimsRule"
	return(Output)

}


