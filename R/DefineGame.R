DefineGame <- function(n, V) {
	vec <- c(0, 1)
	lst <- lapply(numeric(n), function(x) vec)
	Amat <- as.matrix(expand.grid(lst))[-1, ]
	Amat <- Amat[order(rowSums(Amat)), ]
	Lmat <- Amat

	#####
	
	Orden <- NULL
	for (i in 1:n) {
		tmp <- t(combn(n, i))
		tmp2 <- as.list(as.data.frame((tmp)))
		tmp3 <- do.call(paste, tmp2)
		Orden <- c(Orden, tmp3)
	}


	OrdenL <- Orden
	Orden <- gsub(" ", "", Orden)

	#####
	C <- ncol(Amat)
	F <- nrow(Amat)


	for (i in seq(1, F)) {
		for (j in seq(1, C)) {
			if (Amat[i, j] == 1) 
				Amat[i, j] <- j
		}
	}

	for (i in seq(1, F)) {
		for (j in seq(1, C)) {
			if (Amat[i, j] == 0) 
				Amat[i, j] <- "z"
		}
	}

	matrix.text <- function(mtext, sep = " ", collapse = NULL) {
		if (is.null(collapse)) 
			apply(mtext, 1, paste, collapse = sep)
		else paste(apply(mtext, 1, paste, collapse = sep), collapse = collapse)
	}

	Amat <- matrix.text(Amat)
	Amat <- gsub("z", "", Amat)
	Amat <- gsub(" ", "", Amat)
	Lab <- Amat
	rownames(Lmat) <- Lab
	Lmat <- Lmat[Orden, ]
	Cmat <- Lmat
	####
	for (i in 2:n) {
		for (j in 1:(2^n - 1)) {
			if (Cmat[j, i] != 0) {
				Cmat[j, i] <- i
			}
		}

	}

	####
	
	Output <- list(Lex = V, Ag = n, Lmat = Lmat, Cmat = Cmat)
	class(Output) <- "Game"
	return(Output)

}

