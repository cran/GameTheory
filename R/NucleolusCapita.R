NucleolusCapita <- function(x, type = "Gains") {

	## Numero de Agentes
	
	n <- x$Ag
	
	Amat<-x$Lmat
	
	## Ajuste
	
	q<-apply(Amat,1,sum)
	Amat<-Amat*q

	## Valores de las coaliciones    
	
	z <- as.matrix(x$Lex)
	V <- as.vector(z)

	## Deficion del programa lineal
	
	N <- dim(Amat)[1]
    Amat[N,]<-c(rep(1,n))
    
	## Juego de Ganacias
	
	if (type == "Gains") {
		Amat <- cbind(Amat, c(rep(-1, N - 1), 0))
		lprec <- make.lp(0, n + 1)
		set.objfn(lprec, c(rep(0, n), -1))
		add.constraint(lprec, c(rep(0, n), 1), ">=", 0)
		for (i in 2:N - 1) {
			add.constraint(lprec, Amat[i, ], ">=", V[i])
		}
		add.constraint(lprec, Amat[N, ], "=", V[N])
		Lim <- NULL
		for (i in seq(1, n)) {
			Lim <- c(Lim, V[2^(i - 1)])
		}
		Lim <- c(Lim, 0)

		# set.bounds(lprec, lower = c(rep(0, n), 0), upper = c(rep(Inf, n), Inf))
		set.bounds(lprec, lower = c(V[1:n], 0), upper = c(rep(Inf, n), Inf))
		name.lp(lprec, "Nucleolus of a gains game ")
		lp.control(lprec, sense = "min", verbose = "normal")
		#, simplextype = c("dual", "primal"))
		print(lprec)
		FLP <- lprec
		nucleolus <- NULL


		## Actualizacion del programa lineal
		

		for (i in seq(1, n)) {
			solve(lprec)
			select.solution(lprec)
			X <- get.variables(lprec)[i] ### Valor de Xi 
			E <- -get.variables(lprec)[n + 1] ### Valor de Ei
			S <- c(V[i], X, E) ### Valor Individual
			nucleolus <- rbind(nucleolus, S)


			### Matriz de Coeficientes
			
			set.mat(lprec, i + 1, n + 1, 0) ### Eliminamos Ei
			set.mat(lprec, 2^n, i, 0) ### Eliminamos Xi de v(N)
			set.mat(lprec, 2^n - i, n + 1, 0) ### Eliminamos Xi de v(N-1)
			set.constr.type(lprec, "=", i + 1) ### Introducimos el valor obtenido de Xi

			### Vector de terminos independientes
			
			RHS <- as.matrix(get.rhs(lprec))
			RHS[i + 1] <- X
			RHS[2^n - i] <- RHS[2^n - i] + E
			RHS[2^n] <- RHS[2^n] - X
			set.rhs(lprec, RHS)
		}
	}



	if (type == "Cost") {
		Amat <- cbind(Amat, rep(1, N), rep(-1, N))
		lprec <- make.lp(0, n + 2)
		set.objfn(lprec, c(rep(0, n), 1, -1))
		print(lprec)
		add.constraint(lprec, c(rep(0, n), 0, 1), ">=", 0)
		for (i in 2:N - 1) {
			add.constraint(lprec, Amat[i, ], "<=", V[i])
		}
		add.constraint(lprec, c(rep(1, n), 0, 0), "=", V[N])
		Lim <- NULL
		for (i in seq(1, n)) {
			Lim <- c(Lim, V[2^(i - 1)])
		}
		Lim <- c(Lim, 0)


		#set.bounds(lprec, upper = c(V[1:n], Inf), lower = c(rep(-Inf, n), -Inf))
		name.lp(lprec, "Nucleolus of a cost game ")
		lp.control(lprec, sense = "max", verbose = "normal")
		print(lprec)
		FLP <- lprec
		nucleolus <- NULL

		## Actualizacion del programa lineal
		

		for (i in seq(1, n)) {
			solve(lprec)
			select.solution(lprec)
			X <- get.variables(lprec)[i] ### Valor de Xi 
			E <- -get.variables(lprec)[n + 1] ### Valor de Ei
			S <- c(V[i], X, E) ### Valor Individual
			nucleolus <- rbind(nucleolus, S)


			### Matriz de Coeficientes
			
			set.mat(lprec, i + 1, n + 1, 0) ### Eliminamos Ei
			set.mat(lprec, 2^n, i, 0) ### Eliminamos Xi de v(N)
			set.mat(lprec, 2^n - i, n + 1, 0) ### Eliminamos Xi de v(N-1)
			set.constr.type(lprec, "=", i + 1) ### Introducimos el valor obtenido de Xi

			### Vector de terminos independientes
			
			RHS <- as.matrix(get.rhs(lprec))
			RHS[i + 1] <- X
			RHS[2^n - i] <- RHS[2^n - i] - E
			RHS[2^n] <- RHS[2^n] - X
			set.rhs(lprec, RHS)


		}
	}

	nucleolus <- as.matrix(nucleolus)
	colnames(nucleolus) <- c("v(S)", "x(S)", "Ei")
	output <- list(nucleolus = nucleolus, mode = type)
	class(output) <- "Nucleolus"
	return(output)
}