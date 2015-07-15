\name{DefineGame}
\alias{DefineGame}

\title{
Transferable Utility Game
}
\description{
Definition of a Transferable-Utility Game 
}
\usage{
DefineGame(n, V)
}
\arguments{
  \item{n}{
	Number of agents
}
  \item{V}{
Coalition values in lexicographic order
}
}

\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}


\examples{

Lemaire<-DefineGame(3,c(46125,17437.5,5812.5,69187.5,53812.5,30750,90000))
summary(Lemaire)

# Characteristic form of the game 
# Number of agents: 3 
# Coaliton Value(s) 

#        v(i)
# 1   46125.0
# 2   17437.5
# 3    5812.5
# 12  69187.5
# 13  53812.5
# 23  30750.0
# 123 90000.0


}

