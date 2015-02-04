\name{GameTheory-package}
\alias{GameTheory-package}
\alias{GameTheory}
\docType{package}
\title{
Cooperative Game Theory
}
\description{
Implementation of a common set of punctual solutions  for Cooperative Game Theory.
}
\details{
\tabular{ll}{
Package: \tab GameTheory\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-02-04\cr
License: \tab GPL (>= 2)\cr
}

}
\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}
\references{
Aumann, R.J. and Maschler, M., (1985) "Game Theoretic Analysis of a bankruptcy from the Talmud."  \emph{Journal of Economic Theory} \bold{36}, pp.195--213.


O'Neill B. (1982). "A problem of rights arbitration from the Talmud." \emph{Mathematical Social Sciences}, \bold{2}(4), pp.345--371.

Shapley L, Shubik M (1954). "A Method for Evaluating the Distribution of Power in a Committee System." \emph{The American Political Science Review}, \bold{48}(3), 787--792.

Shapley L (1953). \emph{A value for n-person games. In Tucker A, Kuhn H (Eds.), Contributions to the theory of games II (pp. 307--317). Princeton University Press: Princeton NJ.}

Schmeidler D (1969). "The Nucleolus of a characteristic function game." \emph{SIAM Journal of Applied Mathematics}, \bold{17}, 1163--1170.
}


\examples{

## TRANSFERABLE UTILITY

# 3 PLAYER SHAPLEY VALUE

COALITIONS <- c(46125,17437.5,5812.5,69187.5,53812.5,30750,90000)
NAMES <- c("Investor 1","Investor 2","Investor 3")
LEMAIRESHAPLEY <- ShapleyValue(COALITIONS,NAMES)
LEMAIRESHAPLEY

# 3 PLAYER NUCLEOLUS OF A GAINS GAME 

LEMAIRE <- Nucleolus(3,c(
46125,   # v(1)
17437.5, # v(2)
69187.5, # v(12)
5812.5,  # v(3)
53812.5, # v(13)
30750,   # v(23)
90000    # v(123)
))

# 4 PLAYER SHAPLEY VALUE

COALITIONS <- c(26,27,55,57,53,81,83,82,84,110,108,110,110,110,110)
NAMES <- c("Airline 1","Airline 2","Airline 3","Airline 4")
ShapleyValue(COALITIONS,NAMES)

# 4 PLAYER NUCLEOLUS OF A COST GAME 

NucleolusCost(4,c(
26, # v(1)
27, # v(2)
53, # v(12)
55, # v(3)
81, # v(13)
82, # v(23)
108,# v(123)
57, # v(4)
83, # v(14)
84, # v(24)
110,# v(124)
110,# v(34)
110,# v(134)
110,# v(234)
110 # v(1234)
))


## SHAPLEY - SHUBIK POWER INDEX

# 2003 Elections
SEATS<-c(46,42,23,15,9)
PARTIES<-c("CiU","PSC","ERC","PP","ICV")
ShapleyShubik(68,SEATS,PARTIES)

# 2006 Elections
SEATS<-c(48,37,21,14,12,3)
PARTIES<-c("CiU","PSC","ERC","PP","ICV","C's")
ShapleyShubik(68,SEATS,PARTIES)

# 2012 Elections
SEATS<-c(50,20,21,19,13,9,3)
PARTIES<-c("CiU","PSC","ERC","PP","ICV","C's","CUP")
ShapleyShubik(68,SEATS,PARTIES)


## CONFLICTING CLAIMS PROBLEM

## replication of Gallastegui et al. (2003), Table 7.

CLAIMS <- c(158,299,927,2196,4348,6256,13952)
COUNTRIES <- c("Germany","Netherlands","Belgium","Ireland","UK","Spain","France")
INARRA <- AllRules(13500,CLAIMS,COUNTRIES)
INARRA

PlotAll(INARRA,5) ## Display allocations for UK
LorenzRules(INARRA) ## Inequality graph


}
