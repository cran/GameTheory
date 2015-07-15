\name{NucleolusCapita}
\alias{NucleolusCapita}

\title{
Per Capita Nucleolus
}
\description{
This function computes the per capita nucleolus solution of a gains game with a maximum of 4 agents. 
}
\usage{
NucleolusCapita(x, type = "Gains")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class Game
}
  \item{type}{
Specify if the game refers to Gains or Cost
}
}
\details{
  
The per capita nucleolus represents a measure of dissatisfaction per capita of such a coalition. It is also  an individually rational distribution of the worth of the grand coalition in which the maximum per capita dissatisfaction is minimized. Formally, is defined like the nucleolus but taking into the account the per capita excess.
}

\value{
The command returns a table with the following elements:
  \item{v(S)}{Individual value of player \emph{i}}
  \item{x(S)}{Nucleolus solution of the player \emph{i}}
  \item{Ei }{Excess of the player \emph{i}}  
}



\references{

Lemaire J (1991). "Cooperative game theory and its insurance applications." Astin Bulletin, \bold{21}(01), 17--40.	
	
Schmeidler D (1969). "The Nucleolus of a characteristic function game." \emph{SIAM Journal of Applied Mathematics}, \bold{17}, pp.1163--1170.
}
\author{
Sebastian Cano-Berlanga <cano.berlanga@gmail.com>
}

\examples{
	
## DATA FROM LEMAIRE (1991)
	
# Begin defining the game

COALITIONS <- c(46125,17437.5,5812.5,69187.5,53812.5,30750,90000)
LEMAIRE<-DefineGame(3,COALITIONS)

# End defining the game

LEMAIRENUCLEOLUS<-NucleolusCapita(LEMAIRE)
summary(LEMAIRENUCLEOLUS) 

}


