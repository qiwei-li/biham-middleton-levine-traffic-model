\name{runBMLGrid}
\alias{runBMLGrid}
\title{runBMLGrid}

\description{
Simulates BML model
}

\usage{
runBMLGrid(grid, numSteps, ifPlot = FALSE, method)
}

\arguments{
\item{grid}{A BML grid}
\item{numSteps}{An integer representing number of moves of the simulation}
\item{ifPlot}{An logical value representing if the animation of simulation should be plotted}
\item{method}{One of the following methods: "fast", "slow", and "simultaneous"}
}

\value{
return the final grid and plot number of moved cars (velocity) over time
}

\examples{
grid = createBMLGrid(r = 67, c = 127,  density = 0.5)
g = runBMLGrid(grid, numSteps = 1000, ifPlot=TRUE, method = "fast")
g = runBMLGrid(grid, numSteps = 1000, ifPlot=FALSE, method = "slow")
g = runBMLGrid(grid, numSteps = 1000, ifPlot=TRUE, method = "simultaneous")
}

\keyword{BML}