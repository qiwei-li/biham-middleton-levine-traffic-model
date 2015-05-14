\name{runBMLGrid}
\alias{runBMLGrid}
\title{runBMLGrid}

\description{
Simulates BML model
}

\usage{
runBMLGrid(grid, numSteps, ifPlot = FALSE, ifVelocity = FALSE, method)
}

\arguments{
\item{grid}{A BML grid}
\item{numSteps}{An integer representing number of moves of the simulation}
\item{ifPlot}{An logical value representing if to plot the animation of simulation with UNIX based graphic device x11}
\item{ifVelocity}{An logical value representing if to plot the number of moved cars (velocity) over time}
\item{method}{One of the following methods: "fast", "slow", and "simultaneous"}
}

\value{
A BML object
}

\examples{
grid = createBMLGrid(r = 67, c = 127,  density = 0.5)
g = runBMLGrid(grid, numSteps = 50, method = "fast")
g = runBMLGrid(grid, numSteps = 50, method = "slow")
g = runBMLGrid(grid, numSteps = 50, method = "simultaneous")
}

\keyword{BML}