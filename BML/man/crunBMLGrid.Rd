\name{crunBMLGrid}
\alias{crunBMLGrid}
\title{crunBMLGrid}

\description{
Simulates BML model interfacing C code
}

\usage{
crunBMLGrid(grid, numSteps)
}

\arguments{
\item{grid}{A BML grid}
\item{numSteps}{An integer representing number of moves of the simulation}
}

\value{
return the final grid
}

\examples{
grid = createBMLGrid(r = 67, c = 127,  density = 0.5)
g = crunBMLGrid(grid, numSteps = 100)
}

\keyword{BML}