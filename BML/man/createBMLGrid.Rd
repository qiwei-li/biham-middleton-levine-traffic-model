\name{createBMLGrid}
\alias{createBMLGrid}
\title{createBMLGrid}
\description{Create a BML simulation grid}
\usage{
createBMLGrid(r, c, ncars = c(red = 0, blue = 0), density = 0)
}
\arguments{
\item{r}{An integer representing row size of the grid}
\item{c}{An integer representing column size of the grid}
\item{ncars}{An integer vector of length 2 representing number of blue cars and red cars in the grid (optional)}
\item{density}{An numeric value representing the proportion of cars in the grid. Blue cars and red cars will split evenly.}
}
\value{
returns a BML grid
}
\examples{
grid = createBMLGrid(r = 67, c = 127,  density = 0.5) 
grid = createBMLGrid(r = 67, c = 127,  ncars = c(red = 1000, blue = 1500))
}
\keyword{BML}