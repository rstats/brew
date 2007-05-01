\name{brew}
\alias{brew}
\title{Report Brewing For Text and R Output}
\description{
  \code{brew} provides a templating system for text reporting. The syntax is similar to PHP,
  Java Server Pages, Ruby's erb module, and Python's psp module.
}
\usage{
brew(file=stdin(),output=stdout(),text=NULL,envir=parent.frame(),run=TRUE)
}
\arguments{
  \item{file}{A connection, or a character string naming the file
    to read from. stdin() is the default.}
  \item{output}{A connection, or a character string naming the file
    to print to. stdout() is the default.}
  \item{text}{ A character string treated as if it contained lines of a file
    to read from. Only one of \code{file} or \code{text} is used as input. 
    Default is \code{NULL}.}
  \item{envir}{the \code{\link{environment}} in which the input is to
    be evaluated.  Default is the caller's environment, useful for 
    nested \code{brew} calls.}
  \item{run}{Logical to determine if \code{brew} should evaluate the input (\code{run=TRUE}) or 
    just parse it (\code{run=FALSE}). Useful for debugging.}
}
\details{
	The following template contains syntax to exercise all \code{brew} functionality:

\preformatted{
---------------
You won't see this R output, but it will run. <\% foo <- 'bar' \%>
Now foo is <\%=foo\%> and today is <\%=format(Sys.time(),'\%B \%d, \%Y')\%>.
<\%# Comment -- ignored -- useful in testing. 
    Also notice the dash-percent-gt.
    It chops off the trailing newline. 
    You can add it to any percent-gt. -\%>
\% cat("and R one-liner\n")
How about generating a template from a template?
<\%\% foo <- 'fee fi fo fum' \%\%>
foo is still <\%=foo\%>.
---------------
}

	The output is:

\preformatted{
--------------
You won't see this R output, but it will run.
Now foo is bar and today is April 20, 2007.
and R one-liner
How about generating a template from a template?
<\% foo <- 'fee fi fo fum' \%>
foo is still bar.
--------------
}

}
\value{
	When \code{run=TRUE}, the result of the last evaluated expression after brewing the input.
    When \code{run=FALSE}, the parsed expressions after brewing the input.	
}
\author{ Jeffrey Horner <jeff.horner@vanderbilt.edu> }
\seealso{ \code{\link{Sweave}} for the original report generator. }
\examples{

## Don't sully up environment, so use envir=new.env(). Nested brew calls will still work.
brew(system.file("example1.brew",package="brew"),envir=new.env())

## Various ways to print R output 
brew(system.file("catprint.brew",package="brew"),envir=new.env())
rm(iris)

## The example from the Details section
brew(system.file("featurefull.brew",package="brew"),envir=new.env())
}
\keyword{utilities}
