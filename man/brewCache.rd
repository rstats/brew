\name{brewCache}
\alias{brewCache}
\alias{setBufLen}
\alias{brewCacheOn}
\alias{brewCacheOff}
\title{Caching Brew Templates}
\description{
	These functions provide a cache system for brew templates.
}
\usage{
brewCache(envir=NULL)

brewCacheOn()
brewCacheOff()

setBufLen(len=0)
}
\arguments{
  \item{envir}{the \code{\link{environment}} to store text and R expressions for
	each brewed template.}
  \item{len}{length of internal buffers for parsing the templates.}
}
\details{

	brew can cache parsed templates for potential speedup but only when brew
	calls are passed filenames, not connections, and when tplParser
	is NULL.

	brew caching is implemented by storing file names, modification
	times, and the associated text and R expressions in an internal
	environment. Calling \code{brewCache()} with an appropriate
	environment sets the internal cache. Calling without arguments
	returns the internal cache. The cache is exposed this way mainly
	for testing, debugging, performance improvement, etc. and this
	may be off-limits in future releases.

	Simple enough, \code{brewCacheOn()} turns on
	caching of brew templates and is equivalent to calling
	\code{brewCache(envir=new.env(hash=TRUE,parent=globalenv()))}.
	\code{brewCache()} without arguments returns the internal
	environment. Calling \code{brewCacheOff()} turns off caching by
	setting the internal environment to \code{NULL}.

	One thing to note: filenames act as keys in the internal cache
	environment, and brew does nothing to expand them to their full
	paths. Thus, '/home/user/brew.html' and '~usr/brew.html' will
	act as separate keys, although on-disk they may actually point
	to the same file.

	\code{setBufLen()} initializes internal parsing vectors to length \code{len}. Default is 0.
}
\value{
	\code{brewCache()} without arguments returns the internal cache. All others invisibly return \code{NULL}.
}
\author{ Jeffrey Horner <jeff.horner@vanderbilt.edu> }
\seealso{ \code{\link{Sweave}} for the original report generator. }
\examples{

## Turn on caching
brewCacheOn()

## Brew a template
brew(system.file("featurefull.brew",package="brew"),envir=new.env())

## Get the internal cache
cache <- brewCache()

## Inspect
as.list(cache)

## Inspect first file cached in list
as.list(cache)[[1]]

## Inspect environment that contains text and parsed code
as.list(as.list(cache)[[1]]$env)

## Turn off brew caching
brewCacheOff()
rm(cache)
}
\keyword{utilities}
