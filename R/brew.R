#
#  brew - text templating for R (www.r-project.org)
#
#  Copyright (C) 2007 Jeffrey Horner
#
#  brew is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

BRTEXT <- 1
BRCODE <- 2
BRCOMMENT <- 3
BRCATCODE <- 4
BRTEMPLATE <- 5

`brew` <-
function(file=stdin(),output=stdout(),text=NULL,envir=parent.frame(),run=TRUE,parseCode=TRUE,tplParser=NULL,...){

	# Error check input
	closeIcon <- FALSE
	if (is.character(text) && nchar(text[1]) > 0){
		closeIcon <- TRUE
		icon <- textConnection(text[1])
	} else if (inherits(file,'connection') && summary(file)$"can read" == 'yes') {
		icon <- file
	} else if (is.character(file) && file.exists(file)){
		closeIcon <- TRUE
		icon <- file(file,open="rt")
	} else {
		stop('No valid input')
		return(invisible(NULL))
	}

	# Error check output
	if (inherits(output,'connection')){
		if (summary(output)$"can write" != 'yes')
			stop('output connection is not writeable')
	} else if ( !is.character(output) )
		stop('No valid output')

	# Error check env
	if (!is.environment(envir)){
		warning('envir is not a valid environment')
		return(invisible(NULL))
	}

	newline <- FALSE
	state <- BRTEXT
	buf <- code <- tpl <- character()
	buflen <- codelen <- 0
	line <- ''
	
	while(TRUE){
		if (!nchar(line)){
			line <- readLines(icon,1)
		   	if (length(line) != 1) break
			line <- paste(line,"\n",sep='')
			newline <- TRUE
		} else newline <- FALSE
		if (state == BRTEXT){

			# One-liner
			if (newline && ('%' == substr(line,1,1))){
				# build cats
				for (i in buf){
					code[codelen+1] <- paste('cat(',deparse(i),')',sep='')
					codelen <- codelen + 1
				}
				spl <- strsplit(line,"^%",perl=TRUE)[[1]]
				code[codelen+1] <- substr(line,2,nchar(line))
				codelen <- codelen + 1
				line <- ''
				buf <- character(); buflen <- 0
				next
			}

			if (regexpr("<%=",line,perl=TRUE) > 0){
				state <- BRCATCODE
				delim <- "<%="
			} else if (regexpr("<%#",line,perl=TRUE) > 0){
				state <- BRCOMMENT
				delim <- "<%#"
			} else if (regexpr('<%%',line,perl=TRUE) > 0){
				# Template generator, strip a % unless tplParser != NULL
				# so just take off the whole <%% stuff.
				spl <- strsplit(line,'<%%',fixed=TRUE)[[1]]
				if (!is.null(tplParser)){
					buf[buflen+1] <- spl[1]
					buflen <- buflen + 1
				} else {
					buf[buflen+1] <- paste(spl[1],'<%',sep='')
					buflen <- buflen + 1
				}
				line <- paste(spl[-1],collapse='<%%')
				state <- BRTEMPLATE
				next
			} else if (regexpr("<%",line,perl=TRUE) > 0){
				state <- BRCODE
				delim <- "<%"
			}
			if (state != BRTEXT){ # something changed
				spl <- strsplit(line,delim,fixed=TRUE)[[1]]
				if (nchar(spl[1])) {
					buf[buflen+1] <- spl[1]
					buflen <- buflen + 1
				}
				line <- paste(spl[-1],collapse=delim)

				if (buflen) {
					# build cats
					for (i in buf){
						code[codelen+1] <- paste('cat(',deparse(i),')',sep='')
						codelen <- codelen + 1
					}
				}
				buf <- character(); buflen <- 0
			} else {
				buf[buflen+1] <- line
				buflen <- buflen + 1
				line <- ''
			}
		} else {
			if (regexpr("%%>",line,perl=TRUE) > 0){
				spl <- strsplit(line,"%%>",fixed=TRUE)[[1]]
				if (!is.null(tplParser)){
					tpl[length(tpl)+1] <- spl[1]
					# call template parser
					tplBuf <- strsplit(tplParser(tpl,...),'\n',fixed=TRUE)[[1]]
					tplBufLen <- length(tplBuf)
					# add back newline on all but last tplBuf element
					if (tplBufLen > 1){
						for( i in 1:(tplBufLen-1)){
							buf[buflen+1] <- paste(tplBuf[i],'\n',sep='')
							buflen <- buflen + 1
						}
					}
					buf[buflen+1] <- tplBuf[tplBufLen]
					buflen <- buflen + 1
					tpl <- character()
				} else {
					buf[buflen+1] <- paste(spl[1],'%>',sep='')
					buflen <- buflen + 1
				}
				line <- paste(spl[-1],collapse='%%>')
				state <- BRTEXT
				next
			}
			if (regexpr("%>",line,perl=TRUE) > 0){
				spl <- strsplit(line,"%>",fixed=TRUE)[[1]]
				line <- paste(spl[-1],collapse='%>')

				n <- nchar(spl[1])
				# test  for '-' immediately preceding %> will strip trailing newline from line
				if (n > 0) {
					if (substr(spl[1],n,n) == '-') {
						line <- substr(line,1,nchar(line)-1)
						spl[1] <- substr(spl[1],1,n-1)
					}
					buf[buflen+1] <- spl[1]
					buflen <- buflen + 1
				}
				if (state == BRCODE){
					code[codelen+1] <- paste(buf,collapse='')
					codelen <- codelen + 1
				} else if (state == BRCATCODE){
					code[codelen+1] <- paste('cat(',paste(buf,collapse=''),')',sep='')
					codelen <- codelen + 1
				}
				buf <- character(); buflen <- 0
				state <- BRTEXT
			} else {
				if (state == BRTEMPLATE && !is.null(tplParser))
					tpl[length(tpl)+1] <- line
				else {
					buf[buflen+1] <- line
					buflen <- buflen + 1
				}
				line <- ''
			}
		}
	}
	if (state == BRTEXT){
		if (buflen) {
			# build cats
			for (i in buf){
				code[codelen+1] <- paste('cat(',deparse(i),')',sep='')
				codelen <- codelen + 1
			}
		}
	} else {
		warning("Unclosed tag")
	}

	if (closeIcon) close(icon)

	if (run){
		sunk <- FALSE
		if (!exists('.brew.output',where=envir) || !is.null(match.call()$output)) {
			sunk <- TRUE
			assign('.brew.output',output,pos=envir)
			sink(output)
		}

		ret <- try(eval(parse(text=code),envir=envir))

		# sink() will warn if trying to end the real stdout diversion
		if (sunk && unclass(output) != 1) sink()
		if (exists('.brew.output',where=envir)) rm('.brew.output',pos=envir)
		invisible(ret)
	} else if (parseCode){
		invisible(parse(text=code))
	} else {
		invisible(code)
	}
}
