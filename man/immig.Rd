\encoding{UTF-8}
\name{immig}
\alias{immig}
\docType{data}
\title{Data (paired comparisons with undecided and forced \code{NA}s): Negative Attitudes towards Immigrants}
\description{%
  A survey of 98 students was carried out to examine student's (negative) attitudes towards immigrants.
  Four statements had to be compared with regard to higher acceptance.
  The four statements were
  \itemize{
    \item Foreigners increase crime rates (\code{crimRate})
    \item Foreigners take apprenticeship training position away (\code{position})
    \item Foreigners are a strain for the social welfare system (\code{socBurd})
    \item Foreigners threaten our culture (\code{culture})
  }
  The first 6 variables in the dataset indicate the preferences of the students.
  For a given comparison the responses are coded by \eqn{1} if the first item was preferred, \eqn{-1} if the second university was preferred and \eqn{0} if no decision was made.
  The variable \code{ENG} characterises the knowledge of English and the variable \code{SEX} characterises the gender.
}
\usage{immig}
\format{%
  A data frame with 98 observations on the following 9 variables.
  \describe{
    \item{\code{V12}}{\code{crimRate} vs. \code{position}}
    \item{\code{V13}}{\code{crimRate} vs. \code{socBurd}}
    \item{\code{V23}}{\code{position} vs. \code{socBurd}}
    \item{\code{V14}}{\code{crimRate} vs. \code{culture}}
    \item{\code{V24}}{\code{position} vs. \code{culture}}
    \item{\code{V34}}{\code{socBurd} vs. \code{culture}}
    \item{\code{SEX}}{Gender: (1) male, (2) female}
    \item{\code{AGE}}{Age (continuous)}
    \item{\code{NAT}}{Nationality (Factor). Cannot directly be used in \pkg{prefmod}}
  }
}
\references{%
  Weber, D., & Hatzinger, R. (2011).
  A novel approach for modelling paired comparisons data with non-ignorable missing values on students' attitudes towards foreigners.
  \emph{Data Analysis Bulletin, 12}, 11--22.
}
\examples{summary(immig)}
\keyword{datasets}
