\encoding{UTF-8}
\name{issp2000}
\alias{issp2000}
\docType{data}
\title{Data (Likert items): ISSP 2000 Survey on Environmental Issues}
\description{%
In 2000 the International Social Survey Programme (ISSP) has addressed the topic of attitudes to environmental protection and preferred government measures for environmental protection.
This dataset focusses on six items (with a 5-point rating scale (Likert type)) where respondents from Austria and Great Britain were asked about their perception of environmental dangers.
}
\usage{issp2000}
\format{%
A data frame with 1595 observations on the following 11 variables.
The first six variables are items to be answered on a 5-point rating scale (Likert type) with response categories: (1) \emph{extremely dangerous for the environment} to (5) \emph{not dangerous at all for the environment}.
  \describe{%
    \item{\code{CAR}}{air pollution caused by cars}%
    \item{\code{IND}}{air pollution caused by industry}%
    \item{\code{FARM}}{pesticides and chemicals used in farming}%
    \item{\code{WATER}}{pollution of country's rivers, lakes and streams}%
    \item{\code{TEMP}}{a rise in the world's temperature}%
    \item{\code{GENE}}{modifying the genes of certain crops}%
    \item{\code{SEX}}{gender: (1) \emph{male}, (2) \emph{female}}%
    \item{\code{URB}}{location of residence: (1) \emph{urban area}, (2) \emph{suburbs of large cities, small town, county seat} (3) \emph{rural area}}%
    \item{\code{AGE}}{age: (1) \emph{< 40 years}, (2) \emph{41--59 years}, (3) \emph{60+ years}}%
    \item{\code{CNTRY}}{country: (1) \emph{Great Britain}, (2) \emph{Austria}}%
    \item{\code{EDU}}{education: (1) \emph{below A-level/matrice}, (2) \emph{A-level/matrice or higher}}%
  }%
}
\source{%
  ISSP Research Group (2003). \emph{International Social Survey Programme: Environment II -- ISSP 2000.} GESIS Data Archive, Cologne. ZA3440 Data file Version 1.0.0, \doi{10.4232/1.3440}\cr%
  \url{https://www.gesis.org/issp/modules/issp-modules-by-topic/environment/2000}
  (\href{https://www.gesis.org/fileadmin/upload/dienstleistung/daten/umfragedaten/_bgordnung_bestellen/Usage_regulations.pdf}{Usage regulations})
}
\references{%
  Dittrich, R., Francis, B.J., Hatzinger R., Katzenbeisser, W. (2007). A Paired Comparison Approach for the Analysis of Sets of Likert Scale Responses. \emph{Statistical Modelling}, Vol. 7, No. 1, 3--28.
}
\examples{str(issp2000)}
\keyword{datasets}
