% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/right_holder_pseudos.R
\name{right_holder_pseudos}
\alias{right_holder_pseudos}
\title{Right holder pseudonyms}
\usage{
right_holder_pseudos(mode = c("any", "main", "all"))
}
\arguments{
\item{mode}{which pseudonyms to return.}
}
\description{
Table of right holder pseudonyms, or artistic names. Right holders may have
more than one registered pseudonym and, for some of them, we may know which
one is the most popular one. We say that this is the right holder's _main_
pseudonym.
}
\details{
This function accepts a single parameter, `mode`, which determines which
pseudonyms to return. Valid options are:
\describe{
   \item{`any`}{Returns one single main pseudonym for each right holder, if
                available, or any of the non-main pseudonyms otherwise.}
   \item{`main`}{Returns only main pseudonyms, leaving artists without a main
                 pseudonym out. This may return multiple pseudonyms for some
                 right holders, if more than one pseudonym is marked as main.}
   \item{`all`}{returns _all_ pseudonyms for all right holders. This means
                there may be multiple results for some right holders.}
}

Mode `any` is the ideal mode for enriching tables with right holder
pseudonyms, as it guarantees that _i)_ there will be only one pseudonym
per right holde; _ii)_ that this will be the most reasonable choice based
on the data we have.
}
