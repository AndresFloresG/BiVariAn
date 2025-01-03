% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/authenticity_check.R
\name{author_check}
\alias{author_check}
\title{Author checker}
\usage{
author_check(name, credentials, hash_col = "HASH")
}
\arguments{
\item{name}{Initials of the code's author name. (Last name, first name)}

\item{credentials}{Dataframe with the hash code of the tutor. Can be loaded via 'get_credentials' function.}

\item{hash_col}{Name of the column with hash codes. Default is "HASH"}
}
\value{
Text in console whether the statistical analysis is authorized by a tutor or not
}
\description{
Function to check if the name of an author is in a database of validated credentials. Credentials in a database must be stored as HASH. Hash generated by the function uses the following arguments: digest(text, algo= 'sha512', serialize=F). The function is planned to be used by future code implementations.
}
\examples{
\dontrun{
author_check("FGA", credentials, hash_col = "HASH")}


}
