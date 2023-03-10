% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_eic}
\alias{plot_eic}
\title{Plot DB EIC}
\usage{
plot_eic(db, group_id, title = "EIC")
}
\arguments{
\item{db}{`SQLiteConnection`}

\item{group_id}{`numeric(1)` group ID}

\item{title}{`character(1)` title of the plot (by default it show "EIC")}
}
\value{
`plotly`
}
\description{
Plot the EIC for all the sample for a basepeak mass recorded in the database
The line dashed correspond to the area not integrated & the line colored the
retention time range where integrated by XCMS.
It contains a special behavior when the mouse hover a trace : it will display
 all the hovertext of all traces in a unique textbox allowing the user to
  differentiate all the y coordinates of the traces in one shot
}
\examples{
\dontrun{
plot_eic(db, 2)
}
}
