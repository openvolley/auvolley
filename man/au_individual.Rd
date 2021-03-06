% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoring.R
\name{au_individual}
\alias{au_individual}
\title{Calculate individual player scores}
\usage{
au_individual(
  x,
  set_5_weighting = 25/15,
  scoring = au_scoring(),
  infer_sets = FALSE,
  no_set_attacks = c("PR", "PP", "P2")
)
}
\arguments{
\item{x}{datavolley or datavolleyplays: a datavolley object as returned by \code{\link[datavolley:read_dv]{datavolley::dv_read()}}, or the plays component of one}

\item{set_5_weighting}{numeric: the weighting that should be applied to actions from the fifth set of a match (if it went that long)}

\item{scoring}{list: the scores to apply for each outcome. See \code{\link[=au_scoring]{au_scoring()}} for details}

\item{infer_sets}{logical: set this to \code{TRUE} if the scout has not included all setting actions. Some scouts will not include setting actions unless the set is an error, or made by the non-designated setter. Using \code{infer_sets = TRUE} will insert a new setting action (made by the on-court setter) on attacks that don't have a corresponding set action scouted (excluding those in \code{no_set_attacks})}

\item{no_set_attacks}{character: a vector of attack codes for which sets are not expected (setter dumps, attacks on overpasses, second-ball attacks)}
}
\value{
A tibble with a breakdown by player of points in each category, along with
\itemize{
\item \code{points} : each player's total points scored. If set 5 results have been included, points scored in set 5 will be scaled by \code{set_5_weighting}
\item \code{points_per_set}: each player's total points divided by the number of sets they played
\item \code{adjusted_points}: each player's total points but adjusted for game length. The \code{adjusted_points} = \code{points_per_set} * 3 * \code{nmatches} * \code{playing_time} where \code{nmatches} is the average number of matches played by each team, and \code{playing_time} is (number of sets played by the player) / (number of sets played by their team)
}
}
\description{
The Athletes Unlimited league plays matches of 3 sets, each to 25 points. To apply their scoring system to normal volleyball games, we need to decide what to do with sets 4 and 5 (if played). If you only want to count things that happened in the first 3 (or 4) sets, then you should remove data rows associated with sets 5 and/or 4 before calling this function. Otherwise, the \code{set_5_weighting} parameter allows the count of actions from set 5 to be scaled.
}
\details{
Most of the actions that are counted are self-explanatory: serve aces, attack kills, and serve, pass, attack, and set errors. The perhaps-less obvious ones:
\itemize{
\item A pass (serve reception) is counted if it is a perfect or positive pass
\item A set assist is a set that leads to an attack kill
\item Digs are counted on opposition attacks and block cover, if scouted, but not freeball passes. All dig grades except dig errors and digs directly back over the net are counted
\item Only block kills are counted
}
}
\examples{
x <- datavolley::dv_read(datavolley::dv_example_file())
au_individual(x)

}
\references{
https://auprosports.com/volleyball/how-we-play-volleyball/
}
\seealso{
\code{\link[=au_scoring]{au_scoring()}}
}
