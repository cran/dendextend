% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attr_access.R
\name{assign_values_to_nodes_nodePar}
\alias{assign_values_to_nodes_nodePar}
\title{Assign values to nodePar of dendrogram's nodes}
\usage{
assign_values_to_nodes_nodePar(
  dend,
  value,
  nodePar = c("pch", "cex", "col", "xpd", "bg"),
  warn = dendextend_options("warn"),
  ...
)
}
\arguments{
\item{dend}{a dendrogram object}

\item{value}{a new value vector for the nodePar attribute. It should be
the same length as the number of nodes in the tree. If not, it will recycle
the value and issue a warning.}

\item{nodePar}{the value inside nodePar to adjust.
This may contain components named pch, cex, col, xpd, and/or bg.}

\item{warn}{logical (default from dendextend_options("warn") is FALSE).
Set if warning are to be issued, it is safer to keep this at TRUE,
but for keeping the noise down, the default is FALSE.}

\item{...}{not used}
}
\value{
A dendrogram, after adjusting the nodePar attribute in all of its nodes,
}
\description{
Go through the dendrogram nodes and updates the values inside its nodePar

If the value has Inf then the value in edgePar will not be changed.
}
\examples{

\dontrun{

dend <- USArrests[1:5, ] \%>\%
  dist() \%>\%
  hclust("ave") \%>\%
  as.dendrogram()

# reproduces "labels_colors<-"
# although it does force us to run through the tree twice,
# hence "labels_colors<-" is better...
plot(dend)
dend2 <- dend \%>\%
  assign_values_to_nodes_nodePar(value = 19, nodePar = "pch") \%>\%
  assign_values_to_nodes_nodePar(value = c(1, 2), nodePar = "cex") \%>\%
  assign_values_to_nodes_nodePar(value = c(2, 1), nodePar = "col")
plot(dend2)


### Making sure this works for NA with character.
dend \%>\%
  assign_values_to_nodes_nodePar(value = 19, nodePar = "pch") \%>\%
  assign_values_to_nodes_nodePar(value = c("red", NA), nodePar = "col") -> dend2
plot(dend2)
}

}
\seealso{
\link{get_leaves_attr}, \link{assign_values_to_leaves_nodePar}
}
