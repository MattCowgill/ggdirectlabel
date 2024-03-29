% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_linepoint.R
\docType{data}
\name{geom_linepoint}
\alias{geom_linepoint}
\alias{GeomLinePoint}
\title{Line with a dot on the final observation}
\format{
An object of class \code{GeomLinePoint} (inherits from \code{Geom}, \code{ggproto}, \code{gg}) of length 8.
}
\usage{
geom_linepoint(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

GeomLinePoint
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, either as a \code{ggproto} \code{Geom} subclass or as a string naming the
stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than
\code{"stat_count"})}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}
}
\description{
\code{geom_linepoint()} draws a \code{ggplot2::geom_line()} and adds a
\code{ggplot2::geom_point()} at the observation with the maximum x value for each
group.
}
\section{Aesthetics}{

\code{geom_linepoint()} understands the following aesthetics (required aesthetics are in bold):
\itemize{
\item \strong{\code{x}}
\item \strong{\code{y}}
\item \code{alpha}
\item \code{colour}
\item \code{group}
\item \code{linetype}
\item \code{pointfill}
\item \code{pointshape}
\item \code{pointsize}
\item \code{pointstroke}
\item \code{pointshape}
\item \code{linewidth}
\item \code{weight}
}
The aesthetics that begin with 'point' (eg. \code{pointfill}) are passed to
\code{geom_point()} - for example \code{pointfill} is passed to the \code{fill} aesthetic
of \code{geom_point()}.

The \code{x}, \code{y}, \code{alpha}, \code{colour}, and \code{group} aesthetics are passed to both
\code{geom_line()} and \code{geom_point()}.

The \code{linetype}, \code{linewidth}, and \code{weight} aesthetics are passed to \code{geom_line()}.

Learn more about setting these aesthetics in \code{vignette("ggplot2-specs")}.
}

\examples{
library(ggplot2)

ggplot(ggplot2::economics_long, aes(x = date, y = value)) +
  geom_linepoint(aes(col = variable)) +
  facet_wrap(~variable)
}
\seealso{
\code{ggplot2::geom_line}, \code{ggplot2::geom_point}
}
\keyword{datasets}
