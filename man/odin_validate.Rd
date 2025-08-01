% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{odin_validate}
\alias{odin_validate}
\title{Validate odin code}
\usage{
odin_validate(
  expr,
  input_type = NULL,
  compatibility = NULL,
  check_bounds = NULL
)
}
\arguments{
\item{expr}{Odin code as the path to a file (a string), a
character vector of code, or as an expression (typically within
braces \code{{}}).}

\item{input_type}{An optional string describing the type of input
for \code{expr} - must be one of \code{file}, \code{text} or \code{expression}.  If
given, this skips the type detection logic and odin will throw
an error if the wrong type of input is given.  Using this may be
beneficial in programmatic environments.}

\item{compatibility}{Compatibility mode to use.  Valid options are
"warning", which updates code that can be fixed, with warnings,
and "error", which will error.  The option "silent" will
silently rewrite code, but this is not recommended for general
use as eventually the compatibility mode will be removed (this
option is primarily intended for comparing output of odin1 and
odin2 models against old code).  The default, \code{NULL}, currently
corresponds to \code{warning}.}

\item{check_bounds}{Control over static array bounds checking.
This is enabled by default, but is prone to false positives,
erroring where a read or write appears out of bounds but is
actually ok.  This argument exists to allow you to disable the
check and compile the model anyway.  Future versions may allow
specific lines to be ignored, which will provide finer control
and allow you to use the bits of the checks that are actually
helpful.}
}
\value{
A list with elements:
\itemize{
\item \code{success}: boolean, \code{TRUE} if validation was successful
\item \code{result}: Metadata about the model; see Details above for the
format.
\item \code{error}: Either \code{NULL} (if \code{success} is \code{TRUE}) or an error;
see Details above for interpreting this value.
\item \code{compatibility}: A \code{data.frame} of compatibility issues.  This
is formatted similarly to \code{src} within \code{error} (see above), but
also includes \code{type} (a code for the compatibility issue),
\code{description} (a human-readable description of the issue),
\code{original} (the original expression) and \code{value} (the final
expression).
}

The intention is that we won't throw generally from this function.
}
\description{
Validate odin code.  This is primarily intended for use within
other applications.
}
\section{Result}{
On successful validation, we return a list with metadata about the
model.  Currently this contains:
\itemize{
\item \code{time}: The time mode of the model (a string of either
"discrete" or "continuous")
\item \code{parameters}: A \code{data.frame} describing parameters.  Currently the
only column is \code{name}.
\item \code{variables}: A \code{data.frame} describing the model variables.
Currently the only column is \code{name}.
\item \code{data}: A \code{data.frame} describing data used by the model (if it
supports this).  Currently the only column is \code{name}.
}
}

\section{Errors}{
Most errors will have class \code{odin_parse_error}.  These will print
context information if rethrown.  They have fields:
\itemize{
\item \code{message}: The headline error message
\item \code{code}: The odin error code, as listed in \code{vignette("errors")},
and used by \link{odin_error_explain}
\item \code{src}: Source information about the error.  This is a
\code{data.frame} with columns \code{index} (the expression number),
\code{expr} (a list column with the expression), \code{start} (the
starting line; possibly \code{NA}), \code{end} (the finishing line;
possibly \code{NA}), \code{str} (the string containing the literal value
of the expression; possibly \code{NA}) and \code{migrated} (a logical,
indicating if the source has been automatically migrated from
odin1 code).  If any of \code{start}, \code{end} or \code{str} is \code{NA}, all
will be, for all rows.
}

You can get the full rendered message using \code{\link[=conditionMessage]{conditionMessage()}}
on the error object.
}

\examples{
# A successful validation:
odin_validate({
  initial(x) <- 1
  deriv(x) <- a
  a <- parameter()
})

# A failure:
odin_validate({
  initial(x) <- 1
  deriv(x) <- a
})

# Migration warnings
odin_validate({
  initial(x)
  deriv(x) <- a
  a <- user()
})
}
