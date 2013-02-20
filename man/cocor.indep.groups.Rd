\name{cocor.indep.groups}
\alias{cocor.indep.groups}
\title{Compare two correlations based on independent groups}
\usage{
  cocor.indep.groups(r1, r2, n1, n2,
    alternative = "two.sided", method = "all",
    alpha = 0.05)
}
\arguments{
  \item{r1}{A number specifying the correlation measured in
  group 1}

  \item{r2}{A number specifying the correlation measured in
  group 2}

  \item{n1}{An integer defining the size of group 1}

  \item{n2}{An integer defining the size of group 2}

  \item{alternative}{A character string specifying whether
  the alternative hypothesis is two-sided
  ("\code{two.sided}"; default) or one-sided (
  "\code{greater}" or "\code{less}", depending on the
  direction). Optionally, the initial letter of the
  character strings ("\code{t}", "\code{g}", and
  "\code{l})" can be used.}

  \item{method}{A vector of character strings specifying
  the methods to be used (\code{fisher1925} or
  \code{zou2007}). Use \code{all} to apply all methods
  (default). For further information see the method section
  below.}

  \item{alpha}{A number defining the alpha level of
  significance for the hypothesis test. If method
  \code{zou2007} is used, \eqn{1 - alpha} is taken as the
  level of confidence for the confidence interval. The
  default value is \eqn{.05}.}
}
\value{
  Returns an object of the class
  "\code{cocor.indep.groups}" with the following slots
  holding the input parameters described above:
  \item{r1}{Input parameter} \item{r2}{Input parameter}
  \item{n1}{Input parameter} \item{n2}{Input parameter}
  \item{alternative}{Input parameter} \item{alpha}{Input
  parameter} For each method a slot of the same name exists
  with a list containing the following elements:
  \item{statistic}{The value of the test statistic (if
  method \code{fisher1925} is used)}
  \item{distribution}{The distribution of the test
  statistic (if method \code{fisher1925} is used)}
  \item{p.value}{The p-value of the test (if method
  \code{fisher1925} is used)} \item{conf.int}{The
  confidence interval of the difference between the two
  correlations (if method \code{zou2007} is used)}
}
\description{
  Performs a test of significance for the difference
  between two correlation coefficients based on independent
  groups.
}
\section{Methods}{
  The methods make use of Fisher's \eqn{r}-to-\eqn{Z}
  transformation (1921, p. 26):

  \deqn{Z = \frac{1}{2}(ln(1+r) - ln(1-r)).}

  \describe{ \item{fisher1925:}{ \emph{Fisher's (1925) z}

  This significance test was first described in Fisher
  (1925, pp. 161–168) and its test statistic \eqn{z} is
  calculated as

  \deqn{z = \frac{Z_1 - Z_2}{\sqrt{\frac{1}{n_1 - 3} +
  \frac{1}{n_2 - 3}}}.}

  \eqn{Z_1} and \eqn{Z_2} are the two \eqn{Z} transformed
  correlations that are being compared. \eqn{n_1} and
  \eqn{n_2} specify the size of the two groups the
  correlations are based on. The equation is also given for
  example in Peters and van Voorhis (1940, p. 188) and
  Cohen, Cohen, West, and Aiken (2003, p. 49, formula
  2.8.11). }

  \item{zou2007:}{ \emph{Zou's (2007) confidence interval}

  This method calculates the confidence interval of the
  difference between the two correlation coefficients
  \eqn{r_1} and \eqn{r_2}. If the confidence interval
  includes zero, the null hypothesis that the two
  correlations are equal must be retained. If zero is
  outside the confidence interval, the null hypothesis can
  be rejected. A lower and upper bound for the interval
  (\eqn{L} and \eqn{U}, respectively) is given by \deqn{L =
  r_1 - r_2 - \sqrt{(r_1 - l_1)^2 + (u_2 - r_2)^2}} and
  \deqn{U = r_1 - r_2 - \sqrt{(u_1 - r_1)^2 + (r_2 -
  l_2)^2}} (Zou, 2007, p. 409). A lower and upper bound for
  the confidence interval of \eqn{r_1} (\eqn{l_1} and
  \eqn{u_1}) and \eqn{r_2} (\eqn{l_2} and \eqn{u_2}) are
  calculated as \deqn{l = \frac{exp(2l') - 1}{exp(2l') +
  1},} \deqn{u = \frac{exp(2u') - 1}{exp(2u') + 1}} (Zou,
  2007, p. 406), where \deqn{l',u' = Z \pm
  z_{\frac{\alpha}{2}} \sqrt{\frac{1}{n - 3}}} (Zou, 2007,
  p. 406). \eqn{\alpha} denotes the desired alpha level of
  the confidence interval, whereas \eqn{n} specifies the
  size of the group the correlation is based on. }

  }
}
\examples{
# Compare the difference between two correlations based
# on two independent groups:
r1 <- .7  # Correlation measured in group 1
n1 <- 305  # Size of group 1

r2 <- .6  # Correlation measured in group 2
n2 <- 210  # Size of group 2

cocor.indep.groups(r1, r2, n1, n2)
}
\references{
  Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003).
  \emph{Applied Multiple Regression/Correlation Analysis
  for the Behavioral Sciences (3rd ed.)}. Mahwah, NJ:
  Erlbaum.

  Fisher, R. A. (1921). On the probable error of a
  coefficient of correlation deduced from a small sample.
  \emph{Metron}, \emph{1}, 1–32.

  Fisher, R. A. (1925). \emph{Statistical methods for
  research workers}. Edinburgh, Scotland: Oliver and Boyd.
  Retrieved from http://psychclassics.yorku.ca/

  Peters, C. C., & Van Voorhis, W. R. (1940).
  \emph{Statistical procedures and their mathematical
  bases}. New York: McGraw-Hill.

  Zou, G. Y. (2007). Toward using confidence intervals to
  compare correlations. \emph{Psychological Methods},
  \emph{12}, 399–413. doi:10.1037/1082-989X.12.4.399
}
\seealso{
  \link{cocor.dep.groups.overlap}

  \link{cocor.dep.groups.nonoverlap}
}

