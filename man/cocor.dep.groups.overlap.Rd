\name{cocor.dep.groups.overlap}
\alias{cocor.dep.groups.overlap}
\title{Compare two overlapping correlations based on dependent groups}
\usage{
  cocor.dep.groups.overlap(r.jk, r.jh, r.kh, n,
    alternative = "two.sided", method = "all",
    alpha = 0.05)
}
\arguments{
  \item{r.jk}{A number specifying the correlation between
  \eqn{j} and \eqn{k} (this correlation is used for
  comparison)}

  \item{r.jh}{A number specifying the correlation between
  \eqn{j} and \eqn{h} (this correlation is used for
  comparison)}

  \item{r.kh}{A number specifying the correlation between
  \eqn{k} and \eqn{h}}

  \item{n}{An integer defining the size of the group}

  \item{alternative}{A character string specifying whether
  the alternative hypothesis is two-sided
  ("\code{two.sided}"; default) or one-sided (
  "\code{greater}" or "\code{less}", depending on the
  direction). Optionally, the initial letter of the
  character strings ("\code{t}", "\code{g}", and
  "\code{l})" can be used.}

  \item{method}{A vector of character strings specifying
  the methods to be used (\code{pearson1898},
  \code{hotelling1940}, \code{hendrickson1970},
  \code{williams1959}, \code{olkin1967}, \code{dunn1969},
  \code{steiger1980}, \code{meng1992}, \code{hittner2003},
  or \code{zou2007}). Use \code{all} to apply all methods
  (default). For further information see the method section
  below.}

  \item{alpha}{A number defining the alpha level for the
  hypothesis test. If method \code{meng1992} or
  \code{zou2007} is used, \eqn{1 - alpha} is taken as the
  level of confidence for the confidence interval. The
  default value is \eqn{.05}.}
}
\value{
  Returns an object of the class
  "\code{cocor.dep.groups.overlap}" with the following
  slots holding the input parameters described above:
  \item{r.jk}{Input parameter} \item{r.jh}{Input parameter}
  \item{r.kh}{Input parameter} \item{n}{Input parameter}
  \item{alternative}{Input parameter} \item{alpha}{Input
  parameter} For each method a slot of the same name exists
  with a list containing the following elements:
  \item{statistic}{The value of the test statistic (unless
  method \code{zou2007} is used)} \item{distribution}{The
  distribution of the test statistic (unless method
  \code{zou2007} is used)} \item{df}{The degrees of freedom
  of the distribution of the test statistic (if method
  \code{hotelling1940}, \code{hendrickson1970}, or
  \code{williams1959} is used)} \item{p.value}{The p-value
  of the test (unless method \code{zou2007} is used)}
  \item{conf.int}{The confidence interval of the difference
  between the two correlations (if method \code{meng1992}
  or \code{zou2007} is used)}
}
\description{
  Performs a test of significance for the difference
  between two correlations based on dependent groups (e.g.,
  the same group). The two correlations are overlapping,
  i.e., they have one variable in common. The comparison is
  made between \code{r.jk} and \code{r.jh}. The function
  tests whether the correlations between \code{j} and
  \code{k} (\code{r.jk}) and between \code{j} and \code{h}
  (\code{r.jh}) differ in magnitude. Because the
  significance depends on the intercorrelation between
  \code{k} and \code{h} (r.kh), this intercorrelation has
  to be provided as an additional parameter.
}
\section{Methods}{
  Some methods make use of Fisher's r-to-z transformation
  (1921, p. 26):

  \deqn{Z = \frac{1}{2}(ln(1+r) - ln(1-r))}

  \describe{ \item{pearson1898:}{ \emph{Pearson and Filon's
  z (1898)}

  The method was proposed by Pearson and Filon (1898, p.
  259, formula xxxvii).

  Steiger (1980, p. 245 formula 3): \deqn{k = r_{kh}(1 -
  r_{jk}^2 - r_{jh}^2) - \frac{1}{2}(r_{jk}r_{jh})(1 -
  r_{jk}^2 - r_{jh}^2 - r_{kh}^2)}

  Steiger (1980, p. 246, formula 4): \deqn{z =
  \frac{\sqrt{n} (r_{jk} - r_{jh})}{\sqrt{(1 - r_{jk}^2)^2
  + (1 - r_{jh}^2)^2 - 2k}}}

  }

  \item{hotelling1940:}{ \emph{Hotelling's t (1940)}

  Hotelling (1940, p. 278): \deqn{|R| = 1 + 2 r_{jk} r_{jh}
  r_{kh} - r_{jk}^2 - r_{jh}^2 - r_{kh}^2}

  Hotelling (1940, p. 278, formula 7): \deqn{t =
  \frac{(r_{jk} - r_{jh})\sqrt{(n - 3)(1 +
  r_{kh})}}{\sqrt{2|R|}}, df = n - 3}

  The formula can also be found in Steiger (1980, p. 246),
  Glass and Stanley (1984, p. 311, formula 15.7) or
  Hittner, May, and Silver (2003, p. 152).

  }

  \item{williams1959:}{ \emph{Williams' t (1959)}

  This method is a modification of Hotelling's \eqn{t}
  (1940) and was suggested by Williams (1959). Two
  mathematically different formulas for Williams' \eqn{t}
  can be found in the literature. This is the version that
  Hittner, May, and Silver (2003, p. 152) labeled as
  "standard Williams' t". The alternative formula termed as
  "Williams' modified \eqn{t} per Hendrickson, Stanley, and
  Hills (1970)" by Hittner et al. (2003, p. 152) is
  implemented as hendrickson1970. The term "standard
  Williams' t" is also used in the DEPCORR script for SAS
  software by Hittner and May (1998).

  The formula (\code{williams1959}) can be found in Steiger
  (1980, p. 246, formula 7) or Neill and Dunn (1975, p.
  533).

  \deqn{t = (r_{jk} - r_{jh})\sqrt{\frac{(n - 1)(1 +
  r_{kh})}{2(\frac{n - 1}{n - 3})|R|+\bar r^2(1 -
  r_{kh})^3}}, df = n - 3}

  \deqn{\bar r = \frac{r_{jk} + r_{jh}}{2}} \deqn{|R| = 1 +
  2 r_{jk} r_{jh} r_{kh} - r_{jk}^2 - r_{jh}^2 - r_{kh}^2}

  This formula is also in accordance with the results of
  the software DEPCORR by Hittner and May (1998) and DEPCOR
  by Silver, Hittner, and May (2006). The formula reported
  by Boyer, Palachek, and Schucany (1983, p. 76) is
  misleading as the term \eqn{(1 - r_{rk})} is not being
  cubed. There are also errors in the formula described by
  Hittner, May, and Silver (2003, p. 152). \eqn{r_{jk} -
  r_{jh}} is divided instead of being multiplied by the
  square root term. In the denominator of the fraction in
  the square root term, there are also additional
  parentheses so that the whole denominator is multiplied
  by 2. The same errors can be found in Wilcox and Tian
  (2008, p. 107, formula 1):

  \deqn{t = \frac{r_{jk} - r_{jh}}{\sqrt{\frac{(n - 1)(1 +
  r_{kh})}{2((\frac{n - 1}{n - 3})|R|-\bar r^2(1 -
  r_{kh})^3)}}}, df = n - 3}

  }

  \item{olkin1967:}{ \emph{Olkin's z (1967)}

  The formula given by Olkin (1967, p. 112) and
  Hendrickson, Stanley, and Hills (1970, p. 190, formula 2)
  is incorrect. A corrected version can be found in
  Hendrickson and Collins (1970, p. 639). However, in the
  revised version \eqn{n} in the enumerator is decreased by
  1. Glass and Stanley (1970, p. 313, formula 14.19),
  Hittner, May, and Silver (2003, p. 152) and May and
  Hittner (1997a, p. 259; 1997b, p. 480) report a version
  without this decrement. In Olkin's original version
  (1967) \eqn{n} is not reduced, either.

  \deqn{z = \frac{(r_{jk} - r_{jh})\sqrt{n}}{\sqrt{(1 -
  r_{jk}^2)^2 + (1 - r_{jh}^2)^2 - 2 r_{kh}^3 - (2 r_{kh} -
  r_{jk} r_{jh}) (1 - r_{kh}^2 - r_{jk}^2 - r_{jh}^2)}}}

  }

  \item{dunn1969:}{ \emph{Dunn and Clark's z (1969)}

  Dunn and Clark (1969, p. 368, formula 8): \deqn{c =
  \frac{r_{kh}(1 - r_{jk}^2 - r_{jh}^2) - \frac{1}{2}
  r_{jk} r_{jh} (1 - r_{jk}^2 - r_{jh}^2 - r_{kh}^2)}{(1 -
  r_{jk}^2)(1 - r_{jh}^2)}}

  Dunn and Clark (1969, p. 370, formula 15): \deqn{z =
  \frac{(Z_{jk} - Z_{jh})\sqrt{n - 3}}{\sqrt{2 - 2c}}}

  }

  \item{hendrickson1970:}{ \emph{Hendrickson, Stanley, and
  Hills' (1970) modification of Williams' t (1959)}

  This method is a modification of Hotelling's \eqn{t}
  (1940) and was suggested by Williams (1959). Two
  mathematically different formulas of Williams' \eqn{t}
  can be found in the literature. This is the version that
  Hittner, May, and Silver (2003, p. 152) labeled as
  "Williams' modified \eqn{t} per Hendrickson, Stanley, and
  Hills (1970)". The alternative formula termed as
  "standard Williams' t" by Hittner et al. (2003, p. 152)
  is implemented as williams1959.

  The formula can be found in Hendrickson, Stanley, and
  Hills (1970, p. 193), May and Hittner (1997a, p. 259;
  1997b, p. 480), and Hittner et al. (2003, p. 152).

  \deqn{t = \frac{(r_{jk} - r_{jh})\sqrt{(n - 3)(1 +
  r_{kh})}}{\sqrt{2|R|+\frac{(r_{jk} - r_{jh})^2(1 -
  r_{kh})^3}{4(n - 1)}}}, df = n - 3}

  In the formula reported by Dunn and Clark (1971, p. 905,
  formula 1.2), the fraction in the denominator seems to be
  erroneous. The enumerator of this fraction reads as
  \eqn{(n - 3)(r_{jk} + r_{jh})^2(1 - r_{kh})^3} instead of
  \eqn{(r_{jk} - r_{jh})^2(1 - r_{kh})^3}:

  \deqn{t = \frac{(r_{jk} - r_{jh})\sqrt{\frac{(n - 3)(1 +
  r_{kh})}{2|R|}}}{\sqrt{1 + \frac{(n - 3)(r_{jk} +
  r_{jh})^2(1 - r_{kh})^3}{8(n - 1)|R|}}}, df = n - 3}

  }

  \item{steiger1980:}{ \emph{Steiger's (1980) modification
  of Dunn and Clark's z (1969) using average correlations}

  This method was proposed by Steiger (1980) and is a
  modification of Dunn and Clark's work (1969). Instead of
  \eqn{r.jk} and \eqn{r.jh} the mean of the two is being
  used.

  Steiger (1980, p. 247): \deqn{\bar r = \frac{r_{jk} +
  r_{jh}}{2}}

  Steiger (1980, p. 247, formula 10; In the original
  article, there are brackets missing in the denominator):
  \deqn{c = \frac{r_{kh}(1 - 2\bar r^2) - \frac{1}{2}\bar
  r^2(1 - 2\bar r^2 - r_{kh}^2)}{(1 - \bar r^2)^2}}

  Steiger (1980, p. 247, formula 14): \deqn{z =
  \frac{(Z_{jk} - Z_{jh})\sqrt{n - 3}}{\sqrt{2 - 2c}}}

  }

  \item{meng1992:}{ \emph{Meng, Rosenthal, and Rubin's z
  (1992)}

  Meng, Rosenthal, and Rubin (1992, p. 173):
  \deqn{\overline {r^2} = \frac{r_{jk}^2 + r_{jh}^2}{2}}

  Meng et al. (1992, p. 173, formula 3): \deqn{f = \frac{1
  - r_{kh}}{2(1 - \overline {r^2})} \textit{, which must
  be} \le 1}

  Meng et al. (1992, p. 173, formula 2): \deqn{h = \frac{1
  - f\overline {r^2}}{1 - \overline {r^2}}}

  Test statistic (Meng et al., 1992, p. 173, formula 1):
  \deqn{z = (Z_{jk} - Z_{jh}) \sqrt{\frac{n - 3}{2(1 -
  r_{kh})h}}}

  The method also includes a confidence interval of the
  difference between the two correlation coefficients
  \eqn{r_{jk}} and \eqn{r_{jh}} (Meng et al., 1992, p. 173,
  formula 4): \deqn{L, U = Z_{jk} - Z_{jk} \pm
  z_{\frac{\alpha}{2}} \sqrt{\frac{2(1 - r_{kh})h}{n - 3}}}

  }

  \item{hittner2003:}{ \emph{Hittner, May, and Silver's
  (2003) modification of Dunn and Clark's z (1969) using
  backtransformed average Fisher's (1921) zs}

  The idea to backtransform averaged \eqn{Z}s was first
  proposed in Silver and Dunlap (1987) and was applied to
  the comparison of overlapping correlations by Hittner,
  May, and Silver (2003). Their method is based on
  Steiger's approach (1980).

  Described in Silver and Dunlap (1987, p. 146): \deqn{\bar
  Z = \frac{Z_{jk} + Z_{jh}}{2}}

  Silver and Dunlap (1987, p. 146, formula 4): \deqn{\bar
  r_z = \frac{exp(2\bar Z - 1)}{exp(2\bar Z + 1)}}

  Hittner et al. (2003, p. 153): \deqn{c = \frac{r_{kh}(1 -
  2\bar r_z^2) - \frac{1}{2}\bar r_z^2(1 - 2\bar r_z^2 -
  r_{kh}^2)}{(1 - \bar r_z^2)^2}}

  Hittner et al. (2003, p. 153): \deqn{z = \frac{(Z_{jk} -
  Z_{jh})\sqrt{n - 3}}{\sqrt{2 - 2c}}}

  }

  \item{zou2007:}{ \emph{Zou's (2007) confidence interval}

  This method calculates the confidence interval of the
  difference between the two correlation coefficients
  \eqn{r_{jk}} and \eqn{r_{jh}}.

  Zou (2007, p. 406): \deqn{l',u' = Z \pm
  z_{\frac{\alpha}{2}} \sqrt{\frac{1}{n - 3}}}

  Zou (2007, p. 406): \deqn{l = \frac{exp(2l') -
  1}{exp(2l') + 1}} \deqn{u = \frac{exp(2u') - 1}{exp(2u')
  + 1}}

  Zou (2007, p. 409): \deqn{c = \frac{(r_{kh} - \frac{1}{2}
  r_{jk} r_{jh})(1 - r_{jk}^2- r_{jh}^2- r_{kh}^2) +
  r_{kh}^3}{(1 - r_{jk}^2)(1 - r_{jh}^2)}}

  Zou (2007, p. 409): \deqn{L = r_{jk} - r_{jh} -
  \sqrt{(r_{jk} - l_1)^2 + (u_2 - r_{jh})^2 - 2c(r_{jk} -
  l_1)(u_2 - r_{jh})}} \deqn{U = r_{jk} - r_{jh} -
  \sqrt{(u_1 - r_{jk})^2 + (r_{jh} - l_2)^2 - 2c(u_1 -
  r_{jk})(r_{jh} - l_2)}}

  } }
}

\section{Recommended methods}{
  Silver, Hittner, and May (2006) summarize the Monte Carlo
  studies by May and Hittner (1997b) and Hittner, May, and
  Silver (2003) and formulate recommendations for applied
  researchers. They found hendrickson1970, dunn1969,
  steiger1980, meng1992, and hittner2003 to be equally
  acceptable in terms of controlling Type I error and
  power.

  Dunn and Clark (1971) compared hotelling1940 and
  hendrickson1970 with respect to Type I error rate and
  statistical power and suggest to use hendrickson1970.

  Neill and Dunn (1975) compared hotelling1940 and
  williams1959 and recommend williams1959 for small
  samples.

  Steiger (1980) considers pearson1898, hotelling1940,
  williams1959, dunn1969, and steiger1980 in his article.
  He recommends williams1959, dunn1969, and steiger1980 as
  they maintain a better Type I error control at small
  sample sizes than pearson1898 and hotelling1940, with
  williams1959 probably as the best all-round choice.

  Boyer et al. (1983) compared williams1959 with other
  tests and found williams1959 to be the best choice if a
  normal distribution can be assumed.

  May and Hittner (1997b) compared hotelling1940,
  hendrickson1970, olkin1967, and meng1992. They found no
  method to be preferable over the other methods. Instead,
  they indicate that the decision should depend on sample
  size, predictor intercorrelation, effect size, and
  predictor-criterion correlations. In nearly no case a
  difference was found between the two \eqn{t} tests
  (hotelling1940 and hendrickson1970) and between the two
  \eqn{z} tests (olkin1967 and meng1992). In general, the
  \eqn{t} tests provide a greater power than the \eqn{z}
  tests, however, at the expense of type I error rate.

  Hittner et al. (2003) compared hotelling1940,
  hendrickson1970, williams1959, olkin1967, dunn1969,
  steiger1980, meng1992, and hittner2003. They recommend
  not to use hotelling1940, hendrickson1970, olkin1967, as
  they inflate type I error rates. Hittner et al. (2003)
  found williams1959 and dunn1969 to have the best overall
  statistical properties.
}
\examples{
# Compare the difference between the correlations (age, intelligence) and
# (age, shoe size) measured in the same group (all values are fictional):
r.jk <- .2  # Correlation (age, intelligence)
r.jh <- .5  # Correlation (age, shoe size)
r.kh <- .1  # Correlation (intelligence, shoe size)
n <- 315  # Size of the group

cocor.dep.groups.overlap(r.jk, r.jh, r.kh, n)
}
\references{
  Boyer, I. E., Palachek, A. D., & Schucany. W. R. (1983).
  An empirical study of related correlation coefficients.
  \emph{Journal of Educational Statistics}, \emph{8},
  75–86. doi:10.2307/1164871

  Dunn, O. J. & Clark, V. A. (1969). Correlation
  coefficients measured on the same individuals.
  \emph{Journal of the American Statistical Association},
  \emph{64}, 366–377. doi:10.2307/2283746

  Dunn, O. J. & Clark, V. A. (1971). Comparison of tests of
  the equality of dependent correlation coefficients.
  \emph{Journal of the American Statistical Association},
  \emph{66}, 904–908. doi:10.2307/2284252

  Fisher, R. A. (1921). On the probable error of a
  coefficient of correlation deduced from a small sample.
  \emph{Metron}, \emph{1}, 1–32.

  Glass, G. V., & Stanley, J. C. (1970). \emph{Statistical
  methods in eduction and psychology}. Englewood Cliffs,
  NJ: Prentice-Hall.

  Glass, G. V., & Stanley, J. C. (1984). \emph{Statistical
  methods in eduction and psychology (2nd ed.)}. Englewood
  Cliffs, NJ: Prentice-Hall.

  Hendrickson, G. F., Stanley J. C., & Hills, J. R. (1970).
  Olkin's new formula for significance of r13 vs. r23
  compared with Hotelling's method. \emph{American
  Educational Research Journal}, \emph{7}, 189–195.
  doi:10.2307/1162159

  Hendrickson, G. F., & Collins, J. R. (1970). Note
  correcting the results in "Olkin's new formula for the
  significance of r13 vs. r23 compared with Hotelling's
  method". \emph{American Educational Research Journal},
  \emph{7}, 639–641. doi:10.2307/1161847

  Hittner, J. B., & May, K. (1998). DEPCORR: A SAS program
  for comparing dependent correlations. Applied
  Psychological Measurement, 22, 93–94.
  doi:10.1177/01466216980221010

  Hittner, J. B., May, K., & Silver, N. C. (2003). A Monte
  Carlo evaluation of tests for comparing dependent
  correlations. \emph{The Journal of General Psychology},
  \emph{130}, 149–168. doi:10.1080/00221300309601282

  Hotelling, H. (1940). The selection of variates for use
  in prediction, with some comments on the general problem
  of nuisance parameters. \emph{Annals of Mathematical
  Statistics}, \emph{11}, 271–283.
  doi:10.1214/aoms/1177731867

  May, K., & Hittner, J. B., (1997a) - A note on statistics
  for comparing dependent correlations. \emph{Psychological
  Reports}, \emph{80}, 475–480.
  doi:10.2466/pr0.1997.80.2.475

  May, K., & Hittner, J. B. (1997b). Tests for comparing
  dependent correlations revisited: A Monte Carlo study.
  \emph{The Journal of Experimental Education}, \emph{65},
  257–269. doi:10.1080/00220973.1997.9943458

  Meng, X. L., Rosenthal, R., & Rubin, D. B. (1992).
  Comparing correlated correlation coefficients.
  \emph{Psychological Bulletin}, \emph{111}, 172–175.
  doi:10.1037//0033-2909.111.1.172

  Neill, J. J., & Dunn, O. J. (1975). Equality of dependent
  correlation coefficients. \emph{Biometrics}, \emph{31},
  531–543. doi:10.2307/2529435

  Olkin, I. (1967). Correlations revisited. In J. C.
  Stanley (Ed.), \emph{Improving experimental design and
  statistical analysis} (pp. 102–128). Chicago, IL: Rand
  McNally.

  Pearson, K., & Filon, L. N. G. (1898). Mathematical
  contributions to theory of evolution: IV. On the probable
  errors of frequency constants and on the influence of
  random selection and correlation. \emph{Philosophical
  Transactions of the Royal Society of London, Series A},
  \emph{191}, 229–311. doi:10.1098/rsta.1898.0007

  Silver, N. C , & Dunlap, W. P. (1987). Averaging
  correlation coefficients: Should Fisher's Z
  transformation be used? \emph{Journal of Applied
  Psychology}, \emph{72}, 146–148.
  doi:10.1037//0021-9010.72.1.146

  Silver, N. C., Hittner, J. B., & May, K. (2004). Testing
  dependent correlations with nonoverlapping variables: A
  Monte Carlo simulation. \emph{Journal of Experimental
  Education}, \emph{73}, 53–69. doi:10.3200/JEXE.71.1.53-70

  Silver, N. C., Hittner, J. B., & May, K. (2006). A
  FORTRAN 77 program for comparing dependent correlations.
  \emph{Applied Psychological Measurement}, \emph{30},
  152–153. doi:10.1177/0146621605277132

  Steiger, J. H. (1980). Tests for comparing elements of a
  correlation matrix. \emph{Psychological Bulletin},
  \emph{87}, 245–251. doi:10.1037//0033-2909.87.2.245

  Wilcox, R. R., & Tian, T. (2008). Comparing dependent
  correlations. \emph{The Journal of General Psychology},
  \emph{135}, 105–112. doi:10.3200/GENP.135.1.105-112

  Williams, E. J. (1959). The comparison of regression
  variables. \emph{Journal of the Royal Statistical
  Society, Series B}, \emph{21}, 396–399. Retrieved from
  http://www.jstor.org/stable/2983809

  Zou, G. Y. (2007). Toward using confidence intervals to
  compare correlations. \emph{Psychological Methods},
  \emph{12}, 399–413. doi:10.1037/1082-989X.12.4.399
}
\seealso{
  \link{cocor.indep.groups}

  \link{cocor.dep.groups.nonoverlap}
}

