#' Compare two overlapping correlations based on dependent groups
#'
#' Performs a test of significance for the difference between two correlations based on dependent groups (e.g., the same group). The two correlations are overlapping, i.e., they have one variable in common. The comparison is made between \code{r.jk} and \code{r.jh}. The function tests whether the correlations between \code{j} and \code{k} (\code{r.jk}) and between \code{j} and \code{h} (\code{r.jh}) differ in magnitude. Because the significance depends on the intercorrelation between \code{k} and \code{h} (r.kh), this intercorrelation has to be provided as an additional parameter. The function expects correlation coefficients as input.
#'
#'@section Tests:
#' In the following, \eqn{r_{jk}} and \eqn{r_{jh}} are the two correlations that are being compared; \eqn{Z_{jk}} and \eqn{Z_{jh}} are their \eqn{Z} transformed equivalents.
#' \eqn{r_{kh}} is the related correlation that is additionally required.
#' \eqn{n} specifies the size of the group the two correlations are based on.
#' Some tests make use of Fisher's \eqn{r}-to-\eqn{Z} transformation (1921, p. 26):
#'
#' \deqn{Z = \frac{1}{2}(ln(1+r) - ln(1-r)).}{Z = (1/2)(ln(1+r) - ln(1-r)).}
#'
#'\describe{
### pearson1898
#'\item{pearson1898:}{
#'\emph{Pearson and Filon's (1898) z}
#'
#' This test was proposed by Pearson and Filon (1898, p. 259, formula xxxvii).
#' The test statistic \eqn{z} is computed as
#' \deqn{z = \frac{\sqrt{n} (r_{jk} - r_{jh})}{\sqrt{(1 - r_{jk}^2)^2 + (1 - r_{jh}^2)^2 - 2k}}}{z = (\sqrt(n) (r_{jk} - r_{jh}))/(\sqrt((1 - r_{jk}^2)^2 + (1 - r_{jh}^2)^2 - 2k))}
#' (Steiger, 1980, p. 246, formula 4), where
#' \deqn{k = r_{kh}(1 - r_{jk}^2 - r_{jh}^2) - \frac{1}{2}(r_{jk}r_{jh})(1 - r_{jk}^2 - r_{jh}^2 - r_{kh}^2)}{k = r_{kh}(1 - r_{jk}^2 - r_{jh}^2) - (1/2)(r_{jk}r_{jh})(1 - r_{jk}^2 - r_{jh}^2 - r_{kh}^2)}
#' (Steiger, 1980, p. 245 formula 3).
#' }
#'
### hotelling1940
#'\item{hotelling1940:}{
#'\emph{Hotelling's (1940) t}
#'
#' The test statistic \eqn{t} is given by
#' \deqn{t = \frac{(r_{jk} - r_{jh})\sqrt{(n - 3)(1 + r_{kh})}}{\sqrt{2|R|}}}{t = ((r_{jk} - r_{jh})\sqrt((n - 3)(1 + r_{kh})))/(\sqrt(2|R|))}
#' (Hotelling, 1940, p. 278, formula 7) with \eqn{df = n - 3}, where
#' \deqn{|R| = 1 + 2 r_{jk} r_{jh} r_{kh} - r_{jk}^2 - r_{jh}^2 - r_{kh}^2}
#' (Hotelling, 1940, p. 278).
#' The test statistic is also given in Steiger (1980, p. 246), Glass and Stanley (1984, p. 311, formula 15.7), and Hittner, May, and Silver (2003, p. 152).
#' }
#' 
### williams1959
#'\item{williams1959:}{
#'\emph{Williams' (1959) t}
#'
#' This test is a modification of Hotelling's (1940) \eqn{t} and was suggested by Williams (1959).
#' Two mathematically different formulae for Williams' \eqn{t} can be found in the literature (Hittner et al., 2003, p. 152).
#' This is the version that Hittner et al. (2003, p. 152) labeled as "standard Williams' \eqn{t}":
#' \deqn{t = (r_{jk} - r_{jh})\sqrt{\frac{(n - 1)(1 + r_{kh})}{2(\frac{n - 1}{n - 3})|R|+\bar r^2(1 - r_{kh})^3}}}{t = (r_{jk} - r_{jh})\sqrt(((n - 1)(1 + r_{kh}))/(2((n - 1)/(n - 3))|R|+\bar{r}^2(1 - r_{kh})^3))}
#' with \eqn{df = n - 3}, where
#' \deqn{\bar r = \frac{r_{jk} + r_{jh}}{2}}{\bar{r} = (r_{jk} + r_{jh})/2}
#' and
#' \deqn{|R| = 1 + 2 r_{jk} r_{jh} r_{kh} - r_{jk}^2 - r_{jh}^2 - r_{kh}^2.}
#' An alternative formula for Williams' \eqn{t}---termed as "Williams' modified \eqn{t} per Hendrickson, Stanley, and Hills (1970)" by Hittner et al. (2003, p. 152)---is implemented in this function as \code{hendrickson1970} (see below).
#' The test statistic of \code{williams1959} is also given in Steiger (1980, p. 246, formula 7) and Neill and Dunn (1975, p. 533).
#'
#' Results of \code{williams1959} are in accordance with the results of the software DEPCORR by Hittner and May (1998) and DEPCOR by Silver, Hittner, and May (2006).
#' However, we found several typographical errors in formulae that also claim to compute Williams' \eqn{t}.
#' For example, the formula reported by Boyer, Palachek, and Schucany (1983, p. 76) contains an error because the term \eqn{(1 - r_{rk})} is not being cubed.
#' There are also typographical errors in the formula described by Hittner et al. (2003, p. 152). For example, \eqn{r_{jk} - r_{jh}} is divided instead of being multiplied by the square root term, and in the denominator of the fraction in the square root term, there are additional parentheses so that the whole denominator is multiplied by 2.
#' These same errors can also be found in Wilcox and Tian (2008, p. 107, formula 1).
#'}
#'
### olkin1967
#'\item{olkin1967:}{
#'\emph{Olkin's (1967) z}
#'
#' In the original article by Olkin (1967, p. 112) and in Hendrickson, Stanley, and Hills (1970, p. 190, formula 2), the reported formula contains a typographical error.
#' Hendrickson and Collins (1970, p. 639) provide a corrected version.
#' In the revised version, however, \eqn{n} in the enumerator is decreased by 1.
#' This function implements the corrected formula without the decrement.
#' The formula implemented in this function is used by Glass and Stanley (1970, p. 313, formula 14.19), Hittner et al. (2003, p. 152), and May and Hittner (1997a, p. 259; 1997b, p. 480):
#' 
#' \deqn{z = \frac{(r_{jk} - r_{jh})\sqrt{n}}{\sqrt{(1 - r_{jk}^2)^2 + (1 - r_{jh}^2)^2 - 2 r_{kh}^3 - (2 r_{kh} - r_{jk} r_{jh}) (1 - r_{kh}^2 - r_{jk}^2 - r_{jh}^2)}}.}{z = ((r_{jk} - r_{jh})\sqrt(n))/(\sqrt((1 - r_{jk}^2)^2 + (1 - r_{jh}^2)^2 - 2 r_{kh}^3 - (2 r_{kh} - r_{jk} r_{jh}) (1 - r_{kh}^2 - r_{jk}^2 - r_{jh}^2))).}
#'}
#'
### dunn1969
#'\item{dunn1969:}{
#'\emph{Dunn and Clark's (1969) z}
#' 
#' The test statistic \eqn{z} of this test is calculated as
#' \deqn{z = \frac{(Z_{jk} - Z_{jh})\sqrt{n - 3}}{\sqrt{2 - 2c}}}{z = ((Z_{jk} - Z_{jh})\sqrt(n - 3))/(\sqrt(2 - 2c))}
#' (Dunn and Clark, 1969, p. 370, formula 15), where
#' \deqn{c = \frac{r_{kh}(1 - r_{jk}^2 - r_{jh}^2) - \frac{1}{2} r_{jk} r_{jh} (1 - r_{jk}^2 - r_{jh}^2 - r_{kh}^2)}{(1 - r_{jk}^2)(1 - r_{jh}^2)}}{c = (r_{kh}(1 - r_{jk}^2 - r_{jh}^2) - (1/2) r_{jk} r_{jh} (1 - r_{jk}^2 - r_{jh}^2 - r_{kh}^2))/((1 - r_{jk}^2)(1 - r_{jh}^2))}
#' (Dunn and Clark, 1969, p. 368, formula 8).
#'}
#'
### hendrickson1970
#'\item{hendrickson1970:}{
#'\emph{Hendrickson, Stanley, and Hills' (1970) modification of Williams' (1959) t}
#'
#' This test is a modification of Hotelling's (1940) \eqn{t} and was suggested by Williams (1959).
#' Two mathematically different formulae of Williams' (1959) \eqn{t} can be found in the literature.
#' \code{hendrickson1970} is the version that Hittner et al. (2003, p. 152) labeled as "Williams' modified \eqn{t} per Hendrickson, Stanley, and Hills (1970)".
#' An alternative formula termed as "standard Williams' \eqn{t}" by Hittner et al. (2003, p. 152) is implemented as \code{williams1959} (see above).
#' The \code{hendrickson1970} formula can be found in Hendrickson, Stanley, and Hills (1970, p. 193), May and Hittner (1997a, p. 259; 1997b, p. 480), and Hittner et al. (2003, p. 152):
#' \deqn{t = \frac{(r_{jk} - r_{jh})\sqrt{(n - 3)(1 + r_{kh})}}{\sqrt{2|R|+\frac{(r_{jk} - r_{jh})^2(1 - r_{kh})^3}{4(n - 1)}}}}{t = ((r_{jk} - r_{jh})\sqrt((n - 3)(1 + r_{kh})))/(\sqrt(2|R|+\((r_{jk} - r_{jh})^2(1 - r_{kh})^3)/(4(n - 1))))}
#' with \eqn{df = n - 3}.
#' A slightly changed version of this formula was provided by Dunn and Clark (1971, p. 905, formula 1.2), but seems to be erroneous, due to an error in the denominator.
#'}
#'
### steiger1980
#'\item{steiger1980:}{
#'\emph{Steiger's (1980) modification of Dunn and Clark's (1969) z using average correlations}
#'
#' This test was proposed by Steiger (1980) and is a modification of Dunn and Clark's (1969) \eqn{z}.
#' Instead of \eqn{r_{jk}} and \eqn{r_{jh}}, the mean of the two is used.
#' The test statistic \eqn{z} is defined as
#' \deqn{z = \frac{(Z_{jk} - Z_{jh})\sqrt{n - 3}}{\sqrt{2 - 2c}}}{z = ((Z_{jk} - Z_{jh})\sqrt(n - 3))/(\sqrt(2 - 2c))}
#' (Steiger 1980, p. 247, formula 14), where
#' \deqn{\bar r = \frac{r_{jk} + r_{jh}}{2}}{\bar{r} = (r_{jk} + r_{jh})/2}
#' (Steiger, 1980, p. 247)
#' and
#' \deqn{c = \frac{r_{kh}(1 - 2\bar r^2) - \frac{1}{2}\bar r^2(1 - 2\bar r^2 - r_{kh}^2)}{(1 - \bar r^2)^2}}{c = (r_{kh}(1 - 2\bar{r}^2) - (1/2)\bar{r}^2(1 - 2\bar{r}^2 - r_{kh}^2))/((1 - \bar{r}^2)^2)}
#' (Steiger ,1980, p. 247, formula 10; in the original article, there are brackets missing around the divisor).
#'}
#'
### meng1992
#'\item{meng1992:}{
#'\emph{Meng, Rosenthal, and Rubin's (1992) z}
#'
#' This test is based on the test statistic \eqn{z},
#' \deqn{z = (Z_{jk} - Z_{jh}) \sqrt{\frac{n - 3}{2(1 - r_{kh})h}},}{z = (Z_{jk} - Z_{jh}) \sqrt((n - 3)/(2(1 - r_{kh})h),}
#' (Meng et al., 1992, p. 173, formula 1), where
#' \deqn{h = \frac{1 - f\overline{r^2}}{1 - \overline{r^2}}}{h = (1 - f\overline{r^2})/(1 - \overline{r^2})}
#' (Meng et al., 1992, p. 173, formula 2),
#' \deqn{f = \frac{1 - r_{kh}}{2(1 - \overline{r^2})}}{f = (1 - r_{kh})/(2(1 - \overline{r^2}))}
#' (\eqn{f} must be \eqn{\le 1}; Meng et al., 1992, p. 173, formula 3), and
#' \deqn{\overline{r^2} = \frac{r_{jk}^2 + r_{jh}^2}{2}}{\overline{r^2} = (r_{jk}^2 + r_{jh}^2)/2}
#' (Meng et al., 1992, p. 173).
#' This test also constructs a confidence interval of the difference between the two correlation coefficients \eqn{r_{jk}} and \eqn{r_{jh}}:
#' \deqn{L, U = Z_{jk} - Z_{jk} \pm z_{\frac{\alpha}{2}} \sqrt{\frac{2(1 - r_{kh})h}{n - 3}}}{L, U = Z_{jk} - Z_{jk} +- z_{\alpha/2} \sqrt((2(1 - r_{kh})h)/(n - 3))}
#' (Meng et al., 1992, p. 173, formula 4).
#' \eqn{\alpha} denotes the desired alpha level of the confidence interval.
#' If the confidence interval includes zero, the null hypothesis that the two correlations are equal must be retained.
#' If zero is outside the confidence interval, the null hypothesis can be rejected.
#'}
#'
### hittner2003
#'\item{hittner2003:}{
#'\emph{Hittner, May, and Silver's (2003) modification of Dunn and Clark's (1969) z using a backtransformed average Fisher's (1921) Z procedure}
#'
#' The approach to backtransform averaged Fisher's (1921) \eqn{Z}s was first proposed by Silver and Dunlap (1987) and was applied to the comparison of overlapping correlations by Hittner et al. (2003).
#' The test is based on Steiger's (1980) approach.
#' The test statistic \eqn{z} is calculated as
#' \deqn{z = \frac{(Z_{jk} - Z_{jh})\sqrt{n - 3}}{\sqrt{2 - 2c}}}{z = ((Z_{jk} - Z_{jh})\sqrt(n - 3))/(\sqrt(2 - 2c))}
#' (Hittner et al., 2003, p. 153), where
#' \deqn{c = \frac{r_{kh}(1 - 2\bar r_z^2) - \frac{1}{2}\bar r_z^2(1 - 2\bar r_z^2 - r_{kh}^2)}{(1 - \bar r_z^2)^2}}{c = (r_{kh}(1 - 2\bar{r}_z^2) - (1/2)\bar{r}_z^2(1 - 2\bar{r}_z^2 - r_{kh}^2))/((1 - \bar{r}_z^2)^2)}
#' (Hittner et al., 2003, p. 153),
#' \deqn{\bar r_z = \frac{exp(2\bar Z - 1)}{exp(2\bar Z + 1)}}{\bar{r}_z = (exp(2\bar{Z} - 1))/(exp(2\bar{Z} + 1))}
#' (Silver and Dunlap, 1987, p. 146, formula 4), and
#' \deqn{\bar Z = \frac{Z_{jk} + Z_{jh}}{2}}{\bar{Z} = (Z_{jk} + Z_{jh})/2}
#' (Silver and Dunlap, 1987, p. 146).
#'}
#'
### zou2007
#'\item{zou2007:}{
#'\emph{Zou's (2007) confidence interval}
#'
#' This test calculates the confidence interval of the difference between the two correlation coefficients \eqn{r_{jk}} and \eqn{r_{jh}}.
#' If the confidence interval includes zero, the null hypothesis that the two correlations are equal must be retained.
#' If the confidence interval does not include zero, the null hypothesis has to be rejected.
#' A lower and upper bound for the interval (\eqn{L} and \eqn{U}, respectively) is given by
#' \deqn{L = r_{jk} - r_{jh} - \sqrt{(r_{jk} - l_1)^2 + (u_2 - r_{jh})^2 - 2c(r_{jk} - l_1)(u_2 - r_{jh})}}{L = r_{jk} - r_{jh} - \sqrt((r_{jk} - l_1)^2 + (u_2 - r_{jh})^2 - 2c(r_{jk} - l_1)(u_2 - r_{jh}))}
#' and
#' \deqn{U = r_{jk} - r_{jh} - \sqrt{(u_1 - r_{jk})^2 + (r_{jh} - l_2)^2 - 2c(u_1 - r_{jk})(r_{jh} - l_2)}}{U = r_{jk} - r_{jh} - \sqrt((u_1 - r_{jk})^2 + (r_{jh} - l_2)^2 - 2c(u_1 - r_{jk})(r_{jh} - l_2))}
#' (Zou, 2007, p. 409), where
#' \deqn{l = \frac{exp(2l') - 1}{exp(2l') + 1},}{l = (exp(2l') - 1)/(exp(2l') + 1),}
#' \deqn{u = \frac{exp(2u') - 1}{exp(2u') + 1}}{u = (exp(2u') - 1)/(exp(2u') + 1)}
#' (Zou, 2007, p. 406),
#' \deqn{c = \frac{(r_{kh} - \frac{1}{2} r_{jk} r_{jh})(1 - r_{jk}^2- r_{jh}^2- r_{kh}^2) + r_{kh}^3}{(1 - r_{jk}^2)(1 - r_{jh}^2)}}{c = ((r_{kh} - (1/2) r_{jk} r_{jh})(1 - r_{jk}^2- r_{jh}^2- r_{kh}^2) + r_{kh}^3)/((1 - r_{jk}^2)(1 - r_{jh}^2))}
#' (Zou, 2007, p. 409), and
#' \deqn{l',u' = Z \pm z_{\frac{\alpha}{2}} \sqrt{\frac{1}{n - 3}}}{l',u' = Z +- z_{\alpha/2} \sqrt(1/(n - 3))}
#' (Zou, 2007, p. 406).
#' \eqn{\alpha} denotes the desired alpha level of the confidence interval.
#'}
#'
#'}
#'
#' @param r.jk A number specifying the correlation between \eqn{j} and \eqn{k} (this correlation is used for comparison)
#' @param r.jh A number specifying the correlation between \eqn{j} and \eqn{h} (this correlation is used for comparison)
#' @param r.kh A number specifying the correlation between \eqn{k} and \eqn{h}
#' @param n An integer defining the size of the group
#' @param alternative A character string specifying whether the alternative hypothesis is two-sided ("\code{two.sided}"; default) or one-sided ( "\code{greater}" or "\code{less}", depending on the direction). Optionally, the initial letter of the character strings ("\code{t}", "\code{g}", and "\code{l})" can be used.
#' @param test A vector of character strings specifying the tests to be used (\code{pearson1898}, \code{hotelling1940}, \code{hendrickson1970}, \code{williams1959}, \code{olkin1967}, \code{dunn1969}, \code{steiger1980}, \code{meng1992}, \code{hittner2003}, or \code{zou2007}). Use \code{all} to apply all tests (default). For further information see the tests section below.
#' @param alpha A number defining the alpha level for the hypothesis test. The default value is \eqn{.05}.
#' @param conf.level A number defining the level of confidence for the confidence interval (if test \code{meng1992} or \code{zou2007} is used). The default value is \eqn{.95}.
#' @param null.value A number defining the hypothesized difference between the two correlations used for testing the null hypothesis. The default value is \eqn{0}. If the value is other than \eqn{0}, only the test \code{zou2007} that uses a confidence interval is available.
#' @param data.name A character string giving the name(s) of the data. If \code{data.name} is \code{NULL}, the data names of \code{r.jk}, \code{r.jh}, and \code{r.kh} are used.
#' @param var.labels A vector of 3 character strings specifying the labels for j, k, and h (in this order).
#' @param return.htest A logical indicating whether the result should be returned as a list containing a list of class 'htest' for each test. The default value is \code{FALSE}.
#'
#' @return Returns an object of the class 'cocor.dep.groups.overlap' with the following slots holding the input parameters described above:
#' \item{r.jk}{Input parameter}
#' \item{r.jh}{Input parameter}
#' \item{r.kh}{Input parameter}
#' \item{n}{Input parameter}
#' \item{alternative}{Input parameter}
#' \item{alpha}{Input parameter}
#' \item{conf.level}{Input parameter}
#' \item{null.value}{Input parameter}
#' \item{data.name}{Input parameter}
#' \item{var.labels}{Input parameter}
#' For each test a slot of the same name exists with a list containing the following elements:
#' \item{statistic}{The value of the test statistic (unless test \code{zou2007} is used).}
#' \item{distribution}{The distribution of the test statistic (unless test \code{zou2007} is used).}
#' \item{df}{The degrees of freedom of the distribution of the test statistic (if test \code{hotelling1940}, \code{hendrickson1970}, or \code{williams1959} is used).}
#' \item{p.value}{The p-value of the test (unless test \code{zou2007} is used).}
#' \item{conf.int}{The confidence interval of the difference between the two correlations (if test \code{meng1992} or \code{zou2007} is used).}
#'
#' @references
#' Boyer, I. E., Palachek, A. D., & Schucany. W. R. (1983). An empirical study of related correlation coefficients.  \emph{Journal of Educational Statistics},  \emph{8}, 75-86. doi:10.2307/1164871
#'
#' Dunn, O. J. & Clark, V. A. (1969). Correlation coefficients measured on the same individuals. \emph{Journal of the American Statistical Association}, \emph{64}, 366-377. doi:10.2307/2283746
#'
#' Dunn, O. J. & Clark, V. A. (1971). Comparison of tests of the equality of dependent correlation coefficients. \emph{Journal of the American Statistical Association}, \emph{66}, 904-908. doi:10.2307/2284252
#'
#' Fisher, R. A. (1921). On the probable error of a coefficient of correlation deduced from a small sample. \emph{Metron}, \emph{1}, 1-32.
#'
#' Glass, G. V., & Stanley, J. C. (1970). \emph{Statistical methods in eduction and psychology}. Englewood Cliffs, NJ: Prentice-Hall.
#'
#' Glass, G. V., & Stanley, J. C. (1984). \emph{Statistical methods in eduction and psychology (2nd ed.)}. Englewood Cliffs, NJ: Prentice-Hall.
#'
#' Hendrickson, G. F., Stanley J. C., & Hills, J. R. (1970). Olkin's new formula for significance of r13 vs. r23 compared with Hotelling's method. \emph{American Educational Research Journal}, \emph{7}, 189-195. doi:10.2307/1162159
#'
#' Hendrickson, G. F., & Collins, J. R. (1970). Note correcting the results in "Olkin's new formula for the significance of r13 vs. r23 compared with Hotelling's method". \emph{American Educational Research Journal}, \emph{7}, 639-641. doi:10.2307/1161847
#'
#' Hittner, J. B., & May, K. (1998). DEPCORR: A SAS program for comparing dependent correlations. Applied Psychological Measurement, 22, 93-94. doi:10.1177/01466216980221010
#'
#' Hittner, J. B., May, K., & Silver, N. C. (2003). A Monte Carlo evaluation of tests for comparing dependent correlations. \emph{The Journal of General Psychology}, \emph{130}, 149-168. doi:10.1080/00221300309601282
#'
#' Hotelling, H. (1940). The selection of variates for use in prediction, with some comments on the general problem of nuisance parameters. \emph{Annals of Mathematical Statistics}, \emph{11}, 271-283. doi:10.1214/aoms/1177731867
#'
#' May, K., & Hittner, J. B., (1997a) - A note on statistics for comparing dependent correlations. \emph{Psychological Reports}, \emph{80}, 475-480. doi:10.2466/pr0.1997.80.2.475
#'
#' May, K., & Hittner, J. B. (1997b). Tests for comparing dependent correlations revisited: A Monte Carlo study. \emph{The Journal of Experimental Education}, \emph{65}, 257-269. doi:10.1080/00220973.1997.9943458
#'
#' Meng, X. L., Rosenthal, R., & Rubin, D. B. (1992). Comparing correlated correlation coefficients. \emph{Psychological Bulletin}, \emph{111}, 172-175. doi:10.1037//0033-2909.111.1.172
#'
#' Neill, J. J., & Dunn, O. J. (1975). Equality of dependent correlation coefficients. \emph{Biometrics}, \emph{31}, 531-543. doi:10.2307/2529435
#'
#' Olkin, I. (1967). Correlations revisited. In J. C. Stanley (Ed.), \emph{Improving experimental design and statistical analysis} (pp. 102-128). Chicago, IL: Rand McNally.
#'
#' Pearson, K., & Filon, L. N. G. (1898). Mathematical contributions to theory of evolution: IV. On the probable errors of frequency constants and on the influence of random selection and correlation. \emph{Philosophical Transactions of the Royal Society of London, Series A}, \emph{191}, 229-311. doi:10.1098/rsta.1898.0007
#'
#' Silver, N. C , & Dunlap, W. P. (1987). Averaging correlation coefficients: Should Fisher's Z transformation be used? \emph{Journal of Applied Psychology}, \emph{72}, 146-148. doi:10.1037//0021-9010.72.1.146
#'
#' Silver, N. C., Hittner, J. B., & May, K. (2004). Testing dependent correlations with nonoverlapping variables: A Monte Carlo simulation. \emph{Journal of Experimental Education}, \emph{73}, 53-69. doi:10.3200/JEXE.71.1.53-70
#'
#' Silver, N. C., Hittner, J. B., & May, K. (2006). A FORTRAN 77 program for comparing dependent correlations. \emph{Applied Psychological Measurement}, \emph{30}, 152-153. doi:10.1177/0146621605277132
#'
#' Steiger, J. H. (1980). Tests for comparing elements of a correlation matrix. \emph{Psychological Bulletin}, \emph{87}, 245-251. doi:10.1037//0033-2909.87.2.245
#'
#' Wilcox, R. R., & Tian, T. (2008). Comparing dependent correlations. \emph{The Journal of General Psychology}, \emph{135}, 105-112. doi:10.3200/GENP.135.1.105-112
#'
#' Williams, E. J. (1959). The comparison of regression variables. \emph{Journal of the Royal Statistical Society, Series B}, \emph{21}, 396-399. Retrieved from http://www.jstor.org/stable/2983809
#'
#' Zou, G. Y. (2007). Toward using confidence intervals to compare correlations. \emph{Psychological Methods}, \emph{12}, 399-413. doi:10.1037/1082-989X.12.4.399
#'
#' @seealso
#' \link{cocor}, \link{cocor.indep.groups}, \link{cocor.dep.groups.nonoverlap}, \link{as.htest}
#'
#' @examples
#' # Compare the difference between the correlations (age, intelligence) and
#' # (age, shoe size) measured in the same group (all values are fictional):
#' r.jk <- .2  # Correlation (age, intelligence)
#' r.jh <- .5  # Correlation (age, shoe size)
#' r.kh <- .1  # Correlation (intelligence, shoe size)
#' n <- 315  # Size of the group
#'
#' cocor.dep.groups.overlap(r.jk, r.jh, r.kh, n, var.labels=c("age", "intelligence", "shoe size"))
#'
#' @export
cocor.dep.groups.overlap <- function(r.jk, r.jh, r.kh, n, alternative="two.sided", test="all", alpha=.05, conf.level=.95, null.value=0, data.name=NULL, var.labels=NULL, return.htest=FALSE) {
  for(x in c("r.jk", "r.jh", "r.kh")) {
    validate.numeric(get(x), x)
    validate.numeric.range(get(x), x, -1, 1)
  }

  validate.numeric(n, "n", "integer")
  validate.numeric.range(n, "n", 0, Inf)

  for(x in c("alpha", "conf.level")) {
    validate.numeric(get(x), x)
    validate.numeric.range(get(x), x, 0, 1)
  }
  alternative <- validate.alternative(alternative)

  validate.numeric.range(null.value, "null.value", -2, 2)

  if(any(test == "all")) test <- c("pearson1898", "hotelling1940", "williams1959", "olkin1967", "dunn1969", "hendrickson1970", "steiger1980", "meng1992", "hittner2003", "zou2007")
  test <- validate.test(test, null.value)

  result <- new("cocor.dep.groups.overlap",
    r.jk=r.jk,
    r.jh=r.jh,
    r.kh=r.kh,
    n=n,
    alternative=alternative,
    test=test,
    alpha=alpha,
    conf.level=conf.level,
    null.value=null.value
  )
  if(!is.null(data.name)) {
    validate.character(data.name, "data.name", 1)
    result@data.name <- data.name
  }
  if(!is.null(var.labels)) {
    validate.character(var.labels, "var.labels", 3)
    result@var.labels <- var.labels
  }

  for(x in test) {
    switch(x,
      pearson1898={
        k <- r.kh * (1 - r.jk^2 - r.jh^2) - 0.5 * r.jk * r.jh * (1 - r.jk^2 - r.jh^2 - r.kh^2)
        z.enum <- sqrt(n) * (r.jk - r.jh)
        z.denom <- sqrt((1 - r.jk^2)^2 + (1 - r.jh^2)^2 - 2 * k)

        statistic <- z.enum / z.denom
        distribution <- "z"
        p.value <- get.p.value(statistic, distribution, alternative)
        result@pearson1898 <- list(distribution=distribution, statistic=statistic, p.value=p.value)
      },
      hotelling1940={
        t.enum <- (r.jk - r.jh) * sqrt((n - 3) * (1 + r.kh))
        t.denom <- sqrt(2 * R(r.jk, r.jh, r.kh))

        statistic <- t.enum / t.denom
        distribution <- "t"
        df <- n - 3

        p.value <- get.p.value(statistic, distribution, alternative, df)
        result@hotelling1940 <- list(distribution=distribution, df=df, statistic=statistic, p.value=p.value)
      },
      williams1959={
        r.mean <- (r.jk + r.jh) / 2

        statistic <- (r.jk - r.jh) * sqrt(((n - 1) * (1 + r.kh)) / (2 * ((n - 1) / (n - 3)) * R(r.jk, r.jh, r.kh) + r.mean^2 * (1 - r.kh)^3))
        distribution <- "t"
        df <- n - 3

        p.value <- get.p.value(statistic, distribution, alternative, df)
        result@williams1959 <- list(distribution=distribution, df=df, statistic=statistic, p.value=p.value)
      },
      olkin1967={
        z.enum <- (r.jk - r.jh) * sqrt(n)
        z.denom <- sqrt((1 - r.jk^2)^2 + (1 - r.jh^2)^2 - 2 * r.kh^3 - (2 * r.kh - r.jk * r.jh) * (1 - r.jk^2 - r.jh^2 - r.kh^2))

        statistic <- z.enum / z.denom
        distribution <- "z"

        p.value <- get.p.value(statistic, distribution, alternative)
        result@olkin1967 <- list(distribution=distribution, statistic=statistic, p.value=p.value)
      },
      dunn1969={
        covariance.enum <- r.kh * (1 - r.jk^2 - r.jh^2) - 0.5 * (r.jk * r.jh) * (1 - r.jk^2 - r.jh^2 - r.kh^2)
        covariance.denom <- (1 - r.jk^2) * (1 - r.jh^2)
        covariance <- covariance.enum / covariance.denom
        z.enum <- sqrt(n - 3) * (fisher.r2z(r.jk) - fisher.r2z(r.jh))
        z.denom <- sqrt(2 - 2 * covariance)

        statistic <- z.enum / z.denom
        distribution <- "z"

        p.value <- get.p.value(statistic, distribution, alternative)
        result@dunn1969 <- list(distribution=distribution, statistic=statistic, p.value=p.value)
      },
      hendrickson1970={
        t.enum <- (r.jk - r.jh) * sqrt((n - 3) * (1 + r.kh))
        t.denom <- sqrt(2 * R(r.jk, r.jh, r.kh) + (((r.jk - r.jh)^2 * (1 - r.kh)^3) / (4 * (n - 1))))

        statistic <- t.enum / t.denom
        distribution <- "t"
        df <- n - 3

        p.value <- get.p.value(statistic, distribution, alternative, df)
        result@hendrickson1970 <- list(distribution=distribution, df=df, statistic=statistic, p.value=p.value)
      },
      steiger1980={
        r.p <- (r.jk + r.jh) / 2 # pool r
        covariance.enum <- r.kh * (1 - 2 * r.p^2) - 0.5 * r.p^2 * (1 - 2 * r.p^2 - r.kh^2)
        covariance.denom <- (1 - r.p^2)^2
        covariance <- covariance.enum / covariance.denom
        z.enum <- sqrt(n - 3) * (fisher.r2z(r.jk) - fisher.r2z(r.jh))
        z.denom <- sqrt(2 - 2 * covariance)

        statistic <- z.enum / z.denom
        distribution <- "z"

        p.value <- get.p.value(statistic, distribution, alternative)
        result@steiger1980 <- list(distribution=distribution, statistic=statistic, p.value=p.value)
      },
      meng1992={
        r.squared.mean <- (r.jk^2 + r.jh^2) / 2
        f <- (1 - r.kh) / (2 * (1 - r.squared.mean))
        if(!is.nan(f) && f > 1) f <- 1
        h <- (1 - f * r.squared.mean) / (1 - r.squared.mean)
        z.difference <- fisher.r2z(r.jk) - fisher.r2z(r.jh)

        statistic <- z.difference * sqrt((n - 3) / (2 * (1 - r.kh) * h))
        distribution <- "z"

        conf.int.term <- qnorm((1 - conf.level)/2, lower.tail=FALSE) * sqrt((2 * (1 - r.kh) * h) / (n - 3))
        conf.int <- c(z.difference - conf.int.term, z.difference + conf.int.term)

        p.value <- get.p.value(statistic, distribution, alternative)
        result@meng1992 <- list(distribution=distribution, statistic=statistic, p.value=p.value, conf.int=conf.int)
      },
      hittner2003={
        r.bt <- fisher.z2r((fisher.r2z(r.jk) + fisher.r2z(r.jh)) / 2) # backtransform pooled Z-transformed rs
        covariance <- (r.kh * (1 - 2 * r.bt^2) - 0.5 * r.bt^2 * (1 - 2 * r.bt^2 - r.kh^2))/(1 - r.bt^2)^2
        z.enum <- sqrt(n - 3) * (fisher.r2z(r.jk) - fisher.r2z(r.jh))
        z.denom <- sqrt(2 - 2 * covariance)

        statistic <- z.enum / z.denom
        distribution <- "z"

        p.value <- get.p.value(statistic, distribution, alternative)
        result@hittner2003 <- list(distribution=distribution, statistic=statistic, p.value=p.value)
      },
      zou2007={
        x <- qnorm((1 - conf.level)/2, lower.tail=FALSE) * sqrt(1/(n - 3))

        c <- ((r.kh - 0.5 * r.jk * r.jh) * (1 - r.jk^2 - r.jh^2 - r.kh^2) + r.kh^3)/((1 - r.jk^2) * (1 - r.jh^2))

        l1 <- fisher.z2r(fisher.r2z(r.jk) - x)
        u1 <- fisher.z2r(fisher.r2z(r.jk) + x)

        l2 <- fisher.z2r(fisher.r2z(r.jh) - x)
        u2 <- fisher.z2r(fisher.r2z(r.jh) + x)

        L <- r.jk - r.jh - sqrt((r.jk - l1)^2 + (u2 - r.jh)^2 - 2 * c * (r.jk - l1) * (u2 - r.jh))
        U <- r.jk - r.jh + sqrt((u1 - r.jk)^2 + (r.jh - l2)^2 - 2 * c * (u1 - r.jk) * (r.jh - l2))

        conf.int <- c(L, U)
        result@zou2007 <- list(conf.int=conf.int)
      },
      stop(paste("Test '", x, "' not found", sep=""))
    )
  }

  if(return.htest) return(as.htest(result))
  result
}
