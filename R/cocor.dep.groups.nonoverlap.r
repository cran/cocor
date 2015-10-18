#' Compare two nonoverlapping correlations based on dependent groups
#'
#' Performs a test of significance for the difference between two correlations based on dependent groups (e.g., the same group). The two correlations are nonoverlapping, i.e., they have no variable in common. The function tests whether the correlations between \code{j} and \code{k} (\code{r.jk}) and between \code{h} and \code{m} (\code{r.hm}) differ in magnitude. Because the significance depends on the pairwise intercorrelations between all of the variables involved (\code{j}, \code{k}, \code{h}, and \code{m}), these intercorrelations have to be provided as additional parameters. The function expects correlation coefficients as input.
#'
#'@section Tests:
#' In the following, \eqn{r_{jk}} and \eqn{r_{hm}} are the two correlations that are being compared; \eqn{Z_{jk}} and \eqn{Z_{hm}} are their \eqn{Z} transformed equivalents.
#' \eqn{r_{jh}}, \eqn{r_{kh}}, \eqn{r_{jm}}, and \eqn{r_{km}} are the related correlations that are also required.
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
#' This test was proposed by Pearson and Filon (1898, p. 262, formula xl).
#' The formula for the test statistic \eqn{z} is computed as
#' \deqn{z = \frac{\sqrt{n} (r_{jk} - r_{hm})}{\sqrt{(1 - r_{jk}^2)^2 + (1 - r_{hm}^2)^2 - k}}}{z = (\sqrt(n) (r_{jk} - r_{hm}))/(\sqrt((1 - r_{jk}^2)^2 + (1 - r_{hm}^2)^2 - k))}
#' (Raghunathan, Rosenthal, and Rubin, 1996, p. 179, formula 1), where
#' \deqn{k = (r_{jh} - r_{jk} r_{kh}) (r_{km} - r_{kh} r_{hm}) + (r_{jm} - r_{jh} r_{hm}) (r_{kh} - r_{jk} r_{jh})}{k = (r_{jh} - r_{jk} r_{kh}) (r_{km} - r_{kh} r_{hm}) + (r_{jm} - r_{jh} r_{hm}) (r_{kh} - r_{jk} r_{jh}) + (r_{jh} - r_{jm} r_{hm}) (r_{km} - r_{jk} r_{jm}) + (r_{jm} - r_{jk} r_{km}) (r_{kh} - r_{km} r_{hm})}
#' \deqn{+ (r_{jh} - r_{jm} r_{hm}) (r_{km} - r_{jk} r_{jm}) + (r_{jm} - r_{jk} r_{km}) (r_{kh} - r_{km} r_{hm})}{}
#' (Raghunathan et al. (1996, p. 179, formula 2).
#' The two formulae can also be found in Steiger (1980, p. 245, formula 2 and p. 246, formula 5).
#'}
#'
### dunn1969
#'\item{dunn1969:}{
#'\emph{Dunn and Clark's (1969) z}
#'
#' The test statistic \eqn{z} of this test is calculated as
#' \deqn{z = \frac{(Z_{jk} - Z_{hm})\sqrt{n - 3}}{\sqrt{2 - 2c}}}{z = ((Z_{jk} - Z_{hm})\sqrt(n - 3))/(\sqrt(2 - 2c))}
#' (Dunn and Clark, 1969, p. 370, formula 15), where
#' \deqn{c = \Bigl(\frac{1}{2} r_{jk} r_{hm} (r_{jh}^2 + r_{jm}^2 + r_{kh}^2 + r_{km}^2) + r_{jh} r_{km} + r_{jm} r_{kh}}{c = ((1/2) r_{jk} r_{hm} (r_{jh}^2 + r_{jm}^2 + r_{kh}^2 + r_{km}^2) + r_{jh} r_{km} + r_{jm} r_{kh} - (r_{jk} r_{jh} r_{jm} + r_{jk} r_{kh} r_{km} + r_{jh} r_{kh} r_{hm} + r_{jm} r_{km} r_{hm}))/((1 - r_{jk}^2)(1 - r_{hm}^2))}
#' \deqn{-(r_{jk} r_{jh} r_{jm} + r_{jk} r_{kh} r_{km} + r_{jh} r_{kh} r_{hm} + r_{jm} r_{km} r_{hm})\Bigr)}{}
#' \deqn{\Big/ \Bigl((1 - r_{jk}^2)(1 - r_{hm}^2)\Bigr)}{}
#' (Dunn and Clark, 1969, p. 368, formula 9).
#'}
#'
### steiger1980
#'\item{steiger1980:}{
#'\emph{Steiger's (1980) modification of Dunn and Clark's (1969) z using average correlations}
#'
#' This test was proposed by Steiger (1980) and is a modification of Dunn and Clark's (1969) \eqn{z}.
#' Instead of \eqn{r_{jk}} and \eqn{r_{hm}} the mean of the two is being used.
#' The test statistic \eqn{z} is given by
#' \deqn{z = \frac{(Z_{jk} - Z_{hm})\sqrt{n - 3}}{\sqrt{2 - 2c}}}{z = ((Z_{jk} - Z_{hm})\sqrt(n - 3))/(\sqrt(2 - 2c))}
#' (Steiger, 1980, p. 247, formula 15), where
#' \deqn{\bar r = \frac{r_{jk} + r_{hm}}{2}}{\bar{r} = (r_{jk} + r_{hm})/2}
#' (Steiger, 1980, p. 247) and
#' \deqn{c = \frac{\frac{1}{2} \bar r^2 (r_{jh}^2 + r_{jm}^2 + r_{kh}^2 + r_{km}^2) + r_{jh} r_{km} + r_{jm} r_{kh} - (\bar r r_{jh} r_{jm} + \bar r r_{kh} r_{km} + r_{jh} r_{kh} \bar r + r_{jm} r_{km} \bar r)}{(1 - \bar r^2)^2}}{c = ((1/2) \bar{r}^2 (r_{jh}^2 + r_{jm}^2 + r_{kh}^2 + r_{km}^2) + r_{jh} r_{km} + r_{jm} r_{kh} - (\bar{r} r_{jh} r_{jm} + \bar{r} r_{kh} r_{km} + r_{jh} r_{kh} \bar{r} + r_{jm} r_{km} \bar{r}))/((1 - \bar{r}^2)^2)}
#' (Steiger, 1980, p. 247, formula 11; in the original article, there are brackets missing around the divisor).
#'}
#'
### raghunathan1996
#'\item{raghunathan1996:}{
#'\emph{Raghunathan, Rosenthal, and Rubin's (1996) modification of Pearson and Filon's (1898) z}
#'
#' This test of Raghunathan, Rosenthal, and Rubin (1996) is based on Pearson and Filon's (1898) \eqn{z}.
#' Unlike Pearson and Filon (1898), Raghunathan et al. (1996) use \eqn{Z} transformed correlation coefficients.
#' The test statistic \eqn{z} is computed as
#' \deqn{z = \sqrt{\frac{n - 3}{2}} \frac{Z_{jk} - Z_{hm}}{\sqrt{1 - \frac{k}{2(1 - r_{jk}^2)(1 - r_{hm}^2)}}}}{z = \sqrt((n - 3)/2)(Z_{jk} - Z_{hm})/(\sqrt(1 - k/(2(1 - r_{jk}^2)(1 - r_{hm}^2))))}
#' (Raghunathan et al., 1996, p. 179, formula 3), where
#' \deqn{k = (r_{jh} - r_{jk} r_{kh}) (r_{km} - r_{kh} r_{hm}) + (r_{jm} - r_{jh} r_{hm}) (r_{kh} - r_{jk} r_{jh})}{k = (r_{jh} - r_{jk} r_{kh}) (r_{km} - r_{kh} r_{hm}) + (r_{jm} - r_{jh} r_{hm}) (r_{kh} - r_{jk} r_{jh}) + (r_{jh} - r_{jm} r_{hm}) (r_{km} - r_{jk} r_{jm}) + (r_{jm} - r_{jk} r_{km}) (r_{kh} - r_{km} r_{hm})}
#' \deqn{+ (r_{jh} - r_{jm} r_{hm}) (r_{km} - r_{jk} r_{jm}) + (r_{jm} - r_{jk} r_{km}) (r_{kh} - r_{km} r_{hm})}{}
#' (Raghunathan et al., 1996, p. 179, formula 2).
#'}
#'
### silver2004
#'\item{silver2004:}{
#'\emph{Silver, Hittner, and May's (2004) modification of Dunn and Clark's (1969) z using a backtransformed average Fisher's (1921) Z procedure}
#'
#' The approach to backtransform averaged Fisher's (1921) \eqn{Z}s was first proposed in Silver and Dunlap (1987) and was applied to the comparison of nonoverlapping correlations by Silver et al. (2004).
#' The test is based on Steiger's (1980) approach.
#' The formula of the test statistic \eqn{z} is given by
#' \deqn{z = \frac{(Z_{jk} - Z_{hm})\sqrt{n - 3}}{\sqrt{2 - 2c}}}{z = ((Z_{jk} - Z_{hm})\sqrt(n - 3))/(\sqrt(2 - 2c))}
#' (Silver et al., 2004, p. 55, formula 5), where
#' \deqn{c = \frac{\frac{1}{2} \bar r_z^2 (r_{jh}^2 + r_{jm}^2 + r_{kh}^2 + r_{km}^2) + r_{jh} r_{km} + r_{jm} r_{kh} - (\bar r_z r_{jh} r_{jm} + \bar r_z r_{kh} r_{km} + r_{jh} r_{kh} \bar r_z + r_{jm} r_{km} \bar r_z)}{(1 - \bar r_z^2)^2}}{c = ((1/2) \bar{r}_z^2 (r_{jh}^2 + r_{jm}^2 + r_{kh}^2 + r_{km}^2) + r_{jh} r_{km} + r_{jm} r_{kh} - (\bar{r}_z r_{jh} r_{jm} + \bar{r}_z r_{kh} r_{km} + r_{jh} r_{kh} \bar{r}_z + r_{jm} r_{km} \bar{r}_z))/((1 - \bar{r}_z^2)^2)}
#' (Silver et al., 2004, p. 56),
#' \deqn{\bar r_z = \frac{exp(2\bar Z - 1)}{exp(2\bar Z + 1)}}{\bar{r}_z = (exp(2\bar{Z} - 1))/(exp(2\bar{Z} + 1))}
#' (Silver and Dunlap, 1987, p. 146, formula 4), and
#' \deqn{\bar Z = \frac{Z_{jk} + Z_{hm}}{2}}{\bar{Z} = (Z_{jk} + Z_{hm})/2}
#' (Silver et al., 2004, p. 55).
#'}
#'
### zou2007
#'\item{zou2007:}{
#'\emph{Zou's (2007) confidence interval}
#'
#' This test calculates the confidence interval of the difference between the two correlations \eqn{r_{jk}} and \eqn{r_{hm}}.
#' If the confidence interval includes zero, the null hypothesis that the two correlations are equal must be retained.
#' If the confidence interval does not include zero, the null hypothesis has to be rejected.
#' A lower and upper bound for the interval (\eqn{L} and \eqn{U}, respectively) is given by
#' \deqn{L = r_{jk} - r_{hm} - \sqrt{(r_{jk} - l_1)^2 + (u_2 - r_{hm})^2 - 2c(r_{jk} - l_1)(u_2 - r_{hm})}}{L = r_{jk} - r_{hm} - \sqrt((r_{jk} - l_1)^2 + (u_2 - r_{hm})^2 - 2c(r_{jk} - l_1)(u_2 - r_{hm}))}
#' and
#' \deqn{U = r_{jk} - r_{hm} + \sqrt{(u_1 - r_{jk})^2 + (r_{hm} - l_2)^2 - 2c(u_1 - r_{jk})(r_{hm} - l_2)}}{U = r_{jk} - r_{hm} + \sqrt((u_1 - r_{jk})^2 + (r_{hm} - l_2)^2 - 2c(u_1 - r_{jk})(r_{hm} - l_2))}
#' (Zou, 2007, pp. 409-410), where
#' \deqn{l = \frac{exp(2l') - 1}{exp(2l') + 1},}{l = (exp(2l') - 1)/(exp(2l') + 1),}
#' \deqn{u = \frac{exp(2u') - 1}{exp(2u') + 1}}{u = (exp(2u') - 1)/(exp(2u') + 1)}
#' (Zou, 2007, p. 406),
#' \deqn{c = \Bigl(\frac{1}{2} r_{jk} r_{hm} (r_{jh}^2 + r_{jm}^2 + r_{kh}^2 + r_{km}^2) + r_{jh} r_{km} + r_{jm} r_{kh}}{c = ((1/2) r_{jk} r_{hm} (r_{jh}^2 + r_{jm}^2 + r_{kh}^2 + r_{km}^2) + r_{jh} r_{km} + r_{jm} r_{kh} - (r_{jk} r_{jh} r_{jm} + r_{jk} r_{kh} r_{km} + r_{jh} r_{kh} r_{hm} + r_{jm} r_{km} r_{hm}))/((1 - r_{jk}^2)(1 - r_{hm}^2))}
#' \deqn{- (r_{jk} r_{jh} r_{jm} + r_{jk} r_{kh} r_{km} + r_{jh} r_{kh} r_{hm} + r_{jm} r_{km} r_{hm})\Bigr)}{}
#' \deqn{\Big/ \Bigl((1 - r_{jk}^2)(1 - r_{hm}^2)\Bigr)}{}
#' (Zou, 2007, p. 409), and
#' \deqn{l',u' = Z \pm z_{\frac{\alpha}{2}} \sqrt{\frac{1}{n - 3}}}{l',u' = Z +- z_{\alpha/2} \sqrt(1/(n - 3))}
#' (Zou, 2007, p. 406).
#' \eqn{\alpha} denotes the desired alpha level of the confidence interval.
#'}
#'
#'}
#'
#' @param r.jk A number specifying the correlation between \eqn{j} and \eqn{k} (this correlation is used for comparison)
#' @param r.hm A number specifying the correlation between \eqn{h} and \eqn{m} (this correlation is used for comparison)
#' @param r.jh A number specifying the correlation between \eqn{j} and \eqn{h}
#' @param r.jm A number specifying the correlation between \eqn{j} and \eqn{m}
#' @param r.kh A number specifying the correlation between \eqn{k} and \eqn{h}
#' @param r.km A number specifying the correlation between \eqn{k} and \eqn{m}
#' @param n An integer defining the size of the group
#' @param alternative A character string specifying whether the alternative hypothesis is two-sided ("\code{two.sided}"; default) or one-sided ("\code{greater}" or "\code{less}", depending on the direction). Optionally, the initial letter of the character strings ("\code{t}", "\code{g}", and "\code{l})" can be used.
#' @param test A vector of character strings specifying the tests to be used (\code{pearson1898}, \code{dunn1969}, \code{steiger1980}, \code{raghunathan1996}, \code{silver2004}, or \code{zou2007}). Use \code{all} to apply all tests (default). For further information see the tests section below.
#' @param alpha A number defining the alpha level for the hypothesis test. The default value is \eqn{.05}.
#' @param conf.level A number defining the level of confidence for the confidence interval (if test \code{zou2007} is used). The default value is \eqn{.95}.
#' @param null.value A number defining the hypothesized difference between the two correlations used for testing the null hypothesis. The default value is \eqn{0}. If the value is other than \eqn{0}, only the test \code{zou2007} that uses a confidence interval is available.
#' @param data.name A character string giving the name of the data/group.
#' @param var.labels A vector of four character strings specifying the labels for j, k, h, and m (in this order).
#' @param return.htest A logical indicating whether the result should be returned as a list containing a list of class 'htest' for each test. The default value is \code{FALSE}.
#'
#' @return Returns an S4 object of class 'cocor.dep.groups.nonoverlap' with the following slots:
#' \item{r.jk}{Input parameter}
#' \item{r.hm}{Input parameter}
#' \item{r.jh}{Input parameter}
#' \item{r.jm}{Input parameter}
#' \item{r.kh}{Input parameter}
#' \item{r.km}{Input parameter}
#' \item{n}{Input parameter}
#' \item{alternative}{Input parameter}
#' \item{alpha}{Input parameter}
#' \item{conf.level}{Input parameter}
#' \item{null.value}{Input parameter}
#' \item{data.name}{Input parameter}
#' \item{var.labels}{Input parameter}
#' \item{diff}{Difference between the two correlations, r.jk and r.hm, that were compared}
#' For each test a slot of the same name exists with a list containing the following elements:
#' \item{statistic}{The value of the test statistic (unless test \code{zou2007} is used).}
#' \item{distribution}{The distribution of the test statistic (unless test \code{zou2007} is used).}
#' \item{p.value}{The p-value of the test (unless test \code{zou2007} is used).}
#' \item{conf.int}{The confidence interval of the difference between the two correlations (if test \code{zou2007} is used).}
#'
#' @references
#' Dunn, O. J., & Clark, V. A. (1969). Correlation coefficients measured on the same individuals. \emph{Journal of the American Statistical Association}, \emph{64}, 366-377. doi:10.2307/2283746
#'
#' Pearson, K., & Filon, L. N. G. (1898). Mathematical contributions to theory of evolution: IV. On the probable errors of frequency constants and on the influence of random selection and correlation. \emph{Philosophical Transactions of the Royal Society of London, Series A}, \emph{191}, 229-311. doi:10.1098/rsta.1898.0007
#'
#' Raghunathan, T. E., Rosenthal, R., & Rubin, D. B., (1996). Comparing correlated but nonoverlapping correlations. \emph{Psychological Methods}, \emph{1}, 178-183. doi:10.1037//1082-989X.1.2.178
#'
#' Silver, N. C., & Dunlap, W. P. (1987). Averaging correlation coefficients: Should Fisher's Z transformation be used? \emph{Journal of Applied Psychology}, \emph{72}, 146-148. doi:10.1037//0021-9010.72.1.146
#'
#' Silver, N. C., Hittner, J. B., & May, K. (2004). Testing dependent correlations with nonoverlapping variables: A Monte Carlo simulation. \emph{Journal of Experimental Education}, \emph{73}, 53-69. doi:10.3200/JEXE.71.1.53-70
#'
#' Steiger, J. H. (1980). Tests for comparing elements of a correlation matrix. \emph{Psychological Bulletin}, \emph{87}, 245-251. doi:10.1037//0033-2909.87.2.245
#'
#' Zou, G. Y. (2007). Toward using confidence intervals to compare correlations. \emph{Psychological Methods}, \emph{12}, 399-413. doi:10.1037/1082-989X.12.4.399
#'
#' @seealso
#' \link{cocor}, \link{cocor.indep.groups}, \link{cocor.dep.groups.overlap}, \link{as.htest}
#'
#' @examples
#' # Compare the difference between the correlations (age, intelligence) and
#' # body mass (index, shoe size) measured in the same group (all values are fictional):
#' r.jk <- .2  # Correlation (age, intelligence)
#' r.hm <- .7  # Correlation (body mass index, shoe size)
#' r.jh <- .4  # Correlation (age, body mass index)
#' r.jm <- .5  # Correlation (age, shoe size)
#' r.kh <- .1  # Correlation (intelligence, body mass index)
#' r.km <- .3  # Correlation (intelligence, shoe size)
#' n <- 232  # Size of the group
#'
#' cocor.dep.groups.nonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n,
#' var.labels=c("age", "intelligence", "body mass index", "shoe size"))
#'
#' @export
cocor.dep.groups.nonoverlap <- function(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n, alternative="two.sided", test="all", alpha=.05, conf.level=.95, null.value=0, data.name=NULL, var.labels=NULL, return.htest=FALSE) {
  for(x in c("r.jk", "r.hm", "r.jh", "r.jm", "r.kh", "r.km")) {
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

  if(any(test == "all")) test <- c("pearson1898", "dunn1969", "steiger1980", "raghunathan1996", "silver2004", "zou2007")
  test <- validate.test(test, null.value)

  result <- new("cocor.dep.groups.nonoverlap",
    r.jk=r.jk,
    r.hm=r.hm,
    r.jh=r.jh,
    r.jm=r.jm,
    r.kh=r.kh,
    r.km=r.km,
    n=n,
    alternative=alternative,
    test=test,
    alpha=alpha,
    conf.level=conf.level,
    null.value=null.value,
    diff=r.jk - r.hm
  )

  if(!is.null(data.name)) {
    validate.character(data.name, "data.name", 1)
    result@data.name <- data.name
  }

  if(!is.null(var.labels)) {
    validate.character(var.labels, "var.labels", 4)
    result@var.labels <- var.labels
  }

  validate.logical(return.htest, "return.htest")

  for(x in test) {
    switch(x,
      pearson1898={
        k <- (r.jh - r.jk * r.kh) * (r.km - r.kh * r.hm) + (r.jm - r.jh * r.hm) * (r.kh - r.jk * r.jh) + (r.jh - r.jm * r.hm) * (r.km - r.jk * r.jm) + (r.jm - r.jk * r.km) * (r.kh - r.km * r.hm)
        statistic <- (sqrt(n) * (r.jk - r.hm)) / sqrt(((1 - r.jk^2)^2 + (1 - r.hm^2)^2) - k)
        distribution <- "z"

        p.value <- get.p.value(statistic, distribution, alternative)
        result@pearson1898 <- list(distribution=distribution, statistic=statistic, p.value=p.value)
      },
      dunn1969={
        covariance.enum <- (r.jh - r.jk * r.kh) * (r.km - r.kh * r.hm) + (r.jm - r.jh * r.hm) * (r.kh - r.jk * r.jh) + (r.jh - r.jm * r.hm) * (r.km - r.jk * r.jm) + (r.jm - r.jk * r.km) * (r.kh - r.km * r.hm)
        covariance.denom <- 2 * (1 - r.jk^2) * (1 - r.hm^2)
        covariance <- covariance.enum / covariance.denom
        z.enum <- sqrt(n - 3) * (fisher.r2z(r.jk) - fisher.r2z(r.hm))
        z.denom <- sqrt(2 - 2 * covariance)

        statistic <- z.enum / z.denom
        distribution <- "z"

        p.value <- get.p.value(statistic, distribution, alternative)
        result@dunn1969 <- list(distribution=distribution, statistic=statistic, p.value=p.value)
      },
      steiger1980={
        r.p <- (r.jk + r.hm) / 2
        covariance.enum <- (r.jh - r.p * r.kh) * (r.km - r.kh * r.p) + (r.jm - r.jh * r.p) * (r.kh - r.p * r.jh) + (r.jh - r.jm * r.p) * (r.km - r.p * r.jm) + (r.jm - r.p * r.km) * (r.kh - r.km * r.p)
        covariance.denom <- 2 * (1 - r.p^2)^2
        covariance <- covariance.enum / covariance.denom
        z.enum <- sqrt(n - 3) * (fisher.r2z(r.jk) - fisher.r2z(r.hm))
        z.denom <- sqrt(2 - 2 * covariance)

        statistic <- z.enum / z.denom
        distribution <- "z"

        p.value <- get.p.value(statistic, distribution, alternative)
        result@steiger1980 <- list(distribution=distribution, statistic=statistic, p.value=p.value)
      },
      raghunathan1996={
        k <- (r.jh - r.jk * r.kh) * (r.km - r.kh * r.hm) + (r.jm - r.jh * r.hm) * (r.kh - r.jk * r.jh) + (r.jh - r.jm * r.hm) * (r.km - r.jk * r.jm) + (r.jm - r.jk * r.km) * (r.kh - r.km * r.hm)

        statistic <- sqrt((n - 3)/2) * (fisher.r2z(r.jk) - fisher.r2z(r.hm)) / sqrt(1 - (k / (2 * (1 - r.jk^2) * (1 - r.hm^2))))
        distribution <- "z"

        p.value <- get.p.value(statistic, distribution, alternative)
        result@raghunathan1996 <- list(distribution=distribution, statistic=statistic, p.value=p.value)
      },
      silver2004={
        r.bt <- fisher.z2r((fisher.r2z(r.jk) + fisher.r2z(r.hm)) / 2) # backtransform pooled Z-transformed rs
        covariance.enum <- (r.jh - r.bt * r.kh) * (r.km - r.kh * r.bt) + (r.jm - r.jh * r.bt) * (r.kh - r.bt * r.jh) + (r.jh - r.jm * r.bt) * (r.km - r.bt * r.jm) + (r.jm - r.bt * r.km) * (r.kh - r.km * r.bt)
        covariance.denom <- 2 * (1 - r.bt^2)^2
        covariance <- covariance.enum / covariance.denom
        z.enum <- sqrt(n - 3) * (fisher.r2z(r.jk) - fisher.r2z(r.hm))
        z.denom <- sqrt(2 - 2 * covariance)

        statistic <- z.enum / z.denom
        distribution <- "z"

        p.value <- get.p.value(statistic, distribution, alternative)
        result@silver2004 <- list(distribution=distribution, statistic=statistic, p.value=p.value)
      },
      zou2007={
        c <- (0.5 * r.jk * r.hm * (r.jh^2 + r.jm^2 + r.kh^2+ r.km^2) + r.jh * r.km + r.jm * r.kh
        - (r.jk * r.jh * r.jm + r.jk * r.kh * r.km + r.jh * r.kh * r.hm + r.jm * r.km * r.hm))/((1 - r.jk^2) * (1 - r.hm^2))

        x <- qnorm((1 - conf.level)/2, lower.tail=FALSE) * sqrt(1/(n - 3))

        l1 <- fisher.z2r(fisher.r2z(r.jk) - x)
        u1 <- fisher.z2r(fisher.r2z(r.jk) + x)

        l2 <- fisher.z2r(fisher.r2z(r.hm) - x)
        u2 <- fisher.z2r(fisher.r2z(r.hm) + x)

        L <- r.jk - r.hm - sqrt((r.jk - l1)^2 + (u2 - r.hm)^2 - 2 * c * (r.jk - l1) * (u2 - r.hm))
        U <- r.jk - r.hm + sqrt((u1 - r.jk)^2 + (r.hm - l2)^2 - 2 * c * (u1 - r.jk) * (r.hm - l2))

        conf.int <- c(L, U)
        result@zou2007 <- list(conf.int=conf.int)
      },
      stop(paste("Test '", x, "' not found", sep=""))
    )
  }

  if(return.htest) return(as.htest(result))
  result
}
