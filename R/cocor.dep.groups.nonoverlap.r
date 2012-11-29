#' Compare two nonoverlapping correlations based on dependent groups
#'
#' Performs a test of significance for the difference between two correlations based on dependent groups (e.g., the same group). The two correlations are nonoverlapping, i.e., they have no variable in common. The function tests whether the correlations between \code{j} and \code{k} (\code{r.jk}) and between \code{h} and \code{m} (\code{r.hm}) differ in magnitude. Because the significance depends on the pairwise intercorrelations between all of the variables involved (\code{j}, \code{k}, \code{h}, and \code{m}), these intercorrelations have to be provided as additional parameters.
#'
#'@section Methods:
#' Some methods make use of Fisher's r-to-z transformation (1921, p. 26):
#'
#' \deqn{Z = \frac{1}{2}(ln(1+r) - ln(1-r))}
#'
#'\describe{
### pearson1898
#'\item{pearson1898:}{
#'\emph{Pearson and Filon's z (1898)}
#'
#' The method was proposed by Pearson and Filon (1898, p. 262, formula xl).
#'
#' Raghunathan, Rosenthal, and Rubin (1996, p. 179, formula 2):
#' \deqn{k = (r_{jh} - r_{jk} r_{kh}) (r_{km} - r_{kh} r_{hm}) + (r_{jm} - r_{jh} r_{hm}) (r_{kh} - r_{jk} r_{jh}) + (r_{jh} - r_{jm} r_{hm}) (r_{km} - r_{jk} r_{jm})}
#' \eqn{+ (r_{jm} - r_{jk} r_{km}) (r_{kh} - r_{km} r_{hm})}
#'
#' Raghunathan et al. (1996, p. 179, formula 1):
#' \deqn{z = \frac{\sqrt{n} (r_{jk} - r_{hm})}{\sqrt{(1 - r_{jk}^2)^2 + (1 - r_{hm}^2)^2 - k}}}
#'
#' The two formulas can also be found in Steiger (1980, p. 245, formula 2, p. 246, formula 5).
#'
#'}
#'
### dunn1969
#'\item{dunn1969:}{
#'\emph{Dunn and Clark's z (1969)}
#'
#' Dunn and Clark (1969, p. 368, formula 9):
#' \deqn{c = \frac{1}{2} r_{jk} r_{hm} (r_{jh}^2 + r_{jm}^2 + r_{kh}^2 + r_{km}^2) + r_{jk} r_{hm} + r_{jm} r_{kh} - (r_{jk} r_{jh} r_{jm} + r_{jk} r_{kh} r_{km} + r_{jh} r_{kh} r_{hm} + r_{jm} r_{km} r_{hm})}
#' \deqn{\Big/ ((1 - r_{jk}^2)(1 - r_{hm}^2))}
#'
#' Dunn and Clark (1969, p. 370, formula 15):
#' \deqn{z = \frac{(Z_{jk} - Z_{jh})\sqrt{n - 3}}{\sqrt{2 - 2c}}}
#'
#'}
#'
### steiger1980
#'\item{steiger1980:}{
#'\emph{Steiger's (1980) modification of Dunn and Clark's z (1969) using average correlations}
#'
#' This method was proposed by Steiger (1980) and is a modification of Dunn and Clark's work (1969). Instead of \eqn{r.jk} and \eqn{r.hm} the mean of the two is being used.
#'
#' Steiger (1980, p. 247):
#' \deqn{\bar r = \frac{r_{jk} + r_{hm}}{2}}
#'
#' Steiger (1980, p. 247, formula 11; In the original article, there are brackets missing in the denominator):
#' \deqn{c = \frac{\frac{1}{2} \bar r^2 (r_{jh}^2 + r_{jm}^2 + r_{kh}^2 + r_{km}^2) + \bar r^2 + r_{jm} r_{kh} - (\bar r r_{jh} r_{jm} + \bar r r_{kh} r_{km} + r_{jh} r_{kh} \bar r + r_{jm} r_{km} \bar r)}{(1 - \bar r^2)^2}}
#'
#' Steiger (1980, p. 247, formula 15):
#' \deqn{z = \frac{(Z_{jk} - Z_{hm})\sqrt{n - 3}}{\sqrt{2 - 2c}}}
#'}
#'
### raghunathan1996
#'\item{raghunathan1996:}{
#'\emph{Raghunathan, Rosenthal, and Rubin's (1996) modification of Pearson and Filon's z (1898)}
#'
#' This method of Raghunathan, Rosenthal, and Rubin (1996) is based on Pearson and Filon (1898). Unlike Pearson and Filon (1898), Raghunathan et al. (1996) use \eqn{z}-transformed correlation coefficients.
#'
#' Raghunathan et al. (1996, p. 179, formula 2):
#' \deqn{k = (r_{jh} - r_{jk} r_{kh}) (r_{km} - r_{kh} r_{hm}) + (r_{jm} - r_{jh} r_{hm}) (r_{kh} - r_{jk} r_{jh}) + (r_{jh} - r_{jm} r_{hm}) (r_{km} - r_{jk} r_{jm})}
#' \eqn{+ (r_{jm} - r_{jk} r_{km}) (r_{kh} - r_{km} r_{hm})}
#'
#' Raghunathan et al. (1996, p. 179, formula 3):
#' \deqn{z = \sqrt{\frac{n - 3}{2}} \frac{Z_{jk} - Z_{hm}}{\sqrt{1 - \frac{k}{2(1 - r_{jk}^2)(1 - r_{hm}^2)}}}}
#'
#'}
#'
### silver2004
#'\item{silver2004:}{
#'\emph{Silver, Hittner, and May's (2004) modification of Dunn and Clark's z (1969) using backtransformed average Fisher's (1921) zs}
#'
#' The idea to backtransform averaged \eqn{Z}s was first proposed in Silver and Dunlap (1987) and was applied to the comparison of nonoverlapping correlations by Silver, Hittner, and May (2004). Their method is based on Steiger's approach (1980).
#'
#' Silver et al. (2004, p. 55):
#' \deqn{\bar Z = \frac{Z_{jk} + Z_{hm}}{2}}
#'
#' Silver and Dunlap (1987, p. 146, formula 4):
#' \deqn{\bar r_z = \frac{exp(2\bar Z - 1)}{exp(2\bar Z + 1)}}
#'
#' Silver et al. (2004, p. 56):
#' \deqn{c = \frac{\frac{1}{2} \bar r_z^2 (r_{jh}^2 + r_{jm}^2 + r_{kh}^2 + r_{km}^2) + \bar r_z^2 + r_{jm} r_{kh} - (\bar r_z r_{jh} r_{jm} + \bar r_z r_{kh} r_{km} + r_{jh} r_{kh} \bar r_z + r_{jm} r_{km} \bar r_z)}{(1 - \bar r_z^2)^2}}
#'
#' Silver et al. (2004, p. 55, formula 5);
#' \deqn{z = \frac{(Z_{jk} - Z_{hm})\sqrt{n - 3}}{\sqrt{2 - 2c}}}
#'
#'}
#'
### zou2007
#'\item{zou2007:}{
#'\emph{Zou's (2007) confidence interval}
#'
#' This method calculates the confidence interval of the difference between the two correlations \eqn{r_{jk}} and \eqn{r_{hm}}.
#'
#' Zou (2007, p. 406):
#' \deqn{l',u' = Z \pm z_{\frac{\alpha}{2}} \sqrt{\frac{1}{n - 3}}}
#'
#' Zou (2007, p. 406):
#' \deqn{l = \frac{exp(2l') - 1}{exp(2l') + 1}}
#' \deqn{u = \frac{exp(2u') - 1}{exp(2u') + 1}}
#'
#' Zou (2007, p. 409):
#' \deqn{c = \frac{1}{2} r_{jk} r_{hm} (r_{jh}^2 + r_{jm}^2 + r_{kh}^2 + r_{km}^2) + r_{jk} r_{hm} + r_{jm} r_{kh} - (r_{jk} r_{jh} r_{jm} + r_{jk} r_{kh} r_{km} + r_{jh} r_{kh} r_{hm} + r_{jm} r_{km} r_{hm})}
#' \deqn{\Big/ ((1 - r_{jk}^2)(1 - r_{hm}^2))}
#'
#' Zou (2007, pp. 409–410):
#' \deqn{L = r_{jk} - r_{hm} - \sqrt{(r_{jk} - l_1)^2 + (u_2 - r_{hm})^2 - 2c(r_{jk} - l_1)(u_2 - r_{hm})}}
#' \deqn{U = r_{jk} - r_{hm} - \sqrt{(u_1 - r_{jk})^2 + (r_{hm} - l_2)^2 - 2c(u_1 - r_{jk})(r_{hm} - l_2)}}
#'
#'}
#'}
#'
#' @section Recommended methods:
#'
#' Silver et al. (2004) compared pearson1898, dunn1969, steiger1980, hittner2003. Their recommendation is to use dunn1969, steiger1980, and hittner2003, as they offer lower Type I error rates and higher statistical power than pearson1898.
#'
#' Steiger (1980) compared pearson1898, dunn1969, and steiger1980. He recommends dunn1969 and steiger1980 as they maintain a better Type I error control at small sample sizes than pearson1898.
#'
#' In a direct comparison, Raghunathan et al. (1996) found raghunathan1996 to be superior to pearson1898 with respect to both type I error rate and statistical power.
#'
#' @param r.jk A number specifying the correlation between \eqn{j} and \eqn{k} (this correlation is used for comparison)
#' @param r.hm A number specifying the correlation between \eqn{h} and \eqn{m} (this correlation is used for comparison)
#' @param r.jh A number specifying the correlation between \eqn{j} and \eqn{h}
#' @param r.jm A number specifying the correlation between \eqn{j} and \eqn{m}
#' @param r.kh A number specifying the correlation between \eqn{k} and \eqn{h}
# #' @param r.km A number specifying the correlation between \eqn{k} and \eqn{m}
#' @param n An integer defining the size of the group
#' @param alternative A character string specifying whether the alternative hypothesis is two-sided ("\code{two.sided}"; default) or one-sided ( "\code{greater}" or "\code{less}", depending on the direction). Optionally, the initial letter of the character strings ("\code{t}", "\code{g}", and "\code{l})" can be used.
#' @param method A vector of character strings specifying the methods to be used (\code{pearson1898}, \code{dunn1969}, \code{steiger1980}, \code{raghunathan1996}, \code{silver2004}, or \code{zou2007}). Use \code{all} to apply all methods (default). For further information see the method section below.
#' @param alpha A number defining the alpha level for the hypothesis test. If method \code{zou2007} is used, \eqn{1 - alpha} is taken as the level of confidence for the confidence interval. The default value is \eqn{.05}.
#'
#' @return Returns an object of the class "\code{cocor.dep.groups.nonoverlap}" with the following slots holding the input parameters described above:
#' \item{r.jk}{Input parameter}
#' \item{r.hm}{Input parameter}
#' \item{r.jh}{Input parameter}
#' \item{r.jm}{Input parameter}
#' \item{r.kh}{Input parameter}
#' \item{r.km}{Input parameter}
#' \item{n}{Input parameter}
#' \item{alternative}{Input parameter}
#' \item{alpha}{Input parameter}
#' For each method a slot of the same name exists with a list containing the following elements:
#' \item{statistic}{The value of the test statistic (unless method \code{zou2007} is used)}
#' \item{distribution}{The distribution of the test statistic (unless method \code{zou2007} is used)}
#' \item{p.value}{The p-value of the test (unless method \code{zou2007} is used)}
#' \item{conf.int}{The confidence interval of the difference between the two correlations (if method \code{zou2007} is used)}
#'
#' @references
#' Dunn, O. J., & Clark, V. A. (1969). Correlation coefficients measured on the same individuals. \emph{Journal of the American Statistical Association}, \emph{64}, 366–377. doi:10.2307/2283746
#'
#' Pearson, K., & Filon, L. N. G. (1898). Mathematical contributions to theory of evolution: IV. On the probable errors of frequency constants and on the influence of random selection and correlation. \emph{Philosophical Transactions of the Royal Society of London, Series A}, \emph{191}, 229–311. doi:10.1098/rsta.1898.0007
#'
#' Raghunathan, T. E., Rosenthal, R., & Rubin, D. B., (1996). Comparing correlated but nonoverlapping correlations. \emph{Psychological Methods}, \emph{1}, 178–183. doi:10.1037//1082-989X.1.2.178
#'
#' Silver, N. C., & Dunlap, W. P. (1987). Averaging correlation coefficients: Should Fisher's Z transformation be used? \emph{Journal of Applied Psychology}, \emph{72}, 146–148. doi:10.1037//0021-9010.72.1.146
#'
#' Silver, N. C., Hittner, J. B., & May, K. (2004). Testing dependent correlations with nonoverlapping variables: A Monte Carlo simulation. \emph{Journal of Experimental Education}, \emph{73}, 53–69. doi:10.3200/JEXE.71.1.53-70
#'
#' Steiger, J. H. (1980). Tests for comparing elements of a correlation matrix. \emph{Psychological Bulletin}, \emph{87}, 245–251. doi:10.1037//0033-2909.87.2.245
#'
#' Zou, G. Y. (2007). Toward using confidence intervals to compare correlations. \emph{Psychological Methods}, \emph{12}, 399–413. doi:10.1037/1082-989X.12.4.399
#'
#' @seealso
#' \link{cocor.indep.groups}
#'
#' \link{cocor.dep.groups.overlap}
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
#' cocor.dep.groups.nonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n)
#'
#' @export
cocor.dep.groups.nonoverlap <- function(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n, alternative="two.sided", method="all", alpha=.05) {
  for(x in c("r.jk", "r.hm", "r.jh", "r.jm", "r.kh", "r.km")) {
    check.variable(get(x), x)
    check.variable.range(get(x), x, -1, 1)
  }

  check.variable(n, "n", "integer")
  check.variable.range(n, "n", 0, Inf)

  check.variable(alpha, "alpha")
  check.variable.range(alpha, "alpha", 0, 1)
  alternative <- check.alternative(alternative)

  if(any(method == "all")) method <- c("pearson1898", "dunn1969", "steiger1980", "raghunathan1996", "silver2004", "zou2007")

  result <- new("cocor.dep.groups.nonoverlap",
    r.jk=r.jk,
    r.hm=r.hm,
    r.jh=r.jh,
    r.jm=r.jm,
    r.kh=r.kh,
    r.km=r.km,
    n=n,
    alternative=alternative,
    method=method,
    alpha=alpha
  )

  for(m in method) {
    switch(m,
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
        r.bt <- fisher.z2r((fisher.r2z(r.jk) + fisher.r2z(r.hm)) / 2) # backtransform pooled z-transformed rs
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

        x <- qnorm(alpha/2, lower.tail=FALSE) * sqrt(1/(n - 3))

        l1 <- fisher.z2r(fisher.r2z(r.jk) - x)
        u1 <- fisher.z2r(fisher.r2z(r.jk) + x)

        l2 <- fisher.z2r(fisher.r2z(r.hm) - x)
        u2 <- fisher.z2r(fisher.r2z(r.hm) + x)

        L <- r.jk - r.hm - sqrt((r.jk - l1)^2 + (u2 - r.hm)^2 - 2 * c * (r.jk - l1) * (u2 - r.hm))
        U <- r.jk - r.hm + sqrt((u1 - r.jk)^2 + (r.hm - l2)^2 - 2 * c * (u1 - r.jk) * (r.hm - l2))

        conf.int <- c(L, U)
        result@zou2007 <- list(conf.int=conf.int)
      },
      stop(paste("Method '", m, "' not found", sep=""))
    )
  }

  result
}
