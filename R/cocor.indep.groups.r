#' Compare two correlations based on independent groups
#'
#' Performs a test of significance for the difference between two correlation coefficients based on independent groups. The function expects correlation coefficients as input.
#'
#'@section Tests:
#' The tests make use of Fisher's \eqn{r}-to-\eqn{Z} transformation (1921, p. 26):
#'
#' \deqn{Z = \frac{1}{2}(ln(1+r) - ln(1-r)).}{Z = (1/2)(ln(1+r) - ln(1-r)).}
#'
#'\describe{
### fisher1925
#'\item{fisher1925:}{
#' \emph{Fisher's (1925) z}
#' 
#' This significance test was first described in Fisher (1925, pp. 161-168) and its test statistic \eqn{z} is calculated as
#' 
#' \deqn{z = \frac{Z_1 - Z_2}{\sqrt{\frac{1}{n_1 - 3} + \frac{1}{n_2 - 3}}}.}{z = (Z_1 - Z_2)/(\sqrt(1/(n_1 - 3) + 1/(n_2 - 3))).}
#'
#' \eqn{Z_1} and \eqn{Z_2} are the two \eqn{Z} transformed correlations that are being compared.
#' \eqn{n_1} and \eqn{n_2} specify the size of the two groups the correlations are based on.
#' The equation is also given for example in Peters and van Voorhis (1940, p. 188) and Cohen, Cohen, West, and Aiken (2003, p. 49, formula 2.8.11).
#' }
#'
### zou2007
#'\item{zou2007:}{
#' \emph{Zou's (2007) confidence interval}
#'
#' This test calculates the confidence interval of the difference between the two correlation coefficients \eqn{r_1} and \eqn{r_2}.
#' If the confidence interval includes zero, the null hypothesis that the two correlations are equal must be retained.
#' If the confidence interval does not include zero, the null hypothesis has to be rejected.
#' A lower and upper bound for the interval (\eqn{L} and \eqn{U}, respectively) is given by
#' \deqn{L = r_1 - r_2 - \sqrt{(r_1 - l_1)^2 + (u_2 - r_2)^2}}{L = r_1 - r_2 - \sqrt((r_1 - l_1)^2 + (u_2 - r_2)^2)}
#' and
#' \deqn{U = r_1 - r_2 - \sqrt{(u_1 - r_1)^2 + (r_2 - l_2)^2}}{U = r_1 - r_2 - \sqrt((u_1 - r_1)^2 + (r_2 - l_2)^2)}
#' (Zou, 2007, p. 409).
#' A lower and upper bound for the confidence interval of \eqn{r_1} (\eqn{l_1} and \eqn{u_1}) and \eqn{r_2} (\eqn{l_2} and \eqn{u_2}) are calculated as
#' \deqn{l = \frac{exp(2l') - 1}{exp(2l') + 1},}{l = (exp(2l') - 1)/(exp(2l') + 1),}
#' \deqn{u = \frac{exp(2u') - 1}{exp(2u') + 1}}{u = (exp(2u') - 1)/(exp(2u') + 1)}
#' (Zou, 2007, p. 406), where
#' \deqn{l',u' = Z \pm z_{\frac{\alpha}{2}} \sqrt{\frac{1}{n - 3}}}{l',u' = Z +- z_{\alpha/2} \sqrt(1/(n - 3))}
#' (Zou, 2007, p. 406).
#' \eqn{\alpha} denotes the desired alpha level of the confidence interval, whereas \eqn{n} specifies the size of the group the correlation is based on.
#' }
#'
#'}
#'
#' @param r1.jk A number specifying the correlation between j and k measured in group 1
#' @param r2.hm A number specifying the correlation between h and m measured in group 2
#' @param n1 An integer defining the size of group 1
#' @param n2 An integer defining the size of group 2
#' @param alternative A character string specifying whether the alternative hypothesis is two-sided ("\code{two.sided}"; default) or one-sided ( "\code{greater}" or "\code{less}", depending on the direction). Optionally, the initial letter of the character strings ("\code{t}", "\code{g}", and "\code{l})" can be used.
#' @param test A vector of character strings specifying the tests to be used (\code{fisher1925} or \code{zou2007}). Use \code{all} to apply all tests (default). For further information see the tests section below.
#' @param alpha A number defining the alpha level for the hypothesis test. The default value is \eqn{.05}.
#' @param conf.level A number defining the level of confidence for the confidence interval (if test \code{zou2007} is used). The default value is \eqn{.95}.
#' @param null.value A number defining the hypothesized difference between the two correlations used for testing the null hypothesis. The default value is \eqn{0}. If the value is other than \eqn{0}, only the test \code{zou2007} that uses a confidence interval is available.
#' @param data.name A character string giving the name(s) of the data. If \code{data.name} is \code{NULL}, the data names of \code{r1.jk} and \code{r2.hm} are used.
#' @param var.labels A vector of 4 character strings specifying the labels for j, k, h, and m (in this order).
#' @param return.htest A logical indicating whether the result should be returned as a list containing a list of class 'htest' for each test. The default value is \code{FALSE}.
#'
#' @return Returns an object of the class 'cocor.indep.groups' with the following slots holding the input parameters described above:
#' \item{r1.jk}{Input parameter}
#' \item{r2.hm}{Input parameter}
#' \item{n1}{Input parameter}
#' \item{n2}{Input parameter}
#' \item{alternative}{Input parameter}
#' \item{alpha}{Input parameter}
#' \item{conf.level}{Input parameter}
#' \item{null.value}{Input parameter}
#' \item{data.name}{Input parameter}
#' \item{var.labels}{Input parameter}
#' For each test a slot of the same name exists with a list containing the following elements:
#' \item{statistic}{The value of the test statistic (if test \code{fisher1925} is used).}
#' \item{distribution}{The distribution of the test statistic (if test \code{fisher1925} is used).}
#' \item{p.value}{The p-value of the test (if test \code{fisher1925} is used).}
#' \item{conf.int}{The confidence interval of the difference between the two correlations (if test \code{zou2007} is used).}
#'
#' @references
#' Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). \emph{Applied Multiple Regression/Correlation Analysis for the Behavioral Sciences (3rd ed.)}. Mahwah, NJ: Erlbaum.
#'
#' Fisher, R. A. (1921). On the probable error of a coefficient of correlation deduced from a small sample. \emph{Metron}, \emph{1}, 1-32.
#'
#' Fisher, R. A. (1925). \emph{Statistical methods for research workers}. Edinburgh, Scotland: Oliver and Boyd. Retrieved from http://psychclassics.yorku.ca/
#'
#' Peters, C. C., & Van Voorhis, W. R. (1940). \emph{Statistical procedures and their mathematical bases}. New York: McGraw-Hill.
#'
#' Zou, G. Y. (2007). Toward using confidence intervals to compare correlations. \emph{Psychological Methods}, \emph{12}, 399-413. doi:10.1037/1082-989X.12.4.399
#'
#' @seealso
#' \link{cocor}, \link{cocor.dep.groups.overlap}, \link{cocor.dep.groups.nonoverlap}, \link{as.htest}
#'
#' @examples
#' # Compare the difference between two correlations based
#' # on two independent groups:
#' r1.jk <- .7  # Correlation between age and intelligence measured in group 1
#' n1 <- 305    # Size of group 1
#'
#' r2.hm <- .6  # Correlation between age and intelligence measured in group 2
#' n2 <- 210    # Size of group 2
#'
#' cocor.indep.groups(r1.jk, r2.hm, n1, n2, var.labels=c("age", "intelligence", "age", "intelligence"))
#'
#' @export
cocor.indep.groups <- function(r1.jk, r2.hm, n1, n2, alternative="two.sided", test="all", alpha=.05, conf.level=.95, null.value=0, data.name=NULL, var.labels=NULL, return.htest=FALSE) {
  for(x in c("n1", "n2")) {
    validate.numeric(get(x), x, "integer")
    validate.numeric.range(get(x), x, 0, Inf)
  }

  for(x in c("r1.jk", "r2.hm")) {
    validate.numeric(get(x), x)
    validate.numeric.range(get(x), x, -1, 1)
  }

  for(x in c("alpha", "conf.level")) {
    validate.numeric(get(x), x)
    validate.numeric.range(get(x), x, 0, 1)
  }
  alternative <- validate.alternative(alternative)

  validate.numeric.range(null.value, "null.value", -2, 2)

  if(any(test == "all")) test <- c("fisher1925", "zou2007")
  test <- validate.test(test, null.value)

  result <- new("cocor.indep.groups",
    r1.jk=r1.jk,
    n1=n1,
    r2.hm=r2.hm,
    n2=n2,
    test=test,
    alternative=alternative,
    alpha=alpha,
    conf.level=conf.level,
    null.value=null.value
  )
  if(!is.null(data.name)) {
    validate.character(data.name, "data.name", 1)
    result@data.name <- data.name
  }
  if(!is.null(var.labels)) {
    validate.character(var.labels, "var.labels", 4)
    result@var.labels <- var.labels
  }

  for(x in test) {
    switch(x,
      fisher1925={
        statistic <- (fisher.r2z(r1.jk) - fisher.r2z(r2.hm)) / sqrt(1/(n1 - 3) + 1/(n2 - 3))
        distribution <- "z"

        p.value <- get.p.value(statistic, distribution, alternative)
        result@fisher1925 <- list(statistic=statistic, distribution=distribution, p.value=p.value)
      },
      zou2007={
        z <- qnorm((1 - conf.level)/2, lower.tail=FALSE)

        l1 <- fisher.z2r(fisher.r2z(r1.jk) - z * sqrt(1/(n1 - 3)))
        u1 <- fisher.z2r(fisher.r2z(r1.jk) + z * sqrt(1/(n1 - 3)))

        l2 <- fisher.z2r(fisher.r2z(r2.hm) - z * sqrt(1/(n2 - 3)))
        u2 <- fisher.z2r(fisher.r2z(r2.hm) + z * sqrt(1/(n2 - 3)))

        L <- r1.jk - r2.hm - sqrt((r1.jk - l1)^2 + (u2 - r2.hm)^2)
        U <- r1.jk - r2.hm + sqrt((u1 - r1.jk)^2 + (r2.hm - l2)^2)

        conf.int <- c(L, U)
        result@zou2007 <- list(conf.int=conf.int)
      },
      stop(paste("Test '", x, "' not found", sep=""))
    )
  }

  if(return.htest) return(as.htest(result))
  result
}
