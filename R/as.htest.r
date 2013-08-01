#' Convert to a list of class 'htest'
#'
#' Convert a cocor result object of the classes 'cocor.indep.groups', 'cocor.dep.groups.overlap', or 'cocor.dep.groups.nonoverlap' to a list of class 'htest'.
#'
# \usage{
# as.htest(x)
#
# ## S4 method for the classes 'cocor.indep.groups', 'cocor.dep.groups.overlap', and 'cocor.dep.groups.nonoverlap'.
# as.htest(x)
# }
#'
#' @param x A cocor result object of the classes 'cocor.indep.groups', 'cocor.dep.groups.overlap', or 'cocor.dep.groups.nonoverlap'.
#'
#' @return Returns a list containing a list of class 'htest' for the result of each test with the following elements:
#' \item{data.name}{A character string giving the names of the data.}
#' \item{estimate}{The two correlations that have been compared and the related correlations.}
#' \item{method}{A character string indicating the performed test.}
#' \item{null.value}{The specified hypothesized value of the difference between the two correlations.}
#' \item{alternative}{A character string describing the alternative hypothesis.}
#' \item{parameter}{The degrees of freedom of the distribution of the test statistic.}
#' \item{statistic}{The value of the test statistic.}
#' \item{p.value}{The p-value of the test.}
#' \item{conf.int}{The confidence interval of the difference between the two correlations.}
#'
#' @seealso
#' \link{cocor}, \link{cocor.indep.groups}, \link{cocor.dep.groups.overlap}, \link{cocor.dep.groups.nonoverlap}
#'
#' @examples
#' data("aptitude")
#'
#' cocor.result <- cocor(~knowledge + intelligence.a | logic + intelligence.a, aptitude$sample1)
#' as.htest(cocor.result)
#'
#' @noRd
#' @docType methods
#' @exportMethod as.htest
#' @rdname as.htest
setGeneric("as.htest", function(x) standardGeneric("as.htest"))

#' @noRd
#' @aliases as.htest,cocor-method
#' @include 0helper.r
#' @rdname as.htest
setMethod("as.htest", "cocor",
  function(x) {
    htest.list <- list()
    for(test in names(all.tests)) {
      if(.hasSlot(x, test)) {
        r <- slot(x, test)

        conf.int <- r$conf.int
        if(!is.null(conf.int)) attr(conf.int, "conf.level") <- x@conf.level

        statistic <- r$statistic
        names(statistic) <- r$distribution

        estimate <- switch(class(x),
          cocor.indep.groups=c(r1.jk=x@r1.jk, r2.hm=x@r2.hm),
          cocor.dep.groups.overlap=c(r.jk=x@r.jk, r.jh=x@r.jh, r.kh=x@r.kh),
          cocor.dep.groups.nonoverlap=c(r.jk=x@r.jk, r.hm=x@r.hm, r.jh=x@r.jh, r.jm=x@r.jm, r.kh=x@r.kh, r.km=x@r.km)
        )

        htest <- list(
          statistic=statistic,
          parameter=c(df=r$df),
          p.value=r$p.value,
          estimate=estimate,
          null.value=c("difference in correlations"=x@null.value),
          alternative=x@alternative,
          method=all.tests[[test]],
          data.name=data.description(x@data.name, x@var.labels),
          conf.int=conf.int
        )
        class(htest) <- "htest"
        htest.list[[test]] <- htest
      }
    }

    htest.list
  }
)
