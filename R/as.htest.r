#' Convert to a list of class 'htest'
#'
#' Convert a cocor result object of class 'cocor.indep.groups', 'cocor.dep.groups.overlap', or 'cocor.dep.groups.nonoverlap' to a list of class 'htest'.
#'
#' @param result.object A cocor result object of class 'cocor.indep.groups', 'cocor.dep.groups.overlap', or 'cocor.dep.groups.nonoverlap'.
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
#' @docType methods
#' @rdname as.htest
#' @export
setGeneric("as.htest", function(result.object) standardGeneric("as.htest"))

#' @aliases as.htest,cocor-method
#' @include 0helper.r
#' @rdname as.htest
setMethod("as.htest", "cocor",
  function(result.object) {
    htest.list <- list()
    for(test in names(all.tests)) {
      if(.hasSlot(result.object, test)) {
        test.object <- slot(result.object, test)

        conf.int <- test.object$conf.int
        if(!is.null(conf.int)) attr(conf.int, "conf.level") <- result.object@conf.level

        statistic <- test.object$statistic
        names(statistic) <- test.object$distribution

        estimate <- switch(class(result.object),
          cocor.indep.groups=c(r1.jk=result.object@r1.jk, r2.hm=result.object@r2.hm),
          cocor.dep.groups.overlap=c(r.jk=result.object@r.jk, r.jh=result.object@r.jh, r.kh=result.object@r.kh),
          cocor.dep.groups.nonoverlap=c(r.jk=result.object@r.jk, r.hm=result.object@r.hm, r.jh=result.object@r.jh, r.jm=result.object@r.jm, r.kh=result.object@r.kh, r.km=result.object@r.km)
        )

        htest <- list(
          statistic=statistic,
          parameter=c(df=test.object$df),
          p.value=test.object$p.value,
          estimate=estimate,
          null.value=c("difference in correlations"=result.object@null.value),
          alternative=result.object@alternative,
          method=all.tests[[test]],
          data.name=data.description(result.object@data.name, result.object@var.labels),
          conf.int=conf.int
        )
        class(htest) <- "htest"
        htest.list[[test]] <- htest
      }
    }

    htest.list
  }
)
