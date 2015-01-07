#' Get result parameters of a cocor result object 
#'
#' Returns result parameters of a cocor result object of class 'cocor.indep.groups', 'cocor.dep.groups.overlap', or 'cocor.dep.groups.nonoverlap' as a list with each slot representing a list element.
#'
#' @param result.object A cocor result object of class 'cocor.indep.groups', 'cocor.dep.groups.overlap', or 'cocor.dep.groups.nonoverlap'.
#' @param test A vector of character strings specifying the tests to be returned (e.g., \code{pearson1898}, \code{dunn1969}...). Use \code{all} to return the results of all tests (default).
#'
#' @return Returns a list containing all result parameters as list elements:
#' \item{diff}{Difference between the two correlations that were compared.}
#' \item{statistic}{The value of the test statistic (unless test zou2007 is used).}
#' \item{distribution}{The distribution of the test statistic (unless test zou2007 is used).}
#' \item{df}{The degrees of freedom of the distribution of the test statistic (only for result objects of class 'cocor.dep.groups.overlap' if test hotelling1940, hendrickson1970, or williams1959 is used).}
#' \item{p.value}{The p-value of the test (unless test zou2007 is used).}
#' \item{conf.int}{The confidence interval of the difference between the two correlations (if test zou2007 is used).}
#'
#' @seealso
#' \link{get.cocor.input}, \link{cocor}, \link{cocor.indep.groups}, \link{cocor.dep.groups.overlap}, \link{cocor.dep.groups.nonoverlap}
#'
#' @examples
#' data("aptitude")
#'
#' cocor.result <- cocor(~knowledge + intelligence.a | logic + intelligence.a, aptitude$sample1)
#' get.cocor.results(cocor.result)
#'
#' @include 0helper.r
#'
#' @docType methods
#' @rdname get.cocor.results
#' @export
setGeneric("get.cocor.results", function(result.object, test="all") standardGeneric("get.cocor.results"))

#' @aliases
#'   get.cocor.results,-method
#'   get.cocor.results,cocor-method
#'   get.cocor.results,cocor.indep.groups-method
#'   get.cocor.results,cocor.dep.groups.overlap-method
#'   get.cocor.results,cocor.dep.groups.nonoverlap-method
#' @rdname get.cocor.results
setMethod("get.cocor.results", "cocor",
  function(result.object, test="all") {
    if(any(test == "all")) test <- names(all.tests)

    result.parameters <- list(diff=result.object@diff)

    for(test in names(all.tests)) {
      if(.hasSlot(result.object, test)) result.parameters[[test]] <- slot(result.object, test)
    }

    result.parameters
  }
)
