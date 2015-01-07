#' Get input parameters of a cocor result object 
#'
#' Returns input parameters of a cocor result object of class 'cocor.indep.groups', 'cocor.dep.groups.overlap', or 'cocor.dep.groups.nonoverlap' as a list with each slot representing a list element.
#'
#' @param result.object A cocor result object of class 'cocor.indep.groups', 'cocor.dep.groups.overlap', or 'cocor.dep.groups.nonoverlap'.
#'
#' @return Returns a list containing all input parameters as list elements:
#' \item{r1.jk}{A number specifying the correlation between j and k measured in group 1 (only for result objects of class 'cocor.indep.groups').}
#' \item{r2.hm}{A number specifying the correlation between h and m measured in group 2 (only for result objects of class 'cocor.indep.groups').}
#' \item{n1}{An integer defining the size of group 1 (only for result objects of class 'cocor.indep.groups').}
#' \item{n2}{An integer defining the size of group 2 (only for result objects of class 'cocor.indep.groups').}
#' \item{r.jk}{A number specifying the correlation between j and k (only for result objects of class 'cocor.dep.groups.overlap' and 'cocor.dep.groups.nonoverlap').}
#' \item{r.jh}{A number specifying the correlation between j and h (only for result objects of class 'cocor.dep.groups.overlap').}
#' \item{r.hm}{A number specifying the correlation between h and m (only for result objects of class 'cocor.dep.groups.nonoverlap').}
#' \item{n}{An integer defining the size of the group (only for result objects of class 'cocor.dep.groups.overlap' and 'cocor.dep.groups.nonoverlap').}
#' \item{alternative}{A character string specifying whether the alternative hypothesis is two-sided ("two.sided") or one-sided ("greater" or "less", depending on the direction).}
#' \item{alpha}{A number defining the alpha level for the hypothesis test.}
#' \item{conf.level}{A number defining the level of confidence for the confidence interval.}
#' \item{null.value}{A number defining the hypothesized difference between the two correlations used for testing the null hypothesis.}
#' \item{data.name}{A vector of character strings describing the data/groups.}
#' \item{var.labels}{A vector of four character strings specifying the labels for j, k, h, and m (in this order).}
#'
#' @seealso
#' \link{get.cocor.results}, \link{cocor}, \link{cocor.indep.groups}, \link{cocor.dep.groups.overlap}, \link{cocor.dep.groups.nonoverlap}
#'
#' @examples
#' data("aptitude")
#'
#' cocor.result <- cocor(~knowledge + intelligence.a | logic + intelligence.a, aptitude$sample1)
#' get.cocor.input(cocor.result)
#'
#' @include 0helper.r
#'
#' @docType methods
#' @rdname get.cocor.input
#' @export
setGeneric("get.cocor.input", function(result.object) standardGeneric("get.cocor.input"))

#' @aliases
#'   get.cocor.input,-method
#'   get.cocor.input,cocor-method
#'   get.cocor.input,cocor.indep.groups-method
#'   get.cocor.input,cocor.dep.groups.overlap-method
#'   get.cocor.input,cocor.dep.groups.nonoverlap-method
#' @rdname get.cocor.input
setMethod("get.cocor.input", "cocor",
  function(result.object) {
    input.parameters <- list(
      alternative=result.object@alternative,
      alpha=result.object@alpha,
      conf.level=result.object@conf.level,
      null.value=result.object@null.value,
      data.name=result.object@data.name,
      var.labels=result.object@var.labels
    )

    input.parameters <- c(input.parameters, switch(class(result.object),
      cocor.indep.groups=list(r1.jk=result.object@r1.jk, r2.hm=result.object@r2.hm, n1=result.object@n1, n2=result.object@n2),
      cocor.dep.groups.overlap=list(r.jk=result.object@r.jk, r.jh=result.object@r.jh, r.kh=result.object@r.kh, n=result.object@n),
      cocor.dep.groups.nonoverlap=list(r.jk=result.object@r.jk, r.hm=result.object@r.hm, r.jh=result.object@r.jh, r.jm=result.object@r.jm, r.kh=result.object@r.kh, r.km=result.object@r.km, n=result.object@n)
    ))

    input.parameters
  }
)
