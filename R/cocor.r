#' Compare two correlations based on either dependent or independent groups
#'
#' Performs a test of significance for the difference between two correlations based on either dependent or independent groups. Dependent correlations can be either overlapping (they share a variable) or nonoverlapping (they have no variable in common). The function expects raw data input from which the correlations are calculated.
#'
#'\describe{
#'  \item{Comparison of two correlations based on independent groups}{
#'    The \code{formula} parameter for the comparison of two correlations based on independent groups can either be \code{~a + b | a + b}, \code{~a + b | a + c}, or \code{~a + b | c + d}. The variables of the first correlation -- \code{a} and \code{b} before the "\code{|}" character -- must refer to columns in the data.frame/matrix of the first element in the list of the \code{data} object, whereas the variables of the second correlation -- \code{a}, \code{b}, \code{c}, and \code{d} after the "\code{|}" character -- must refer to columns in the data.frame/matrix of the second element in the list.
#'  }
#'
#'  \item{Comparison of two overlapping correlations based on dependent groups}{
#'    The \code{formula} parameterfor correlations based on dependent groups with overlapping variables must follow the pattern \code{~a + b | a + c}. The variables of the two correlation -- \code{a}, \code{b}, and \code{c} -- must refer to columns in the data.frame/matrix of the \code{data} object.
#'  }
#'
#'  \item{Comparison of two nonoverlapping correlations based on dependent groups}{
#'    The \code{formula} for correlations based on dependent groups with nonoverlapping variables must have the form \code{~a + b | c + d}. The variables of the two correlation -- \code{a}, \code{b}, \code{c}, and \code{d} -- must refer to columns in the data.frame/matrix of the \code{data} object.
#'  }
#'}
#'
#' @param formula A formula specifying the correlations and their underlying variables (See details).
#' @param data A list holding two data.frames/matrices for independent groups or a single data.frame/matrix for dependent groups that contain the variables specified in \code{formula} as columns.
#' @param use A character string giving a test for computing covariances in the presence of missing values. This must be (an abbreviation of) one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (see \link[stats:cor]{cor} in package \link[stats]{stats}).
#' @param alternative A character string specifying whether the alternative hypothesis is two-sided ("\code{two.sided}"; default) or one-sided ( "\code{greater}" or "\code{less}", depending on the direction). Optionally, the initial letter of the character strings ("\code{t}", "\code{g}", and "\code{l})" can be used.
#' @param test For the tests available, see \link{cocor.indep.groups}, \link{cocor.dep.groups.overlap}, and \link{cocor.dep.groups.nonoverlap}. Use \code{all} to apply all tests (default).
#' @param alpha A number defining the alpha level for the hypothesis test. The default value is \eqn{.05}.
#' @param conf.level A number defining the level of confidence for the confidence interval (if a test is used that calculates confidence intervals). The default value is \eqn{.95}.
#' @param null.value A number defining the hypothesized difference between the two correlations used for testing the null hypothesis. The default value is \eqn{0}. If the value is other than \eqn{0}, only the test \code{zou2007} that uses a confidence interval is available.
#' @param return.htest A logical indicating whether the result should be returned as a list containing a list of class 'htest' for each test. The default value is \code{FALSE}.
#'
#' @return Returns an object of the class 'cocor.indep.groups', 'cocor.dep.groups.overlap', or 'cocor.dep.groups.nonoverlap' depending on the invoked comparison function.
#'
#' @seealso
#' \link{cocor.indep.groups}, \link{cocor.dep.groups.overlap}, \link{cocor.dep.groups.nonoverlap}, \link{as.htest}
#'
#' @examples
#' data("aptitude")
#'
#' # Compare two correlations based on two independet groups
#' cocor(~logic + intelligence.a | logic + intelligence.a, aptitude)
#'
#' # Compare two correlations based on two depenendent groups
#' # The correlations are overlapping
#' cocor(~knowledge + intelligence.a | logic + intelligence.a, aptitude$sample1)
#' cocor(~knowledge + intelligence.a | logic + intelligence.a, aptitude$sample2)
#' # The correlations are nonoverlapping
#' cocor(~logic + intelligence.b | knowledge + intelligence.a, aptitude$sample1)
#' cocor(~logic + intelligence.b | knowledge + intelligence.a, aptitude$sample2)
#'
#' # Return result as a list of class 'htest'
#' cocor(~knowledge + intelligence.b | logic + intelligence.a, aptitude$sample1, return.htest=TRUE)
#'
#' @export
cocor <- function(formula, data, use="everything", alternative="two.sided", test="all", alpha=.05, conf.level=.95, null.value=0, return.htest=FALSE) {
  if(!(is(data, "data.frame") || is(data, "matrix") || (is(data, "list") && length(data) == 2 && (is(data[[1]], "data.frame") || is(data[[1]], "matrix")) && (is(data[[2]], "data.frame") || is(data[[2]], "matrix"))))) stop("The parameter 'data' must be a data.frame/matrix for correlations based on dependent groups or a list containing two data.frames/matrices for correlations based on two independent groups") # validate data format

  valid.formula <- validate.formula(formula) # validate formula
  unique.variable.count <- length(all.vars(formula))
  if(!valid.formula && is(data, "list")) stop("For correlations based on independent groups, the parameter 'formula' must be a formula of the form ~a + b | a + b, ~a + b | a + c, or ~a + b | c + d")
  if(!valid.formula || (!is(data, "list") && !(unique.variable.count %in% c(3,4)))) stop("For correlations based on dependent groups, the parameter 'formula' must be a formula of the form ~a + b | a + c if the two correlations are overlapping or ~a + b | c + d if the two correlations are nonoverlapping.")

  comparison.case <- { # determine case of comparison
    if(is(data, "list")) "indep.groups"
    else if(unique.variable.count == 3) "dep.groups.overlap"
    else if(unique.variable.count == 4) "dep.groups.nonoverlap"
    else stop("Could not classify data input")
  }

  variables <- as.character(c(formula[[2]][[2]][[2]], formula[[2]][[2]][[3]], formula[[2]][[3]][[2]], formula[[2]][[3]][[3]])) # extract variables
  if(comparison.case == "dep.groups.overlap") {
    j <- variables[duplicated(variables)]
    variables <- variables[variables != j]
    k <- variables[1]
    h <- variables[2]
    variable.count <- 3
  } else {
    j <- variables[1]
    k <- variables[2]
    h <- variables[3]
    m <- variables[4]
    variable.count <- 4
  }

  for(x in c("j", "k", "h", "m")[1:variable.count]) { # validate variables
    if(comparison.case == "indep.groups") {
      i <- ifelse(x %in% c("j","k"), 1, 2)
      data.test <- data[[i]]
    } else {
      i <- 0
      data.test <- data
    }

    if(!(get(x) %in% colnames(data.test))) stop(paste("Could not find column '", get(x), "' in the ", class(data.test), " that is provided by the parameter 'data'", if(i > 0) paste(" as the ", switch(i, "first", "second"), " element in the list", sep=""), sep=""))
    if(any(!is.finite(data.test[,get(x)]))) stop(paste("All elements of the variable '", get(x), "' must be finite", sep=""))
    if(!is.numeric(data.test[,get(x)])) stop(paste("The variable '", get(x), "' must be numeric", sep=""))
  }

  data.name <- deparse(substitute(data),width.cutoff=200,nlines=1)

  switch(comparison.case, # calculate correlations and pass them to the comparison functions
    indep.groups={
      r1.jk <- cor(data[[1]][,j], data[[1]][,k], use = use)
      r2.hm <- cor(data[[2]][,h], data[[2]][,m], use = use)
      n1 <- nrow(data[[1]])
      n2 <- nrow(data[[2]])
      var.labels <- c(j, k, h, m)
      cocor.indep.groups(r1.jk, r2.hm, n1, n2, alternative, test, alpha, conf.level, null.value, data.name, var.labels, return.htest)
    },
    dep.groups.overlap={
      r.jk <- cor(data[,j], data[,k], use = use)
      r.jh <- cor(data[,j], data[,h], use = use)
      r.kh <- cor(data[,k], data[,h], use = use)
      n <- nrow(data)
      var.labels <- c(j, k, h)
      cocor.dep.groups.overlap(r.jk, r.jh, r.kh, n, alternative, test, alpha, conf.level, null.value, data.name, var.labels, return.htest)
    },
    dep.groups.nonoverlap={
      r.jk <- cor(data[,j], data[,k], use = use)
      r.hm <- cor(data[,h], data[,m], use = use)
      r.jh <- cor(data[,j], data[,h], use = use)
      r.jm <- cor(data[,j], data[,m], use = use)
      r.kh <- cor(data[,k], data[,h], use = use)
      r.km <- cor(data[,k], data[,m], use = use)
      n <- nrow(data)
      var.labels <- c(j, k, h, m)
      cocor.dep.groups.nonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n, alternative, test, alpha, conf.level, null.value, data.name, var.labels, return.htest)
    }
  )
}
