#' @import methods

fisher.r2z <- function(r) 0.5 * (log(1 + r) - log(1 - r)) #fisher transformation
fisher.z2r <- function(z) (exp(2 * z) - 1) / (exp(2 * z) + 1) #backward fisher transformation

fround <- function(x, digits=2) sprintf(paste("%.",digits,"f",sep=""), x)

R <- function(r.jk, r.jh, r.kh) 1 + 2 * r.jk * r.jh * r.kh - r.jk^2 - r.jh^2 - r.kh^2

methods <- c(
  pearson1898="Pearson and Filon's z (1898)",
  fisher1925="Fisher's z (1925)",
  hotelling1940="Hotelling's t (1940)",
  williams1959="Williams' t (1959)",
  hendrickson1970="Hendrickson, Stanley, and Hills' (1970) modification of Williams' t (1959)",
  olkin1967="Olkin's z (1967)",
  dunn1969="Dunn and Clark's z (1969)",
  steiger1980="Steiger's (1980) modification of Dunn and Clark's z (1969) using average correlations",
  meng1992="Meng, Rosenthal, and Rubin's z (1992)",
  raghunathan1996="Raghunathan, Rosenthal, and Rubin's (1996) modification of Pearson and Filon's z (1898)",
  hittner2003="Hittner, May, and Silver's (2003) modification of Dunn and Clark's z (1969) using a backtransformed average Fisher's (1921) Z procedure",
  silver2004="Silver, Hittner, and May's (2004) modification of Dunn and Clark's z (1969) using a backtransformed average Fisher's (1921) Z procedure",
  zou2007="Zou's (2007) confidence interval"
)

get.p.value <- function(statistic, distribution="z", alternative="two.sided", df=NULL) {
  if(alternative == "two.sided") statistic <- abs(statistic)

  dist.fun <- paste("p", switch(distribution, z="norm", t="t"), sep="")

  params <- list(statistic)
  if(!is.null(df)) params <- c(params, df)
  x <- do.call(dist.fun, params)

  switch(alternative,
    two.sided=2 * (1 - x),
    greater=1 - x,
    less=x
  )
}

check.alternative <- function(alternative) {
  switch(substr(alternative,1,1),
    t="two.sided",
    g="greater",
    l="less",
    stop("The parameter 'alternative' must be either 'two.sided', 'greater', or 'less' (or just the initial letter).")
  )
}

check.variable <- function(x, name, type="number") {
  if(length(x) != 1 || is.na(x) || !is.finite(x) || (type == "integer" && x %% 1 != 0)) stop(paste("The parameter '", name, "' must be a single ", type, sep=""))
}

check.variable.range <- function(x, name, lower, upper) {
  if(x < lower || x > upper) stop(paste("The parameter '", name, "' must be a single number between ", lower, " and ", upper, sep=""))
}

print.alternative <- function(alternative, variable1, variable2) {
  paste("Null hypothesis: ", variable1, " is equal to ", variable2, "\n",
  "Alternative hypothesis: ", variable1,
    switch(alternative, two.sided=" is not equal to ", greater=" is greater than ", less=" is less than "),
    variable2, " (", switch(alternative, two.sided="two", "one"), "-sided)", sep=""
  )
}

print.test.statistic <- function(object, method, r1.name=NULL, r2.name=NULL) {
  r <- slot(object, method)

  retained <- "Null hypothesis retained"
  rejected <- "Null hypothesis rejected"

  s <- {
    if(!is.null(r$statistic)) {
      evaluation <- {
        if(r$p.value <= object@alpha) rejected
        else retained
      }
      paste("  ", r$distribution, " = ", fround(r$statistic,4), if(!is.null(r$df)) paste(", df = ", r$df, sep=""), ", p-value = ", fround(r$p.value,4), "\n  ", evaluation, sep="")
    } else NULL
  }

  if(!is.null(r$conf.int)) {
    evaluation <- switch(object@alternative,
      two.sided={
        if(r$conf.int[1] < 0 && r$conf.int[2] > 0) paste(retained, " (Interval includes 0)", sep="")
        else paste(rejected, " (Interval does not include 0)", sep="")
      },
      greater={
        if(r$conf.int[1] > 0) paste(rejected, " (Lower boundary > 0)", sep="")
        else paste(retained, " (Lower boundary <= 0)", sep="")
      },
      less={
        if(r$conf.int[2] < 0) paste(retained, " (Upper boundary < 0)", sep="")
        else paste(retained, " (Upper boundary >= 0)", sep="")
      }
    )

    if(!is.null(s)) s <- paste(s, "\n", sep="")
    s <- paste(s, "  ", (1 - object@alpha) * 100, "% confidence interval for ", r1.name, " - ", r2.name, ": ", paste(fround(r$conf.int,4), collapse=" "), "\n  ", evaluation, sep="")
  }

  paste(method, ": ", methods[method], "\n", s, "\n\n", sep="")
}

setClass("cocor.indep.groups",
  representation(
    r1="numeric",
    n1="numeric",
    r2="numeric",
    n2="numeric",
    alternative="character",
    method="character",
    alpha="numeric",
    fisher1925="list",
    zou2007="list"
  )
)

setMethod("show", "cocor.indep.groups",
  function(object) {
    cat("\n  Results of a comparison of two correlations based on independent groups\n\n",
    "Comparison between r1 = ", object@r1, " and r2 = ", object@r2, "\n",
    "Group sizes: n1 = ", object@n1, ", n2 = ", object@n2, "\n",
    print.alternative(object@alternative, "r1", "r2"), "\n",
    "Alpha: ", object@alpha, "\n\n", sep="")

    for(m in object@method) {
      cat(print.test.statistic(object, m, "r1", "r2"))
    }
  }
)



setClass("cocor.dep.groups.overlap",
  representation(
    r.jk="numeric",
    r.jh="numeric",
    r.kh="numeric",
    n="numeric",
    alternative="character",
    method="character",
    alpha="numeric",
    pearson1898="list",
    hotelling1940="list",
    williams1959="list",
    hendrickson1970="list",
    olkin1967="list",
    dunn1969="list",
    steiger1980="list",
    meng1992="list",
    hittner2003="list",
    zou2007="list"
  )
)

setMethod("show", "cocor.dep.groups.overlap",
  function(object) {
    cat(
      "\n  Results of a comparison of two overlapping correlations based on dependent groups\n\n",
      "Comparison between r.jk = ", object@r.jk, " and r.jh = ", object@r.jh, "\n",
      "Related correlation: r.kh = ", object@r.kh, "\n",
      "Group size: n = ", object@n, "\n",
      print.alternative(object@alternative, "r.jk", "r.jh"), "\n",
      "Alpha: ", object@alpha, "\n\n", sep=""
    )

    for(m in object@method) {
      cat(print.test.statistic(object, m, "r.jk", "r.jh"))
    }
  }
)



setClass("cocor.dep.groups.nonoverlap",
  representation(
    r.jk="numeric",
    r.hm="numeric",
    r.jh="numeric",
    r.jm="numeric",
    r.kh="numeric",
    r.km="numeric",
    n="numeric",
    alternative="character",
    method="character",
    alpha="numeric",
    pearson1898="list",
    dunn1969="list",
    steiger1980="list",
    raghunathan1996="list",
    silver2004="list",
    zou2007="list"
  )
)

setMethod("show", "cocor.dep.groups.nonoverlap",
  function(object) {
    cat(
      "\n  Results of a comparison of two nonoverlapping correlations based on dependent groups\n\n",
      "Comparison between r.jk = ", object@r.jk, " and r.hm = ", object@r.hm, "\n",
      "Related correlations: r.jh = ", object@r.jh, ", r.jm = ", object@r.jm,", r.kh = ", object@r.kh, ", r.km = ", object@r.km, "\n",
      "Group size: n = ", object@n, "\n",
      print.alternative(object@alternative, "r.jk", "r.hm"), "\n",
      "Alpha: ", object@alpha, "\n\n", sep=""
    )

    for(m in object@method) {
      cat(print.test.statistic(object, m, "r.jk", "r.hm"))
    }
  }
)

