#' @import methods stats

fisher.r2z <- function(r) 0.5 * (log(1 + r) - log(1 - r)) #fisher transformation
fisher.z2r <- function(z) (exp(2 * z) - 1) / (exp(2 * z) + 1) #backward fisher transformation

fround <- function(x, digits=2) sprintf(paste("%.",digits,"f",sep=""), x) # round with fixed number of decimal places

R <- function(r.jk, r.jh, r.kh) 1 + 2 * r.jk * r.jh * r.kh - r.jk^2 - r.jh^2 - r.kh^2

all.tests <- c( # names of all implemented tests and their abbreviations
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

get.p.value <- function(statistic, distribution="z", alternative="two.sided", df=NULL) { # calculate p-value of a test
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

validate.alternative <- function(alternative) {
  switch(substr(alternative,1,1),
    t="two.sided",
    g="greater",
    l="less",
    stop("The parameter 'alternative' must be either 'two.sided', 'greater', or 'less' (or just the initial letter).")
  )
}

validate.numeric <- function(x, name, type="number") {
  if(any(is.na(x))) stop(paste("The parameter '", name, "' is NA", sep=""))
  if(any(is.infinite(x))) stop(paste("The parameter '", name, "' must be finite", sep=""))
  if(!is(x, "numeric") || length(x) != 1 || (type == "integer" && x %% 1 != 0)) stop(paste("The parameter '", name, "' must be a single ", type, sep=""))
}

validate.character <- function(x, name, vector.length) {
  if(any(is.na(x))) stop(paste("The parameter '", name, "' is NA", sep=""))
  if(!is(x, "character") || !(length(x) %in% vector.length)) stop(paste("The parameter '", name, "' must be a vector of ", paste(vector.length, collapse=" or "), " character strings", sep=""))
}

validate.logical <- function(x, name) {
  if(!is.null(x) && any(is.na(x))) stop(paste("The parameter '", name, "' is NA", sep=""))
  if(!is(x, "logical")) stop(paste("The parameter '", name, "' must be a logical", sep=""))
}

validate.numeric.range <- function(x, name, lower, upper) {
  if(x < lower || x > upper) stop(paste("The parameter '", name, "' must be a single number between ", lower, " and ", upper, sep=""))
}

validate.formula <- function(f) {
  is(f,"formula") && length(f) == 2 && length(f[[2]]) == 3 && length(f[[2]][[3]]) == 3 && length(f[[2]][[2]]) == 3 && length(f[[2]][[2]][[2]]) == 1 && length(f[[2]][[2]][[2]]) == 1 && length(f[[2]][[3]][[2]]) == 1 && length(f[[2]][[3]][[2]]) == 1 && f[[2]][[1]] == "|" && f[[2]][[2]][[1]] == "+"  && f[[2]][[3]][[1]] == "+" && length(all.vars(f)) >= 2 && length(all.vars(f)) <= 4 && f[[2]][[2]][[2]] != f[[2]][[2]][[3]] && f[[2]][[3]][[2]] != f[[2]][[3]][[3]]
}

validate.test <- function(test, null.value) {
  not.zou2007 <- test != "zou2007"
  if(null.value != 0 && any(not.zou2007)) {
    warning(paste("The ", ifelse(sum(not.zou2007) == 1, "test", "tests"), " '", paste(test[not.zou2007], collapse="', '"), "' can only be used if null.value = 0. Using the test 'zou2007' for comparison.", sep=""))
    "zou2007"
  } else test
}

paste.hypotheses <- function(alternative, variable1, variable2, null.value) {
  null.hypothesis <- {
    if(null.value == 0) paste(variable1, " is equal to ", variable2, sep="")
    else paste("the difference between ", variable1, " and ", variable2, " is ", null.value, sep="")
  }

  alternative.hypothesis <- {
    if(null.value == 0) paste(variable1, " is ", switch(alternative, two.sided="not equal to", greater="greater than", less="less than"), " ", variable2, sep="")
    else paste("the difference between ", variable1, " and ", variable2, " is ", switch(alternative, two.sided="not equal to", greater="greater than", less="less than"), " ", null.value, sep="")
  }

  paste("Null hypothesis: ", null.hypothesis, "\n",
  "Alternative hypothesis: ", alternative.hypothesis, " (", switch(alternative, two.sided="two", "one"), "-sided)", sep="")
}

paste.test.statistic <- function(object, test, r1.name=NULL, r2.name=NULL) {
  r <- slot(object, test)

  retained <- "Null hypothesis retained"
  rejected <- "Null hypothesis rejected"

  s <- {
    if(!is.null(r$statistic)) {
      evaluation <- {
        if(!is.nan(r$p.value) && r$p.value <= object@alpha) rejected
        else retained
      }
      paste("  ", r$distribution, " = ", fround(r$statistic,4), if(!is.null(r$df)) paste(", df = ", r$df, sep=""), ", p-value = ", fround(r$p.value,4), "\n  ", evaluation, sep="")
    } else NULL
  }

  if(!is.null(r$conf.int)) {
    evaluation <- {
      if(any(is.nan(r$conf.int))) {
        retained
      } else {
        switch(object@alternative,
          two.sided={
            if(r$conf.int[1] < object@null.value && r$conf.int[2] > object@null.value) paste(retained, " (Interval includes ", object@null.value, ")", sep="")
            else paste(rejected, " (Interval does not include ", object@null.value, ")", sep="")
          },
          greater={
            if(r$conf.int[1] > object@null.value) paste(rejected, " (Lower boundary > ", object@null.value, ")", sep="")
            else paste(retained, " (Lower boundary <= ", object@null.value, ")", sep="")
          },
          less={
            if(r$conf.int[2] < object@null.value) paste(rejected, " (Upper boundary < ", object@null.value, ")", sep="")
            else paste(retained, " (Upper boundary >= ", object@null.value, ")", sep="")
          }
        )
      }
    }

    if(!is.null(s)) s <- paste(s, "\n", sep="")
    s <- paste(s, "  ", object@conf.level * 100, "% confidence interval for ", r1.name, " - ", r2.name, ": ", paste(fround(r$conf.int,4), collapse=" "), "\n  ", evaluation, sep="")
  }

  paste(test, ": ", all.tests[[test]], "\n", s, "\n\n", sep="")
}

data.description <- function(data.name, var.labels) {
  if(length(data.name) < 2) { # cocor.dep.groups.overlap, cocor.dep.groups.nonoverlap
    paste(data.name, if(length(data.name) == 1 && length(var.labels) > 0) ": ", if(length(var.labels) > 0) paste(c("j", "k", "h", "m")[1:length(var.labels)], " = ", var.labels, collapse=", ", sep=""), sep="")
  } else if(length(data.name) == 2) { # cocor.indep.groups
    paste(data.name[1], if(length(var.labels) > 0) paste(": ", paste(c("j", "k"), " = ", var.labels[1:2], collapse=", ", sep=""), sep=""), "; ", data.name[2], if(length(var.labels) > 0) paste(": ", paste(c("h", "m"), " = ", var.labels[3:4], collapse=", ", sep=""), sep=""), sep="")
  }
}

paste.data.description <- function(data.name, var.labels) {
  x <- data.description(data.name, var.labels)
  if(length(x) == 1 && x != "") x <- paste("Data: ", x, "\n", sep="")
  x
}

setClass("cocor.indep.groups", # class for result object of cocor.indep.groups()
  representation(
    r1.jk="numeric",
    n1="numeric",
    r2.hm="numeric",
    n2="numeric",
    diff="numeric",
    alternative="character",
    test="character",
    alpha="numeric",
    conf.level="numeric",
    null.value="numeric",
    data.name="character",
    var.labels="character",
    fisher1925="list",
    zou2007="list"
  )
)

setMethod("show", "cocor.indep.groups",
  function(object) {
    cat("\n  Results of a comparison of two correlations based on independent groups\n\n",
    "Comparison between r1.jk ", if(length(object@var.labels) > 0) paste("(", object@var.labels[1], ", ", object@var.labels[2], ") ", sep=""), "= ", round(object@r1.jk,4), " and r2.hm ", if(length(object@var.labels) > 0) paste("(", object@var.labels[3], ", ", object@var.labels[4], ") ", sep=""), "= ", round(object@r2.hm,4), "\n",
    "Difference: r1.jk - r2.hm = ", round(object@diff,4), "\n",
    paste.data.description(object@data.name, object@var.labels),
    "Group sizes: n1 = ", object@n1, ", n2 = ", object@n2, "\n",
    paste.hypotheses(object@alternative, "r1.jk", "r2.hm", object@null.value), "\n",
    "Alpha: ", object@alpha, "\n\n", sep="")

    for(m in object@test) {
      cat(paste.test.statistic(object, m, "r1.jk", "r2.hm"))
    }
  }
)



setClass("cocor.dep.groups.overlap", # class for result object of cocor.dep.groups.overlap()
  representation(
    r.jk="numeric",
    r.jh="numeric",
    r.kh="numeric",
    n="numeric",
    diff="numeric",
    alternative="character",
    test="character",
    alpha="numeric",
    conf.level="numeric",
    null.value="numeric",
    data.name="character",
    var.labels="character",
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
      "Comparison between r.jk ", if(length(object@var.labels) > 0) paste("(", object@var.labels[1], ", ", object@var.labels[2], ") ", sep=""), "= ", round(object@r.jk,4), " and r.jh ", if(length(object@var.labels) > 0) paste("(", object@var.labels[1], ", ", object@var.labels[3], ") ", sep=""), "= ", round(object@r.jh,4), "\n",
      "Difference: r.jk - r.jh = ", round(object@diff,4), "\n",
      "Related correlation: r.kh = ", round(object@r.kh,4), "\n",
      paste.data.description(object@data.name, object@var.labels),
      "Group size: n = ", object@n, "\n",
      paste.hypotheses(object@alternative, "r.jk", "r.jh", object@null.value), "\n",
      "Alpha: ", object@alpha, "\n\n", sep=""
    )

    for(m in object@test) {
      cat(paste.test.statistic(object, m, "r.jk", "r.jh"))
    }
  }
)



setClass("cocor.dep.groups.nonoverlap", # class for result object of cocor.dep.groups.nonoverlap()
  representation(
    r.jk="numeric",
    r.hm="numeric",
    r.jh="numeric",
    r.jm="numeric",
    r.kh="numeric",
    r.km="numeric",
    n="numeric",
    diff="numeric",
    alternative="character",
    test="character",
    alpha="numeric",
    conf.level="numeric",
    null.value="numeric",
    data.name="character",
    var.labels="character",
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
      "Comparison between r.jk ", if(length(object@var.labels) > 0) paste("(", object@var.labels[1], ", ", object@var.labels[2], ") ", sep=""), "= ", round(object@r.jk,4), " and r.hm ", if(length(object@var.labels) > 0) paste("(", object@var.labels[3], ", ", object@var.labels[4], ") ", sep=""), "= ", round(object@r.hm,4), "\n",
      "Difference: r.jk - r.hm = ", round(object@diff,4), "\n",
      "Related correlations: r.jh = ", round(object@r.jh,4), ", r.jm = ", round(object@r.jm,4),", r.kh = ", round(object@r.kh,4), ", r.km = ", round(object@r.km,4), "\n",
      paste.data.description(object@data.name, object@var.labels),
      "Group size: n = ", object@n, "\n",
      paste.hypotheses(object@alternative, "r.jk", "r.hm", object@null.value), "\n",
      "Alpha: ", object@alpha, "\n\n", sep=""
    )

    for(m in object@test) {
      cat(paste.test.statistic(object, m, "r.jk", "r.hm"))
    }
  }
)

setClassUnion("cocor", c("cocor.indep.groups", "cocor.dep.groups.overlap", "cocor.dep.groups.nonoverlap"))
