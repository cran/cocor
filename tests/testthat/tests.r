library(testthat)

context("Formula validation")

test_that("Valid formulas are accepted", {
  expect_that(validate.formula(~ a + b | a + b), equals(TRUE))
  expect_that(validate.formula(~ a + b | a + c), equals(TRUE))
  expect_that(validate.formula(~ a + b | c + d), equals(TRUE))
})

test_that("Invalid formulas are rejected", {
  expect_that(validate.formula("~ a + b | a + c"), equals(FALSE))
  expect_that(validate.formula(~ a), equals(FALSE))
  expect_that(validate.formula(~ a + b), equals(FALSE))
  expect_that(validate.formula(~ a + b + c), equals(FALSE))
  expect_that(validate.formula(~ a + b + a + c), equals(FALSE))
  expect_that(validate.formula(~ a + a | a + a), equals(FALSE))
  expect_that(validate.formula(~ a + a | b + b), equals(FALSE))
  expect_that(validate.formula(~ a + b + c | a + c), equals(FALSE))
  expect_that(validate.formula(~ a + b | b + c + d), equals(FALSE))
  expect_that(validate.formula(~ a | b + a + c), equals(FALSE))
  expect_that(validate.formula(y ~ a + b | a + c), equals(FALSE))
  expect_that(validate.formula(~ a | a + c), equals(FALSE))
  expect_that(validate.formula(~ a + b | a), equals(FALSE))
  expect_that(validate.formula(~ a | b), equals(FALSE))
  expect_that(validate.formula(~ a + a | a + c), equals(FALSE))
  expect_that(validate.formula(~ a + b | a + a), equals(FALSE))
  expect_that(validate.formula(~ a + a | b + c), equals(FALSE))
  expect_that(validate.formula(~ a + b | c + c), equals(FALSE))
})

test_that("Invalid data are rejected", {
  expect_error(cocor(~a + b | c + d, 1:4), "The parameter 'data' must be")
  expect_error(cocor(~a + b | c + d, list(1,2)), "The parameter 'data' must be")
  expect_error(cocor(~a + b | c + d + e, list(data.frame(1),data.frame(1))), "For correlations based on independent groups, the parameter 'formula' must be")
  expect_error(cocor(~a + b | c + d + e, data.frame(1)), "For correlations based on dependent groups, the parameter 'formula' must be")
  expect_error(cocor(~a + b | a + b, data.frame(1)), "For correlations based on dependent groups, the parameter 'formula' must be")

  expect_error(cocor(~a + b | a + b, list(data.frame(a=1:2,c=3:4),data.frame(a=1:2,b=3:4))), "Could not find column 'b' .* first")
  expect_error(cocor(~a + b | a + b, list(data.frame(a=1:2,b=3:4),data.frame(a=1:2,c=3:4))), "Could not find column 'b' .* second")
  expect_error(cocor(~a + b | a + c, data.frame(a=1:2,c=3:4)), "Could not find column 'b'")

  expect_error(cocor.indep.groups(Inf, .82, 60, 60), "The parameter 'r1.jk' must be finite")
  expect_error(cocor.indep.groups(.92, .82, -Inf, 60), "The parameter 'n1' must be finite")
  expect_error(cocor(~a + b | a + c, data.frame(a=1:2,b=c("1","2"))), "The variable 'b' must be numeric")
})

test_that("Missing data is handled correctly", {
  expect_error(cocor.indep.groups(NA, .82, 60, 60), "The parameter 'r1.jk' is NA")
  expect_error(cocor.dep.groups.overlap(.82, .60, .3, NA), "The parameter 'n' is NA")
  expect_error(cocor.dep.groups.nonoverlap(.6, NA,.8,.82, .60, .1, 60), "The parameter 'r.hm' is NA")


  # cocor.dep.groups.overlap
  d <- swiss
  r <- cocor(~Fertility + Agriculture | Fertility + Examination, d, na.action="na.omit")
  expect_that(r@n, equals(47))

  d[1:10, "Catholic"] <- NA # set 10 cases in column to NA

  r <- cocor(~Fertility + Agriculture | Fertility + Examination, d, na.action="na.omit")
  expect_that(r@n, equals(47))

  d[1:10, "Agriculture"] <- NA # set 10 cases in column to NA

  expect_error(cocor(~Fertility + Agriculture | Fertility + Examination, d, na.action="na.fail"), "missing values in object")

  r <- cocor(~Fertility + Agriculture | Fertility + Examination, d, na.action="na.omit")
  expect_that(r@n, equals(37))

  d[1:nrow(d), "Agriculture"] <- NA # set whole column to NA

  expect_error(cocor(~Fertility + Agriculture | Fertility + Examination, d, na.action="na.omit"), "No cases left in the data set after running na.action")


  # cocor.indep.groups
  d2 <- list(swiss, swiss)
  d2[[1]][1:10, "Agriculture"] <- NA # set 10 cases in column to NA

  expect_error(cocor(~Fertility + Agriculture | Fertility + Examination, d2, na.action="na.fail"), "missing values in object")

  r <- cocor(~Fertility + Agriculture | Fertility + Examination, d2, na.action="na.omit")
  expect_that(r@n1, equals(37))

  d2[[1]][1:nrow(d), "Agriculture"] <- NA # et whole column to NA

  expect_error(cocor(~Fertility + Agriculture | Fertility + Examination, d2, na.action="na.omit"), "No cases left in the first data set after running na.action")
})

test_that("Output is correct", {
  expect_output(print(cocor.indep.groups(.92, .82, 60, 50, data.name="foo")), "Data: foo")
  expect_output(print(cocor.indep.groups(.92, .82, 60, 50, data.name=c("foo", "bar"))), "Data: foo; bar")
  expect_output(print(cocor.indep.groups(.92, .82, 60, 50, var.labels=c("foo", "bar", "bay", "baz"))), "bar")
  expect_output(print(cocor.indep.groups(.92, .82, 60, 50, data.name="mydata", var.labels=c("foo", "bar", "bay", "baz"))), "Data: mydata: j = foo, k = bar, h = bay, m = baz")
  expect_output(print(cocor.indep.groups(.92, .82, 60, 50, data.name=c("data1", "data2"), var.labels=c("foo", "bar", "bay", "baz"))), "Data: data1: j = foo, k = bar; data2: h = bay, m = baz")
  expect_output(print(cocor(~Fertility + Agriculture | Fertility + Examination, swiss)), "Data: swiss: j = Fertility, k = Agriculture, h = Examination")
  expect_output(print(cocor(~a + b | c + d, list(dataset1=data.frame(a=1:3,b=1:3),dataset2=data.frame(c=2:4,d=2:4)))), "Data: dataset1: j = a, k = b; dataset2: h = c, m = d")
  
  expect_output(print(cocor.indep.groups(r1.jk=-.045, r2.hm=.221, n1=160, n2=87, alternative="less", data.name="foo")), "Null hypothesis rejected \\(Upper boundary < 0\\)")
  expect_output(print(cocor.indep.groups(r1.jk=-.045, r2.hm=.221, n1=160, n2=87, alternative="greater", data.name="foo")), "Null hypothesis retained \\(Lower boundary <= 0\\)")
  expect_output(print(cocor.indep.groups(r1.jk=.221, r2.hm=-.045, n1=160, n2=87, alternative="less", data.name="foo")), "Null hypothesis retained \\(Upper boundary >= 0\\)")
  expect_output(print(cocor.indep.groups(r1.jk=.221, r2.hm=-.045, n1=160, n2=87, alternative="greater", data.name="foo")), "Null hypothesis rejected \\(Lower boundary > 0\\)")
})


##====================##
## cocor.indep.groups ##
##====================##

context("cocor.indep.groups")

## fisher1925
test_that("Test fisher1925", {
  # Guilford, J. P., & Fruchter, B. (1978). Fundamental statistics in psychology and eduction (6th ed.). NY: McGraw-Hill. p. 164

  r <- cocor.indep.groups(.92, .82, 60, 50, test="fisher1925")
  expect_that(round(r@fisher1925$statistic, 2), equals(2.19))  # the value in the book is actually 2.18


  # Glass, G. V., & Stanley, J. C. (1970). Statistical methods in eduction and psychology. Englewood Cliffs, NJ: Prentice-Hall. p. 312

  r <- cocor.indep.groups(.71, .28, 200, 78, test="fisher1925")
  expect_that(round(r@fisher1925$statistic, 2), equals(4.42)) # the value in the book is actually 4.40


  # Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). Applied Multiple Regression/Correlation Analysis for the Behavioral Sciences (3rd ed.). Mahwah, NJ: Erlbaum. Formulas 2.8.4 (p. 45) and 2.8.11 (p. 49).

  r <- cocor.indep.groups(.657, .430, 62, 143, test="fisher1925")
  expect_that(round(r@fisher1925$statistic, 6), equals(2.110737)) # the actual calculation in the paper is done with only two digits and yields 2.13
  expect_that(r@fisher1925$distribution, equals("z"))


  # Excel spreadsheet from http://stat-help.com/spreadsheets.html

  r <- cocor.indep.groups(.4, .5, 100, 100, test="fisher1925")
  expect_that(round(r@fisher1925$statistic, 10), equals(-0.8751012342))
  expect_that(round(r@fisher1925$p.value, 10), equals(0.3815188256))
})

## zou2007
test_that("Test zou2007", {
  # Zou, G. Y. (2007). Toward Using Confidence Intervals to Compare Correlations. Psychological Methods, 12, 399-413. pp. 406, 409

  r <- cocor.indep.groups(.49, .36, 145, 87, test="zou2007")
  expect_that(round(r@zou2007$conf.int, 3), equals(c(-.087,.359)))
})

##==========================##
## cocor.dep.groups.overlap ##
##==========================##

context("cocor.dep.groups.overlap")

## hotelling1940
test_that("Test hotelling1940", {
  # May, K., & Hittner, J. B. (1997). A note on statistics for comparing dependent correlations. Psychological Reports, 80, 475-480.

  r <- cocor.dep.groups.overlap(.5, .2, 0, 50, test="hotelling1940")
  expect_that(r@hotelling1940$distribution, equals("t"))
  expect_that(round(r@hotelling1940$statistic, 2), equals(1.73))

  r <- cocor.dep.groups.overlap(.5, .2, .1, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(1.80))

  r <- cocor.dep.groups.overlap(.5, .2, .2, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(1.89))

  r <- cocor.dep.groups.overlap(.5, .2, .3, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(2.01))

  r <- cocor.dep.groups.overlap(.5, .2, .4, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(2.17))

  r <- cocor.dep.groups.overlap(.5, .2, .5, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(2.38))

  r <- cocor.dep.groups.overlap(.5, .2, .6, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(2.68))

  r <- cocor.dep.groups.overlap(.5, .2, .7, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(3.16))

  r <- cocor.dep.groups.overlap(.5, .2, .8, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(4.07))


  r <- cocor.dep.groups.overlap(.7, .4, 0, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(2.46))

  r <- cocor.dep.groups.overlap(.7, .4, .1, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(2.42))

  r <- cocor.dep.groups.overlap(.7, .4, .2, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(2.45))

  r <- cocor.dep.groups.overlap(.7, .4, .3, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(2.53))

  r <- cocor.dep.groups.overlap(.7, .4, .4, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(2.67))

  r <- cocor.dep.groups.overlap(.7, .4, .5, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(2.89))

  r <- cocor.dep.groups.overlap(.7, .4, .6, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(3.22))

  r <- cocor.dep.groups.overlap(.7, .4, .7, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(3.78))

  r <- cocor.dep.groups.overlap(.7, .4, .8, 50, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(4.91))


  # Glass, G. V., & Stanley, J. C. (1984). Statistical methods in eduction and psychology (2nd ed.). Englewood Cliffs, NJ: Prentice-Hall. p. 311

  r <- cocor.dep.groups.overlap(.612, .541, .466, 157, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 3), equals(1.158))


  # Guilford, J. P., & Fruchter, B. (1978). Fundamental statistics in psychology and eduction (6th ed.). NY: McGraw-Hill. p. 164

  r <- cocor.dep.groups.overlap(.55, .45, .60, 200, test="hotelling1940")
  expect_that(round(r@hotelling1940$statistic, 2), equals(1.91))
})

## hendrickson1970
test_that("Test hendrickson1970", {
  # May, K., & Hittner, J. B. (1997). A note on statistics for comparing dependent correlations. Psychological Reports, 80, 475-480. p. 477

  r <- cocor.dep.groups.overlap(.5, .2, .3, 50, test="hendrickson1970")
  expect_that(r@hendrickson1970$distribution, equals("t"))
  expect_that(round(r@hendrickson1970$statistic, 2), equals(2.01))
})

## williams1959
test_that("Test williams1959", {
  # depcor: Silver, N. C., Hittner, J. B. and May, M. (2006). A FORTRAN 77 Program for Comparing Dependent Correlations. Applied Psychological Measurement, 30, 152-153. doi:10.1177/0146621605277132

  r <- cocor.dep.groups.overlap(.3, .4, .5, 100, test="williams1959")
  expect_that(r@williams1959$distribution, equals("t"))
  expect_that(round(r@williams1959$statistic, 4), equals(-1.0767))
  expect_that(round(r@williams1959$p.value, 4), equals(0.2843))
})

## olkin1967
test_that("Test olkin1967", {
  # May, K., & Hittner, J. B. (1997). A note on statistics for comparing dependent correlations. Psychological Reports, 80, 475-480.

  r <- cocor.dep.groups.overlap(.5, .2, .3, 50, test="olkin1967")
  expect_that(round(r@olkin1967$statistic, 2), equals(2.00))
  expect_that(r@olkin1967$distribution, equals("z"))


  # Glass, G. V., & Stanley, J. C. (1970). Statistical methods in eduction and psychology. Englewood Cliffs, NJ: Prentice-Hall. p. 314

  r <- cocor.dep.groups.overlap(.56, .43, .52, 100, test="olkin1967")
  expect_that(round(r@olkin1967$statistic, 2), equals(1.59)) # the value in the book is actually 1.60
})

## dunn1969
test_that("Test dunn1969", {
  # Dunn, O. J. & Clark, V. A. (1969). Correlation coefficients measured on the same individuals. Journal of the American Statistical Association, 64, 366-377.

  r <- cocor.dep.groups.overlap(.3, .4, .5, 100, test="dunn1969")
  expect_that(round(r@dunn1969$statistic, 4), equals(-1.0730))
  expect_that(r@dunn1969$distribution, equals("z"))
  expect_that(round(r@dunn1969$p.value, 4), equals(0.2833))


  # Excel spreadsheet from http://stat-help.com/spreadsheets.html

  r <- cocor.dep.groups.overlap(.5, .6, .7, 100, test="dunn1969")
  expect_that(round(r@dunn1969$statistic, 10), equals(-1.5838851123))
  expect_that(round(r@dunn1969$p.value, 10), equals(0.1132198659))
})

## steiger1980
test_that("Test steiger1980", {
  # Steiger, J. H. (1980). Tests for comparing elements of a correlation matrix. Psychological Bulletin, 87, 245-251.
  # Case A (p. 249)

  r <- cocor.dep.groups.overlap(r.jk=.4, r.jh=.5, r.kh=.1, n=103, test="steiger1980")
  expect_that(round(r@steiger1980$statistic, 7), equals(-0.8887185)) # The value in the paper is actually -.8913. A value closer to that (-0.8904025) can be obtained using the covariance of .0042 reported in the paper.
  expect_that(r@steiger1980$distribution, equals("z"))


  # depcor: Silver, N. C., Hittner, J. B. and May, M. (2006). A FORTRAN 77 Program for Comparing Dependent Correlations. Applied Psychological Measurement, 30, 152-153. doi:10.1177/0146621605277132

  r <- cocor.dep.groups.overlap(.3, .4, .5, 100, test="steiger1980")
  expect_that(round(r@steiger1980$statistic, 4), equals(-1.0718))
  expect_that(round(r@steiger1980$p.value, 4), equals(0.2838))
})

## hittner2003
test_that("Test hittner2003", {
  # depcor: Silver, N. C., Hittner, J. B. and May, M. (2006). A FORTRAN 77 Program for Comparing Dependent Correlations. Applied Psychological Measurement, 30, 152-153. doi:10.1177/0146621605277132

  r <- cocor.dep.groups.overlap(.3, .4, .5, 100, test="hittner2003")
  expect_that(round(r@hittner2003$statistic, 4), equals(-1.0715))
  expect_that(round(r@hittner2003$p.value, 4), equals(0.2839))
})

## meng1992
test_that("Test meng1992", {
  # Meng, X. L., Rosenthal, R., & Rubin, D. B. (1992). Comparing correlated correlation coefficients. Psychological Bulletin, 111, 172-175.

  r <- cocor.dep.groups.overlap(r.jk=.63, r.jh=-.03, r.kh=-.19, n=15, alternative="greater", test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(1.68))
  expect_that(r@meng1992$distribution, equals("z"))
  expect_that(round(r@meng1992$p.value, 3), equals(.047))
  expect_that(round(r@meng1992$conf.int, 3), equals(c(-0.129, 1.672))) # the upper boundary reported in the paper is actually 1.671


  # depcor: Silver, N. C., Hittner, J. B. and May, M. (2006). A FORTRAN 77 Program for Comparing Dependent Correlations. Applied Psychological Measurement, 30, 152-153. doi:10.1177/0146621605277132

  r <- cocor.dep.groups.overlap(.3, .4, .5, 100, test="meng1992")
  expect_that(round(r@meng1992$statistic, 4), equals(-1.0707))
  expect_that(round(r@meng1992$p.value, 4), equals(0.2843))


  # May, K., & Hittner, J. B. (1997). A note on statistics for comparing dependent correlations. Psychological Reports, 80, 475-480.

  r <- cocor.dep.groups.overlap(.5, .2, 0, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(1.62))

  r <- cocor.dep.groups.overlap(.5, .2, .1, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(1.70))

  r <- cocor.dep.groups.overlap(.5, .2, .2, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(1.80))

  r <- cocor.dep.groups.overlap(.5, .2, .3, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(1.91))

  r <- cocor.dep.groups.overlap(.5, .2, .4, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(2.06))

  r <- cocor.dep.groups.overlap(.5, .2, .5, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 3), equals(2.245)) # the value reported in the paper is actually 2.24

  r <- cocor.dep.groups.overlap(.5, .2, .6, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(2.50))

  r <- cocor.dep.groups.overlap(.5, .2, .7, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(2.87))

  r <- cocor.dep.groups.overlap(.5, .2, .8, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(3.50))


  r <- cocor.dep.groups.overlap(.7, .4, 0, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(2.03))

  r <- cocor.dep.groups.overlap(.7, .4, .1, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(2.10))

  r <- cocor.dep.groups.overlap(.7, .4, .2, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(2.20))

  r <- cocor.dep.groups.overlap(.7, .4, .3, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(2.32))

  r <- cocor.dep.groups.overlap(.7, .4, .4, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(2.47))

  r <- cocor.dep.groups.overlap(.7, .4, .5, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(2.66))

  r <- cocor.dep.groups.overlap(.7, .4, .6, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(2.94))

  r <- cocor.dep.groups.overlap(.7, .4, .7, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(3.35))

  r <- cocor.dep.groups.overlap(.7, .4, .8, 50, test="meng1992")
  expect_that(round(r@meng1992$statistic, 2), equals(4.05))
})

## zou2007
test_that("Test zou2007", {
  # Zou, G. Y. (2007). Toward Using Confidence Intervals to Compare Correlations. Psychological Methods, 12, 399-413.

  r <- cocor.dep.groups.overlap(.396, .179, .088, 66, test="zou2007")
  expect_that(round(r@zou2007$conf.int, 3), equals(c(-.093,.517)))
})

##=============================##
## cocor.dep.groups.nonoverlap ##
##=============================##

context("cocor.dep.groups.nonoverlap")

## pearson1898
test_that("Test pearson1898", {
  # Raghunatan, T. E., Rosenthal, R., & Rubin, D. B. (1996). Comparing Correlated but Nonoverlapping Correlations. Psychological Methods, 1, 178-183. p. 180

  r <- cocor.dep.groups.nonoverlap(.38, .25, .45, .53, .31, .55, 603, test="pearson1898")
  expect_that(round(r@pearson1898$statistic, 2), equals(2.88))
  expect_that(r@pearson1898$distribution, equals("z"))
})

## dunn1969
test_that("Test dunn1969", {
  # Excel spreadsheet from http://stat-help.com/spreadsheets.html

  r <- cocor.dep.groups.nonoverlap(.3, .4, .5, .6, .7, .8, 100, test="dunn1969")
  expect_that(round(r@dunn1969$statistic, 2), equals(-1.06))
  expect_that(r@dunn1969$distribution, equals("z"))
  expect_that(round(r@dunn1969$p.value, 10), equals(0.2892685563))


  # depcor: Silver, N. C., Hittner, J. B. and May, M. (2006). A FORTRAN 77 Program for Comparing Dependent Correlations. Applied Psychological Measurement, 30, 152-153. doi:10.1177/0146621605277132

  r <- cocor.dep.groups.nonoverlap(r.jk=.3, r.hm=.4, r.jh=.5, r.kh=.6, r.km=.7, r.jm=.8, 100, test="dunn1969")
  expect_that(round(r@dunn1969$statistic, 4), equals(-1.0723))
  expect_that(round(r@dunn1969$p.value, 4), equals(0.2836))
})

## steiger1980
test_that("Test steiger1980", {
  # Steiger, J. H. (1980). Tests for comparing elements of a correlation matrix. Psychological Bulletin, 87, 245-251.
  # Case B (p. 249)

  # unambiguously reported values: r.jm = .5, r.kh = .5, r.km = .7, n = 103
  # values confirmed using the fisher z-values: r.jk = .5, r.hm = .6
  # guessed: r.jh = .8 (in the paper it says r.jk = .8, this seems to be wrong. it should be r.jh = .8)

  r <- cocor.dep.groups.nonoverlap(r.jk=.5, r.hm=.6, r.jh=.8, r.jm=.5, r.kh=.5, r.km=.7, n=103, test="steiger1980")
  expect_that(round(r@steiger1980$statistic, 4), equals(-1.4050)) # the value in the paper is actually -1.4045
  expect_that(r@steiger1980$distribution, equals("z"))


  # depcor: Silver, N. C., Hittner, J. B. and May, M. (2006). A FORTRAN 77 Program for Comparing Dependent Correlations. Applied Psychological Measurement, 30, 152-153. doi:10.1177/0146621605277132

  r <- cocor.dep.groups.nonoverlap(r.jk=.3, r.hm=.4, r.jh=.5, r.kh=.6, r.km=.7, r.jm=.8, n=100, test="steiger1980")
  expect_that(round(r@steiger1980$statistic, 4), equals(-1.0744))
  expect_that(round(r@steiger1980$p.value, 4), equals(0.2827))
})

## raghunathan1996
test_that("Test raghunathan1996", {
  # Raghunatan, T. E., Rosenthal, R., & Rubin, D. B. (1996). Comparing Correlated but Nonoverlapping Correlations. Psychological Methods, 1, 178-183. p. 180

  r <- cocor.dep.groups.nonoverlap(.38, .25, .45, .53, .31, .55, 603, test="raghunathan1996")
  expect_that(round(r@raghunathan1996$statistic, 6), equals(2.869174))  # The value reported in the paper is a little higher (2.98) because only two decimal places are considered for the z-transformed coefficients.
  expect_that(r@raghunathan1996$distribution, equals("z"))
})

## silver2004
test_that("Test silver2004", {
  # depcor: Silver, N. C., Hittner, J. B. and May, M. (2006). A FORTRAN 77 Program for Comparing Dependent Correlations. Applied Psychological Measurement, 30, 152-153. doi:10.1177/0146621605277132

  r <- cocor.dep.groups.nonoverlap(r.jk=.3, r.hm=.4, r.jh=.5, r.kh=.6, r.km=.7, r.jm=.8, n=100, test="silver2004")
  expect_that(round(r@silver2004$statistic, 4), equals(-1.0737))
  expect_that(r@silver2004$distribution, equals("z"))
  expect_that(round(r@silver2004$p.value, 4), equals(0.2830))
})

## zou2007
test_that("Test zou2007", {
  # Zou, G. Y. (2007). Toward Using Confidence Intervals to Compare Correlations. Psychological Methods, 12, 399-413.
  # The correlation between r.jk and r.hm calculated in the paper seems to be incorrect (.0917 instead of .0891), the given formula is, however, correct. The same formula is used in dunn1969 and it also provides .0891.

  r <- cocor.dep.groups.nonoverlap(r.jk=.396, r.hm=.189, r.jh=.208, r.kh=.023, r.km=.423, r.jm=.143, n=66, test="zou2007")
  expect_that(round(r@zou2007$conf.int, 3), equals(c(-.096,.501))) # the actual value in the paper is -.096 and .500
})
