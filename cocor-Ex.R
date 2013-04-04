pkgname <- "cocor"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('cocor')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("cocor.dep.groups.nonoverlap")
### * cocor.dep.groups.nonoverlap

flush(stderr()); flush(stdout())

### Name: cocor.dep.groups.nonoverlap
### Title: Compare two nonoverlapping correlations based on dependent
###   groups
### Aliases: cocor.dep.groups.nonoverlap

### ** Examples

# Compare the difference between the correlations (age, intelligence) and
# body mass (index, shoe size) measured in the same group (all values are fictional):
r.jk <- .2  # Correlation (age, intelligence)
r.hm <- .7  # Correlation (body mass index, shoe size)
r.jh <- .4  # Correlation (age, body mass index)
r.jm <- .5  # Correlation (age, shoe size)
r.kh <- .1  # Correlation (intelligence, body mass index)
r.km <- .3  # Correlation (intelligence, shoe size)
n <- 232  # Size of the group

cocor.dep.groups.nonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n)



cleanEx()
nameEx("cocor.dep.groups.overlap")
### * cocor.dep.groups.overlap

flush(stderr()); flush(stdout())

### Name: cocor.dep.groups.overlap
### Title: Compare two overlapping correlations based on dependent groups
### Aliases: cocor.dep.groups.overlap

### ** Examples

# Compare the difference between the correlations (age, intelligence) and
# (age, shoe size) measured in the same group (all values are fictional):
r.jk <- .2  # Correlation (age, intelligence)
r.jh <- .5  # Correlation (age, shoe size)
r.kh <- .1  # Correlation (intelligence, shoe size)
n <- 315  # Size of the group

cocor.dep.groups.overlap(r.jk, r.jh, r.kh, n)



cleanEx()
nameEx("cocor.indep.groups")
### * cocor.indep.groups

flush(stderr()); flush(stdout())

### Name: cocor.indep.groups
### Title: Compare two correlations based on independent groups
### Aliases: cocor.indep.groups

### ** Examples

# Compare the difference between two correlations based
# on two independent groups:
r1 <- .7  # Correlation measured in group 1
n1 <- 305  # Size of group 1

r2 <- .6  # Correlation measured in group 2
n2 <- 210  # Size of group 2

cocor.indep.groups(r1, r2, n1, n2)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
