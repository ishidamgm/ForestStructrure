### raster2_test.R
library(devtools)
library(roxygen2)
library(testthat)

document(pkg = ".", roclets = NULL, quiet = FALSE)
install.packages(".")
install.packages("/Users/ishidam/Dropbox/00D/00/kuraiyama/all/R/raster2_0.1.1.tgz")
install.packages("~/Dropbox/00D/00/kuraiyama/all/R/raster2_0.1.1.tgz", repos = NULL, type = .Platform$pkgType)
library(raster2)
help(package="raster2")
remove.packages(raster2)

library(devtools)
install_github("ishidamgm/rpackage",force = T)
library(rpackage)
help(package="rpackage")
#remove.packages(rpackage)
hello()
hello2()

install_github("ishidamgm/rpackage")

hello()
hello2()
help(package="rpackage")

#
# >>> /usr/bin/git push
# fatal: No configured push destination.
# Either specify the URL from the command-line or configure a remote repository using
#
# git remote add <name> <url>
#
#   and then push using the remote name
#
# git push <name>
#



