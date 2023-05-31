# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)

# Hack for testthat to allow running tests without needing an R package. This requires calling
# test_file or test_dir directly form the R console.
rlang::env_unlock(env = asNamespace('testthat'))
rlang::env_binding_unlock(env = asNamespace('testthat'))
assign('rstudio_tickle', function(){}, envir = asNamespace('testthat'))
rlang::env_binding_lock(env = asNamespace('testthat'))
rlang::env_lock(asNamespace('testthat'))
