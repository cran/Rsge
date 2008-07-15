.First.lib <-  function(libname, pkgname) {
  require(snow);
  cat(paste("\nWelcome to Rsge\n"));
  cat(paste("    Version:", packageDescription(pkgname, field = "Version"), "\n\n"));
}

