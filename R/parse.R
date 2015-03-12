
#' Parse output from MS
#'
#' \code{parseMS} parses results from an MS simulation, returning a list of results.
#'
#' @param file filename to MS simulation results.
#'
#' @return A list containing each simulation's data.
#'
#' @examples
#' res <- parseMS(system.file("extdata", "ms-01.sim", package="mstools"))
#' summary(sapply(res, function(x) x$segsites))
#'
#' @export
parseMS <- function(file) {
   # simple MS parser
   lns <- readLines(file)
   header <- lns[1:3]
   body <- lns[-c(1:3)]
   brks <- body == "//"
   groups <- cumsum(brks)
   sims <- split(body, groups)
   # sanity check
   stopifnot(all(sapply(sims, '[[', 1) == "//"))
   lapply(seq_along(sims), function(i) {
       # drop //
       x <- sims[[i]][-1] # -1 to drop //
       # parse positions and segsites
       pos_str <- unlist(strsplit(sub("positions: (.*) *$", "\\1", x[2]), " "))
       pos <- as.numeric(pos_str)
       segsites <- as.integer(sub("segsites: (\\d+)", "\\1", x[1]))
       haps <- x[-c(1, 2)] # drop header
       haps <- haps[-length(haps)] # drop empty line
       list(sim=i, position=pos, segsites=segsites, haplotypes=haps)
   })
}








