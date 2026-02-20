##' @title Pedigree Example Data Set
##' @name ks1988
##' @description Pedigree example data set. Reproducible results.
##'     Check data and outputs in reference (\insertCite{ks;textual}{yabaf}).
##' @format data frame with four columns: replication, block, treat and response
##' @usage data(ks1988)
##' @references \insertAllCited{}
##' @rdname ks1988
##' @docType data
##' @importFrom Rdpack reprompt
NULL

##' @title Concrete interface for Pedigrees
##' @description Specialized Concrete Definition.
##' @name Pedigree-class
##' @exportClass Pedigree
##' @seealso [Breeder() and SplitPlot()] for the pure virtual and concrete
##'     abstract template.
##' @field structure list with the additive relationship matrix.
##' @import methods AGHmatrix
##' @examples
##' data(ks1988, package = "yabaf")
##' b <- Breeder(ks1988)
##' p <- Pedigree(b)
##' p$structure$additive
##' @export Pedigree
Pedigree <- setRefClass(
    Class = "Pedigree",
    contains = "Breeder",
    fields = c("structure" = "list"),
    methods = list(
        initialize = function(d) {
            if(inherits(d, "Breeder")) {
                .self$data <- d$data
                .self$log <- d$log
                .self$structure <- .self$process()
            }
        },
        process = function() {

            ## check duplicates and uniqueness
            if ( anyDuplicated(.self$data) )
                stop("Pedigree with duplicated entries.")
           
            if ( nrow(.self$data) != length(unique(.self$data[, "calf"])) )
                stop("Pedigree with non-unique entries.")

            A <- AGHmatrix::Amatrix(subset(transform(.self$data,
                                                     Ind = calf,
                                                     Par1 = ifelse(is.na(sire), 0, sire),
                                                     Par2 = ifelse(is.na(dam), 0, dam)),
                                           select = c(Ind, Par1, Par2)),
                                    ploidy = 2, verify = FALSE)
            ## Id <- diag(nrow = nrow(A))
            ## R <- Matrix(chol(A), sparse = TRUE)
            ## d <- Matrix::diag(R)
            ## T <- Matrix::tril(x = Matrix::t(R) %*% Matrix::Diagonal(x = 1/d), k = -1)
            ## L <- Id + T
            ## D <- Matrix::Diagonal(x = d^2) 
            .self$appendLog(event = "structure")
            .self$structure <-  list("size" = nrow(A), "additive" = A)
        },
        asList = function() {
            return(list(data = .self$data, structure = .self$structure, log = .self$log))
        }
    )
)

setValidity(
    Class = "Pedigree",
    method = function(object) {
        if (all(c("calf", "sire", "dam") %in% names(object$data))) {
            TRUE
        } else {
            "pedigrees must contain calf, sire and dam data"            
        }
    }
)
