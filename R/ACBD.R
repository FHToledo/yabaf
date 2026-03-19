##' @title ACBD Example Data Set
##' @name wheat
##' @description Augmented complete block design example data set. Reproducible results.
##'     Check data and outputs.
##' @format data frame with six columns: row, column, block, treat, role and response
##' @usage data(wheat)
##' @rdname wheat
##' @docType data
##' @importFrom Rdpack reprompt
NULL

##' @title Concrete interface for ACBD designs
##' @description Specialized Concrete Definition.
##' @name ACBD-class
##' @exportClass ACBD
##' @seealso [Breeder() and SplitPlot()] for the pure virtual and concrete
##'     abstract template.
##' @field report list or analytical summary.
##' @import methods asreml
##' @examples
##' data(wheat, package = "yabaf")
##' b <- Breeder(wheat)
##' a <- ACBD(b)
##' a$report$fit$sigma
##' @export ACBD
ACBD <- setRefClass(
    Class = "ACBD",
    contains = "Breeder",
    fields = c("report" = "list"),
    methods = list(
        initialize = function(d) {
            if(inherits(d, "Breeder")) {
                .self$data <- d$data
                .self$log <- d$log

                if (!is.factor(.self$data$row))
                    .self$data$row <- factor(.self$data$row)

                if (!is.factor(.self$data$column))
                    .self$data$column <- factor(.self$data$column)

                if (!is.factor(.self$data$role))
                    .self$data$role <- factor(.self$data$role)

                if (!is.factor(.self$data$block))
                    .self$data$block <- factor(.self$data$block)

                if (!is.factor(.self$data$treat))
                    .self$data$treat <- factor(.self$data$treat)
                
                .self$report <- .self$analyze()
            }
        },
        analyze = function() {
            asreml::asreml.options(maxit = 100, workspace = 3E8, pworkspace = 1E8, trace = FALSE)
            fit <- asreml::asreml(response ~ role + at(role, "check"):treat,
                                  random = ~ block + at(role, "test"):treat,
                                  residual = ~ ar1(row):ar1(column),
                                  data = .self$data)
            ll <- summary(fit)$loglik
            reml <- -2 * ll
            sigma <- summary(fit)$sigma
            anova <- asreml::wald(fit)
            vc <- summary(fit)$varcomp
            checks <- subset(predict(fit, classify = "role:treat", levels = list(role = "check"))$pvals,
                             status == "Estimable", select = -status)
            tests <- subset(predict(fit, classify = "role:treat", levels = list(role = "test"))$pvals,
                            !(treat %in% as.character(checks$treat)) & status == "Estimable", select = -status)
            .self$appendLog(event = "report")
            .self$report <-  list("fit" = list("LogLik" = ll, "REML" = reml, "sigma" = sigma),
                                  "anova" = anova,
                                  "variance" = as.data.frame(vc),
                                  "means" = rbind(checks, tests))
        },
        asList = function() {
            return(list(data = .self$data, report = .self$report, log = .self$log))
        }
    )
)

setValidity(
    Class = "ACBD",
    method = function(object) {
        if (all(c("role", "row", "column", "block", "treat", "response") %in% names(object$data))) {
            TRUE
        } else {
            "ACBD must contain role, row, column, block, treat and response data"            
        }
        if (all(levels(object$data$role) %in% c("check", "test"))) {
            TRUE
        } else {
            "ACBD must contain check and test as levels of role"
        }
    }
)
