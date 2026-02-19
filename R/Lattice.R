##' @title Concrete interface for Lattice designs
##' @description Specialized Concrete Definition.
##' @name Lattice-class
##' @exportClass Lattice
##' @seealso [Breeder() and SplitPlot()] for the pure virtual and concrete
##'     abstract template.
##' @field report list or analytical summary.
##' @import methods lme4 emmeans
##' @examples
##' data(gg1984, package = "yabaf")
##' b <- Breeder(gg1984)
##' l <- Lattice(b)
##' l$report$fit$sigma
##' @export Lattice
Lattice <- setRefClass(
    Class = "Lattice",
    contains = "Breeder",
    fields = c("report" = "list"),
    methods = list(
        initialize = function(d) {
            if(inherits(d, "Breeder")) {
                .self$data <- d$data
                .self$log <- d$log
                .self$report <- .self$analyze()
            }
        },
        analyze = function() {
            fit <- lme4::lmer(response ~ replication + treat + (1 | replication:block),
                              data = .self$data)
            ll   <- as.numeric(logLik(fit))
            reml <- lme4::REMLcrit(fit)
            sigma <- sigma(fit)
            anova <- emmeans::joint_tests(fit, lmer.df = "kenward-roger")
            vc <- lme4::VarCorr(fit)
            means <- emmeans::emmeans(fit, specs = ~ treat, lmer.df = "kenward-roger")
            .self$appendLog(event = "report")
            .self$report <-  list("fit" = list("LogLik" = ll, "REML" = reml, "sigma" = sigma),
                                  "anova" = anova,
                                  "variance" = as.data.frame(vc),
                                  "means" = means)
        },
        asList = function() {
            return(list(data = .self$data, report = .self$report, log = .self$log))
        }
    )
)

setValidity(
    Class = "Lattice",
    method = function(object) {
        if (all(c("replication", "block", "treat", "response") %in% names(object$data))) {
            TRUE
        } else {
            "lattice must contain replication, block, treat and response data"            
        }
    }
)
