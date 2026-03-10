##' @title Concrete interface for ACBD designs
##' @description Specialized Concrete Definition.
##' @name ACBD-class
##' @exportClass ACBD
##' @seealso [Breeder() and SplitPlot()] for the pure virtual and concrete
##'     abstract template.
##' @field report list or analytical summary.
##' @import methods lme4 asreml emmeans
##' @examples
##' data(stg12025, package = "yabaf")
##' b <- Breeder(stg12025)
##' l <- ACBD(b)
##' l$report$fit$sigma
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
        .self$report <- .self$analyze()
      }
    },
    analyze = function() {
      fit <- lme4::lmer(response ~ 1 + role + (1|block) + (1|treat) + (1|row) + (1|column), data = stg12025)
      ll     <- as.numeric(logLik(fit))
      reml <- lme4::REMLcrit(fit)
      sigma <- sigma(fit)
      anova <- emmeans::joint_tests(fit, lmer.df = "kenward-roger")
      vc <- lme4::VarCorr(fit)
      means <- fixef(fit)["(Intercept)"] + ranef(fit)$treat
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
  Class = "ACBD",
  method = function(object) {
    if (all(c("role", "row", "column", "block", "treat", "response") %in% names(object$data))) {
      TRUE
    } else {
      "ACBD must contain role, row, column, block, treat and response data"            
    }
  }
)


