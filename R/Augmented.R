##' @title Concrete interface for Augmented designs
##' @description Specialized Concrete Definition.
##' @name Augmented-class
##' @exportClass Augmented
##' @seealso [Breeder() and SplitPlot()] for the pure virtual and concrete
##'     abstract template.
##' @field report list or analytical summary.
##' @import methods asreml 
##' @examples
##' data("burgueno.unreplicated", package = "agridat")
##' dat <- burgueno.unreplicated
##' names(dat)[names(dat) == "gen"] <- "treat"
##' names(dat)[names(dat) == "col"] <- "column"
##' names(dat)[names(dat) == "yield"] <- "response"
##' dat$role <- ifelse(dat$treat == "G000", "check", "test")
##' b <- Breeder(dat)
##' a <- Augmented(b)
##' a$report$fit$sigma
##' @export Augmented
Augmented <- setRefClass(
  Class = "Augmented",
  contains = "Breeder",
  fields = c("report" = "list"),
  methods = list(
    initialize = function(d) {
      if (inherits(d, "Breeder")) {
        .self$data <- d$data
        .self$log <- d$log
        
        if (!is.factor(.self$data$row))
          .self$data$row <- factor(.self$data$row)
        
        if (!is.factor(.self$data$column))
          .self$data$column <- factor(.self$data$column)
        
        if (!is.factor(.self$data$role))
          .self$data$role <- factor(.self$data$role)
        
        if (!is.factor(.self$data$treat))
          .self$data$treat <- factor(.self$data$treat)
        
        .self$report <- .self$analyze()
      }
    },
    
    analyze = function() {
      asreml::asreml.options(
        maxit = 100,
        workspace = 3E8,
        pworkspace = 1E8,
        trace = FALSE
      )
      
      fit <- asreml::asreml(
        fixed = response ~ role + at(role, "check"):treat,
        random = ~ at(role, "test"):treat,
        residual = ~ ar1(row):ar1(column),
        data = .self$data
      )
      
      ll <- summary(fit)$loglik
      reml <- -2 * ll
      sigma <- summary(fit)$sigma
      anova <- asreml::wald(fit)
      vc <- summary(fit)$varcomp
      
      checks <- subset(
        predict(
          fit,
          classify = "role:treat",
          levels = list(role = "check")
        )$pvals,
        status == "Estimable",
        select = -status
      )
      
      tests <- subset(
        predict(
          fit,
          classify = "role:treat",
          levels = list(role = "test")
        )$pvals,
        !(treat %in% as.character(checks$treat)) & status == "Estimable",
        select = -status
      )
      
      .self$appendLog(event = "report")
      
      .self$report <- list(
        fit = list(
          LogLik = ll,
          REML = reml,
          sigma = sigma
        ),
        anova = anova,
        variance = as.data.frame(vc),
        means = rbind(checks, tests)
      )
    },
    
    asList = function() {
      return(list(
        data = .self$data,
        report = .self$report,
        log = .self$log
      ))
    }
  )
)

setValidity(
  Class = "Augmented",
  method = function(object) {
    if (!all(c("role", "row", "column", "treat", "response") %in% names(object$data))) {
      return("Augmented must contain role, row, column, treat and response data")
    }
    
    if (!all(levels(object$data$role) %in% c("check", "test"))) {
      return("Augmented must contain check and test as levels of role")
    }
    
    TRUE
  }
)