#' Analysis of Mixture Experiment
#'
#' @param DF Data Frame with design and Response
#' @param resp Character name of response column
#'
#' @return a list
#' @export
#'
#' @examples des <- SLDnew(3, 2, center=1, axial=1)
#' @examples extension <- c(415, 484,424,400,409,342,456, 460,450, 411)
#' @examples desrun1 <- data.frame(0.2+0.4*des, y=extension)
#' @examples MixtureAnalysis(desrun1)
MixtureAnalysis <- function(DF, resp="y"){
  respval <- eval(parse(text=paste0("DF$",resp)))

  xvars <- setdiff(names(DF),resp)

  xvars0 <- xvars[-1]
  xvars1 <- paste(xvars0, collapse=",")
  xvars2 <- paste(xvars,  collapse=",")
  xvars3 <- paste(xvars,  collapse="+")

  DF$respval <- respval
  fit0 <- eval(parse(text=paste0("lm(respval ~ (",xvars3,")^2-1,data=DF)")))
  fit <- eval(parse(text=paste0("rsm::rsm(respval ~ FO(",xvars1,")+TWI(", xvars2,"),data=DF)")))
  mixaov <- matrix(NA, 5, 6)
  row.names(mixaov) <- c("Regression","Linear","Quadratic","Residual Error", "Total")
  colnames(mixaov) <- c("Df","Seq SS","Adj SS","Adj MS","F","p")

  Anovafit <- car::Anova(fit)
  mixaov[2:4,3] <- Anovafit$`Sum Sq`
  mixaov[2:4,1] <- Anovafit$Df
  mixaov[2:3,5] <- Anovafit$`F value`[1:2]
  mixaov[2:3,6] <- Anovafit$`Pr(>F)`[1:2]

  fit2 <- lm(respval ~ 1,data=DF)
  mixaov[2:4,2] <- anova(fit)$`Sum Sq`
  mixaov[5,1] <- anova(fit2)$Df
  mixaov[5,2] <- anova(fit2)$`Sum Sq`
  mixaov[1,1] <- mixaov[2,1]+mixaov[3,1]
  mixaov[1,2] <- mixaov[2,2]+mixaov[3,2]
  mixaov[1,3] <- mixaov[1,2]
  mixaov[1:4,4] <- mixaov[1:4,3]/mixaov[1:4,1]
  mixaov[1,5] <- mixaov[1,4]/mixaov[4,4]
  mixaov[1,6] <- 1-pf(mixaov[1,5], mixaov[1,1], mixaov[1,4])
  options(knitr.kable.NA = '')
  list(kable(mixaov, digits=c(0,2,2,2,2,3)),
       fit0)
}
