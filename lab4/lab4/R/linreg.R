#' Linear regression class
#'
#' Calculate linear regression from a dataset
#' 
#'@param formula Formula of the regression
#'@param data Data set used
#'@export class linreg
#'

#Things left -> linear regresion lines and summary

linreg <- function(formula, data){
#Model the matrix X and extract y matrix
  x <- model.matrix(formula, data)
  y <- data[all.vars(formula)[1]]
  name <- deparse(substitute(data))
  QR <- qr(x)
  betahat <- solve.qr(QR, y)
  yhat <- as.vector(unlist(x%*%betahat))
  res <- as.vector(unlist(y - x %*% betahat))
  df <-  (nrow(x) - QR$rank)
  se2 <- sum(res^2)/df
  variance <- diag(chol2inv(QR$qr) * se2)
  std_error <- sqrt(variance)
  tb <- betahat/std_error
  p_values <- 2*pt(abs(tb), df, lower.tail=FALSE)
  value <- list(name = name, x =x, formula = formula, 
                df = df, residuals = res, fitted.values = yhat, 
                coefficients = betahat, tb = tb, se2 = se2, 
                var = variance, p_values = p_values, std_error=std_error)
  attr(value, "class") <- "linreg"
  value
}

#'Detect linreg class
#'
#'Detects if the function is a linreg class
#'
#'@param x object to identify
#'@export

is.linreg <- function(x){
  return(class(x) == "linreg")
}


#'Plot linreg class
#'
#'Plot two figures with the linear regression 
#'
#'@param x linreg class object 
#'@export

plot.linreg <- function(x){
  stres <-resid(x)/sd(resid(x))
  print(ggplot()+aes(pred(x), resid(x)) +
          geom_point()+
          geom_smooth(span=1.5, colour="red",method="loess", se=FALSE)+
          ggtitle("Residuals vs Fitted")+
          xlab("Fitted values")+
          ylab("Residuals")+theme_liu())
  print(ggplot()+aes(pred(x), sqrt(abs(stres))) +
           geom_point()+
           geom_smooth(span=1.5, colour="red",method="loess", se=FALSE)+
           ggtitle("Scale-Location")+
           xlab("Fitted values")+
           ylab("Squared Standard residuas")+theme_liu())

}

#'pred linreg
#'
#'Return the fitted values of the linear regression
#'
#'@param x linreg class object
#'@export

pred <- function(x){
  return(x$fitted.values)
}

#'coef linreg
#'
#'Return the coefficients of the linear regression
#'
#'@param x linreg class object
#'@export

coef.linreg <- function(x){
  UseMethod("coef", x)
}

#'resid linreg
#'
#'Return the residuals of the linear regression
#'
#'@param x linreg class object
#'@export
resid.linreg <- function(x){
  UseMethod("resid", x )
}

#' print linreg
#' 
#' Print the linreg class
#' 
#'@param x linreg class object
#'@param ... other parameters
#'@export

print.linreg <- function(x, ...) {
  cat("\nCall:\n",
      "linreg(formula = ",paste(deparse(x$formula), sep = "\n", collapse = "\n"),", data = ",x$name,")", "\n\n", sep = "")
  cat("Coefficients: \n")
  cat(" ")
  cat(names(x$coefficients))
  cat(" ")
  cat(sep="\n")
  cat(sep="      ",x$coefficients)
}


#'summary linreg
#'
#'print the summary of the linreg class object
#'
#'@param x linreg class object
#'@param ... other parameters
#'@export

summary.linreg <- function(x, ...){
  "Returns a summary of the linear regression"
  
  svar<-data.frame("Variable"=as.character(names(x$coefficients)),"Estimate"=round(x$coefficients,3),"Std Error"=round(x$std_error,3),"T"=round(x$tb,3),"P"=round(x$p_values,5),stringsAsFactors = FALSE)
  
  cat("Call: \n")
  cat(paste0("linreg(formula = ",format(x$formula),", data = ",x$name,")\n\n"))
  
  cat(names(svar),sep="  ","\n")
  for(i in 1:nrow(svar)){
    cat(paste(svar[i,],collapse = " "),sep="",collapse=" ***\n")
  }
  cat("",sep="\n")
  cat(paste("Residual standard error:",sqrt(x$se2),"on", x$df, "degrees of freedom"))
}

#'theme function
#'
#'Theme for LIU plots
#'
#'@param base_size size of base
#'@param base_family font
#'
#'


theme_liu <- function (base_size = 11, base_family = "sans") 
{  
  half_line <- base_size/2
  theme(
    line = element_line(colour = "black", size = 0.5, 
                        linetype = 1, lineend = "butt"), 
    rect = element_rect(fill = "white", colour = "black",
                        size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain",
                        colour = "white", size = base_size,
                        lineheight = 0.9,  hjust = 0.5,
                        vjust = 0.5, angle = 0, 
                        margin = margin(), debug = FALSE), 
    axis.line = element_blank(), 
    axis.text = element_text(size = rel(1), colour = "grey30"),
    axis.text.x = element_text(margin = margin(t = 0.8*half_line/2), 
                               vjust = 1), 
    axis.text.y = element_text(margin = margin(r = 0.8*half_line/2),
                               hjust = 1),
    axis.ticks = element_line(colour = "grey20"),
    axis.ticks.length = unit(half_line/2, "pt"), 
    axis.title.x = element_text(size = rel(1.2),margin = margin(t = half_line,
                                                                b = half_line*1.5)),
    axis.title.y = element_text(size = rel(1.2), angle = 90, 
                                margin = margin(r = half_line*1.5,
                                                l = half_line)),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    
    legend.background = element_rect(colour = NA), 
    legend.margin = margin(6,6,6,6),
    legend.key = element_rect(fill = "grey95", colour = "white"),
    legend.key.size = unit(1.2, "lines"), 
    legend.key.height = NULL,
    legend.key.width = NULL, 
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0), 
    legend.title.align = NULL, 
    legend.position = "right", 
    legend.direction = NULL,
    legend.justification = "center", 
    legend.box = NULL, 
    legend.spacing = NULL,
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.box.margin = NULL,
    legend.box.background = NULL,
    legend.box.spacing = NULL,
    
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_blank(), 
    panel.grid.major = element_line(colour = "grey95"), 
    panel.grid.minor = element_line(colour = "grey95", size = 0.25), 
    #panel.margin = unit(half_line, "pt"), 
    panel.margin.x = NULL, 
    panel.margin.y = NULL, panel.ontop = FALSE, 
    panel.spacing = element_blank(), 
    panel.spacing.x = element_blank(), 
    panel.spacing.y = element_blank(), 
    plot.subtitle = element_blank(), 
    plot.caption = element_blank(), 
    
    
    strip.background = element_rect(fill = "grey85", colour = NA),
    strip.text = element_text(colour = "grey10", size = rel(0.8)),
    strip.text.x = element_text(size=1), 
    strip.text.y = element_text(angle = -90, 
                                margin = margin(l = half_line, 
                                                r = half_line)),
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"), 
    strip.placement = "outside" ,
    
    plot.background = element_rect(fill = "mediumturquoise"), 
    plot.title = element_text(size = rel(2), 
                              margin = margin(b = half_line*2.5, l=half_line)),
    plot.margin = margin(0.8,0.8,0.8,0.8,"cm"),
    complete = TRUE)
}