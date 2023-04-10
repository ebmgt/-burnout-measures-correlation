#This file is best used within R Studio and 
#  using R Studio's Document outline for navigation
# rbadgett@kumc.edu
### Start =======================================
version
citation(package = "base", lib.loc = NULL, auto = NULL)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

##* Functions -----
function_libraries_install <- function(packages){
  install.packages(setdiff(packages, rownames(installed.packages())))
  for(package_name in packages)
  {
    #library(package_name,character.only=TRUE,quietly=TRUE);
    library(package_name,character.only=TRUE, quietly = FALSE);
  }
}
function_plot_print <- function (plotname, plotheight, plotwidth){
  # Prints current image in R studio
  rstudioapi::savePlotAsImage(
    paste(plotname,' -- ',current.date,'.png',sep=''),
    format = "png", width = plotwidth, height = plotheight)
}
##* Essential packages -----
packages_essential <- c("tcltk",'stringr','openxlsx')
function_libraries_install(packages_essential)

##* Graphics --------------------------
#windows(600, 600, pointsize = 12) # Size 600x600
devAskNewPage(ask = FALSE)
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
old.par <- par(no.readonly=T)
plot.new()
xmin <- par("usr")[1] + strwidth("A")
xmax <- par("usr")[2] - strwidth("A")
ymin <- par("usr")[3] + 1.2*strheight("A")
ymax <- par("usr")[4] - strheight("A")

##* Libraries and libraries------------------------------------
#*** Meta-analysis and positive deviance----
packages_meta <- c("metafor", #
                   'meta',   # Meta-analysis
                   'boot',   # inv.logit to identify deviants
                   'grid',   # Forest and blobbogram
                   'gemtc',  # Blobbogram
                   'enc'     # Campbell collection individual study TEs
)
function_libraries_install(packages_meta)

packages_mixed_regression <- c("lme4", #
                   'report', # Easy grab of r values
                   'AICcmodavg', # Compare AICs
                   'caret' #RMSE
)
function_libraries_install(packages_mixed_regression)

##* Constants declaration -------------------
`%notin%` <- Negate(`%in%`)

##* Formatted text --------------------
(current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
(current.date.pretty <- as.character(strftime (Sys.time(), format="%m/%d/%Y", tz="", usetz=FALSE)))
plot.new()
p.value <- 0.045
p.value <- as.character(sprintf(p.value, fmt='%#.3f'))
text(0.1,0.1,p.value)
plot.new()
res <- NULL
res$I2 <- 30.123
I2.value <- formatC(res$I2, digits=1, format="f")
I2.label <- bquote(I^2 ~ "=" ~ .(I2.value) ~ "%")
text(0.25,0.25,I2.label)
I2.summary.label <- bquote("RE Model ("~ I^2 ~ "= " ~ .(I2.value) ~ "%)")
text(0.5,0.5,I2.summary.label)
R2 = 50
R2.value <- formatC(R2, digits=1, format="f")
R2.label <- bquote(R^2 ~ "=" ~ .(R2.value) ~ "%")
text(xmin,ymax-2.2* strheight("A"),adj=c(0,1),R2.label)

#_________---------
# 1. META-ANALYSIS OF CORRELATON OEFFICIENTS----------------------
## Data grab (CORRELATIONS) ===================================
# co <- read.table("https://data.princeton.edu/eco572/datasets/cohhpop.dat", col.names=c("age","pop"), header=FALSE)
file.filter   <- matrix(c("Spreadsheets","*correl*.csv;*correl*.xls;*correl*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
filename      <- choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
file.extension<- substr(filename,regexpr("\\.[^\\.]*$", filename)+1, nchar(filename))
if (file.extension == 'csv'){
  data.import   <- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
}else{
  data.import   <- read.xlsx(filename)
  }

  data.import <- data.import[!(is.na(data.import$Author)),]
  data.import$studlab <- paste(data.import$Authors,', ',data.import$Year,sep='')

  for (i in 5:14){
    data.import[,i] <- as.numeric(data.import[,i])
  }
  
  
data.correlation <- data.import

##* Remove rows or anyone? (left of comma) -------
data.correlation <- data.correlation[!(data.correlation$Author == '' | is.na(data.correlation$Author)),]
# Remove Li-Sauerwine or Ong due to dual-item MBI (DI-MBI), standard?
# Remove Brady or Yellowlees sensitivity analysis?
# mydata2 <- mydata[!(mydata$Author == 'Brady'),]

#meta.prop <- meta::metaprop(Respondents, Size, studlab = studlab, subgroup = Language, random = TRUE, hakn = TRUE, data = data.correlation)
#summary(meta.prop)
#meta::forest(meta.prop, sortvar = Year, subgroup = TRUE,  colgap.forest.left = '4mm', print.Q.subgroup = FALSE, print.pval.Q = FALSE, print.I2.ci = TRUE, xlim=c(0,1), print.tau2 = FALSE, fixed = FALSE, xlab=paste("Correlation with ", scale, sep=''))


##* Parameters -------
##** Select which scale to study -------
scale <- tk_select.list(c('Emotional exhaustion','Depersonalization',''), preselect = 'Emotional exhaustion', multiple = FALSE,
                                title = "\n\nWhich are we studying?\n\n")
if (scale == 'Depersonalization'){
  figno = 2
  data.correlation$outcome <- as.numeric(data.correlation$Correlation.depersonalization)
}else{
  figno = 1
  data.correlation$outcome <- as.numeric(data.correlation$Correlation.exhaustion)
  }
data.correlation <- data.correlation[!(is.na(data.correlation$outcome)),]

##* Meta-analysis-----
##* # 02/26/2023: Size replaced with Respondents
meta.correlation <- meta::metacor(outcome, Respondents, studlab = studlab, subgroup = Language, random = TRUE, hakn = TRUE, data = data.correlation)
# exclude = Study %in% c('Olson'), 
bob <- (summary(meta.correlation))
##resid_hetstat = FALSE
##if both subfroups > 1 then
##resid_hetstat = TRUE
##* Forest plot -----
meta::forest(meta.correlation, sortvar = Year, 
             fixed = FALSE, common = FALSE, random = TRUE, 
             subgroup = TRUE, print.Q.subgroup = FALSE, 
             resid.hetstat = resid_hetstat, 
             print.I2.ci = TRUE, print.tau2 = FALSE, print.pval.Q = FALSE,
             xlab=paste("Correlation with ", scale, sep=''), colgap.forest.left = '4mm', xlim=c(0,1))
grid.text(paste("Correlation of the SIBOQ with MBI ", scale,sep=''), 0.5, 0.9, gp = gpar(fontsize = 14, fontface = "bold"))

##** Export plot------------------------
PlotName <- paste("Figure ", figno,". Forest plot - SIBOQ with ", scale, ' - ',sep='')
p.width = 800
p.height <- max(180,100)*2 + length(meta.correlation$studlab)*25 + length(unique(meta.correlation$data$.byvar)) * 80

(current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))

Directory <- '../Plots/'

rstudioapi::savePlotAsImage( # Print at 800*plotheight
  paste(Directory,PlotName,current.date,'.png',sep=''),
  format = "png", width = p.width, height = p.height)

#_________---------
# 2. MIXED REGRESSION OF RATES -----------------------------
### Data grab (RATES) ===================================
# co <- read.table("https://data.princeton.edu/eco572/datasets/cohhpop.dat", col.names=c("age","pop"), header=FALSE)
file.filter      <- matrix(c("Spreadsheets","*rate*.csv;*rate*.xls;*rate*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
filename2        <- choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
file.extension2  <- substr(filename2,regexpr("\\.[^\\.]*$", filename2)+1, nchar(filename2))
if (file.extension2 == 'csv'){
  data.import   <- read.csv(filename2, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
}else{
  data.import   <- read.xlsx(filename2)}

head(data.import)
mydata <- data.import
nrow(mydata)

## Data cleaning -----

##* Remove rows or anyone? (left of comma) -------
##* # Not sure why below did not remove Li-Sauerwine!
mydata <- mydata[!(mydata$Author == '' | is.na(mydata$Author)),]
nrow(mydata)
mydata <- mydata[!(mydata$Author == 'Olson' & mydata$Grouping != 'Specialty'),]
nrow(mydata)
mydata <- mydata[!(mydata$Author == 'Knox' & mydata$Grouping != 'All respondents'),]
nrow(mydata)

# Remove overlapping
# This does not matter after edits of 07/17/2022
# Update, this removed Li-Sauerwine 10/22/2022
mydata <-  mydata[!(mydata$Grouping == 'All'),] # This preserves Knox, Trockel, and Yellowlees which is "All respondents"
mydata <-  mydata[!(is.na(mydata$Grouping)),] # This preserves Knox, Trockel, and Yellowlees which is "All respondents"
nrow(mydata)

# Sensitivity analysis?
#mydata2 <- mydata[!(mydata$Author == 'Brady' | mydata$Author == 'Yellowlees'),]

# At this point, you can aLSo skip down to the CONFOUNDERS REGRESSION

#* Allow MBI-DI as a gold standard? -----
yes.no <- tk_select.list(c('Yes','No',''), preselect = 'Yes', multiple = FALSE,
                                title = "\n\nAllow MBI-DI as a gold standard?\n\n")
mydata$Gold <- NULL
if (yes.no == 'Yes')
  {
  # 40 comparisons per below
# INCLUDE MBI-DI
  mydata$Gold <- ifelse(is.na(mydata$MBI.full.either), mydata$MBI.DI, mydata$MBI.full.either)
  lmer.out.either <- lmer(Gold  ~ Mini.Z + (1 | Author), data = mydata) # BEST!!! same with REML=T
}else{
  # 35 comparisons per below
  # EXCLUDE MBI-DI
  mydata$Gold <- mydata$MBI.full.either
  lmer.out.full <- lmer(Gold  ~ Mini.Z + (1 | Author), data = mydata) # BEST!!! same with REML=T
}

length(mydata$Gold[!is.na(mydata$Gold) & !is.na(mydata$Mini.Z)])

# Only allowing the full MBI as gold standard
report(lmer.out.full)
(lmer.out.full.summary <- summary(lmer.out.full)) # Allowing either the full MBI or the MBI-DI as gold standard
length(lmer.out.full.summary$residuals)
# Allowing the full MBI0DI as well for gold standard
report(lmer.out.either)
(lmer.out.either.summary <- summary(lmer.out.either)) # Allowing either the full MBI or the MBI-DI as gold standard
length(lmer.out.either.summary$residuals)

##** Comparing MBI-FULL vs MBI-DI as good standards -----------------
# Lower indicates a more parsimonious model, relative to a model fit with a higher AIC
AIC(logLik(lmer.out.either))
AIC(logLik(lmer.out.full))
exp( 
  (
  round( AIC(logLik(lmer.out.full)),0) - 
  round( AIC(logLik(lmer.out.either)),0) 
  ) /2)
anova(lmer.out.either, lmer.out.full, test = "Chisq", refit = F)

report(lmer.out.either)
summary(report(lmer.out.either))
##* Regression =======================
##**  Mixed model -----
# Total: https://slcladal.github.io/regression.html
# For predictions that use a mixed-effects modeling approach, there are four different R2 values
# reported in the model summaries: 
# marginal (fixed) 
# conditional (total)
# fitted, 
# predictive.

#a_mixed = lmer(MBI.full  ~ Mini.Z, data = mydata)
#a_mixed = lmer(MBI.full  ~(1 | Mini.Z), data = mydata)
#a_mixed = lmer(MBI.full  ~Mini.Z + (1 | Respondents.Source), data = mydata)
#a_mixed = lmer(MBI.full  ~Mini.Z + Response.rate + (1 | Author), data = mydata)

if (yes.no == 'Yes')
{
  # 39 comparisons per below
  mydata$Gold <- ifelse(is.na(mydata$MBI.full.either), mydata$MBI.DI, mydata$MBI.full.either)
}else{
  # 35 comparisons per below
  mydata$Gold <- mydata$MBI.full.either
  
}
length(mydata$Gold[!is.na(mydata$Gold) & !is.na(mydata$Mini.Z)])
a_mixed <- lmer(Gold  ~ Mini.Z + (1 | Author), data = mydata) # BEST!!! same with REML=T
#writeLines(confint(a_mixed))
a_mixed
a_mixed.summary <- summary(a_mixed)
# Custom name for this list
paste('a_mixed.summary',length(a_mixed.summary$residuals) , sep='')
new_name <- paste('a_mixed.summary',length(a_mixed.summary$residuals) , sep='')
assign(new_name, a_mixed.summary)
writeLines(paste0('Number of observations: ', as.character(length(a_mixed.summary$residuals))))
a_mixed.summary38

(REPORT <- report (a_mixed))
summary(REPORT)

#a_mixed <- lmer(Gold  ~ Mini.Z + Rate.physicians + (1 | Author), data = mydata) # BEST!!! same with REML=T

##*** Report package ----------
# https://easystats.github.io/report/
a_mixed.summary$vcov
(R2REPORT <- as.data.frame(REPORT))
(R2_Mixed <- round(R2REPORT$Fit[9]*100,1))
(R2_Fixed <- round(R2REPORT$Fit[10]*100,1))
(R_Mixed  <- (R2REPORT$Fit[9])^(1/2))
(R_Fixed  <- (R2REPORT$Fit[10])^(1/2))
summary(as.data.frame(REPORT))

##**  Linear, fixed model -----

mydata$Gold <- NULL
if (yes.no == 'Yes')
{
  # 39 comparisons per below
  mydata$Gold <- ifelse(is.na(mydata$MBI.full.either), mydata$MBI.DI, mydata$MBI.full.either)
}else{
  # 35 comparisons per below
  mydata$Gold <- mydata$MBI.full.either
  
}

length(mydata$Gold[!is.na(mydata$Gold)])


mydata <- mydata[order(mydata$Gold),]
lm.out <- lm(Gold  ~ Mini.Z, data = mydata)
(lm.out.summary <- summary(lm.out)) #ADD WEIGHTING BY SIZE ??
(R2_Fixed <- round(lm.out.summary$adj.r.squared*100,0))
(R_Fixed <- round(lm.out.summary$adj.r.squared^(1/2),2))
length(lm.out$residuals)

# Weighted ? No
(lm.out <- lm(Gold ~ Mini.Z, data = mydata)) #ADD WEIGHTING BY SIZE? weights = mydata$Size,
(lm.out.summary <- summary(lm.out)) #ADD WEIGHTING BY SIZE ??
(R2_Fixed <- round(lm.out.summary$adj.r.squared*100,0))
(R_Fixed <- round(lm.out.summary$adj.r.squared^(1/2),2))

mydata$Mini.Z.adjusted <- lm.out.summary$coefficients[1] + mydata$Mini.Z * lm.out.summary$coefficients[2]

##** Comparing mixed and fixed/linear models -----------------
# Lower AIC indicates a more parsimonious model, relative to a model fit with a higher AIC
AIC(logLik(a_mixed))
AIC(logLik(lm.out))
anova(a_mixed, lm.out, test = "Chisq", refit = F)

##** Comparing MBI.Full.either with MBI.DI as criterion standard -----------------
# Lower indicates a more parsimonious model, relative to a model fit with a higher AIC
# From WikiPedia: As an example, suppose that there are three candidate models, whose AIC values are 100, 102, and 110. Then the second model is exp((100 − 102)/2) = 0.368 times as probable as the first model to minimize the information loss. Similarly, the third model is exp((100 − 110)/2) = 0.007 times as probable as the first model to minimize the information loss.
AIC(logLik(a_mixed))
AIC(logLik(a_mixed.all))
BIC(logLik(a_mixed))
BIC(logLik(a_mixed.all))
anova(a_mixed, a_mixed.all, test = "Chisq", refit = F)
aictab(c(a_mixed, a_mixed.all),c('a_mixed', 'a_mixed.all'))

# Per Wikipedia:
exp( ( AIC(logLik(a_mixed)) - AIC(logLik(a_mixed.all)) ) / 2 )

##* Plot ------------------
# Study color
mydata$color <- 'gray' # Ong 
for(i in 1:nrow(mydata))
{
  if (mydata$Author[i]=='Knox'){mydata$color[i] <- 'green'}
  if (mydata$Author[i]=='Olson'){mydata$color[i] <- 'orangered'}
  if (mydata$Author[i]=='Trockel'){mydata$color[i] <- '#00CCCC'} #teal
  if (mydata$Author[i]=='Brady'){mydata$color[i] <- 'black'}
  if (mydata$Author[i]=='Kemper'){mydata$color[i] <- 'purple'}
  if (mydata$Author[i]=='Yellowlees'){mydata$color[i] <- 'gold'} # yellow
  if (mydata$Author[i]=='Coate'){mydata$color[i] <- 'gold'}
  if (mydata$Author[i]=='Ong'){mydata$color[i] <- 'gray'}
}
# Point size
mydata$PointSize <- 0.5 + (mydata$Size - min(mydata$Size))/(max(mydata$Size)-min(mydata$Size))

par(mar=c(8,4.1,4.1,2.1), mfrow=c(1,1))
plot(NULL,NULL,xlab='SIBOQ (burned out %)',ylab='MBI (burned out %)',xlim=c(0,100),ylim=c(0,100), main="Prediction of MBI from the SIBOQ")

points(mydata$Mini.Z,mydata$Gold,col=mydata$color, pch=19, cex=mydata$PointSize)
#Ong (make sure is not hidden behind another point)
points(mydata$Mini.Z[mydata$Author=='Ong'],mydata$Gold[mydata$Author=='Ong'],col=mydata$color[mydata$Author=='Ong'], pch=19, cex=mydata$PointSize)

# Display mixed or linear regression?
regression <- tk_select.list(c('Fixed', 'Mixed',''), preselect = 'Fixed', multiple = FALSE,
                             title = "\n\nWhich regression lines to plot \n(mixed actually uses its fixed values?\n\n")
intercept <- NULL
if (regression == 'Mixed')
{
  # Y intercept
  intercept   <- a_mixed.summary$coefficients[1]
  # Slope
  slope       <- a_mixed.summary$coefficients[2]
  slope.upr   <- a_mixed.summary$coefficients[2] + a_mixed.summary$coefficients[2,2]*1.96
  slope.lower <- a_mixed.summary$coefficients[2] - a_mixed.summary$coefficients[2,2]*1.96
  # Confidence interval
  CI          <- a_mixed.summary$coefficients[1,2]*1.96
}else{
  # Y intercept
  intercept <- lm.out.summary$coefficients[1]
  # Slope
  slope       <- lm.out.summary$coefficients[2]
  slope.upr   <- lm.out.summary$coefficients[2] + lm.out.summary$coefficients[2,2]*1.96
  slope.lower <- lm.out.summary$coefficients[2] - lm.out.summary$coefficients[2,2]*1.96
  # Confidence interval
  CI        <- lm.out.summary$coefficients[1,2]*1.96
  }
(intercept)
(slope)
(CI)

#** Prediction -----
interval <- tk_select.list(c('confidence', 'prediction',''), preselect = 'prediction', multiple = FALSE,
title = "\n\nWhich prediction lines to plot?\n\n")

cofactor.range = seq(10, 90, 2)
cofactor.range <- as.list(cofactor.range)
is.list(cofactor.range)
is.vector(cofactor.range)
length(cofactor.range)

if (regression == 'Mixed')
  {
  preds <- stats::predict (a_mixed, level = 0.95, interval = interval, type='response')
}else{ # newmods=cofactor.range,
  preds <- stats::predict(lm.out, newmods=cofactor.range, level = 0.95, interval = interval, type='response')
}

if (interval == 'confidence')
{
  color = 'gray'
}else{ # newmods=cofactor.range,
  color = 'blue'
}

(nrow(preds))
preds <- data.frame(cofactor.range, preds)

#** Draw lines -----
abline(intercept, slope)

# Prediction/confidence limits
preds <- preds[order(preds$lwr),]
lines(preds$fit, preds$lwr, lty="dashed", col=color)
preds <- preds[order(preds$upr),]
lines(preds$fit, preds$upr, lty="dashed", col=color)

#** legend ---------
legend("topleft", adj = 0, xjust = 1, inset = c(-0.01,0), c(paste("Regression line (", regression,")", sep=''), paste("95% prediction limits", sep='')), pch = NULL, pt.bg = "white", bty = "n", border = "white", lty=c("solid","dashed"), col=c("black","blue"))
# values
text(75,20,adj=c(0,1),bquote(R^2 ~ " (mixed) = " ~ .(R2_Mixed) ~ "%"))
text(75,14,adj=c(0,1),bquote(R^2 ~ " (fixed) = " ~ .(R2_Fixed) ~ "%"))
#text(80,10,adj=c(0,1),bquote(R   ~ " (fixed) = " ~ .(R_Fixed)))
text(75, 8,adj=c(0,1), paste0('Observations: ', as.character(length(a_mixed.summary$residuals)))  )

##** Notes -----
mtext(expression(paste(bold("Note: "))), side = 1, line = 4.2,adj=0,cex=1)
formula <- paste("MBI = ", round(intercept,2), " + ", round(slope,2), " * SIBOQ +/- ", round(CI,2), " (",regression, " regression)", sep='')
mtext(formula, side = 1, line = 5.2,adj=0,cex=1)
mtext(expression(paste("Points colors: Brady=black, Kemper=purple; Knox=green; Olson=red; Ong=gray; \nTrockel=teal, Sierra Sacramento Valley Medical Society (Coate and Yellowlees)=yellow")), side = 1, line = 7.2,adj=0,cex=1)
#Footer <- paste(Footer,"\n","* Based on rate conversion derived from Olson PMID 30467949",sep='')

##** Export plot------------------------
PlotName <- paste('Figure 1. SIBOQ predicting MBI (', regression, ') - ', sep='')
Directory <- '../Plots/'

(current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))

rstudioapi::savePlotAsImage( # Print at 800*plotheight
  paste(Directory,PlotName,current.date,'.png',sep=''),
  format = "png", width = 800, height = 600)  # 500 or 800

# ________-----
## 3. CONFOUNDERS REGRESSION ------
##* Confounders of mixed regression -------
# Get your data from the Data Grab at "2. MIXED REGRESSION OF RATES"
length(mydata$Gold[!is.na(mydata$Gold)])

#** Allow MBI-DI as a gold standard? -----
yes.no <- tk_select.list(c('Yes','No',''), preselect = 'Yes', multiple = FALSE,
                         title = "\n\nAllow MBI-DI as a gold standard?\n\n")
mydata$Gold <- NULL
if (yes.no == 'Yes')
{
  # 39 comparisons per below
  # INCLUDE MBI-DI
  mydata$Gold <- ifelse(is.na(mydata$MBI.full.either), mydata$MBI.DI, mydata$MBI.full.either)
}else{
  # 35 comparisons per below
  # EXCLUDE MBI-DI
  mydata$Gold <- mydata$MBI.full.either
}

length(mydata$Gold[!is.na(mydata$Gold)])

##** Select level of analysis -------
level <- tk_select.list(c('Study level','Subgroup level',''), preselect = 'Subgroup level', multiple = FALSE,
                        title = "\n\nWhat level of analysis?\n\n")
if (level == 'Study level'){
  mydata.confounders <- mydata[mydata$Grouping %in% c('All respondents','Year of survey'),]
}else{
  mydata.confounders <- mydata #
}

# subscale.ratio is analyzed at the subgroup level
mydata.confounders <- mydata.confounders[order(mydata.confounders$Gold),]
mydata.confounders$Respondents <- round(mydata.confounders$Size * mydata.confounders$Response.rate/100,0)
mydata.confounders$subscale.ratio <- mydata.confounders$MBI.DP/mydata.confounders$MBI.EE
mydata.confounders$Gold <- ifelse(is.na(mydata.confounders$MBI.full.either), mydata.confounders$MBI.DI, mydata.confounders$MBI.full.either)

mydata.confounders <- mydata.confounders[!is.na(mydata.confounders$Gold),]
mydata.confounders <- mydata.confounders[!is.na(mydata.confounders$subscale.ratio),]
mydata.confounders <- mydata.confounders[!is.na(mydata.confounders$Mini.Z),]
length(mydata.confounders)

glm.out <- glm(Gold  ~ Mini.Z, data = mydata.confounders)
glm.out <- glm(Gold  ~ Mini.Z + subscale.ratio, data = mydata.confounders)
  (glm.out$coefficients[3])
  exp(glm.out$coefficients[3])
  confint(glm.out, 'subscale.ratio', level=0.95)
  library(domir)
  summary <- NULL
  # Old dominance analysis; da<-dominanceAnalysis(lm.out1)
  lm.out <- lm(Gold  ~ Mini.Z, data = mydata.confounders)
  lm.out <- lm(Gold  ~ Mini.Z + subscale.ratio, data = mydata.confounders)
  #lm.out <- lm(Gold  ~ Mini.Z + MBI.DP, data = mydata.confounders)
  #lm.out <- lm(Gold  ~ MBI.EE + MBI.DP, data = mydata.confounders)
  length(lm.out$residuals)
  summary(lm.out)
  confint(lm.out, level=0.95)
  domin.out <- domin(Gold  ~ Mini.Z + subscale.ratio, 
        lm, 
        list("summary", "r.squared"), 
        data = mydata.confounders)
  summary(domin.out)
  summary(lm.out)["r.squared"]
(glm.out2 <- glm(glm.out$residuals ~ mydata.confounders$subscale.ratio)) 
fitted(glm.out)
summary <- summary(glm.out)
# resid(lm.out) is same as lm.out$residuals
# Or use from https://www.r-tutor.com/elementary-statistics/simple-linear-regression/residual-plot
glm.out2 <- glm(glm.out$residuals ~ mydata.confounders$subscale.ratio)
glm.out2 <- summary(glm.out2)
plot (mydata.confounders$subscale.ratio, glm.out$residuals, xlab= 'Subscale ratio (lm.out$model[[3]])', ylab='Residulas', main='Subscale ratio predicting residuals')
abline(glm.out2$coefficients[1],glm.out2$coefficients[2], col='blue')
r2 <- as.numeric(glm.out2$adj.r.squared)
(r2 <- sprintf(r2*100, fmt='%#.1f%%'))
(pvalue <- sprintf("%1.3f", coef(glm.out2)[2,4] ))
summary <- paste ('Subgroups: ',length(mydata.confounders$subscale.ratio),'. r2= ',r2,', p=',pvalue ,sep='')
text(par("usr")[2] - nchar(summary)*strwidth("A"),par("usr")[3]+1.4*strheight("A") ,cex=1,adj=c(0,0), summary, font=1)


# other confounders analyzed at the study level
mydata.confounders <- mydata[mydata$Grouping %in% c('Year of survey','All respondents'),] # 'Year of survey' gets Kemper
lm.out <- lm(Gold  ~ Mini.Z, data = mydata.confounders)
lm.out <- lm(Gold  ~ Mini.Z + Size, data = mydata.confounders)
lm.out <- lm(Gold  ~ Mini.Z + Response.rate, data = mydata.confounders)
lm.out <- lm(Gold  ~ Mini.Z + Rate.female, data = mydata.confounders)
lm.out <- lm(Gold  ~ Mini.Z + Age, data = mydata.confounders)
lm.out <- lm(Gold  ~ Mini.Z + Rate.trainees, data = mydata.confounders)

(lm.out2 <- lm(lm.out$residuals ~ Response.rate, data = mydata)) 
plot(mydata.confounders$Mini.Z, mydata.confounders$Gold)
(lm.out.summary <- summary(lm.out)) #ADD WEIGHTING BY SIZE ??
(R2_Fixed <- round(lm.out.summary$adj.r.squared*100,0))
(R_Fixed <- round(lm.out.summary$adj.r.squared^(1/2),2))


# Insig: Size, Rate.physicians
(lm.out <- lm(Gold ~ Mini.Z, data = mydata)) #ADD WEIGHTING BY SIZE? weights = mydata$Size,
(lm.out <- lm(Gold ~ Size + Mini.Z, data = mydata)) #ADD WEIGHTING BY SIZE? weights = mydata$Size,
(lm.out <- lm(Gold ~ Response.rate + Mini.Z, data = mydata)) 
(lm.out <- lm(Gold ~ Rate.female + Mini.Z, data = mydata)) 
(lm.out <- lm(lm.out$residuals ~ Response.rate, data = mydata)) 
(lm.out.summary <- summary(lm.out))
(R2_Fixed <- round(lm.out.summary$adj.r.squared*100,1))
(R_Fixed <- round(lm.out.summary$adj.r.squared^(1/2),2))

format(lm.out.summary$coefficients[2,4],scientific = FALSE)

length(mydata$Response.rate)
length(lm.out$residuals)

plot(lm.out$model$Response.rate,lm.out$residuals,xlab='Response rate',ylab="Residual after regression with the SIBOQ", xlim=c(0,100),ylim=c(0,100), main="Prediction of MBI from SIBOQ")
abline(lm.out.summary$coefficients[1],lm.out.summary$coefficients[2])

# Confounders in metacor
data.correlation <- data.import
data.correlation$DP.EE <- data.correlation$Correlation.depersonalization/data.correlation$Correlation.exhaustion 

##* Confounders of individual scales -------
##** Select which scale to study -------
scale <- tk_select.list(c('Emotional exhaustion','Depersonalization',''), preselect = 'Emotional exhaustion', multiple = FALSE,
                        title = "\n\nWhich are we studying?\n\n")
if (scale == 'Depersonalization'){
  data.correlation$outcome <- data.correlation$Correlation.depersonalization
}else{
  data.correlation$outcome <- data.correlation$Correlation.exhaustion
}
data.correlation <- data.correlation[!(is.na(data.correlation$outcome)),]

##** Meta-analysis-----
data.correlation$outcome
data.correlation$Size
meta.correlation <- meta::metacor(outcome, Size, studlab = studlab, random = TRUE, hakn = TRUE, data = data.correlation)
summary(meta.correlation)

covar <- tk_select.list(c('Age','DP.EE','Rate.female','Rate.physicians','Response.rate','Size'), preselect = 'Age', multiple = FALSE,
                        title = "\n\nSelect your covariabe?\n\n")

meta.correlation$data[covar]

if (covar == 'Age'){
  meta.regression <- metareg(meta.correlation, formula = ~Age, hakn = TRUE)
}else if(covar == 'DP.EE'){
  meta.regression <- metareg(meta.correlation, formula = ~DP.EE, hakn = TRUE)
}else if(covar == 'Rate.female'){
  meta.regression <- metareg(meta.correlation, formula = ~Rate.female, hakn = TRUE)
}else if(covar == 'Rate.physicians'){
  meta.regression <- metareg(meta.correlation, formula = ~Rate.physicians, hakn = TRUE)
}else if(covar == 'Response.rate'){
  meta.regression <- metareg(meta.correlation, formula = ~Response.rate, hakn = TRUE)
}else{
  meta.regression <- metareg(meta.correlation, formula = ~Size, hakn = TRUE)
}

summary(meta.regression)
meta.regression$
  
  meta.regression$pval[1][[1]]

title 
meta::bubble(meta.regression,  backtransf=FALSE, ylab=paste(scale, ' ()'), main=title)
text(par("usr")[2],par("usr")[4]-1.2<- paste('Bubble plot - ', scale,' - ', covar,' - ',current.date, sep='')
     5*strheight("A"),cex=1.2,adj=c(1,0),paste("p (correlation) = ",sprintf(meta.regression$pval[2], fmt='%#.3f'), sep=""), font=1)

(plotname <- paste(title,'.png',sep=''))


#_________---------
## 4. MBI_DI vs full MBI ======
### Data grab (RATES) ===================================
# co <- read.table("https://data.princeton.edu/eco572/datasets/cohhpop.dat", col.names=c("age","pop"), header=FALSE)
file.filter      <- matrix(c("Spreadsheets","*SIBOQ*.csv;*SIBOQ*.xls;*SIBOQ*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
filename2        <- choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
file.extension2  <- substr(filename2,regexpr("\\.[^\\.]*$", filename2)+1, nchar(filename2))
data.import <- NULL
if (file.extension2 == 'csv'){
  data.import   <- read.csv(filename2, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
}else{
  data.import   <- read.xlsx(filename2)
  }

head(data.import)
mydata <- data.import
## Data cleaning -----
##* Remove rows or anyone? (left of comma) -------
mydata <- mydata[!(mydata$Author == '' | is.na(mydata$Author)),]
mydata <- mydata[!(is.na(mydata$MBI.full.either) | is.na(mydata$MBI.DI)),]
mydata <- mydata[(mydata$Grouping =="All respondents" | mydata$Grouping =="Specialty"),]
nrow(mydata)

mydata$color <- 'gray' # Ong 
for(i in 1:nrow(mydata))
{
  if (mydata$Author[i]=='Knox'){mydata$color[i] <- 'green'}
  if (mydata$Author[i]=='Olson'){mydata$color[i] <- 'orangered'}
  if (mydata$Author[i]=='Trockel'){mydata$color[i] <- '#00CCCC'} #teal
  if (mydata$Author[i]=='Brady'){mydata$color[i] <- 'black'}
  if (mydata$Author[i]=='Kemper'){mydata$color[i] <- 'purple'}
  if (mydata$Author[i]=='Yellowlees'){mydata$color[i] <- 'Yellow'}
  if (mydata$Author[i]=='Li'){mydata$color[i] <- 'blue'} # Added to this analysis 10/23/2022 although excluded from main analysis
  if (mydata$Author[i]=='Ong'){mydata$color[i] <- 'gray'}
}
# Point size
mydata$PointSize <- 0.5 + (mydata$Size - min(mydata$Size))/(max(mydata$Size)-min(mydata$Size))

# par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
par(mar=c(8,4.1,4.1,2.1), mfrow=c(1,1))
plot(NULL, NULL, xlab='MBI-DI', ylab='Full MBI', xlim=c(0,100),ylim=c(0,100), main="Prediction of the MBI from MBI-DI")
points(mydata$MBI.DI,mydata$MBI.full.either,col=mydata$color, pch=19, cex=mydata$PointSize)

regression <- 'fixed'
(lm.out <- lm(MBI.full.either ~ MBI.DI, data = mydata)) #ADD WEIGHTING BY SIZE? weights = mydata$Size,
(lm.out.summary <- summary(lm.out)) 
(R2_Fixed <- round(lm.out.summary$adj.r.squared*100,))
R2.label <- bquote(R^2 ~ "=" ~ .(R2_Fixed ) ~ "%")
(R_Fixed <- sprintf(lm.out.summary$adj.r.squared^(1/2),fmt='%#.2f'))
R.label <- bquote(R ~ "=" ~ .(R_Fixed ) ~ "")
(CI        <- lm.out.summary$coefficients[1,2]*1.96)
# Y intercept
(intercept <- lm.out.summary$coefficients[1])
# Slope
(slope       <- lm.out.summary$coefficients[2])
abline(intercept, slope)

#** legend ---------
legend("topleft", adj = 0, xjust = 1, inset = c(-0.01,0), c(paste("Regression line (", regression,")", sep='')), pch = NULL, pt.bg = "white", bty = "n", border = "white", lty=c("solid"), col=c("black"))
# values
text(75,20,adj=c(0,1),bquote(R^2 ~ " (fixed) = " ~ .(R2_Fixed) ~ "%"))
text(75,14,adj=c(0,1),bquote(R ~ " (fixed) = " ~ .(R.label)))
#text(80,10,adj=c(0,1),bquote(R   ~ " (fixed) = " ~ .(R_Fixed)))
text(75, 8,adj=c(0,1), paste0('Observations: ', as.character(length(a_mixed.summary$residuals)))  )

##** Notes -----
mtext(expression(paste(bold("Notes: "))), side = 1, line = 4,adj=0,cex=1)
mtext("References: Olson, 2019 (red); Trockel, 2018 (teal)", side = 1, line = 5,adj=0,cex=1)

#Footer <- paste(Footer,"\n","* Based on rate conversion derived from Olson PMID 30467949",sep='')

##** Export plot------------------------
PlotName <- paste('eFigure 1. MBI-DI predicting MBI (', regression, ') - ', sep='')
Directory <- '../Plots/'

(current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))

rstudioapi::savePlotAsImage( # Print at 800*plotheight
  paste(Directory,PlotName,current.date,'.png',sep=''),
  format = "png", width = 800, height = 600)  # 500 or 800

#_________---------
## 5. BASELINE CHARACTERISTICS ======
##* TABLE 1 ======
Brady <- mydata[(mydata$Author == 'Brady'),]
#MBI.full.rate  <- ddply(mydata, .(Resident, Resident.position),  summarize, Resident.cases2 = 
sum(Brady$Size*Brady$MBI.full/100)/sum(Brady$Size)
sum(Brady$Size*Brady$Mini.Z/100)/sum(Brady$Size)
##* Data grab (RATES) ===================================
# co <- read.table("https://data.princeton.edu/eco572/datasets/cohhpop.dat", col.names=c("age","pop"), header=FALSE)
file.filter      <- matrix(c("Spreadsheets","*.csv;*.xls;*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
# "_studies included - summary.xlsx"
# XLSX IS IN STUDIES FOLDER
filename3        <- choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
file.extension3  <- substr(filename3,regexpr("\\.[^\\.]*$", filename2)+1, nchar(filename2))
data.import <- NULL
if (file.extension3 == 'csv'){
  data.import   <- read.csv(filename3, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
}else{
  data.import   <- read.xlsx(filename3)
}

head(data.import)

##* Response rate ======
meta1 <- metaprop(Respondents , Size , studlab = Author, subgroup = NULL, data=data.import,  verbose=FALSE, fixed=FALSE, method="GLMM", hakn=FALSE, title = "Meta-analysis of results")
(summary(meta1))

