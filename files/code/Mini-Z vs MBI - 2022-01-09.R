#This file is best used within R Studio
# rbadgett@kumc.edu
### Start =======================================
version
citation(package = "base", lib.loc = NULL, auto = NULL)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#setwd("../plots")
#Directory <- getwd()

##* Functions -----
function_libraries_install <- function(packages){
  install.packages(setdiff(packages, rownames(installed.packages())))
  for(package_name in packages)
  {
    #library(package_name,character.only=TRUE,quietly=TRUE);
    library(package_name,character.only=TRUE, quietly = FALSE);
  }
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
# grid.raster(mypng, .3, .3, width=.25)

##* Libraries and libraries------------------------------------
#*** Meta-analysis and positive deviance----
packages_meta <- c("metafor", #
                   'meta',   # Meta-analysis
                   'boot',   # inv.logit to identify deviants
                   'grid',   # Forest and blobbogram
                   'gemtc'   # Blobbogram
)
function_libraries_install(packages_meta)

if (!require('lme4')) # Mixed effects
{
  install <- tk_messageBox(type = c('okcancel'), "Need to install lme4", caption = "Library needed")
  if (install == 'cancel') {end}
  install.packages('lme4');
  library('lme4')
}
if (!require('report')) # Report
{
  install <- tk_messageBox(type = c('okcancel'), "Need to install report", caption = "Library needed")
  if (install == 'cancel') {end}
  install.packages('report');
  library('report')
}

##* Constants declaration -------------------
`%notin%` <- Negate(`%in%`)
LF <- "\n"
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
KUCrimson = "#e8000d"
KUYellow = "#ffc82d"
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
I2.label <- bquote(I^2  ~ "%")
I2.label <- bquote(I^2 ~ "=" ~ .(I2.value) ~ "%")
I2.label
text(0.25,0.25,I2.label)
I2.summary.label <- bquote("RE Model ("~ I^2 ~ "= " ~ .(I2.value) ~ "%)")
text(0.5,0.5,I2.summary.label)
R2 = 50
R2.value <- formatC(R2, digits=1, format="f")
R2.label <- bquote(R^2  ~ "%")
R2.label <- bquote(R^2 ~ "=" ~ .(R2.value) ~ "%")
R2.label
text(xmin,ymax-1.1* strheight("A"),adj=c(0,1),R2.label)

### Data grab (RATES) ===================================
# co <- read.table("https://data.princeton.edu/eco572/datasets/cohhpop.dat", col.names=c("age","pop"), header=FALSE)
file.filter   <- matrix(c("Spreadsheets","MINI-Z*.csv;MINI-Z*.xls;MINI-Z*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
filename      <- choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
file.extension<- substr(filename,regexpr("\\.[^\\.]*$", filename)+1, nchar(filename))
if (file.extension == 'csv'){
  data.import   <- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
}else{
  data.import   <- read.xlsx(filename)}
  
mydata <- data.import

##* ReMove rows or anyone? (left of comma) -------
mydata <- mydata[!(mydata$Author == '' | is.na(mydata$Author)),]
# Remove Brady or Yellowless sensitivity analysis?
mydata2 <- mydata[!(mydata$Author == 'Ong'),]
#mydata2 <- mydata[!(mydata$Author == 'Brady' | mydata$Author == 'Yellowlees'),]

# Remove overlapping
# Oops, this removes Trockel and Yellowless
mydata <-  mydata[(ydata$Grouping == 'All respondents'),]

## Baseline characeteristics ======
Brady <- mydata[(mydata$Author == 'Brady'),]
#MBI.full.rate  <- ddply(mydata, .(Resident, Resident.position),  summarize, Resident.cases2 = 
sum(Brady$Size*Brady$MBI.full/100)/sum(Brady$Size)
sum(Brady$Size*Brady$Mini.Z/100)/sum(Brady$Size)

##* Table 1 ======
unique(mydata$Author)

## REGRESSION Analyses ====================
##* Correlation (do not report this R ) ====================
corr <- cor.test(mydata$Mini.Z, mydata$MBI.full)
corr$estimate
corr$estimate^2
R  = round(corr$estimate,2)
(R2 = corr$estimate^2)
R2.value <- formatC(R2, digits=2, format="f")
R2.label <- bquote(R^2 ~ "=" ~ .(R2.value) ~ "%")

##* Regression =======================
##**  Mixed model -----
# Total: https://slcladal.github.io/regression.html
# For predictions that use a mixed-effects modeling approach, there are four different R2 values
# reported in the model summaries: 
# marginal (fixed) = 0.39
# conditional (total)= 0.73
# fitted, and
# predictive.

#a_mixed = lmer(MBI.full  ~ Mini.Z, data = mydata)
#a_mixed = lmer(MBI.full  ~(1 | Mini.Z), data = mydata)
#a_mixed = lmer(MBI.full  ~Mini.Z + (1 | Respondents.Source), data = mydata)
a_mixed = lmer(MBI.full  ~Mini.Z + Response.rate + (1 | Author), data = mydata)
a_mixed <- lmer(MBI.full  ~ Mini.Z + (1 | Author), data = mydata) # BEST!!! same with REML=T
(a_mixed.summary <- summary(a_mixed))
#abline(a_mixed.summary$coefficients[1],a_mixed.summary$coefficients[2])
#confint(a_mixed)
##*** Report package ----------
# https://easystats.github.io/report/
REPORT <- report (a_mixed)
summary(REPORT)
#(R2REPORT <- as.data.frame(REPORT))
(R2_Mixed <- round(R2REPORT$Fit[8]*100,1))
(R2_Fixed <- round(R2REPORT$Fit[9]*100,1))
(R_Mixed  <- (R2REPORT$Fit[8])^(1/2))
(R_Fixed  <- (R2REPORT$Fit[9])^(1/2))

# Below has no predictive value
mydata$Mini.Z.adjusted <- a_mixed.summary$coefficients[1] + mydata$Mini.Z * a_mixed.summary$coefficients[2]

##**  Linear model -----
#(lm.out <- summary(lm(MBI.full ~ Mini.Z, data = mydata)))
# Weighted
(lm.out <- lm(MBI.full ~ Mini.Z, data = mydata)) #ADD WEIGHTING BY SIZE? weights = mydata$Size,
(lm.out.summary <- summary(lm.out)) #ADD WEIGHTING BY SIZE ??
(R2_Fixed <- round(lm.out.summary$adj.r.squared*100,1))
(R_Fixed <- round(lm.out.summary$adj.r.squared^(1/2),2))

mydata$Mini.Z.adjusted <- lm.out.summary$coefficients[1] + mydata$Mini.Z * lm.out.summary$coefficients[2]

##** Comparing mixed and fixed/linear models -----------------
AIC(logLik(a_mixed))
AIC(logLik(lm.out))
anova(a_mixed, lm.out, test = "Chisq", refit = F)

## Plot ------------------
# Study color
mydata$color <- 'orangered'
for(i in 1:nrow(mydata))
{
  if (mydata$Author[i]=='Knox'){mydata$color[i] <- 'green'}
  if (mydata$Author[i]=='Trockel'){mydata$color[i] <- '#00CCCC'} #teal
  if (mydata$Author[i]=='Brady'){mydata$color[i] <- 'black'}
  if (mydata$Author[i]=='Yellowlees'){mydata$color[i] <- 'gray'}
}
# Point size
mydata$PointSize <- 0.5 + (mydata$Size - min(mydata$Size))/(max(mydata$Size)-min(mydata$Size))

par(mar=c(7,4.1,4.1,2.1), mfrow=c(1,1))
plot(NULL,NULL,xlab='Mini-Z (burned out %)',ylab='MBI (burned out %)',xlim=c(0,100),ylim=c(0,100), main="Prediction of MBI from MINI-Z")

points(mydata$Mini.Z,mydata$MBI.full,col=mydata$color, pch=19, cex=mydata$PointSize)

# From mixed regression
abline(a_mixed.summary$coefficients[1],a_mixed.summary$coefficients[2], col="red")
#abline(a_mixed.summary$coefficients[1] + a_mixed.summary$coefficients[1,2]*1.96,a_mixed.summary$coefficients[2], lty = 3)
#abline(a_mixed.summary$coefficients[1] - a_mixed.summary$coefficients[1,2]*1.96,a_mixed.summary$coefficients[2], lty = 3)
#a_mixed.summary$coefficients[1,2]*1.96

# From linear regression
abline(lm.out.summary$coefficients[1],lm.out.summary$coefficients[2])
abline(lm.out.summary$coefficients[1] + lm.out.summary$coefficients[1,2]*1.96,lm.out.summary$coefficients[2], lty = 3)
abline(lm.out.summary$coefficients[1] - lm.out.summary$coefficients[1,2]*1.96,lm.out.summary$coefficients[2], lty = 3)
lm.out.summary$coefficients[1,2]*1.96


legend("topleft", adj = 0, xjust = 1, inset = c(-0.01,-0.06), c("Regression line (fixed)","95% Confidence limits"), pch = NULL, pt.bg = "white", bty = "n", border = "white", lty=c("solid","dashed"), col=c("black","blue"))

text(80,30,adj=c(0,1),bquote(R^2 ~ " (mixed) = " ~ .(R2_Mixed) ~ "%"))
text(80,20,adj=c(0,1),bquote(R^2 ~ " (fixed) = " ~ .(R2_Fixed) ~ "%"))
text(80,10,adj=c(0,1),bquote(R ~ " (fixed) = " ~ .(R_Fixed)))

# Fixed
CI <- round(lm.out.summary$coefficients[1,2]*1.96,1)
# Mixed
#CI <- round(a_mixed.summary$coefficients[1,2]*1.96,1)

##** Notes -----
mtext(expression(paste(bold("Note: "))), side = 1, line = 4.2,adj=0,cex=1)
# Pre-Trockel
#mtext(expression(paste("MBI = 15.3 + 1.08 * Mini-Z +/- 7.2")), side = 1, line = 5.2,adj=0,cex=1)
# With Trockel
#mtext(expression(paste("MBI = 15.2 + 1.08 * Mini-Z +/- 7.0")), side = 1, line = 5.2,adj=0,cex=1)
# With Trockel and partial Brady
#mtext(expression(paste("MBI = 19.9 + 0.91 * Mini-Z +/- ",CI,sep='')), side = 1, line = 5.2,adj=0,cex=1)
# With Trockel and Brady
formula <- paste("MBI = ", 14.8, " + ", 0.927," * Mini-Z +/- ",CI,sep='')
mtext(formula, side = 1, line = 5.2,adj=0,cex=1)
mtext(expression(paste("Points colors: Brad=black, Olson=orange; Knox=green; Trockel=teal, Yellowless=gray")), side = 1, line = 6.2,adj=0,cex=1)
#Footer <- paste(Footer,"\n","* Based on rate conversion derived from Olson PMID 30467949",sep='')

##** Export plot------------------------
PlotName <- 'MINI-Z predicting MBI - '
Directory <- 'Plots/'

(current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))

rstudioapi::savePlotAsImage( # Print at 800*plotheight
  paste(Directory,PlotName,current.date,'.png',sep=''),
  format = "png", width = 800, height = 600)  # 500 or 800

## Data grab (CORRELATIONS) ===================================
# co <- read.table("https://data.princeton.edu/eco572/datasets/cohhpop.dat", col.names=c("age","pop"), header=FALSE)
file.filter   <- matrix(c("Spreadsheets","correl*.csv;correl*.xls;correl*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
filename      <- choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
file.extension<- substr(filename,regexpr("\\.[^\\.]*$", filename)+1, nchar(filename))
if (file.extension == 'csv'){
  data.import   <- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
}else{
  data.import   <- read.xlsx(filename)}

data.correlation <- data.import

##* ReMove rows or anyone? (left of comma) -------
data.correlation <- data.correlation[!(data.correlation$Author == '' | is.na(data.correlation$Author)),]
# Remove Brady or Yellowless sensitivity analysis?
mydata2 <- mydata[!(mydata$Author == 'Ong'),]

data.correlation <- data.correlation[!(is.na(data.correlation$Author)),]

data.correlation$studlab <- paste(data.correlation$Authors,', ',data.correlation$Year,sep='')

data.correlation$Size <- as.numeric(data.correlation$Size)

scale <- 'Emotional exhaustion'
scale <- 'Depersonalization'

if (scale == 'Depersonalization'){data.correlation$outcome <- data.correlation$Correlation.depersonalization}else{data.correlation$outcome <- data.correlation$Correlation.exhaustion}

##* Mata-analysis of correlation coefficients -----
meta.correlation <- meta::metacor(outcome, Size, studlab = studlab, data = data.correlation)
# exclude = Study %in% c('Olson'), 

meta::forest(meta.correlation, sortvar = Size, xlim=c(0,1),print.tau2 = FALSE, fixed = FALSE, xlab=paste("Correlation with ", scale,sep=''))
grid.text(paste("Correlation of single item Mini-Z with MBI ", scale,sep=''), 0.5, 0.9, gp = gpar(fontsize = 14, fontface = "bold"))

##** Export plot------------------------
PlotName <- paste('forest plot - Mini-Z with ', scale, ' - ',sep='')
p.width = 700; p.height = 400
Directory <- '../Plots/'

rstudioapi::savePlotAsImage( # Print at 800*plotheight
  paste(Directory,PlotName,current.date,'.png',sep=''),
  format = "png", width = p.width, height = p.height)
