
#Packages used
library(dplyr)
library(tidyverse)
library(readxl)
library(psych)
library(GPArotation)
library(ggplot2)
library(lavaan)
library(knitr)
library(kableExtra)
library(magick)
library(brms)
library(loo)
library(tidybayes)
library(bayestestR)
library(sjPlot)
library(cowplot)
library(grid)
library(gridExtra)
library(ggpubr)


# Cleaning data -------------------------------

# Load raw data (plus make R read it as numeric)
file <- ""       #Enter file address
myCols <- as.character(read_excel(file, n_max = 1, col_names = FALSE))
d <- read_excel(file, skip = 2, col_names = myCols) %>%
  subset(Progress==100)  #No participants are excluded here

#Remove participants who failed the attention checks
table(d$Attention_check_1_6)
table(d$Attention_check_2_6)
d<-d[!(d$Attention_check_1_6 < 35),]
d<-d[!(d$Attention_check_1_6 > 65),]
d<-d[!(d$Attention_check_2_6 < 35),]
d<-d[!(d$Attention_check_2_6 > 65),]

#Create ID column
d$ParticipantID <- seq.int(nrow(d))

#Make Sex into variable Men
d$Men[grep(1, d$Sex)] <- 1
d$Men[grep(2, d$Sex)] <- 0
d$Men[grep(3, d$Sex)] <- 0

#Make a binary ethnicity variable
d$Ethnicity[d$Ethnicity == "1,2"] <- 111
d$Ethnicity[d$Ethnicity == "1,2,3"] <- 111
d$Ethnicity[d$Ethnicity == "1,2,6"] <- 111
d$Ethnicity[d$Ethnicity == "1,3"] <- 111
d$Ethnicity[d$Ethnicity == "1,3,5"] <- 111
d$Ethnicity[d$Ethnicity == "1,3,6"] <- 111
d$Ethnicity[d$Ethnicity == "1,4"] <- 111
d$Ethnicity[d$Ethnicity == "1,6"] <- 111
d$Ethnicity[d$Ethnicity == "2"] <- 111
d$Ethnicity[d$Ethnicity == "2,4"] <- 111
d$Ethnicity[d$Ethnicity == "2,6"] <- 111
d$Ethnicity[d$Ethnicity == "2,7"] <- 111
d$Ethnicity[d$Ethnicity == "3"] <- 111
d$Ethnicity[d$Ethnicity == "4"] <- 111
d$Ethnicity[d$Ethnicity == "4,5"] <- 111
d$Ethnicity[d$Ethnicity == "4,6"] <- 111
d$Ethnicity[d$Ethnicity == "4,7"] <- 111
d$Ethnicity[d$Ethnicity == "5"] <- 111
d$Ethnicity[d$Ethnicity == "6"] <- 111
d$Ethnicity[d$Ethnicity == "7"] <- 111
d$Ethnicity[d$Ethnicity == "111"] <- 2

#Combine the highest two education categories (these two not ordinal)
table(d$Education)
d$Education[grep(8, d$Education)] <- 7

#Code religious identification as binary
d$Religion[d$Religion == "2"] <- 0

#Re-name some columns
colnames(d) <- gsub("_6", "", colnames(d))
#Fix DFSN colnames which shoudln't have removed "_6" from
d <- d %>% rename(DFSN_6SocCon = DFSNSocCon)

#Coding reversals and renaming
#DFS Nation
d$DFSN_2IneCon <- 100 - d$DFSN_2IneCon
d$DFSN_4IneCon <- 100 - d$DFSN_4IneCon
d$DFSN_6SocCon <- 100 - d$DFSN_6SocCon
d$DFSN_8SocCon <- 100 - d$DFSN_8SocCon
d$DFSN_10SocCon <- 100 - d$DFSN_10SocCon
#Issues
d$Issue1_TaxHelpPoor <- 100 - d$Issue1_TaxHelpPoor
d$Issue2_TaxAdresInqty <- 100 - d$Issue2_TaxAdresInqty
d$Issue5_GovControl <- 100 - d$Issue5_GovControl
d$Issue9_GlobeWarm <- 100 - d$Issue9_GlobeWarm
d$Issue10_AffirmActn <- 100 - d$Issue10_AffirmActn
d$Issue14_AbrtnBetter <- 100 - d$Issue14_AbrtnBetter
d$Issue16_RelignInGov <- 100 - d$Issue16_RelignInGov
#Right Wing Authoritarianism
d$RWA01r <- 100 - d$RWA01r
d$RWA03r <- 100 - d$RWA03r
d$RWA06r <- 100 - d$RWA06r
d$RWA07r <- 100 - d$RWA07r
d$RWA10r <- 100 - d$RWA10r
d$RWA12r <- 100 - d$RWA12r
d$RWA13r <- 100 - d$RWA13r
d$RWA15r <- 100 - d$RWA15r
d$RWA17r <- 100 - d$RWA17r
#Social Dominance Orientation
d$SDO05r <- 100 - d$SDO05r
d$SDO06r <- 100 - d$SDO06r
d$SDO07r <- 100 - d$SDO07r
d$SDO08r <- 100 - d$SDO08r
d$SDO13r <- 100 - d$SDO13r
d$SDO14r <- 100 - d$SDO14r
d$SDO15r <- 100 - d$SDO15r
d$SDO16r <- 100 - d$SDO16r

#Make point estimates for scales
#DFS Nation
modelDFS_nat <- 
  'Inequality =~ DFSN_1InePro + DFSN_2IneCon + DFSN_3InePro + DFSN_4IneCon
   Social_control =~ DFSN_5SocPro + DFSN_6SocCon + DFSN_7SocPro + DFSN_8SocCon + DFSN_9SocPro + DFSN_10SocCon'
cfaDFS_nat <- cfa(modelDFS_nat, data = d, estimator = "DWLS")
cfaDFS_nat <- lavPredict(cfaDFS_nat) %>%
  as.data.frame() %>%
  rename(cfaInequality_nat = Inequality) %>%
  rename(cfaSocial_control_nat = Social_control)

#SDO-RWA
modelSDO_RWA <- 
  'SDO =~ SDO01 + SDO02 + SDO03 + SDO04 + SDO05r + SDO06r + SDO07r + SDO08r + SDO09 + SDO10 + SDO11 + SDO12 + SDO13r + SDO14r + SDO15r + SDO16r
   RWA =~ RWA01r + RWA02 + RWA03r + RWA04 + RWA05 + RWA06r + RWA07r + RWA08 + RWA09 + RWA10r + RWA11 + RWA12r + RWA13r + RWA14 + RWA15r + RWA16 + RWA17r + RWA18'
cfaSDO_RWA <- cfa(modelSDO_RWA, data = d, estimator = "DWLS")
cfaSDO_RWA <- lavPredict(cfaSDO_RWA) %>%
  as.data.frame() %>%
  rename(cfaSDO = SDO) %>%
  rename(cfaRWA = RWA)

d <- cbind(d, cfaDFS_nat, cfaSDO_RWA)

#Create long version of dataset
dLong <- pivot_longer(d,
                      cols = starts_with("Issue"),
                      names_to = "Issue",
                      values_to = "Score")

#Create dummy for Economic and Social issues
dLong$Conservatism[dLong$Issue=="Issue1_TaxHelpPoor"] <- "Economic"
dLong$Conservatism[dLong$Issue=="Issue2_TaxAdresInqty"] <- "Economic"
dLong$Conservatism[dLong$Issue=="Issue3_TaxHealthcare"] <- "Economic"
dLong$Conservatism[dLong$Issue=="Issue4_GovDependence"] <- "Economic"
dLong$Conservatism[dLong$Issue=="Issue5_GovControl"] <- "Economic"
dLong$Conservatism[dLong$Issue=="Issue6_LimitedGov"] <- "Economic"
dLong$Conservatism[dLong$Issue=="Issue7_CompetCaptlsm"] <- "Economic"
dLong$Conservatism[dLong$Issue=="Issue8_HomelessRespo"] <- "Economic"
dLong$Conservatism[dLong$Issue=="Issue9_GlobeWarm"] <- "Economic"
dLong$Conservatism[dLong$Issue=="Issue10_AffirmActn"] <- "Economic"
dLong$Conservatism[dLong$Issue=="Issue11_MarriageHetr"] <- "Social"
dLong$Conservatism[dLong$Issue=="Issue12_GayAdoption"] <- "Social"
dLong$Conservatism[dLong$Issue=="Issue13_Euthanasia"] <- "Social"
dLong$Conservatism[dLong$Issue=="Issue14_AbrtnBetter"] <- "Social"
dLong$Conservatism[dLong$Issue=="Issue15_NoReligTrust"] <- "Social"
dLong$Conservatism[dLong$Issue=="Issue16_RelignInGov"] <- "Social"
dLong$Conservatism[dLong$Issue=="Issue17_MascClothing"] <- "Social"
dLong$Conservatism[dLong$Issue=="Issue18_WomensPlace"] <- "Social"
dLong$Conservatism[dLong$Issue=="Issue19_CrckdwnDrugs"] <- "Social"
dLong$Conservatism[dLong$Issue=="Issue20_IntgrtLifsty"] <- "Social"

#Code Men as a dummy
dLong$Men[dLong$Men == "1"] <- "Man"
dLong$Men[dLong$Men == "0"] <- "Not man"

#Code religious identification as dummy
dLong$Religion[dLong$Religion == "1"] <- "Religious"
dLong$Religion[dLong$Religion == "0"] <- "Not religious"

#Normalise the continuous & ordinal predictors (mean=0,SD=1)
dLong$cfaInequality_nat <- scale(dLong$cfaInequality_nat)
dLong$cfaSocial_control_nat <- scale(dLong$cfaSocial_control_nat)
dLong$cfaSDO <- scale(dLong$cfaSDO)
dLong$cfaRWA <- scale(dLong$cfaRWA)
dLong$Age <- scale(dLong$Age)
dLong$Education <- scale(dLong$Education)
dLong$Income <- scale(dLong$Income)

#Transform outcome scores so they vary between (as near as makes no difference) 0 and 1 ('near' is required by the beta regression models)
#Create function to normalise data
normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
dLong$Score <- normalise(dLong$Score)
dLong$Score[dLong$Score == 0] <- 0.1
dLong$Score[dLong$Score == 1] <- 0.9


# Extra: Descriptive statistics --------------------------------------------------

#Basic variables
describe(d$`Duration (in seconds)`)

table(d$Sex)
table(d$Men)
table(d$Sex_3_TEXT)

describe(d$Age)
ggplot(d, aes(Age)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray") + xlim(15,80)

table(d$Ethnicity)

table(d$Religion)

table(d$Education)
describe(d$Education)
ggplot(d, aes(Education)) + 
  geom_histogram(bins = 7, color = "black", fill = "lightgray")

table(d$Income)
describe(d$Income)
ggplot(d, aes(Income)) + 
  geom_histogram(bins = 13, color = "black", fill = "lightgray")

#Key variables
describe(d$cfaInequality_nat)
ggplot(d, aes(cfaInequality_nat)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$cfaSocial_control_nat)
ggplot(d, aes(cfaSocial_control_nat)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

#Issues
describe(d$Issue1_TaxHelpPoor)
ggplot(d, aes(Issue1_TaxHelpPoor)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue2_TaxAdresInqty)
ggplot(d, aes(Issue2_TaxAdresInqty)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue3_TaxHealthcare)
ggplot(d, aes(Issue3_TaxHealthcare)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue4_GovDependence)
ggplot(d, aes(Issue4_GovDependence)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue5_GovControl)
ggplot(d, aes(Issue5_GovControl)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue6_LimitedGov)
ggplot(d, aes(Issue6_LimitedGov)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue7_CompetCaptlsm)
ggplot(d, aes(Issue7_CompetCaptlsm)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue8_HomelessRespo)
ggplot(d, aes(Issue8_HomelessRespo)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue9_GlobeWarm)
ggplot(d, aes(Issue9_GlobeWarm)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue10_AffirmActn)
ggplot(d, aes(Issue10_AffirmActn)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue11_MarriageHetr)
ggplot(d, aes(Issue11_MarriageHetr)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue12_GayAdoption)
ggplot(d, aes(Issue12_GayAdoption)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue13_Euthanasia)
ggplot(d, aes(Issue13_Euthanasia)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue14_AbrtnBetter)
ggplot(d, aes(Issue14_AbrtnBetter)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue15_NoReligTrust)
ggplot(d, aes(Issue15_NoReligTrust)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue16_RelignInGov)
ggplot(d, aes(Issue16_RelignInGov)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue17_MascClothing)
ggplot(d, aes(Issue17_MascClothing)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue18_WomensPlace)
ggplot(d, aes(Issue18_WomensPlace)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue19_CrckdwnDrugs)
ggplot(d, aes(Issue19_CrckdwnDrugs)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$Issue20_IntgrtLifsty)
ggplot(d, aes(Issue20_IntgrtLifsty)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")


#Individual DFS items
describe(d$DFSN_1InePro)
ggplot(d, aes(DFSN_1InePro)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$DFSN_2IneCon)
ggplot(d, aes(DFSN_2IneCon)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$DFSN_3InePro)
ggplot(d, aes(DFSN_3InePro)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$DFSN_4IneCon)
ggplot(d, aes(DFSN_4IneCon)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$DFSN_5SocPro)
ggplot(d, aes(DFSN_5SocPro)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$DFSN_6SocCon)
ggplot(d, aes(DFSN_6SocCon)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$DFSN_7SocPro)
ggplot(d, aes(DFSN_7SocPro)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$DFSN_8SocCon)
ggplot(d, aes(DFSN_8SocCon)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$DFSN_9SocPro)
ggplot(d, aes(DFSN_9SocPro)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")

describe(d$DFSN_10SocCon)
ggplot(d, aes(DFSN_10SocCon)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")


# DFS reliability ------------------------------------------------------

#DFS Inequality Nation
dDFSIN <- data.frame(d$DFSN_1InePro,d$DFSN_2IneCon,d$DFSN_3InePro,d$DFSN_4IneCon)
omega(dDFSIN, nfactors = 1)

#DFS Social Control Nation
dDFSSN <- data.frame(d$DFSN_5SocPro,d$DFSN_6SocCon,d$DFSN_7SocPro,d$DFSN_8SocCon,d$DFSN_9SocPro,d$DFSN_10SocCon)
omega(dDFSSN, nfactors = 1)


# Tables 1 and 2: Factor analyses of DFS --------------------------------------------

#Nation Inequality (4 items) and Social Control (6 items)
cfamodel1 <- 
  'Inequality =~ DFSN_1InePro + DFSN_2IneCon + DFSN_3InePro + DFSN_4IneCon
   Social_control =~ DFSN_5SocPro + DFSN_6SocCon + DFSN_7SocPro + DFSN_8SocCon + DFSN_9SocPro + DFSN_10SocCon'
cfamodel1 <- cfa(cfamodel1, data = d, estimator = "DWLS")
sum1 <- summary(cfamodel1, fit.measures = TRUE)
sum1fits <- cbind.data.frame(CFI = sum1$fit[9], RMSEA = sum1$fit[11], SRMR = sum1$fit[19])
s.sol1 <- standardizedsolution(cfamodel1) %>%
  filter(op == "=~") %>% 
  select(Factor=lhs, Issue=rhs, Loading=est.std, CI_low=ci.lower, CI_up=ci.upper, Z=z, p=pvalue)  %>% 
  mutate_if(is.numeric, round, digits=3)
s.sol1$CI <- paste(s.sol1$CI_low, s.sol1$CI_up, sep=", ")
s.sol1 <- subset(s.sol1, select=-c(CI_low, CI_up))
s.sol1 <- s.sol1[, c(1, 2, 3, 6, 4, 5)]

#Nation single dimension
cfamodel2 <- 
  'Single_factor =~ DFSN_1InePro + DFSN_2IneCon + DFSN_3InePro + DFSN_4IneCon +
                    DFSN_5SocPro + DFSN_6SocCon + DFSN_7SocPro + DFSN_8SocCon + DFSN_9SocPro + DFSN_10SocCon'
cfamodel2 <- cfa(cfamodel2, data = d, estimator = "DWLS")
sum2 <- summary(cfamodel2,fit.measures = TRUE)
sum2fits <- cbind.data.frame(CFI = sum2$fit[9], RMSEA = sum2$fit[11], SRMR = sum2$fit[19])

#Nation pro-trait and con-trait wordings
cfamodel3 <- 
  'Pro_trait =~ DFSN_1InePro + DFSN_3InePro + DFSN_5SocPro + DFSN_7SocPro + DFSN_9SocPro
   Con_trait =~ DFSN_2IneCon + DFSN_4IneCon + DFSN_6SocCon + DFSN_8SocCon + DFSN_10SocCon'
cfamodel3 <- cfa(cfamodel3, data = d, estimator = "DWLS")
sum3 <- summary(cfamodel3,fit.measures = TRUE)
sum3fits <- cbind.data.frame(CFI = sum3$fit[9], RMSEA = sum3$fit[11], SRMR = sum3$fit[19])

#Table of DFS item loadings
Dimension <- c(rep_len("Inequality", 4), rep_len("Social Control", 6))
Wording <- c("When some Americans get a lot of resources, they do not share any of their resources with other Americans.",
             "When some Americans get a lot of resources, they share all of these resources with other Americans. [Item reversed]",
             "Some Americans do not get the resources that other Americans have, which means that some Americans have less than others.",
             "Some Americans try to make sure that no Americans get fewer resources than other Americans. [Item reversed]",
             
             "All Americans have to follow all of the USA’s rules all of the time.",
             "An American does not follow some of the USA's rules because they do not agree with them. [Item reversed]",
             "The USA punishes an American very harshly, because they have repeatedly broken the USA’s rules.",
             "The USA does not punish an American after they have broken a US rule which you don't think is important. [Item reversed]",
             "Some Americans don’t agree with a decision made by the USA’s leaders, but they have to follow the leaders’ decision anyway.",
             "An American does not follow the decision of the USA’s leaders, because they don't agree with the leaders' decision. [Item reversed]")
Pvalues <- s.sol1$p %>%
  ifelse(. < 0.001, "<0.001", .)
Issues <- cbind.data.frame(Dimension, Wording, 
                           Loading = s.sol1$Loading,
                           "CI (95%)" = s.sol1$CI, 
                           "Z score" = s.sol1$Z, 
                           "p value" = Pvalues) 
kable(Issues,
      format = 'latex',
      digits = 3,
      booktabs = TRUE,
      align = 'l') %>%
  add_header_above(c("Standardised item loadings for the DFS's two dimensions" = 6),
                   align = 'l', italic = TRUE) %>%
  add_header_above(c("Table 1" = 6), align = 'l') %>%
  collapse_rows(columns = 1:2, valign = "top", latex_hline = "major") %>%
  pack_rows("", 1, 4) %>%
  pack_rows("", 5, 10) %>%
  kable_styling(bootstrap_options = "condensed", latex_options="scale_down") %>%
  save_kable("Table 1.png", density = 1000)

#Table of fit indices
sumAll <- rbind('DFS: Inequality and Social Control dimensions' = sum1fits,
                'DFS: Single dimension' = sum2fits,
                'DFS: Pro-trait and con-trait dimensions' = sum3fits) %>%
  rownames_to_column() %>%
  rename(Model = rowname) %>%
  mutate_if(is.numeric, round, digits=3)
kable(sumAll,
      format = 'latex',
      digits = 3,
      booktabs = TRUE,
      align = 'l') %>%
  add_header_above(c("Fit indices for models using the DFS data" = 4),
                   align = 'l', italic = TRUE) %>%
  add_header_above(c("Table 2" = 4), align = 'l') %>%
  collapse_rows(columns = 1:1, valign = "top", latex_hline = "linespace") %>%
  kable_styling(bootstrap_options = "condensed", latex_options = "scale_down") %>%
  footnote(general = "CFI is the Comparative Fit Index, RMSEA is the Root Mean Square Error of Approximation, and SRMR is the Standardised Root Mean Residual",
           footnote_as_chunk = T, threeparttable = T) %>%
  save_kable("Table 2.png")

#Correlation between dimensions
standardizedsolution(cfamodel1) %>%
  filter(op == "~~") %>% 
  filter(lhs == "Inequality") %>% 
  filter(rhs == "Social_control")


# Tables 3 and 4: Factor analyses of attitudes ----------------------------------------

#Two dimensions
cfamodel1 <- 
  'Economic =~ Issue1_TaxHelpPoor + Issue2_TaxAdresInqty + Issue3_TaxHealthcare + Issue4_GovDependence + Issue5_GovControl + 
                Issue6_LimitedGov + Issue7_CompetCaptlsm + Issue8_HomelessRespo + Issue9_GlobeWarm + Issue10_AffirmActn 
   Social =~ Issue11_MarriageHetr + Issue12_GayAdoption + Issue13_Euthanasia + Issue14_AbrtnBetter + Issue15_NoReligTrust +
             Issue16_RelignInGov + Issue17_MascClothing + Issue18_WomensPlace + Issue19_CrckdwnDrugs + Issue20_IntgrtLifsty'
cfamodel1 <- cfa(cfamodel1, data = d, estimator = "DWLS")
sum1 <- summary(cfamodel1,fit.measures = TRUE)
sum1fits <- cbind.data.frame(CFI = sum1$fit[9], RMSEA = sum1$fit[11], SRMR = sum1$fit[19])
s.sol1 <- standardizedsolution(cfamodel1) %>%
  filter(op == "=~") %>% 
  select(Factor=lhs, Issue=rhs, Loading=est.std, CI_low=ci.lower, CI_up=ci.upper, Z=z, p=pvalue)  %>% 
  mutate_if(is.numeric, round, digits=3)
s.sol1$CI <- paste(s.sol1$CI_low, s.sol1$CI_up, sep=", ")
s.sol1 <- subset(s.sol1, select=-c(CI_low, CI_up))
s.sol1 <- s.sol1[, c(1, 2, 3, 6, 4, 5)]

#One dimension
cfamodel2 <- 
  'Conservatism =~ Issue1_TaxHelpPoor + Issue2_TaxAdresInqty + Issue3_TaxHealthcare + Issue4_GovDependence + Issue5_GovControl + 
                Issue6_LimitedGov + Issue7_CompetCaptlsm + Issue8_HomelessRespo + Issue9_GlobeWarm + Issue10_AffirmActn + 
                Issue11_MarriageHetr + Issue12_GayAdoption + Issue13_Euthanasia + Issue14_AbrtnBetter + Issue15_NoReligTrust +
                Issue16_RelignInGov + Issue17_MascClothing + Issue18_WomensPlace + Issue19_CrckdwnDrugs + Issue20_IntgrtLifsty'
cfamodel2 <- cfa(cfamodel2, data = d, estimator = "DWLS")
sum2 <- summary(cfamodel2, fit.measures = TRUE)
sum2fits <- cbind.data.frame(CFI = sum2$fit[9], RMSEA = sum2$fit[11], SRMR = sum2$fit[19])

#Pro- and con-trait dimensions
cfamodel3 <- 
  'Pro_trait =~ Issue3_TaxHealthcare + Issue4_GovDependence + Issue6_LimitedGov + Issue7_CompetCaptlsm + Issue8_HomelessRespo +
                Issue11_MarriageHetr + Issue12_GayAdoption + Issue13_Euthanasia + Issue15_NoReligTrust + Issue17_MascClothing + Issue18_WomensPlace + Issue19_CrckdwnDrugs + Issue20_IntgrtLifsty
   Con_trait =~ Issue1_TaxHelpPoor + Issue2_TaxAdresInqty + Issue5_GovControl + Issue9_GlobeWarm + Issue10_AffirmActn +
                Issue14_AbrtnBetter + Issue16_RelignInGov'
cfamodel3 <- cfa(cfamodel3, data = d, estimator = "DWLS")
sum3 <- summary(cfamodel3,fit.measures = TRUE)
sum3fits <- cbind.data.frame(CFI = sum3$fit[9], RMSEA = sum3$fit[11], SRMR = sum3$fit[19])

#Table of fit indices
sumAll <- rbind('Issues: Economic and social dimensions' = sum1fits,
                'Issues: Single conservatism dimension' = sum2fits,
                'Issues: Pro-trait and con-trait dimensions' = sum3fits) %>%
  rownames_to_column() %>%
  rename(Model = rowname) %>%
  mutate_if(is.numeric, round, digits=3)
kable(sumAll,
      format = 'latex',
      digits = 3,
      booktabs = TRUE,
      align = 'l') %>%
  add_header_above(c("Fit indices for models of issues in US society and politics" = 4),
                   align = 'l', italic = TRUE) %>%
  add_header_above(c("Table 3" = 4), align = 'l') %>%
  collapse_rows(columns = 1:1, valign = "top", latex_hline = "linespace") %>%
  kable_styling(bootstrap_options = "condensed", latex_options = "scale_down") %>%
  footnote(general = "CFI is the Comparative Fit Index, RMSEA is the Root Mean Square Error of Approximation, and SRMR is the Standardised Root Mean Residual",
           footnote_as_chunk = T, threeparttable = T) %>%
  save_kable("Table 3.png")

#Table of issue loadings
Dimension <- c(rep_len("Economic", 10), rep_len("Social", 10))
Wording <- c("The government should help the poor and needy using tax dollars from the rich. [Item reversed]",
             "Higher taxes (primarily for the wealthy) are necessary to address inequity/injustice in society. [Item reversed]",
             "I dislike the idea of tax dollars funding other people’s health care.",
             "Government programs encourage people to become dependent and lazy, rather than encouraging work and independence.",
             "Gas and electric companies should be under governmental control. [Item reversed]",
             "A smaller government with limited power would improve the standard of living for all.",
             "Competitive capitalism produces economic growth, more jobs and the highest standards of living.",
             "The homeless are responsible for their own problems.",
             "Global warming presents a threat to the future of humans. [Item reversed]",
             "I support “affirmative action” policies in workplaces and schools. [Item reversed]",
             
             "Marriage is meant to be the union of one man and one woman only.",
             "I fear that homosexual people adopting children will result in there being more homosexual people in America over time.",
             "Euthanasia of a human is wrong under all circumstances.",
             "It is better for a woman to have an abortion than to raise a child she does not want. [Item reversed]",
             "I find it hard to trust people who are not religious.",
             "Religious expression has no place in government. [Item reversed]",
             "Men should wear masculine clothing.",
             "Women should stay at home and parent children, and men should work to Sup port their families.",
             "The government must crack down on people who produce or distribute illegal drugs.",
             "People who immigrate to America from other countries should adopt an American way of life, rather than living as they did in the countries they came from.")
Pvalues <- s.sol1$p %>%
  ifelse(. < 0.001, "<0.001", .)
Issues <- cbind.data.frame(Dimension, Wording, 
                           Loading = s.sol1$Loading,
                           "CI (95%)" = s.sol1$CI, 
                           "Z score" = s.sol1$Z, 
                           "p value" = Pvalues) 
kable(Issues,
      format = 'latex',
      digits = 3,
      booktabs = TRUE,
      align = 'l') %>%
  add_header_above(c("Standardised loadings for 20 issues in US politics" = 6),
                   align = 'l', italic = TRUE) %>%
  add_header_above(c("Table 4" = 6), align = 'l') %>%
  collapse_rows(columns = 1:2, valign = "top", latex_hline = "major") %>%
  pack_rows("", 1, 10) %>%
  pack_rows("", 11, 20) %>%
  kable_styling(bootstrap_options = "condensed", latex_options="scale_down") %>%
  save_kable("Table 4.png", density = 1000)


# Demographic variables predicting attitudes --------------------------------

Model1 <- brm(
  Score ~ 1 + Men + Age + Education + Income +
    (1 | ParticipantID) + (1 + Men + Age + Education + Income | Conservatism/Issue),
  data = dLong,
  family = Beta,
  prior = c(prior(normal(0, 0.5), class = b),
            prior(exponential(3), class = sd),
            prior(gamma(1, 0.01), class = phi)),
  control = list(adapt_delta = 0.9999, max_treedepth = 12),
  iter = 20000, cores = 4)
summary(Model1)

#Plot dimension-level estimates
forModel1ForestA <- Model1 %>%
  spread_draws(b_MenNotman, r_Conservatism[Conservatism,par]) %>%
  filter(par == "MenNotman") %>%
  median_qi(condition_mean = b_MenNotman+ r_Conservatism) %>%
  rename(Predictors = par)
forModel1ForestB <- Model1 %>%
  spread_draws(b_Age, r_Conservatism[Conservatism,par]) %>%
  filter(par == "Age") %>%
  median_qi(condition_mean = b_Age + r_Conservatism) %>%
  rename(Predictors = par)
forModel1ForestC <- Model1 %>%
  spread_draws(b_Education, r_Conservatism[Conservatism,par]) %>%
  filter(par == "Education") %>%
  median_qi(condition_mean = b_Education + r_Conservatism) %>%
  rename(Predictors = par)
forModel1ForestD <- Model1 %>%
  spread_draws(b_Income, r_Conservatism[Conservatism,par]) %>%
  filter(par == "Income") %>%
  median_qi(condition_mean = b_Income + r_Conservatism) %>%
  rename(Predictors = par)
forModel1Forest <- rbind(forModel1ForestA, forModel1ForestB, 
                         forModel1ForestC, forModel1ForestD) %>% 
  mutate(across('Predictors', str_replace, 'MenNotman', 'A) Not a man'),
         across('Predictors', str_replace, 'Age', 'B) Age'),
         across('Predictors', str_replace, 'Education', 'C) Education'),
         across('Predictors', str_replace, 'Income', 'D) Income'))
Model1plot <-
  ggplot(forModel1Forest,
         aes(y = Conservatism, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval(position = position_dodge(-0.4)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15, 17, 15)) +
  xlab("") +
  ylab("") +
  scale_y_discrete(limits = rev(c("Economic", "Social")))
ggsave("Extra: Demographic dimensions.png",
       plot = Model1plot, bg = "white", 
       width = 16, height = 13, units = "cm",
       dpi = 300)

#Plot issue-level estimates
forModel1ForestA <- Model1 %>%
  spread_draws(b_MenNotman, r_Conservatism[Conservatism,par], `r_Conservatism:Issue`[Issue,par]) %>%
  filter(par == "MenNotman") %>%
  subset(!(Conservatism %in% "Economic" & str_detect(Issue, "Social_"))) %>%  #Filter only those rows that agree (Econ + Econ, then Soc + Soc)
  subset(!(Conservatism %in% "Social" & str_detect(Issue, "Economic_"))) %>%
  median_qi(condition_mean = b_MenNotman + r_Conservatism + `r_Conservatism:Issue`) %>%
  rename(Predictors = par)
forModel1ForestB <- Model1 %>%
  spread_draws(b_Age, r_Conservatism[Conservatism,par], `r_Conservatism:Issue`[Issue,par]) %>% 
  filter(par == "Age") %>%
  subset(!(Conservatism %in% "Economic" & str_detect(Issue, "Social_"))) %>%  #Filter only those rows that agree (Econ + Econ, then Soc + Soc)
  subset(!(Conservatism %in% "Social" & str_detect(Issue, "Economic_"))) %>%
  median_qi(condition_mean = b_Age + r_Conservatism + `r_Conservatism:Issue`) %>%
  rename(Predictors = par)
forModel1Forestp1 <- rbind(forModel1ForestA, forModel1ForestB) %>% 
  mutate(Issue = gsub('Economic_', '', Issue),
         Issue = gsub('Social_', '', Issue),
         across('Predictors', str_replace, 'MenNotman', 'A) Not a man'),
         across('Predictors', str_replace, 'Age', 'B) Age'))
Model1plotp1 <-
  ggplot(forModel1Forestp1,
         aes(y = Issue, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval(position = position_dodge(-0.65)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15)) +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = c(-0.61, 0.61), breaks=c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)) +
  scale_y_discrete(limits = c("Issue20_IntgrtLifsty", "Issue19_CrckdwnDrugs", "Issue18_WomensPlace", "Issue17_MascClothing", "Issue16_RelignInGov", 
                              "Issue15_NoReligTrust", "Issue14_AbrtnBetter", "Issue13_Euthanasia", "Issue12_GayAdoption", "Issue11_MarriageHetr",
                              "Issue10_AffirmActn", "Issue9_GlobeWarm", "Issue8_HomelessRespo", "Issue7_CompetCaptlsm", "Issue6_LimitedGov", 
                              "Issue5_GovControl", "Issue4_GovDependence", "Issue3_TaxHealthcare", "Issue2_TaxAdresInqty", "Issue1_TaxHelpPoor")) +
  theme(text = element_text(size = 18),
        plot.margin = margin(5.5, 5.5, 0.00001, 5.5, "pt"))
forModel1ForestC <- Model1 %>%
  spread_draws(b_Education, r_Conservatism[Conservatism,par], `r_Conservatism:Issue`[Issue,par]) %>%
  filter(par == "Education") %>%
  subset(!(Conservatism %in% "Economic" & str_detect(Issue, "Social_"))) %>%  #Filter only those rows that agree (Econ + Econ, then Soc + Soc)
  subset(!(Conservatism %in% "Social" & str_detect(Issue, "Economic_"))) %>%
  median_qi(condition_mean = b_Education + r_Conservatism + `r_Conservatism:Issue`) %>%
  rename(Predictors = par)
forModel1ForestD <- Model1 %>%
  spread_draws(b_Income, r_Conservatism[Conservatism,par], `r_Conservatism:Issue`[Issue,par]) %>%
  filter(par == "Income") %>%
  subset(!(Conservatism %in% "Economic" & str_detect(Issue, "Social_"))) %>%  #Filter only those rows that agree (Econ + Econ, then Soc + Soc)
  subset(!(Conservatism %in% "Social" & str_detect(Issue, "Economic_"))) %>%
  median_qi(condition_mean = b_Income + r_Conservatism + `r_Conservatism:Issue`) %>%
  rename(Predictors = par)
forModel1Forestp2 <- rbind(forModel1ForestC, forModel1ForestD) %>% 
  mutate(Issue = gsub('Economic_', '', Issue),
         Issue = gsub('Social_', '', Issue),
         across('Predictors', str_replace, 'Education', 'C) Education'),
         across('Predictors', str_replace, 'Income', 'D) Income'))
Model1plotp2 <-
  ggplot(forModel1Forestp2,
         aes(y = Issue, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval(position = position_dodge(-0.65)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15)) +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = c(-0.61, 0.61), breaks=c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)) +
  scale_y_discrete(limits = c("Issue20_IntgrtLifsty", "Issue19_CrckdwnDrugs", "Issue18_WomensPlace", "Issue17_MascClothing", "Issue16_RelignInGov", 
                              "Issue15_NoReligTrust", "Issue14_AbrtnBetter", "Issue13_Euthanasia", "Issue12_GayAdoption", "Issue11_MarriageHetr",
                              "Issue10_AffirmActn", "Issue9_GlobeWarm", "Issue8_HomelessRespo", "Issue7_CompetCaptlsm", "Issue6_LimitedGov", 
                              "Issue5_GovControl", "Issue4_GovDependence", "Issue3_TaxHealthcare", "Issue2_TaxAdresInqty", "Issue1_TaxHelpPoor")) +
  theme(text = element_text(size = 18),
        plot.margin = margin(0.00001, 5.5, 0.00001, 5.5, "pt"))
Model1plots <- plot_grid(Model1plotp1 + theme(legend.justification = c(0, 0.5)),
                         Model1plotp2 + theme(legend.justification = c(0, 0.5)),
                         align = 'v',
                         ncol = 2,
                         nrow = 1, 
                         labels = c('A', 'B'))
ggsave("Extra: Demographic issues.png",       #Save the joint plot
       plot = Model1plots, bg = "white", 
       width = 20, height = 9,
       dpi = 300)


# Figures 1 and 2: DFS predicting attitudes --------------------------------

Model2 <- brm(
  Score ~ 1 + cfaInequality_nat + cfaSocial_control_nat + Men + Age + Education + Income +
    (1 | ParticipantID) + (1 + cfaInequality_nat + cfaSocial_control_nat + Men + Age + Education + Income | Conservatism/Issue),
  data = dLong,
  family = Beta,
  prior = c(prior(normal(0, 0.5), class = b),
            prior(exponential(3), class = sd),
            prior(gamma(1, 0.01), class = phi)),
  control = list(adapt_delta = 0.9999, max_treedepth = 12),
  iter = 20000, cores = 4)
summary(Model2)

#Plot dimensions
forModel2plotA <- Model2 %>%
  spread_draws(b_cfaInequality_nat, r_Conservatism[Conservatism,par]) %>%
  filter(par == "cfaInequality_nat") %>%
  median_qi(condition_mean = b_cfaInequality_nat + r_Conservatism) %>%
  rename(Predictors = par)
forModel2plotB <- Model2 %>%
  spread_draws(b_cfaSocial_control_nat, r_Conservatism[Conservatism,par]) %>%
  filter(par == "cfaSocial_control_nat") %>%
  median_qi(condition_mean = b_cfaSocial_control_nat + r_Conservatism) %>%
  rename(Predictors = par)
forModel2plot <- rbind(forModel2plotA, forModel2plotB) %>% 
  mutate(across('Predictors', str_replace, 'cfaInequality_nat', 'A) DFS Inequality'),
         across('Predictors', str_replace, 'cfaSocial_control_nat', 'B) DFS Social Control'))
Model2plot <-
  ggplot(forModel2plot,
         aes(y = Conservatism, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval() + 
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15)) +
  xlab("") + ylab("") + 
  xlim(-0.05,0.6) +
  scale_y_discrete(limits = rev(c("Economic", "Social")))
ggsave("Fig 1: DFS and issues.png", 
       plot = Model2plot, 
       width = 9, height = 4, 
       dpi = 300)

#Compare coefficients for DFS dimensions on social conservatism
forModel2plotA <- Model2 %>%
  spread_draws(b_cfaInequality_nat, r_Conservatism[Conservatism,par]) %>%
  filter(par == "cfaInequality_nat" & Conservatism == "Social") %>%
  mutate(b_cfaInequality_nat = b_cfaInequality_nat + r_Conservatism)
forModel2plotB <- Model2 %>%
  spread_draws(b_cfaSocial_control_nat, r_Conservatism[Conservatism,par]) %>%
  filter(par == "cfaSocial_control_nat" & Conservatism == "Social") %>%
  mutate(b_cfaSocial_control_nat = b_cfaSocial_control_nat + r_Conservatism)
#See overlap of distribution
overlap(forModel2plotA$b_cfaInequality_nat, forModel2plotB$b_cfaSocial_control_nat)
plot(overlap(forModel2plotA$b_cfaInequality_nat, forModel2plotB$b_cfaSocial_control_nat))
#Calculate and plot difference between the distributions
postDiff <- forModel2plotA$b_cfaInequality_nat - forModel2plotB$b_cfaSocial_control_nat %>%
  as.data.frame()
ggplot(postDiff, aes(.)) + 
  geom_histogram(bins = 100, color = "black", fill = "lightgray")
quantile(postDiff, c(0.025, 0.975)) #So 95% confidence interval of the difference lies above 0

#Plot issues
forModel2plotA <- Model2 %>%
  spread_draws(b_cfaInequality_nat, r_Conservatism[Conservatism,par], `r_Conservatism:Issue`[Issue,par]) %>%
  filter(par == "cfaInequality_nat") %>%
  subset(!(Conservatism %in% "Economic" & str_detect(Issue, "Social_"))) %>%  #Filter only those rows that agree (Econ + Econ, then Soc + Soc)
  subset(!(Conservatism %in% "Social" & str_detect(Issue, "Economic_"))) %>%
  median_qi(condition_mean = b_cfaInequality_nat + r_Conservatism + `r_Conservatism:Issue`) %>%
  rename(Predictors = par)
forModel2plotB <- Model2 %>%
  spread_draws(`b_cfaSocial_control_nat`, r_Conservatism[Conservatism,par], `r_Conservatism:Issue`[Issue,par]) %>%
  filter(par == "cfaSocial_control_nat") %>%
  subset(!(Conservatism %in% "Economic" & str_detect(Issue, "Social_"))) %>%  #Filter only those rows that agree (Econ + Econ, then Soc + Soc)
  subset(!(Conservatism %in% "Social" & str_detect(Issue, "Economic_"))) %>%
  median_qi(condition_mean = `b_cfaSocial_control_nat` + r_Conservatism + `r_Conservatism:Issue`) %>%
  rename(Predictors = par)
forModel2plot <- rbind(forModel2plotA, forModel2plotB) %>% 
  mutate(across('Issue', str_replace, 'Economic_Issue1_TaxHelpPoor', 'Tax to help poor (reversed)'),
         across('Issue', str_replace, 'Economic_Issue2_TaxAdresInqty', 'Tax to address inequality (reversed)'),
         across('Issue', str_replace, 'Economic_Issue3_TaxHealthcare', 'Dislike taxation for healthcare'),
         across('Issue', str_replace, 'Economic_Issue4_GovDependence', 'Govnmt creates dependence'),
         across('Issue', str_replace, 'Economic_Issue5_GovControl', 'Govnmt ownership of energy (reversed)'),
         across('Issue', str_replace, 'Economic_Issue6_LimitedGov', 'Smaller government'),
         across('Issue', str_replace, 'Economic_Issue7_CompetCaptlsm', 'Competitive capitalism is good'),
         across('Issue', str_replace, 'Economic_Issue8_HomelessRespo', 'Homeless responsible for their problems'),
         across('Issue', str_replace, 'Economic_Issue9_GlobeWarm', 'Global warming a threat (reversed)'),
         across('Issue', str_replace, 'Economic_Issue10_AffirmActn', 'Affirmative action (reversed)'),
         across('Issue', str_replace, 'Social_Issue11_MarriageHetr', 'Only hetero marriage'),
         across('Issue', str_replace, 'Social_Issue12_GayAdoption', 'Against gay adoption'),
         across('Issue', str_replace, 'Social_Issue13_Euthanasia', 'Euthanasia is wrong'),
         across('Issue', str_replace, 'Social_Issue14_AbrtnBetter', 'Pro-abortion (reversed)'),
         across('Issue', str_replace, 'Social_Issue15_NoReligTrust', 'Only trust religious people'),
         across('Issue', str_replace, 'Social_Issue16_RelignInGov', 'Separate religion and govnmt (reversed)'),
         across('Issue', str_replace, 'Social_Issue17_MascClothing', 'Men should wear masculine clothes'),
         across('Issue', str_replace, 'Social_Issue18_WomensPlace', "Women's place is in the home"),
         across('Issue', str_replace, 'Social_Issue19_CrckdwnDrugs', 'Crackdown on drugs'),
         across('Issue', str_replace, 'Social_Issue20_IntgrtLifsty', 'Migrants should assimilate'),
         across('Predictors', str_replace, 'cfaInequality_nat', 'A) DFS Inequality'),
         across('Predictors', str_replace, 'cfaSocial_control_nat', 'B) DFS Social Control'))
forModel2plotA <- forModel2plot %>%    #For plot of economic issues
  slice(1:10, 21:30)
forModel2plotB <- forModel2plot %>%    #For plot of social issues
  slice(11:20, 31:40)
Model2plotA <-  #Economic plot
  ggplot(forModel2plotA,
         aes(y = Issue, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval() + 
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15)) + 
  scale_color_manual(values = c("#F8766D", "#00BFC4")) + 
  scale_x_continuous(limits = c(-0.2, 1.15), breaks=c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2)) +
  labs(y = NULL, x = NULL) +
  scale_y_discrete(limits = c("Affirmative action (reversed)", "Global warming a threat (reversed)", "Homeless responsible for their problems", 
                              "Competitive capitalism is good", "Smaller government", 
                              "Govnmt ownership of energy (reversed)", "Govnmt creates dependence", "Dislike taxation for healthcare", 
                              "Tax to address inequality (reversed)", "Tax to help poor (reversed)")) +
  theme(text = element_text(size = 15))
Model2plotB <-  #Social plot
  ggplot(forModel2plotB,
         aes(y = Issue, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval() + 
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15)) + 
  scale_color_manual(values = c("#F8766D", "#00BFC4")) + 
  scale_x_continuous(limits = c(-0.2, 1.15), breaks=c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2)) +
  labs(y = NULL, x = NULL) +
  scale_y_discrete(limits = c("Migrants should assimilate", "Crackdown on drugs", "Women's place is in the home", 
                              "Men should wear masculine clothes", "Separate religion and govnmt (reversed)", 
                              "Only trust religious people", "Pro-abortion (reversed)", "Euthanasia is wrong", 
                              "Against gay adoption", "Only hetero marriage")) +
  theme(text = element_text(size = 15))
Model2plot <- plot_grid(Model2plotA + theme(legend.position="none"),     #Combine the plots into one joint plot
                        Model2plotB + theme(legend.position="none"),
                        align = 'v',
                        nrow = 2, ncol = 1, 
                        labels = c('A', 'B'))
legend <- get_legend(                #Create legend for both plots
  Model2plotA + theme(legend.box.margin = margin(0, 0, 0, 12)))
Model2plot <- plot_grid(Model2plot, legend,      #Attach legend to the joint plot
                        rel_heights = c(4, 0.5),
                        rel_widths = c(1.7, 0.3))   #rel_widths is width of plot
ggsave("Fig 2: DFS and issues.png",       #Save the joint plot
       plot = Model2plot, bg = "white", 
       width = 15, height = 9, 
       dpi = 300)

# Figures 3 and 4: DFS predicting attitudes with Religion --------------------------------

Model3 <- brm(
  Score ~ 1 + cfaInequality_nat + cfaSocial_control_nat + Religion + Men + Age + Education + Income +
    (1 | ParticipantID) + (1 + cfaInequality_nat + cfaSocial_control_nat + Religion + Men + Age + Education + Income | Conservatism/Issue),
  data = dLong,
  family = Beta,
  prior = c(prior(normal(0, 0.5), class = b),
            prior(exponential(3), class = sd),
            prior(gamma(1, 0.01), class = phi)),
  control = list(adapt_delta = 0.9999, max_treedepth = 12),
  iter = 20000, cores = 4)
summary(Model3)

#Plot pooled estimates with religion
forModel3plotA <- Model3 %>%
  spread_draws(b_cfaInequality_nat, r_Conservatism[Conservatism,par]) %>%
  filter(par == "cfaInequality_nat") %>%
  median_qi(condition_mean = b_cfaInequality_nat + r_Conservatism) %>%
  rename(Predictors = par)
forModel3plotB <- Model3 %>%
  spread_draws(b_cfaSocial_control_nat, r_Conservatism[Conservatism,par]) %>%
  filter(par == "cfaSocial_control_nat") %>%
  median_qi(condition_mean = b_cfaSocial_control_nat + r_Conservatism) %>%
  rename(Predictors = par)
forModel3plotC <- Model3 %>%
  spread_draws(b_ReligionReligious, r_Conservatism[Conservatism,par]) %>%
  filter(par == "ReligionReligious") %>%
  median_qi(condition_mean = b_ReligionReligious + r_Conservatism) %>%
  rename(Predictors = par)
forModel3plot <- rbind(forModel3plotA, forModel3plotB, forModel3plotC) %>% 
  mutate(across('Predictors', str_replace, 'cfaInequality_nat', 'A) DFS Inequality'),
         across('Predictors', str_replace, 'cfaSocial_control_nat', 'B) DFS Social Control'),
         across('Predictors', str_replace, 'ReligionReligious', 'C) Religion'))
Model3plot <-
  ggplot(forModel3plot,
         aes(y = Conservatism, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval(position = position_dodge(-0.15)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15, 16)) + 
  scale_color_manual(values = c("#F8766D", "#00BFC4", "#7CAE00")) + 
  xlab("") + ylab("") + 
  xlim(-0.2,0.8) +
  scale_y_discrete(limits = rev(c("Economic", "Social")))
ggsave("Fig 3: DFS and dimensions.png", 
       plot = Model3plot, 
       width = 9, height = 4, 
       dpi = 300)

#Plot issue-level estimates with religion
forModel3plotA <- Model3 %>%
  spread_draws(b_cfaInequality_nat, r_Conservatism[Conservatism,par], `r_Conservatism:Issue`[Issue,par]) %>%
  filter(par == "cfaInequality_nat") %>%
  subset(!(Conservatism %in% "Economic" & str_detect(Issue, "Social_"))) %>%  #Filter only those rows that agree (Econ + Econ, then Soc + Soc)
  subset(!(Conservatism %in% "Social" & str_detect(Issue, "Economic_"))) %>%
  median_qi(condition_mean = b_cfaInequality_nat + r_Conservatism + `r_Conservatism:Issue`) %>%
  rename(Predictors = par)
forModel3plotB <- Model3 %>%
  spread_draws(`b_cfaSocial_control_nat`, r_Conservatism[Conservatism,par], `r_Conservatism:Issue`[Issue,par]) %>%
  filter(par == "cfaSocial_control_nat") %>%
  subset(!(Conservatism %in% "Economic" & str_detect(Issue, "Social_"))) %>%  #Filter only those rows that agree (Econ + Econ, then Soc + Soc)
  subset(!(Conservatism %in% "Social" & str_detect(Issue, "Economic_"))) %>%
  median_qi(condition_mean = `b_cfaSocial_control_nat` + r_Conservatism + `r_Conservatism:Issue`) %>%
  rename(Predictors = par)
forModel3plotC <- Model3 %>%
  spread_draws(`b_ReligionReligious`, r_Conservatism[Conservatism,par], `r_Conservatism:Issue`[Issue,par]) %>%
  filter(par == "ReligionReligious") %>%
  subset(!(Conservatism %in% "Economic" & str_detect(Issue, "Social_"))) %>%  #Filter only those rows that agree (Econ + Econ, then Soc + Soc)
  subset(!(Conservatism %in% "Social" & str_detect(Issue, "Economic_"))) %>%
  median_qi(condition_mean = `b_ReligionReligious` + r_Conservatism + `r_Conservatism:Issue`) %>%
  rename(Predictors = par)
forModel3plot <- rbind(forModel3plotA, forModel3plotB, forModel3plotC) %>% 
  mutate(across('Issue', str_replace, 'Economic_Issue1_TaxHelpPoor', 'Tax to help poor (reversed)'),
         across('Issue', str_replace, 'Economic_Issue2_TaxAdresInqty', 'Tax to address inequality (reversed)'),
         across('Issue', str_replace, 'Economic_Issue3_TaxHealthcare', 'Dislike taxation for healthcare'),
         across('Issue', str_replace, 'Economic_Issue4_GovDependence', 'Govnmt creates dependence'),
         across('Issue', str_replace, 'Economic_Issue5_GovControl', 'Govnmt ownership of energy (reversed)'),
         across('Issue', str_replace, 'Economic_Issue6_LimitedGov', 'Smaller government'),
         across('Issue', str_replace, 'Economic_Issue7_CompetCaptlsm', 'Competitive capitalism is good'),
         across('Issue', str_replace, 'Economic_Issue8_HomelessRespo', 'Homeless responsible for their problems'),
         across('Issue', str_replace, 'Economic_Issue9_GlobeWarm', 'Global warming a threat (reversed)'),
         across('Issue', str_replace, 'Economic_Issue10_AffirmActn', 'Affirmative action (reversed)'),
         across('Issue', str_replace, 'Social_Issue11_MarriageHetr', 'Only hetero marriage'),
         across('Issue', str_replace, 'Social_Issue12_GayAdoption', 'Against gay adoption'),
         across('Issue', str_replace, 'Social_Issue13_Euthanasia', 'Euthanasia is wrong'),
         across('Issue', str_replace, 'Social_Issue14_AbrtnBetter', 'Pro-abortion (reversed)'),
         across('Issue', str_replace, 'Social_Issue15_NoReligTrust', 'Only trust religious people'),
         across('Issue', str_replace, 'Social_Issue16_RelignInGov', 'Separate religion and govnmt (reversed)'),
         across('Issue', str_replace, 'Social_Issue17_MascClothing', 'Men should wear masculine clothes'),
         across('Issue', str_replace, 'Social_Issue18_WomensPlace', "Women's place is in the home"),
         across('Issue', str_replace, 'Social_Issue19_CrckdwnDrugs', 'Crackdown on drugs'),
         across('Issue', str_replace, 'Social_Issue20_IntgrtLifsty', 'Migrants should assimilate'),
         across('Predictors', str_replace, 'cfaInequality_nat', 'A) DFS Inequality'),
         across('Predictors', str_replace, 'cfaSocial_control_nat', 'B) DFS Social Control'),
         across('Predictors', str_replace, 'ReligionReligious', 'C) Religion'))
forModel3plotA <- forModel3plot %>%
  slice(1:10, 21:30, 41:50)
forModel3plotB <- forModel3plot %>%
  slice(11:20, 31:40, 51:60)
Model3plotA <-
  ggplot(forModel3plotA,
         aes(y = Issue, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval(position = position_dodge(-0.45)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15, 16)) + 
  scale_color_manual(values = c("#F8766D", "#00BFC4", "#7CAE00")) + 
  scale_x_continuous(limits = c(-0.2, 1.15), breaks=c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2)) +
  labs(y = NULL, x = NULL) +
  scale_y_discrete(limits = c("Affirmative action (reversed)", "Global warming a threat (reversed)", "Homeless responsible for their problems", 
                              "Competitive capitalism is good", "Smaller government", 
                              "Govnmt ownership of energy (reversed)", "Govnmt creates dependence", "Dislike taxation for healthcare", 
                              "Tax to address inequality (reversed)", "Tax to help poor (reversed)")) +
  theme(text = element_text(size = 15))
Model3plotB <-
  ggplot(forModel3plotB,
         aes(y = Issue, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval(position = position_dodge(-0.45)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15, 16)) + 
  scale_color_manual(values = c("#F8766D", "#00BFC4", "#7CAE00")) + 
  scale_x_continuous(limits = c(-0.2, 1.15), breaks=c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2)) +
  labs(y = NULL, x = NULL) +
  scale_y_discrete(limits = c("Migrants should assimilate", "Crackdown on drugs", "Women's place is in the home", 
                              "Men should wear masculine clothes", "Separate religion and govnmt (reversed)", 
                              "Only trust religious people", "Pro-abortion (reversed)", "Euthanasia is wrong", 
                              "Against gay adoption", "Only hetero marriage")) +
  theme(text = element_text(size = 15))
Model3plots <- plot_grid(Model3plotA + theme(legend.position="none"),     #Combine the plots into one joint plot
                         Model3plotB + theme(legend.position="none"),
                         align = 'v',
                         nrow = 2, ncol = 1, 
                         labels = c('A', 'B'))
legend <- get_legend(
  Model3plotA + theme(legend.box.margin = margin(0, 0, 0, 12)))
Model3plots <- plot_grid(Model3plots, legend,
                         rel_heights = c(4, 0.5),
                         rel_widths = c(1.7, 0.3))
ggsave("Fig 4: DFS and issues.png",
       plot = Model3plots, bg = "white", 
       width = 15, height = 9, 
       dpi = 300)


# Supplementary Figure 1: Interaction of Inequality and Social Control ---------------------------

Model5 <- brm(
  Score ~ 1 + cfaInequality_nat + cfaSocial_control_nat + cfaSocial_control_nat:cfaInequality_nat + Religion + Men + Age + Education + Income +
    (1 | ParticipantID) + (1 + cfaInequality_nat + cfaSocial_control_nat + cfaSocial_control_nat:cfaInequality_nat + Religion + Men + Age + Education + Income | Conservatism/Issue),
  data = dLong,
  family = Beta,
  prior = c(prior(normal(0, 0.5), class = b),
            prior(exponential(3), class = sd),
            prior(gamma(1, 0.01), class = phi)),
  control = list(adapt_delta = 0.9999, max_treedepth = 12),
  iter = 20000, cores = 4)
summary(Model5)

#Plot issue-level estimates of the interaction
forModel5plot <- Model5 %>%
  spread_draws(`b_cfaInequality_nat:cfaSocial_control_nat`, r_Conservatism[Conservatism,par], `r_Conservatism:Issue`[Issue,par]) %>%
  filter(par == "cfaInequality_nat:cfaSocial_control_nat") %>%
  subset(!(Conservatism %in% "Economic" & str_detect(Issue, "Social_"))) %>%  #Filter only those rows that agree (Econ + Econ, then Soc + Soc)
  subset(!(Conservatism %in% "Social" & str_detect(Issue, "Economic_"))) %>%
  median_qi(condition_mean = `b_cfaInequality_nat:cfaSocial_control_nat` + r_Conservatism + `r_Conservatism:Issue`) %>%
  rename(Predictor = par) %>% 
  mutate(across('Issue', str_replace, 'Economic_Issue1_TaxHelpPoor', 'Tax to help poor (reversed)'),
         across('Issue', str_replace, 'Economic_Issue2_TaxAdresInqty', 'Tax to address inequality (reversed)'),
         across('Issue', str_replace, 'Economic_Issue3_TaxHealthcare', 'Dislike taxation for healthcare'),
         across('Issue', str_replace, 'Economic_Issue4_GovDependence', 'Govnmt creates dependence'),
         across('Issue', str_replace, 'Economic_Issue5_GovControl', 'Govnmt ownership of energy (reversed)'),
         across('Issue', str_replace, 'Economic_Issue6_LimitedGov', 'Smaller government'),
         across('Issue', str_replace, 'Economic_Issue7_CompetCaptlsm', 'Competitive capitalism is good'),
         across('Issue', str_replace, 'Economic_Issue8_HomelessRespo', 'Homeless responsible for their problems'),
         across('Issue', str_replace, 'Economic_Issue9_GlobeWarm', 'Global warming a threat (reversed)'),
         across('Issue', str_replace, 'Economic_Issue10_AffirmActn', 'Affirmative action (reversed)'),
         across('Issue', str_replace, 'Social_Issue11_MarriageHetr', 'Only hetero marriage'),
         across('Issue', str_replace, 'Social_Issue12_GayAdoption', 'Against gay adoption'),
         across('Issue', str_replace, 'Social_Issue13_Euthanasia', 'Euthanasia is wrong'),
         across('Issue', str_replace, 'Social_Issue14_AbrtnBetter', 'Pro-abortion (reversed)'),
         across('Issue', str_replace, 'Social_Issue15_NoReligTrust', 'Only trust religious people'),
         across('Issue', str_replace, 'Social_Issue16_RelignInGov', 'Separate religion and govnmt (reversed)'),
         across('Issue', str_replace, 'Social_Issue17_MascClothing', 'Men should wear masculine clothes'),
         across('Issue', str_replace, 'Social_Issue18_WomensPlace', "Women's place is in the home"),
         across('Issue', str_replace, 'Social_Issue19_CrckdwnDrugs', 'Crackdown on drugs'),
         across('Issue', str_replace, 'Social_Issue20_IntgrtLifsty', 'Migrants should assimilate'),
         across('Predictor', str_replace, 'cfaInequality_nat:cfaSocial_control_nat', 'Interaction of DFS Inequality and Social Control'))
forModel5plotA <- forModel5plot %>% 
  slice(1:10, 21:30, 41:50)
forModel5plotB <- forModel5plot %>% 
  slice(11:20, 31:40, 51:60)
Model5plotA <-
  ggplot(forModel5plotA,
         aes(y = Issue, x = condition_mean,
             color = Predictor,
             shape = Predictor,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval() + 
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17)) + 
  scale_color_manual(values = c("#F8766D")) + 
  labs(y = NULL, x = NULL) +
  scale_y_discrete(limits = c("Affirmative action (reversed)", "Global warming a threat (reversed)", "Homeless responsible for their problems", 
                              "Competitive capitalism is good", "Smaller government", 
                              "Govnmt ownership of energy (reversed)", "Govnmt creates dependence", "Dislike taxation for healthcare", 
                              "Tax to address inequality (reversed)", "Tax to help poor (reversed)")) +
  theme(text = element_text(size = 13))
Model5plotB <-
  ggplot(forModel5plotB,
         aes(y = Issue, x = condition_mean,
             color = Predictor,
             shape = Predictor,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval() + 
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17)) + 
  scale_color_manual(values = c("#F8766D")) + 
  labs(y = NULL, x = NULL) +
  scale_y_discrete(limits = c("Migrants should assimilate", "Crackdown on drugs", "Women's place is in the home", 
                              "Men should wear masculine clothes", "Separate religion and govnmt (reversed)", 
                              "Only trust religious people", "Pro-abortion (reversed)", "Euthanasia is wrong", 
                              "Against gay adoption", "Only hetero marriage")) +
  theme(text = element_text(size = 13))
Model5plots <- plot_grid(Model5plotA + theme(legend.position="none"),
                         Model5plotB + theme(legend.position="none"),
                         align = 'h',
                         nrow = 1, ncol = 2, 
                         labels = c('A', 'B'))
legend <- get_legend( 
  Model5plotA + theme(legend.box.margin = margin(0, 0, 0, 12)))
Model5plots <- plot_grid(Model5plots, legend,
                         nrow = 2, ncol = 1,
                         rel_heights = c(4, 0.5),
                         rel_widths = c(1.6, 0.4))
ggsave("Fig Sup 1: DFS Ine-Soc interaction estimates.png",
       plot = Model5plots, bg = "white", 
       width = 15, height = 6, 
       dpi = 300)

#Plot pooled estimates
forModel5plot <- Model5 %>%
  spread_draws(`b_cfaInequality_nat:cfaSocial_control_nat`, r_Conservatism[Conservatism,par]) %>%
  filter(par == "cfaInequality_nat:cfaSocial_control_nat") %>%
  median_qi(condition_mean = `b_cfaInequality_nat:cfaSocial_control_nat` + r_Conservatism) %>%
  rename(Predictor = par) %>% 
  mutate(across('Predictor', str_replace, 'cfaInequality_nat:cfaSocial_control_nat', 'Interaction of DFS Inequality and Social Control'))
Model5plot <-
  ggplot(forModel5plot,
         aes(y = Conservatism, x = condition_mean,
             color = Predictor,
             shape = Predictor,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval() + 
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15)) +
  xlab("") + ylab("") + 
  xlim(-0.1,0.6) +
  scale_y_discrete(limits = rev(c("Economic", "Social")))
ggsave("Extra: Ine-Soc pooled.png", 
       plot = Model5plot, 
       width = 12, height = 4, 
       dpi = 300)

#Plot all interactions
Issues <- data.frame(Issue = unique(dLong$Issue))  #Create list of issues
Issues$Conservatism <- c(rep("Economic", length.out = 10), rep("Social", length.out = 10))  #Index if economic or social
condIssues <- conditional_effects(Model5,  #Get conditional effects
                                  conditions = Issues, 
                                  effects = "cfaSocial_control_nat:cfaInequality_nat",
                                  re_formula = . ~ 1 + (1 + cfaInequality_nat + cfaSocial_control_nat + cfaSocial_control_nat:cfaInequality_nat + Religion + Men + Age + Education + Income | Conservatism/Issue))
Interaction_effects <- plot(condIssues, plot = FALSE)[[1]] +
  labs(x = "DFS Social Control depending on Inequality",
       y = "Economic Issues (#1-10) and Social Issues (#11-20)")
ggsave("Extra: DFS Soc-Ine interactions.png", 
       Interaction_effects, 
       width = 13, height = 6, 
       dpi = 350)


# Figure 5: Interactions with Religion -------------------------------------------------------------------------

Model4 <- brm(
  Score ~ 1 + cfaInequality_nat + cfaSocial_control_nat + cfaInequality_nat:Religion + cfaSocial_control_nat:Religion + Religion + Men + Age + Education + Income +
    (1 | ParticipantID) + (1 + cfaInequality_nat + cfaSocial_control_nat + cfaInequality_nat:Religion + cfaSocial_control_nat:Religion + Religion + Men + Age + Education + Income | Conservatism/Issue),
  data = dLong,
  family = Beta,
  prior = c(prior(normal(0, 0.5), class = b),
            prior(exponential(3), class = sd),
            prior(gamma(1, 0.01), class = phi)),
  control = list(adapt_delta = 0.9999, max_treedepth = 12),
  iter = 20000, cores = 4)

#Plot issue-level estimates for the interactions (Supplementary Figure 2)
forModel4plotA <- Model4 %>%
  spread_draws(`b_cfaInequality_nat:ReligionReligious`, r_Conservatism[Conservatism,par], `r_Conservatism:Issue`[Issue,par]) %>%
  filter(par == "cfaInequality_nat:ReligionReligious") %>%
  subset(!(Conservatism %in% "Economic" & str_detect(Issue, "Social_"))) %>%  #Filter only those rows that agree (Econ + Econ, then Soc + Soc)
  subset(!(Conservatism %in% "Social" & str_detect(Issue, "Economic_"))) %>%
  median_qi(condition_mean = `b_cfaInequality_nat:ReligionReligious` + r_Conservatism + `r_Conservatism:Issue`) %>%
  rename(Predictors = par)
forModel4plotB <- Model4 %>%
  spread_draws(`b_cfaSocial_control_nat:ReligionReligious`, r_Conservatism[Conservatism,par], `r_Conservatism:Issue`[Issue,par]) %>%
  filter(par == "cfaSocial_control_nat:ReligionReligious") %>%
  subset(!(Conservatism %in% "Economic" & str_detect(Issue, "Social_"))) %>%  #Filter only those rows that agree (Econ + Econ, then Soc + Soc)
  subset(!(Conservatism %in% "Social" & str_detect(Issue, "Economic_"))) %>%
  median_qi(condition_mean = `b_cfaSocial_control_nat:ReligionReligious` + r_Conservatism + `r_Conservatism:Issue`) %>%
  rename(Predictors = par)
forModel4plot <- rbind(forModel4plotA, forModel4plotB) %>% 
  mutate(across('Issue', str_replace, 'Economic_Issue1_TaxHelpPoor', 'Tax to help poor (reversed)'),
         across('Issue', str_replace, 'Economic_Issue2_TaxAdresInqty', 'Tax to address inequality (reversed)'),
         across('Issue', str_replace, 'Economic_Issue3_TaxHealthcare', 'Dislike taxation for healthcare'),
         across('Issue', str_replace, 'Economic_Issue4_GovDependence', 'Govnmt creates dependence'),
         across('Issue', str_replace, 'Economic_Issue5_GovControl', 'Govnmt ownership of energy (reversed)'),
         across('Issue', str_replace, 'Economic_Issue6_LimitedGov', 'Smaller government'),
         across('Issue', str_replace, 'Economic_Issue7_CompetCaptlsm', 'Competitive capitalism is good'),
         across('Issue', str_replace, 'Economic_Issue8_HomelessRespo', 'Homeless responsible for their problems'),
         across('Issue', str_replace, 'Economic_Issue9_GlobeWarm', 'Global warming a threat (reversed)'),
         across('Issue', str_replace, 'Economic_Issue10_AffirmActn', 'Affirmative action (reversed)'),
         across('Issue', str_replace, 'Social_Issue11_MarriageHetr', 'Only hetero marriage'),
         across('Issue', str_replace, 'Social_Issue12_GayAdoption', 'Against gay adoption'),
         across('Issue', str_replace, 'Social_Issue13_Euthanasia', 'Euthanasia is wrong'),
         across('Issue', str_replace, 'Social_Issue14_AbrtnBetter', 'Pro-abortion (reversed)'),
         across('Issue', str_replace, 'Social_Issue15_NoReligTrust', 'Only trust religious people'),
         across('Issue', str_replace, 'Social_Issue16_RelignInGov', 'Separate religion and govnmt (reversed)'),
         across('Issue', str_replace, 'Social_Issue17_MascClothing', 'Men should wear masculine clothes'),
         across('Issue', str_replace, 'Social_Issue18_WomensPlace', "Women's place is in the home"),
         across('Issue', str_replace, 'Social_Issue19_CrckdwnDrugs', 'Crackdown on drugs'),
         across('Issue', str_replace, 'Social_Issue20_IntgrtLifsty', 'Migrants should assimilate'),
         across('Predictors', str_replace, 'cfaInequality_nat:ReligionReligious', 'A) Interaction of DFS Inequality and religion'),
         across('Predictors', str_replace, 'cfaSocial_control_nat:ReligionReligious', 'B) Interaction of DFS Social Control and religion'))
forModel4plotA <- forModel4plot %>%
  slice(1:10, 21:30)
forModel4plotB <- forModel4plot %>%
  slice(11:20, 31:40)
Model4plotA <- 
  ggplot(forModel4plotA,
         aes(y = Issue, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval(position = position_dodge(-0.45)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15)) + 
  scale_color_manual(values = c("#F8766D", "#00BFC4")) + 
  labs(y = NULL, x = NULL) +
  scale_y_discrete(limits = c("Affirmative action (reversed)", "Global warming a threat (reversed)", "Homeless responsible for their problems", 
                              "Competitive capitalism is good", "Smaller government", 
                              "Govnmt ownership of energy (reversed)", "Govnmt creates dependence", "Dislike taxation for healthcare", 
                              "Tax to address inequality (reversed)", "Tax to help poor (reversed)")) +
  theme(text = element_text(size = 15))
Model4plotB <- 
  ggplot(forModel4plotB,
         aes(y = Issue, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval(position = position_dodge(-0.45)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15)) + 
  scale_color_manual(values = c("#F8766D", "#00BFC4")) + 
  labs(y = NULL, x = NULL) +
  scale_y_discrete(limits = c("Migrants should assimilate", "Crackdown on drugs", "Women's place is in the home", 
                              "Men should wear masculine clothes", "Separate religion and govnmt (reversed)", 
                              "Only trust religious people", "Pro-abortion (reversed)", "Euthanasia is wrong", 
                              "Against gay adoption", "Only hetero marriage")) +
  theme(text = element_text(size = 15))
Model4plot <- plot_grid(Model4plotA + theme(legend.position="none"),
                        Model4plotB + theme(legend.position="none"),
                        align = 'v',
                        nrow = 2, ncol = 1, 
                        labels = c('A', 'B'))
legend <- get_legend(
  Model4plotA + theme(legend.box.margin = margin(0, 0, 0, 12)))
Model4plot <- plot_grid(Model4plot, legend,
                        nrow = 2, ncol = 1,
                        rel_heights = c(4, 0.5),
                        rel_widths = c(1.6, 0.4))
ggsave("Fig Sup 2: DFS religion interaction estimates.png",
       plot = Model4plot, bg = "white", 
       width = 11, height = 9, 
       dpi = 300)

#Plot pooled estimates
forModel4plotA <- Model4 %>%
  spread_draws(`b_cfaInequality_nat:ReligionReligious`, r_Conservatism[Conservatism,par]) %>%
  filter(par == "cfaInequality_nat:ReligionReligious") %>%
  median_qi(condition_mean = `b_cfaInequality_nat:ReligionReligious` + r_Conservatism) %>%
  rename(Predictors = par)
forModel4plotB <- Model4 %>%
  spread_draws(`b_cfaSocial_control_nat:ReligionReligious`, r_Conservatism[Conservatism,par]) %>%
  filter(par == "cfaSocial_control_nat:ReligionReligious") %>%
  median_qi(condition_mean = `b_cfaSocial_control_nat:ReligionReligious` + r_Conservatism) %>%
  rename(Predictors = par)
forModel4plot <- rbind(forModel4plotA, forModel4plotB) %>% 
  mutate(across('Predictors', str_replace, 'cfaInequality_nat:ReligionReligious', 'A) Interaction of DFS Nation Inequality and religion'),
         across('Predictors', str_replace, 'cfaSocial_control_nat:ReligionReligious', 'B) Interaction of DFS Nation Social Control and religion'))
Model4plot <-
  ggplot(forModel4plot,
         aes(y = Conservatism, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval(position = position_dodge(-0.25)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15)) +
  xlab("") + ylab("") + 
  xlim(-0.2,0.6) +
  scale_y_discrete(limits = rev(c("Economic", "Social")))
ggsave("Extra: DFS-religion pooled.png", 
       plot = Model4plot, 
       width = 12, height = 4, 
       dpi = 300)

#Plot all interactions
Issues <- data.frame(Issue = unique(dLong$Issue))
Issues$Conservatism <- c(rep("Economic", length.out = 10), rep("Social", length.out = 10))
condIssues <- conditional_effects(Model4,
                                  conditions = Issues, 
                                  re_formula = . ~ 1 + (1 + cfaInequality_nat + cfaSocial_control_nat + cfaInequality_nat:Religion + cfaSocial_control_nat:Religion + Religion + Men + Age + Education + Income | Conservatism/Issue))
Inequality_effects <- plot(condIssues, plot = FALSE)[[8]] +
  labs(x = "DFS Inequality depending on religion",
       y = "Economic Issues (#1-10) and Social Issues (#11-20)")
Social_control_effects <- plot(condIssues, plot = FALSE)[[9]] +
  labs(x = "DFS Social Control depending on religion",
       y = "")
Interaction_effects <- grid.arrange(Inequality_effects, Social_control_effects,
                                    ncol = 2)
ggsave("Extra: DFS religion interactions.png", 
       Interaction_effects, 
       width = 13, height = 6, 
       dpi = 350)

#Plot items 11 and 12
Issue_11 <- Issues %>%
  slice(11)
condIssue_11 <- conditional_effects(Model4,              #Get conditional effects
                                   conditions = Issue_11, 
                                   re_formula = . ~ 1 + (1 + cfaInequality_nat + cfaSocial_control_nat + cfaInequality_nat:Religion + cfaSocial_control_nat:Religion + Religion + Men + Age + Education + Income | Conservatism/Issue))
Int_1 <- plot(condIssue_11, plot = FALSE)[[9]] +
  labs(x = "DFS Social Control",
       y = "Only hetero marriage") +
  ylim(0,1)
Issue_12 <- Issues %>%
  slice(12)
condIssue_12 <- conditional_effects(Model4,               #Get conditional effects
                                    conditions = Issue_12, 
                                    re_formula = . ~ 1 + (1 + cfaInequality_nat + cfaSocial_control_nat + cfaInequality_nat:Religion + cfaSocial_control_nat:Religion + Religion + Men + Age + Education + Income | Conservatism/Issue))
Int_2 <- plot(condIssue_12, plot = FALSE)[[9]] +
  labs(x = "DFS Social Control",
       y = "Against gay adoption") +
  ylim(0,1)
legend <- get_legend(Int_1)
Int_plots <- grid.arrange(Int_1 + theme(legend.position="none"), 
                          Int_2 + theme(legend.position="none"),
                          legend,
                          nrow = 1, widths = c(1, 1, 0.33)) %>%
  as_ggplot() +
  draw_plot_label(label = c("A", "B"), size = 10,
                  x = c(0.005, 0.435), y = c(1, 1))
ggsave("Fig 5: Social Control-religion interactions.png", 
       Int_plots, bg = "white",
       width = 9, height = 4, 
       dpi = 350)


# Supplementary Figure 3: Parabolic relationships with DFS Social Control --------------------------

#Economic issues
Plot1 <- ggplot(d, aes(x=Issue1_TaxHelpPoor, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Tax to help poor (reversed)") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot2 <- ggplot(d, aes(x=Issue2_TaxAdresInqty, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Tax to address inequality (reversed)") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot3 <- ggplot(d, aes(x=Issue3_TaxHealthcare, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Dislike taxation for healthcare") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot4 <- ggplot(d, aes(x=Issue4_GovDependence, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Govnmt creates dependence") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot5 <- ggplot(d, aes(x=Issue5_GovControl, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Govnmt ownership of energy (reversed)") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot6 <- ggplot(d, aes(x=Issue6_LimitedGov, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("'Smaller government") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot7 <- ggplot(d, aes(x=Issue7_CompetCaptlsm, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Competitive capitalism is good") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot8 <- ggplot(d, aes(x=Issue8_HomelessRespo, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Homeless responsible for their problems") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot9 <- ggplot(d, aes(x=Issue9_GlobeWarm, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Global warming a threat (reversed)") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot10 <- ggplot(d, aes(x=Issue10_AffirmActn, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Affirmative action (reversed)") +
  ylab("") + 
  theme(text = element_text(size = 12.5))

#Social issues
Plot11 <- ggplot(d, aes(x=Issue11_MarriageHetr, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Only hetero marriage") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot12 <- ggplot(d, aes(x=Issue12_GayAdoption, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Against gay adoption") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot13 <- ggplot(d, aes(x=Issue13_Euthanasia, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Euthanasia is wrong") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot14 <- ggplot(d, aes(x=Issue14_AbrtnBetter, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Pro-abortion (reversed)") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot15 <- ggplot(d, aes(x=Issue15_NoReligTrust, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Only trust religious people") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot16 <- ggplot(d, aes(x=Issue16_RelignInGov, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Separate religion and govnmt (reversed)") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot17 <- ggplot(d, aes(x=Issue17_MascClothing, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Men should wear masculine clothes") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot18 <- ggplot(d, aes(x=Issue18_WomensPlace, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Women's place is in the home") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot19 <- ggplot(d, aes(x=Issue19_CrckdwnDrugs, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Crackdown on drugs") +
  ylab("") + 
  theme(text = element_text(size = 12.5))
Plot20 <- ggplot(d, aes(x=Issue20_IntgrtLifsty, y=cfaSocial_control_nat)) + 
  geom_point(size = 0.8)+
  geom_smooth() +
  xlab("Migrants should assimilate") +
  ylab("") + 
  theme(text = element_text(size = 12.5))

#Plot together
U_plots <- plot_grid(Plot1, Plot2, Plot3, Plot4, Plot5, 
                     Plot6, Plot7, Plot8, Plot9, Plot10, 
                     Plot11, Plot12, Plot13, Plot14, Plot15, 
                     Plot16, Plot17, Plot18, Plot19, Plot20, 
                     align = 'h',
                     ncol = 4,
                     nrow = 5)
ggsave("Fig Sup 3: U-shape social control.png",
       plot = U_plots, bg = "white", 
       width = 15, height = 9, 
       dpi = 300)


# Supplementary Table 1: Leave-one-out model comparison ------------------------------------------

Model1loo = loo_subsample(Model1, cores = 4, observations = 1000)

looObs <- obs_idx(Model1loo)    #Set same sub-sample for other ones

Model2loo = loo_subsample(Model2, cores = 4, observations = looObs)

Model3loo = loo_subsample(Model3, cores = 4, observations = looObs)

Model4loo = loo_subsample(Model4, cores = 4, observations = looObs)

Model5loo = loo_subsample(Model5, cores = 4, observations = looObs)

#Compare and print table
LOO_comp <- loo_compare(Model1loo,Model2loo,Model3loo,Model4loo,Model5loo)
LOO_comp <- LOO_comp %>%
  as.data.frame() %>%
  select(elpd_diff, se_diff, subsampling_se_diff) %>%
  mutate_if(is.numeric, round, digits=2) %>%
  rownames_to_column("Model") %>%
  mutate(Model = str_extract(Model, pattern = "[[:digit:]]+")) %>%
  mutate(Model = ifelse(Model == 1, "Demographics",
                       ifelse(Model == 2, "Demographics + DFS",
                        ifelse(Model == 3, "Demographics + DFS + religion",
                         ifelse(Model == 4, "Demographics + DFS + interaction with religion",
                          ifelse(Model == 5, "Demographics + DFS + religion + interaction between DFS dimensions",
                           "other"))))))
kable(LOO_comp,
      format = 'latex',
      digits = 3,
      booktabs = TRUE,
      col.names = c("Model", "Difference in ELPD", "Standard error of the difference", "Standard error due to subsampling uncertainty"),
      align = 'l') %>%
  column_spec(2:4, width = "3cm")  %>%
  add_header_above(c("Model comparison estimates using leave-one-out cross-validation" = 4),
                   align = 'l', italic = TRUE) %>%
  add_header_above(c("Supplementary Table 1" = 4), align = 'l') %>%
  collapse_rows(columns = 1:1, valign = "top", latex_hline = "linespace") %>%
  kable_styling(bootstrap_options = "condensed", latex_options="scale_down") %>%
  footnote(general = "ELPD is the expected log pointwise predictive density. ",
           footnote_as_chunk = T, threeparttable = T) %>%
  save_kable("Table Sup 1.png")



# Extra model for SDO and RWA ------------------------------------------------------------

Model6 <- brm(
  Score ~ 1 + cfaSDO + cfaRWA + Men + Age + Education + Income +
    (1 | ParticipantID) + (1 + cfaSDO + cfaRWA + Men + Age + Education + Income | Conservatism/Issue),
  data = dLong,
  family = Beta,
  prior = c(prior(normal(0, 0.5), class = b),
            prior(exponential(3), class = sd),
            prior(gamma(1, 0.01), class = phi)),
  control = list(adapt_delta = 0.9999, max_treedepth = 12),
  iter = 20000, cores = 4)
summary(Model6)

#Plot dimensions
forModel6plotA <- Model6 %>%
  spread_draws(b_cfaSDO, r_Conservatism[Conservatism,par]) %>%
  filter(par == "cfaSDO") %>%
  median_qi(condition_mean = b_cfaSDO + r_Conservatism) %>%
  rename(Predictors = par)
forModel6plotB <- Model6 %>%
  spread_draws(b_cfaRWA, r_Conservatism[Conservatism,par]) %>%
  filter(par == "cfaRWA") %>%
  median_qi(condition_mean = b_cfaRWA + r_Conservatism) %>%
  rename(Predictors = par)
forModel6plot <- rbind(forModel6plotA, forModel6plotB) %>% 
  mutate(across('Predictors', str_replace, 'cfaSDO', 'A) SDO'),
         across('Predictors', str_replace, 'cfaRWA', 'B) RWA'))
Model6plot <-
  ggplot(forModel6plot,
         aes(y = Conservatism, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval(position = position_dodge(-0.15)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15)) +
  xlab("") + ylab("") + 
  xlim(-0.15,0.7) +
  scale_y_discrete(limits = rev(c("Economic", "Social")))
ggsave("Extra: SDO-RWA and dimensions.png", 
       plot = Model6plot, 
       width = 9, height = 4, 
       dpi = 300)

#Plot issues
forModel6plotA <- Model6 %>%
  spread_draws(b_cfaSDO, r_Conservatism[Conservatism,par], `r_Conservatism:Issue`[Issue,par]) %>%
  filter(par == "cfaSDO") %>%
  subset(!(Conservatism %in% "Economic" & str_detect(Issue, "Social_"))) %>%  #Filter only those rows that agree (Econ + Econ, then Soc + Soc)
  subset(!(Conservatism %in% "Social" & str_detect(Issue, "Economic_"))) %>%
  median_qi(condition_mean = b_cfaSDO + r_Conservatism + `r_Conservatism:Issue`) %>%
  rename(Predictors = par)
forModel6plotB <- Model6 %>%
  spread_draws(`b_cfaRWA`, r_Conservatism[Conservatism,par], `r_Conservatism:Issue`[Issue,par]) %>%
  filter(par == "cfaRWA") %>%
  subset(!(Conservatism %in% "Economic" & str_detect(Issue, "Social_"))) %>% 
  subset(!(Conservatism %in% "Social" & str_detect(Issue, "Economic_"))) %>%
  median_qi(condition_mean = `b_cfaRWA` + r_Conservatism + `r_Conservatism:Issue`) %>%
  rename(Predictors = par)
forModel6plot <- rbind(forModel6plotA, forModel6plotB) %>% 
  mutate(Issue = gsub('Economic_', '', Issue),
         Issue = gsub('Social_', '', Issue),
         across('Predictors', str_replace, 'cfaSDO', 'A) SDO'),
         across('Predictors', str_replace, 'cfaRWA', 'B) RWA'))
forModel6plotA <- forModel6plot %>% 
  slice(1:10, 21:30)
forModel6plotB <- forModel6plot %>% 
  slice(11:20, 31:40)
Model6plotA <-
  ggplot(forModel6plotA,
         aes(y = Issue, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval(position = position_dodge(-0.25)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15)) + 
  scale_color_manual(values = c("#F8766D", "#00BFC4")) + 
  xlim(-0.4,1.15) +
  labs(y = NULL, x = NULL) +
  scale_y_discrete(limits = c("Issue10_AffirmActn", "Issue9_GlobeWarm", "Issue8_HomelessRespo", "Issue7_CompetCaptlsm", "Issue6_LimitedGov", 
                              "Issue5_GovControl", "Issue4_GovDependence", "Issue3_TaxHealthcare", "Issue2_TaxAdresInqty", "Issue1_TaxHelpPoor")) +
  theme(text = element_text(size = 15))
Model6plotB <-  #Social plot
  ggplot(forModel6plotB,
         aes(y = Issue, x = condition_mean,
             color = Predictors,
             shape = Predictors,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval(position = position_dodge(-0.25)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  scale_shape_manual(values = c(17, 15)) + 
  scale_color_manual(values = c("#F8766D", "#00BFC4")) + 
  xlim(-0.4,1.15) +
  labs(y = NULL, x = NULL) +
  scale_y_discrete(limits = c("Issue20_IntgrtLifsty", "Issue19_CrckdwnDrugs", "Issue18_WomensPlace", "Issue17_MascClothing", "Issue16_RelignInGov", 
                              "Issue15_NoReligTrust", "Issue14_AbrtnBetter", "Issue13_Euthanasia", "Issue12_GayAdoption", "Issue11_MarriageHetr")) +
  theme(text = element_text(size = 15))
Model6plot <- plot_grid(Model6plotA + theme(legend.position="none"),  
                        Model6plotB + theme(legend.position="none"),
                        align = 'h',
                        nrow = 1, ncol = 2, 
                        labels = c('A', 'B'))
legend <- get_legend( 
  Model6plotA + theme(legend.box.margin = margin(0, 0, 0, 12)))
Model6plot <- plot_grid(Model6plot, legend, 
                        rel_heights = c(4, 0.5),
                        rel_widths = c(1.7, 0.3))  
ggsave("Extra: SDO-RWA and issues.png",   
       plot = Model6plot, bg = "white", 
       width = 15, height = 7, 
       dpi = 300)









