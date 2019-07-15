#https://gist.github.com/noamross/9d5ae9680fe8357ccd94
#https://www.youtube.com/watch?v=kCXN7CRYKVo
#https://www.r-bloggers.com/first-steps-with-structural-equation-modeling/


library(lavaan)
library(qgraph)
library(semPlot)

galton=read.table("galtonrawlavaan.txt", header = TRUE)
head(galton) #show first few lines
             #Do a single Variable Regression
fit <- lm(Child ~ Parent, data=galton)
summary(fit) #Summary Output
#Same Model as SEM
myModel <- ' # regressions
            Child ~Parent
             # variances and covariances 
             Child~~Child
             Parent~~Parent
             # intercepts 
              Parent~pint*1
             Child~1
           '
#Fit Model
fit <- sem(myModel, data = galton,fixed.x=FALSE)
#Print Fit Statistics
summary(fit, standardized = TRUE)

semPaths(fit, whatLabels = 'std', layout='spring')
#semPaths(fit, "std", edge.label.cex = 0.5, curvePivot = TRUE, layout = "tree")

#------------------------------
# conducting a path analysis using lavaan

library(lavaan)
library(qgraph)
library(semPlot)
library(foreign)

df_crashes <- read.csv('Acc.csv')


df_crashes$Serious_Fatal[df_crashes$Accident_Severity == 1 | df_crashes$Accident_Severity == 2] <- 1
df_crashes$Serious_Fatal[df_crashes$Accident_Severity == 3] <- 0

data_to_model <-  subset(df_crashes, select = c(Light_Conditions, 
                                                Weather_Conditions, 
                                                Road_Surface_Conditions,
                                                Number_of_Casualties,
                                                Speed_limit,
                                                Number_of_Vehicles,
                                                Road_Type,
                                                Serious_Fatal))

sem_model <- "
drivingconditions =~ Light_Conditions + Weather_Conditions + Road_Surface_Conditions

Serious_Fatal ~ Speed_limit + Road_Type + drivingconditions
"

fit <- sem(sem_model, data_to_model)

# use the standardize
sfit <- standardizedsolution(fit)
sfit[pfit$op == "~", ]

semPaths(fit, "std")

#-------------------------------
  

library(lavaan)
library(qgraph)
library(semPlot)
library(foreign)


ccases <- read.spss('ccases.sav', to.data.frame = TRUE, use.value.labels = FALSE)

# latent variables: agreeableness, conscientiosness, extraversion
# use age and male to predict the latent variables

sem_model <- "
agreeableness =~ A1 + A2 + A3
conscientiosness =~ C1 + C2 +C3
extraversion =~ E1 + E2 + E3

agreeableness ~ age + gender
conscientiosness ~ age + gender
extraversion ~ age + gender

"

fit <- sem(sem_model, ccases)

# look at the unstandardise parameter estimates
# This shows just the regression ones

pfit <- parameterestimates(fit)
pfit[pfit$op == "~", ]

# use the standardize
sfit <- standardizedsolution(fit)
sfit[pfit$op == "~", ]

# this tells that one sd increase in age leads to a slight increase in conscientiousness, i.e. 0.081 of SD
# one sd increase in age leads to decrease in agreeable, i.e. older people are less agreeable. -0.139
# males are more agreeable, -0.232
# -0.113 , older people are less extraverted

semPaths(fit, "std")

# thickness of lines and coeff relate to the strength of the relationship between variables
# red is negative relationships, i.e. an increase in sd in x causes a decrease in y
#


