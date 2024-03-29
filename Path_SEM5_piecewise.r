####### SUPPLEMENTARY ANALYSES FOR PIECEWISE SEM #######
# Source: Jon Lefcheck, jslefche@vims.edu 

####### 
#Install required libraries:
install.packages("ape") #Version 3.3
install.packages("caper") # Vresion 0.5.2
install.packages("nlme") # Version 3.1.122
install.packages("lavaan") # Version 0.5.19
install.packages("devtools")


# Load required libraries
library(ape) #Version 3.3
library(caper) # Vresion 0.5.2
library(nlme) # Version 3.1.122
library(lavaan) # Version 0.5.19
library(devtools) #to load development versions of packages.

# Load piecewiseSEM from CRAN (# install.packages("piecewiseSEM")
#or:
# Install development branch from github
install_github("jslefche/piecewiseSEM@devel", build_vignette = TRUE)
# Load library
library(piecewiseSEM) # Version 1.0.0

####### EXAMPLE 1: KELP FOREST (BYRNES ET AL. 2011) ####### 

# Read in data
kelp = read.csv("kelp.csv")

# Convert -Inf to NA
kelp$max_Max.OV[which(kelp$max_Max.OV == -Inf)] = NA

# Log response variables to mirror original analysis of Byrnes et al 2011
kelp$kelp = log(kelp$kelp+1)
kelp$prev.kelp = log(kelp$prev.kelp + 1)
kelp[, 18:23] = log(kelp[, 18:23] + 1)

# Remove rows where response or predictors are NA
vars = c("SITE", "TRANSECT", "YEAR", "max_Max.OV", "prev.kelp", "habitat", "spring_canopy_150", "kelp", 
         "algae_richness", "sessile_invert_richness", "mobile_richness", "richness", "consumer_richness", "linkdensity")
kelp = kelp[, vars]
kelp = na.omit(kelp)

# Create interaction term
kelp$wave_kelp_int = kelp$max_Max.OV * kelp$prev.kelp

# Replicate original analysis

# List structured equations for lavaan
kelp_model = '
  spring_canopy_150 ~ max_Max.OV + prev.kelp + wave_kelp_int + habitat

  kelp ~ max_Max.OV + prev.kelp  + habitat + spring_canopy_150

  richness ~ kelp   + prev.kelp  + habitat + spring_canopy_150

  linkdensity ~ richness  + kelp  + prev.kelp  + habitat + spring_canopy_150
'

# Fit vcov SEM
kelp_model.sem = sem(kelp_model, kelp, estimator = "MLM")

# Summary output with standardized coefficients
summary(kelp_model.sem, standardize = TRUE)

# Get R2 for models
inspect(kelp_model.sem, "rsquare")

####### 
# Now fit piecewise model with random effect

# Create component models and store in list
kelp_pSEM_randomList = psem(
  
  # Predicting spring kelp canopy
  spring_canopy_150 = lme(spring_canopy_150 ~ max_Max.OV * prev.kelp + habitat, random = ~ 1 | SITE, data = kelp),
  
  # Predicting summer kelp density
  kelp = lme(kelp ~ max_Max.OV + prev.kelp  + habitat + spring_canopy_150, random = ~ 1 | SITE, data = kelp),
  
  # Predicting total richness
  richness = lme(richness ~ kelp + prev.kelp + habitat + spring_canopy_150, random = ~ 1 | SITE, data = kelp),
  
  # Predict linkage density
  linkdensity = lme(linkdensity ~ richness + kelp + prev.kelp + habitat + spring_canopy_150, random = ~ 1 | SITE, data = kelp)
  
)

# Run goodness-of-fit tests
summary(kelp_pSEM_randomList,kelp)

# Evaluate path significance using unstandardized coefficients
sem.coefs(kelp_pSEM_randomList, kelp, standardize = "none")

# Obtain standardized regression coefficients
sem.coefs(kelp_pSEM_randomList, kelp, standardize = "scale")

# Explore individual model fits
rsquared(kelp_pSEM_randomList)

####### Fit piecewise SEM with random AND autocorrelation structures======
kelp_pSEM_CAR1List = psem(
  
  # Predicting spring kelp canopy
  spring_canopy_150 = lme(spring_canopy_150 ~ max_Max.OV * prev.kelp + habitat,
                          random = ~ 1 | SITE/TRANSECT, correlation = corCAR1(form = ~ YEAR), data = kelp),
  
  # Predicting summer kelp density
  kelp = lme(kelp ~ max_Max.OV * prev.kelp  + habitat + spring_canopy_150,
             random = ~ 1 | SITE/TRANSECT, correlation = corCAR1(form = ~ YEAR), data = kelp),
  
  # Predicting total richness
  richness = lme(richness ~ kelp + prev.kelp + habitat + spring_canopy_150,
                 random = ~ 1 | SITE/TRANSECT, correlation = corCAR1(form = ~ YEAR), data = kelp),
  
  # Predict linkage density
  linkdensity = lme(linkdensity ~ richness + kelp + prev.kelp + habitat + spring_canopy_150,
                    random = ~ 1 | SITE/TRANSECT, correlation = corCAR1(form = ~ YEAR), data = kelp)
  
)

# Run goodness-of-fit tests
summary(kelp_pSEM_CAR1List, kelp)

# Evaluate path significance using unstandardized coefficients
sem.coefs(kelp_pSEM_CAR1List, kelp, standardize = "none")

# Obtain standardized regression coefficients
sem.coefs(kelp_pSEM_CAR1List, kelp, standardize = "scale")

# Explore individual model fits
sem.model.fits(kelp_pSEM_CAR1List)

####### 

# Break apart total richness into resource (algal & sessile inverts) and consumer components

# List structured equations for lavaan
kelp_model_richness = '
  spring_canopy_150 ~ max_Max.OV + prev.kelp + wave_kelp_int + habitat
  
  kelp ~ max_Max.OV + prev.kelp  + spring_canopy_150 + habitat
  
  algae_richness ~ max_Max.OV + kelp + prev.kelp + spring_canopy_150 + habitat 
  sessile_invert_richness ~ max_Max.OV + kelp + prev.kelp + spring_canopy_150 + habitat 
  mobile_richness ~ max_Max.OV + kelp + prev.kelp + spring_canopy_150 + habitat 
  
  mobile_richness ~~ algae_richness
  mobile_richness ~~ sessile_invert_richness
  sessile_invert_richness ~~ algae_richness
'
# Fit vcov SEM
kelp_model_richness.sem = sem(kelp_model_richness, kelp, estimator = "MLM")

# Return coefficients
summary(kelp_model_richness.sem, standardize = TRUE)

# Fit with piecewise

# Create component models and store in list
kelp_richness_pSEM_randomList = psem(
  
  # Predicting spring kelp canopy
 lme(spring_canopy_150 ~ max_Max.OV + prev.kelp + wave_kelp_int + habitat, random = ~ 1 | SITE, data = kelp),
  
  # Predicting summer kelp density
 lme(kelp ~ max_Max.OV + prev.kelp  + spring_canopy_150 + habitat, random = ~ 1 | SITE, data = kelp),
  
  # Predicting richness
 lme(algae_richness ~ max_Max.OV + kelp + prev.kelp + spring_canopy_150 + habitat , random = ~ 1 | SITE, data = kelp),
  
 lme(sessile_invert_richness ~ max_Max.OV + kelp + prev.kelp + spring_canopy_150 + habitat , random = ~ 1 | SITE, data = kelp),
  
 lme(mobile_richness ~ max_Max.OV + kelp + prev.kelp + spring_canopy_150 + habitat, random = ~ 1 | SITE, data = kelp)
  
)

# Run goodness-of-fit tests
summary(kelp_richness_pSEM_randomList)



####### EXAMPLE 2: ECOLOGICAL SUCCESS OF EUSOCIAL SHRIMPS (DUFFY & MACDONALD 2010) ####### 

# Read in data
synalpheus = read.csv("synalpheus.csv")

# Replace species name
synalpheus$Species = as.character(synalpheus$Species)
synalpheus[synalpheus$Species == "longsm", "Species"] = "longicarpus_small"

# Construct structured equations
synalpheus_model = '
  Host.Range ~ Eusociality.index + Total.Mass.Female

  Rubble.Abundance ~ Eusociality.index + Host.Range
'
# Fit SEM using lavaan
synalpheus.sem = sem(synalpheus_model, synalpheus)

# Explore summary output
summary(synalpheus.sem, standardized = TRUE)

inspect(synalpheus.sem, "rsquare")

####### 

# Repeat but include phylogenetic correlations

# Import phylogenetic tree
synalpheus.tree = read.tree("synalpheus_tree.txt")

# Drop tips not found in the synalpheus dataset
synalpheus.tree = drop.tip(synalpheus.tree, synalpheus.tree$tip.label[!synalpheus.tree$tip.label %in% unique(synalpheus$Species)])

# Order data by tip order
synalpheus = synalpheus[which(synalpheus$Species %in% synalpheus.tree$tip.label), ]
rownames(synalpheus) = synalpheus$Species

# Run model
synalpheus_modelCorList = list(
  
  Host.Range = gls(Host.Range ~ Eusociality.index + Total.Mass.Female, correlation = corBrownian(0.5, synalpheus.tree), na.action = na.omit, synalpheus),
  
  Rubble.Abundance = gls(Rubble.Abundance ~ Eusociality.index + Host.Range, correlation = corBrownian(0.5, synalpheus.tree), na.action = na.omit, synalpheus)

  )

# Check fit
sem.fit(synalpheus_modelCorList, synalpheus)

# Obtain standardized regression coefficients
sem.coefs(synalpheus_modelCorList, synalpheus, standardize = "scale")

# Explore individual model fits
sem.model.fits(synalpheus_modelCorList)

#######

# Repeat with pgls from 'caper' package

# Merge Synalpheus data and tree
synalpheus.merge = comparative.data(synalpheus.tree, synalpheus, names.col = "Species")

# Run model
synalpheus_modelPGLSList = list(
  
  Host.Range = pgls(Host.Range ~ Eusociality.index + Total.Mass.Female, synalpheus.merge),
  
  Rubble.Abundance = pgls(Rubble.Abundance ~ Eusociality.index + Host.Range, synalpheus.merge)
  
)

# Check fit
sem.fit(synalpheus_modelPGLSList, synalpheus.merge)

# Obtain standardized regression coefficients
sem.coefs(synalpheus_modelPGLSList, synalpheus.merge, standardize = "scale")

# Explore individual model fits
sem.model.fits(synalpheus_modelPGLSList)
