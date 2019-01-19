
require(nlme)  # CAN do complex variance structures; can't do GLMM
require(lme4)  # can't do complex variance structures; CAN do GLMM
require(dplyr)

dat <- read.csv("even_size_class_data.csv", stringsAsFactors = FALSE)


# GROWTH
growth <- dat[!is.na(dat$shade),]   # only retain rows for saplings that have a shade tolerance value
growth <- growth[growth$myc=="am" | growth$myc=="ecm", ]  # only keep ecm vs. am saplings
growth <- growth[!is.na(growth$growth),]  # only retain rows for saplings that have a growth rate
growth <- growth[growth$growth<=1,]   # remove probable measurement errors
growth$growth <- ifelse(growth$growth<0, 0, growth$growth)   # convert negative growth rates to "0"

scaled_vars <- c("diam1", "n_con", "n_het", "BAc5", "BAh5", "BAc10", "BAh10", 
                 "BAc15", "BAh15", "BAc20", "BAh20", "gap", "shade")
growth[, scaled_vars] <- scale(growth[, scaled_vars])  # scale numeric variables
growth %>% select(scaled_vars) %>% summarise_all(funs(mean))  # check your work
growth %>% select(scaled_vars) %>% summarise_all(funs(sd))
growth$mycF <- as.factor(growth$myc)

null <- lme(log(growth+1) ~ diam1 + gap,
            random=~1|plot,
            method="ML",
            data = growth)

bio <- lme(log(growth+1) ~ diam1 + gap + 
             n_con + n_het + BAc15 + BAh15,
           random=~1|plot,
           weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                           varExp(form=~BAc15), varExp(form=~BAh15)),
           method="ML",
           data = growth)

add.shade <- lme(log(growth+1) ~ diam1 + gap + 
                   n_con + n_het + BAc15 + BAh15 +
                   shade,
                 random=~1|plot,
                 weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                                 varExp(form=~BAc15), varExp(form=~BAh15)),
                 method="ML",
                 data = growth)

add.myc <- lme(log(growth+1) ~ diam1 + gap + 
                 n_con + n_het + BAc15 + BAh15 +
                 mycF,
               random=~1|plot,
               weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                               varExp(form=~BAc15), varExp(form=~BAh15)),
               method="ML",
               data = growth)

add.all <- lme(log(growth+1) ~ diam1 + gap + 
                 n_con + n_het + BAc15 + BAh15 +
                 shade + mycF + shade*mycF,
               random=~1|plot,
               weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                               varExp(form=~BAc15), varExp(form=~BAh15)),
               method="ML",
               data = growth)

int.shade <- lme(log(growth+1) ~ diam1 + gap + 
                   n_con + n_het +
                   shade*BAc15 + shade*BAh15,
                 random=~1|plot,
                 weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                                 varExp(form=~BAc15), varExp(form=~BAh15)),
                 method="ML",
                 data = growth)

int.myc <- lme(log(growth+1) ~ diam1 + gap + 
                 n_con + n_het +
                 mycF*BAc15 + mycF*BAh15,
               random=~1|plot,
               weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                               varExp(form=~BAc15), varExp(form=~BAh15)),
               method="ML",
               data = growth)

int.all <- lme(log(growth+1) ~ diam1 + gap + 
                 n_con + n_het +
                 shade*BAc15 + shade*BAh15 +
                 mycF*BAc15 + mycF*BAh15 + mycF*shade,
               random=~1|plot,
               weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                               varExp(form=~BAc15), varExp(form=~BAh15)),
               method="ML",
               data = growth)

# using nlme for growth response, do mod comparison
anova(null, bio, add.shade, add.myc, add.all, int.shade, int.myc, int.all)
anova(add.all, int.all)
anova(int.shade, int.all)
anova(int.myc, int.all)
anova(add.myc, int.all)
anova(add.shade, int.all)
# int.all is best model

# check to see if BIC chooses a different model than AIC
# BIC has heavier penality for including lots of variables
AIC(null, bio, add.shade, add.myc, add.all, int.shade, int.myc, int.all)
BIC(null, bio, add.shade, add.myc, add.all, int.shade, int.myc, int.all)
# yields similar result


# aggregate results into table for supplementary material
comp.tab15 <- anova(null, bio, add.shade, add.myc, add.all, int.shade, int.myc, int.all)
comp.tab15 <- as.data.frame(comp.tab15[,"BIC"])
names(comp.tab15) <- "BIC_15"

comp.tab$BIC_15 <- comp.tab15$BIC_15


# test con vs. het differences
# n_con vs. n_het
n_con <- summary(int.all)$coefficients$fixed["n_con"]
n_het <- summary(int.all)$coefficients$fixed["n_het"]
n_con_var <- vcov(int.all)["n_con", "n_con"]
n_het_var <- vcov(int.all)["n_het", "n_het"]
n_con_het_cov <- vcov(int.all)["n_con", "n_het"]

coef.diff <- abs(n_con - n_het)
coef.diff.var <- n_con_var + n_het_var + 2*(n_con_het_cov)
2 * qnorm(0.95, coef.diff, sqrt(coef.diff.var))
# p = 0.014

# BAc vs. BAh
BAc15 <- summary(int.all)$coefficients$fixed["BAc15"]
BAh15 <- summary(int.all)$coefficients$fixed["BAh15"]
BAc15_var <- vcov(int.all)["BAc15", "BAc15"]
BAh15_var <- vcov(int.all)["BAh15", "BAh15"]
BAc_BAh_cov <- vcov(int.all)["BAc15", "BAh15"]

coef.diff <- abs(BAc15 - BAh15)
coef.diff.var <- BAc15_var + BAh15_var + 2*(BAc_BAh_cov)
2 * qnorm(0.95, coef.diff, sqrt(coef.diff.var))
# p = 0.027

# shade*con vs. shade*het
shade_BAc15 <- summary(int.all)$coefficients$fixed["shade:BAc15"]
shade_BAh15 <- summary(int.all)$coefficients$fixed["shade:BAh15"]
shade_BAc15_var <- vcov(int.all)["shade:BAc15", "shade:BAc15"]
shade_BAh15_var <- vcov(int.all)["shade:BAh15", "shade:BAh15"]
shade_con_het_cov <- vcov(int.all)["shade:BAc15", "shade:BAh15"]

coef.diff <- abs(shade_BAc15 - shade_BAh15)
coef.diff.var <- shade_BAc15_var + shade_BAh15_var + 2*(shade_con_het_cov)
2 * qnorm(0.95, coef.diff, sqrt(coef.diff.var))
# 0.012

# myc*con vs. myc*het
myc_BAc15 <- summary(int.all)$coefficients$fixed["BAc15:mycFecm"]
myc_BAh15 <- summary(int.all)$coefficients$fixed["BAh15:mycFecm"]
myc_BAc15_var <- vcov(int.all)["BAc15:mycFecm", "BAc15:mycFecm"]
myc_BAh15_var <- vcov(int.all)["BAh15:mycFecm", "BAh15:mycFecm"]
myc_con_het_cov <- vcov(int.all)["BAc15:mycFecm", "BAh15:mycFecm"]

coef.diff <- abs(myc_BAc15 - myc_BAh15)
coef.diff.var <- myc_BAc15_var + myc_BAh15_var + 2*(myc_con_het_cov)
2 * qnorm(0.95, coef.diff, sqrt(coef.diff.var))
# 0.045


# refit using REML for correct estimates
int.all <- lme(log(growth+1) ~ diam1 + gap + 
                 n_con + n_het +
                 shade*BAc15 + shade*BAh15 +
                 mycF*BAc15 + mycF*BAh15 + mycF*shade,
               random=~1|plot,
               weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                               varExp(form=~BAc15), varExp(form=~BAh15)),
               method="REML",
               data = growth)
summary(int.all)



# SURVIVAL 
surv <- dat[!is.na(dat$shade),]
surv <- surv[surv$myc=="am" | surv$myc=="ecm", ]  # only keep ecm vs. am saplings
scaled_vars <- c("diam1", "n_con", "n_het", "BAc5", "BAh5", "BAc10", "BAh10", 
                 "BAc15", "BAh15", "BAc20", "BAh20", "gap", "shade")
surv[, scaled_vars] <- scale(surv[, scaled_vars])  # scale numeric variables
surv %>% select(scaled_vars) %>% summarise_all(funs(mean))  # check your work
surv %>% select(scaled_vars) %>% summarise_all(funs(sd))
surv$mycF <- factor(surv$myc)

Snull <- glmer(surv ~ diam1 + gap +
                 (1|plot),
               family = binomial(link = "logit"),
               data = surv)

Sbio <- glmer(surv ~ diam1 + gap + 
                n_con + n_het + BAc15 + BAh15 +
                (1|plot),
              family = binomial(link = "logit"),
              data = surv)

Sadd.shade <- glmer(surv ~ diam1 + gap + 
                      n_con + n_het + BAc15 + BAh15 +
                      shade +
                      (1|plot),
                    family = binomial(link = "logit"),
                    data = surv)

Sadd.myc <- glmer(surv ~ diam1 + gap + 
                    n_con + n_het + BAc15 + BAh15 +
                    mycF +
                    (1|plot),
                  family = binomial(link = "logit"),
                  data = surv)

Sadd.all <- glmer(surv ~ diam1 + gap + 
                    n_con + n_het + BAc15 + BAh15 +
                    shade + mycF + shade*mycF +
                    (1|plot),
                  family = binomial(link = "logit"),
                  data = surv)

Sint.shade <- glmer(surv ~ diam1 + gap + 
                      n_con + n_het +
                      shade*BAc15 + shade*BAh15 +
                      (1|plot),
                    family = binomial(link = "logit"),
                    data = surv)

Sint.myc <- glmer(surv ~ diam1 + gap + 
                    n_con + n_het +
                    mycF*BAc15 + mycF*BAh15 +
                    (1|plot),
                  family = binomial(link = "logit"),
                  data = surv)

Sint.all <- glmer(surv ~ diam1 + gap + 
                    n_con + n_het +
                    shade*BAc15 + shade*BAh15 +
                    mycF*BAc15 + mycF*BAh15 + mycF*shade +
                    (1|plot),
                  family = binomial(link = "logit"),
                  data = surv)

# mod comparison
anova(Snull, Sbio, Sadd.shade, Sadd.myc, Sadd.all, Sint.shade, Sint.myc, Sint.all)
anova(Sadd.all, Sint.all)
# add.all and int.all are similar - marginally different, so I'll go with smaller model,
# add.all (BIC is way lower anyway)


# aggregate results into table for supplementary material
comp.tab15 <- anova(Snull, Sbio, Sadd.shade, Sadd.myc, Sadd.all, Sint.shade, Sint.myc, Sint.all)
comp.tab15 <- as.data.frame(comp.tab15[,"BIC"])
names(comp.tab15) <- "BIC_15"

comp.tab$BIC_15 <- comp.tab15$BIC_15


# test con vs. het differences in best survival model - Sadd.all
n_con <- fixed.effects(Sadd.all)["n_con"]
n_het <- fixed.effects(Sadd.all)["n_het"]
n_con_var <- vcov(Sadd.all)["n_con", "n_con"]
n_het_var <- vcov(Sadd.all)["n_het", "n_het"]
n_con_het_cov <- vcov(Sadd.all)["n_con", "n_het"]

coef.diff <- abs(n_con - n_het)
coef.diff.var <- n_con_var + n_het_var + 2*(n_con_het_cov)
2 * qnorm(0.95, coef.diff, sqrt(coef.diff.var))
# p = 0.194

# BAc vs. BAh
BAc15 <- fixed.effects(Sadd.all)["BAc15"]
BAh15 <- fixed.effects(Sadd.all)["BAh15"]
BAc15_var <- vcov(Sadd.all)["BAc15", "BAc15"]
BAh15_var <- vcov(Sadd.all)["BAh15", "BAh15"]
BAc15_het_cov <- vcov(Sadd.all)["BAc15", "BAh15"]

coef.diff <- abs(BAc15 - BAh15)
coef.diff.var <- BAc15_var + BAh15_var + 2*(BAc15_het_cov)
2 * qnorm(0.95, coef.diff, sqrt(coef.diff.var))
# p = 0.449

summary(Sadd.all)

