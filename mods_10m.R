

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
             n_con + n_het + BAc10 + BAh10,
           random=~1|plot,
           weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                           varExp(form=~BAc10), varExp(form=~BAh10)),
           method="ML",
           data = growth)

add.shade <- lme(log(growth+1) ~ diam1 + gap + 
                   n_con + n_het + BAc10 + BAh10 +
                   shade,
                 random=~1|plot,
                 weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                                 varExp(form=~BAc10), varExp(form=~BAh10)),
                 method="ML",
                 data = growth)

add.myc <- lme(log(growth+1) ~ diam1 + gap + 
                 n_con + n_het + BAc10 + BAh10 +
                 mycF,
               random=~1|plot,
               weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                               varExp(form=~BAc10), varExp(form=~BAh10)),
               method="ML",
               data = growth)

add.all <- lme(log(growth+1) ~ diam1 + gap + 
                 n_con + n_het + BAc10 + BAh10 +
                 shade + mycF + shade*mycF,
               random=~1|plot,
               weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                               varExp(form=~BAc10), varExp(form=~BAh10)),
               method="ML",
               data = growth)

int.shade <- lme(log(growth+1) ~ diam1 + gap + 
                   n_con + n_het +
                   shade*BAc10 + shade*BAh10,
                 random=~1|plot,
                 weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                                 varExp(form=~BAc10), varExp(form=~BAh10)),
                 method="ML",
                 data = growth)

int.myc <- lme(log(growth+1) ~ diam1 + gap + 
                 n_con + n_het +
                 mycF*BAc10 + mycF*BAh10,
               random=~1|plot,
               weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                               varExp(form=~BAc10), varExp(form=~BAh10)),
               method="ML",
               data = growth)

int.all <- lme(log(growth+1) ~ diam1 + gap + 
                 n_con + n_het +
                 shade*BAc10 + shade*BAh10 +
                 mycF*BAc10 + mycF*BAh10 + mycF*shade,
               random=~1|plot,
               weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                               varExp(form=~BAc10), varExp(form=~BAh10)),
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
comp.tab10 <- anova(null, bio, add.shade, add.myc, add.all, int.shade, int.myc, int.all)
comp.tab10$mod <- rownames(comp.tab10)
comp.tab10 <- as.data.frame(comp.tab10[,"BIC"])
names(comp.tab10) <- "BIC_10"

comp.tab$BIC_10 <- comp.tab10$BIC_10


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
# p = 0.013

# BAc vs. BAh
BAc10 <- summary(int.all)$coefficients$fixed["BAc10"]
BAh10 <- summary(int.all)$coefficients$fixed["BAh10"]
BAc10_var <- vcov(int.all)["BAc10", "BAc10"]
BAh10_var <- vcov(int.all)["BAh10", "BAh10"]
BAc_BAh_cov <- vcov(int.all)["BAc10", "BAh10"]

coef.diff <- abs(BAc10 - BAh10)
coef.diff.var <- BAc10_var + BAh10_var + 2*(BAc_BAh_cov)
2 * qnorm(0.95, coef.diff, sqrt(coef.diff.var))
# p = 0.032

# shade*con vs. shade*het
shade_BAc10 <- summary(int.all)$coefficients$fixed["shade:BAc10"]
shade_BAh10 <- summary(int.all)$coefficients$fixed["shade:BAh10"]
shade_BAc10_var <- vcov(int.all)["shade:BAc10", "shade:BAc10"]
shade_BAh10_var <- vcov(int.all)["shade:BAh10", "shade:BAh10"]
shade_con_het_cov <- vcov(int.all)["shade:BAc10", "shade:BAh10"]

coef.diff <- abs(shade_BAc10 - shade_BAh10)
coef.diff.var <- shade_BAc10_var + shade_BAh10_var + 2*(shade_con_het_cov)
2 * qnorm(0.95, coef.diff, sqrt(coef.diff.var))
# 0.013

# myc*con vs. myc*het
myc_BAc10 <- summary(int.all)$coefficients$fixed["BAc10:mycFecm"]
myc_BAh10 <- summary(int.all)$coefficients$fixed["BAh10:mycFecm"]
myc_BAc10_var <- vcov(int.all)["BAc10:mycFecm", "BAc10:mycFecm"]
myc_BAh10_var <- vcov(int.all)["BAh10:mycFecm", "BAh10:mycFecm"]
myc_con_het_cov <- vcov(int.all)["BAc10:mycFecm", "BAh10:mycFecm"]

coef.diff <- abs(myc_BAc10 - myc_BAh10)
coef.diff.var <- myc_BAc10_var + myc_BAh10_var + 2*(myc_con_het_cov)
2 * qnorm(0.95, coef.diff, sqrt(coef.diff.var))
# 0.051


# refit using REML for correct estimates
int.all <- lme(log(growth+1) ~ diam1 + gap + 
                 n_con + n_het +
                 shade*BAc10 + shade*BAh10 +
                 mycF*BAc10 + mycF*BAh10 + mycF*shade,
               random=~1|plot,
               weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                               varExp(form=~BAc10), varExp(form=~BAh10)),
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
                n_con + n_het + BAc10 + BAh10 +
                (1|plot),
              family = binomial(link = "logit"),
              data = surv)

Sadd.shade <- glmer(surv ~ diam1 + gap + 
                      n_con + n_het + BAc10 + BAh10 +
                      shade +
                      (1|plot),
                    family = binomial(link = "logit"),
                    data = surv)

Sadd.myc <- glmer(surv ~ diam1 + gap + 
                    n_con + n_het + BAc10 + BAh10 +
                    mycF +
                    (1|plot),
                  family = binomial(link = "logit"),
                  data = surv)

Sadd.all <- glmer(surv ~ diam1 + gap + 
                    n_con + n_het + BAc10 + BAh10 +
                    shade + mycF + shade*mycF +
                    (1|plot),
                  family = binomial(link = "logit"),
                  data = surv)

Sint.shade <- glmer(surv ~ diam1 + gap + 
                      n_con + n_het +
                      shade*BAc10 + shade*BAh10 +
                      (1|plot),
                    family = binomial(link = "logit"),
                    data = surv)

Sint.myc <- glmer(surv ~ diam1 + gap + 
                    n_con + n_het +
                    mycF*BAc10 + mycF*BAh10 +
                    (1|plot),
                  family = binomial(link = "logit"),
                  data = surv)

Sint.all <- glmer(surv ~ diam1 + gap + 
                    n_con + n_het +
                    shade*BAc10 + shade*BAh10 +
                    mycF*BAc10 + mycF*BAh10 + mycF*shade +
                    (1|plot),
                  family = binomial(link = "logit"),
                  data = surv)

# mod comparison
anova(Snull, Sbio, Sadd.shade, Sadd.myc, Sadd.all, Sint.shade, Sint.myc, Sint.all)
anova(Sadd.all, Sint.all)
# add.all and int.all are best, but no diff'ce - choose Sadd.all


# aggregate results into table for supplementary material
comp.tab10 <- anova(Snull, Sbio, Sadd.shade, Sadd.myc, Sadd.all, Sint.shade, Sint.myc, Sint.all)
comp.tab10 <- as.data.frame(comp.tab10[,"BIC"])
names(comp.tab10) <- "BIC_10"

comp.tab$BIC_10 <- comp.tab10$BIC_10


# test con vs. het differences in best survival model - Sadd.all
n_con <- fixed.effects(Sadd.all)["n_con"]
n_het <- fixed.effects(Sadd.all)["n_het"]
n_con_var <- vcov(Sadd.all)["n_con", "n_con"]
n_het_var <- vcov(Sadd.all)["n_het", "n_het"]
n_con_het_cov <- vcov(Sadd.all)["n_con", "n_het"]

coef.diff <- abs(n_con - n_het)
coef.diff.var <- n_con_var + n_het_var + 2*(n_con_het_cov)
2 * qnorm(0.95, coef.diff, sqrt(coef.diff.var))
# p = 0.192

# BAc vs. BAh
BAc10 <- fixed.effects(Sadd.all)["BAc10"]
BAh10 <- fixed.effects(Sadd.all)["BAh10"]
BAc10_var <- vcov(Sadd.all)["BAc10", "BAc10"]
BAh10_var <- vcov(Sadd.all)["BAh10", "BAh10"]
BAc10_het_cov <- vcov(Sadd.all)["BAc10", "BAh10"]

coef.diff <- abs(BAc10 - BAh10)
coef.diff.var <- BAc10_var + BAh10_var + 2*(BAc10_het_cov)
2 * qnorm(0.95, coef.diff, sqrt(coef.diff.var))
# p = 0.381

summary(Sadd.all)

