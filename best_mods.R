
require(nlme)
require(lme4)
require(dplyr)
require(gstat)

# data prep
dat <- read.csv("even_size_class_data.csv", stringsAsFactors = FALSE)

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


surv <- dat[!is.na(dat$shade),]
surv <- surv[surv$myc=="am" | surv$myc=="ecm", ]  # only keep ecm vs. am saplings
scaled_vars <- c("diam1", "n_con", "n_het", "BAc5", "BAh5", "BAc10", "BAh10", 
                 "BAc15", "BAh15", "BAc20", "BAh20", "gap", "shade")
surv[, scaled_vars] <- scale(surv[, scaled_vars])  # scale numeric variables
surv %>% select(scaled_vars) %>% summarise_all(funs(mean))  # check your work
surv %>% select(scaled_vars) %>% summarise_all(funs(sd))
surv$mycF <- factor(surv$myc)


# BEST MODELS
# 5m scale
int.all5 <- lme(log(growth+1) ~ diam1 + gap + 
                 n_con + n_het +
                 shade*BAc5 + shade*BAh5 +
                 mycF*BAc5 + mycF*BAh5 + mycF*shade,
               random=~1|plot,
               weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                               varExp(form=~BAc5), varExp(form=~BAh5)),
               method="REML",
               data = growth)

Sadd.all5 <- glmer(surv ~ diam1 + gap + 
                    n_con + n_het + BAc5 + BAh5 +
                    shade + mycF + shade*mycF +
                    (1|plot),
                  family = binomial(link = "logit"),
                  data = surv)

# 10m scale
int.all10 <- lme(log(growth+1) ~ diam1 + gap + 
                 n_con + n_het +
                 shade*BAc10 + shade*BAh10 +
                 mycF*BAc10 + mycF*BAh10 + mycF*shade,
               random=~1|plot,
               weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                               varExp(form=~BAc10), varExp(form=~BAh10)),
               method="REML",
               data = growth)

Sadd.all10 <- glmer(surv ~ diam1 + gap + 
                    n_con + n_het + BAc10 + BAh10 +
                    shade + mycF + shade*mycF +
                    (1|plot),
                  family = binomial(link = "logit"),
                  data = surv)

#15m scale
int.all15 <- lme(log(growth+1) ~ diam1 + gap + 
                 n_con + n_het +
                 shade*BAc15 + shade*BAh15 +
                 mycF*BAc15 + mycF*BAh15 + mycF*shade,
               random=~1|plot,
               weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                               varExp(form=~BAc15), varExp(form=~BAh15)),
               method="REML",
               data = growth)

Sadd.all15 <- glmer(surv ~ diam1 + gap + 
                    n_con + n_het + BAc15 + BAh15 +
                    shade + mycF + shade*mycF +
                    (1|plot),
                  family = binomial(link = "logit"),
                  data = surv)

# 20m scale
int.all20 <- lme(log(growth+1) ~ diam1 + gap + 
                 n_con + n_het +
                 shade*BAc20 + shade*BAh20 +
                 mycF*BAc20 + mycF*BAh20 + mycF*shade,
               random=~1|plot,
               weights=varComb(varExp(form=~n_con),varExp(form=~n_het),
                               varExp(form=~BAc20), varExp(form=~BAh20)),
               method="REML",
               data = growth)

Sadd.all20 <- glmer(surv ~ diam1 + gap + 
                    n_con + n_het + BAc20 + BAh20 +
                    shade + mycF + shade*mycF +
                    (1|plot),
                  family = binomial(link = "logit"),
                  data = surv)


# TESTING FOR SPATIAL AUTOCORRELATION
g5.vgm <- variogram(resid(int.all5)~1, loc= ~X+Y, data=growth)
g10.vgm <- variogram(resid(int.all10)~1, loc= ~X+Y, data=growth)
g15.vgm <- variogram(resid(int.all15)~1, loc= ~X+Y, data=growth)
g20.vgm <- variogram(resid(int.all20)~1, loc= ~X+Y, data=growth)
plot(g5.vgm, main="Full growth model, 5m scale")

s5.vgm <- variogram(resid(Sadd.all5)~1, loc= ~X+Y, data=surv)
s10.vgm <- variogram(resid(Sadd.all10)~1, loc= ~X+Y, data=surv)
s15.vgm <- variogram(resid(Sadd.all15)~1, loc= ~X+Y, data=surv)
s20.vgm <- variogram(resid(Sadd.all20)~1, loc= ~X+Y, data=surv)
plot(s5.vgm, main="Additive survival model, 5m scale")



# SUMMARY TABLES
tab5 <- data.frame(summary(int.all5)$tTable)
tab5$Predictor <- rownames(tab5)
rownames(tab5) <- NULL
tab5 <- tab5[,c("Predictor","Value","Std.Error","t.value","p.value")]
colnames(tab5) <- c("Predictor","Coefficient","Std error","t","p")
tab5[,2:3] <- round(tab5[,2:3],digits=4)
tab5$t <- round(tab5$t,digits=2)
tab5$p <- round(tab5$p,digits=3)

tab10 <- data.frame(summary(int.all10)$tTable)
tab10$Predictor <- rownames(tab10)
rownames(tab10) <- NULL
tab10 <- tab10[,c("Predictor","Value","Std.Error","t.value","p.value")]
colnames(tab10) <- c("Predictor","Coefficient","Std error","t","p")
tab10[,2:3] <- round(tab10[,2:3],digits=4)
tab10$t <- round(tab10$t,digits=2)
tab10$p <- round(tab10$p,digits=3)

tab15 <- data.frame(summary(int.all15)$tTable)
tab15$Predictor <- rownames(tab15)
rownames(tab15) <- NULL
tab15 <- tab15[,c("Predictor","Value","Std.Error","t.value","p.value")]
colnames(tab15) <- c("Predictor","Coefficient","Std error","t","p")
tab15[,2:3] <- round(tab15[,2:3],digits=4)
tab15$t <- round(tab15$t,digits=2)
tab15$p <- round(tab15$p,digits=3)

tab20 <- data.frame(summary(int.all20)$tTable)
tab20$Predictor <- rownames(tab20)
rownames(tab20) <- NULL
tab20 <- tab20[,c("Predictor","Value","Std.Error","t.value","p.value")]
colnames(tab20) <- c("Predictor","Coefficient","Std error","t","p")
tab20[,2:3] <- round(tab20[,2:3],digits=4)
tab20$t <- round(tab20$t,digits=2)
tab20$p <- round(tab20$p,digits=3)

tab20 <- tab20[,-1]
tab15 <- tab15[,-1]
tab10 <- tab10[,-1]
tabl <- cbind(tab5,tab10,tab15,tab20)
write.csv(tabl,"summary_table_growth_mods.csv", row.names=FALSE)




tab5 <- data.frame(coef(summary(Sadd.all5)))
tab5$Predictor <- rownames(tab5)
rownames(tab5) <- NULL
tab5 <- tab5[,c(5, 1:4)]
colnames(tab5) <- c("Predictor","Coefficient","Std error","z","p")
tab5[,2:3] <- round(tab5[,2:3],digits=4)
tab5$z <- round(tab5$z,digits=2)
tab5$p <- round(tab5$p,digits=3)

tab10 <- data.frame(coef(summary(Sadd.all10)))
tab10$Predictor <- rownames(tab10)
rownames(tab10) <- NULL
tab10 <- tab10[,c(5, 1:4)]
colnames(tab10) <- c("Predictor","Coefficient","Std error","z","p")
tab10[,2:3] <- round(tab10[,2:3],digits=4)
tab10$z <- round(tab10$z,digits=2)
tab10$p <- round(tab10$p,digits=3)

tab15 <- data.frame(coef(summary(Sadd.all15)))
tab15$Predictor <- rownames(tab15)
rownames(tab15) <- NULL
tab15 <- tab15[,c(5, 1:4)]
colnames(tab15) <- c("Predictor","Coefficient","Std error","z","p")
tab15[,2:3] <- round(tab15[,2:3],digits=4)
tab15$z <- round(tab15$z,digits=2)
tab15$p <- round(tab15$p,digits=3)

tab20 <- data.frame(coef(summary(Sadd.all20)))
tab20$Predictor <- rownames(tab20)
rownames(tab20) <- NULL
tab20 <- tab20[,c(5, 1:4)]
colnames(tab20) <- c("Predictor","Coefficient","Std error","z","p")
tab20[,2:3] <- round(tab20[,2:3],digits=4)
tab20$z <- round(tab20$z,digits=2)
tab20$p <- round(tab20$p,digits=3)

tab20 <- tab20[,-1]
tab15 <- tab15[,-1]
tab10 <- tab10[,-1]
tabl <- cbind(tab5,tab10,tab15,tab20)
write.csv(tabl,"summary_table_surv_mods.csv", row.names=FALSE)

