
require(effects)
require(ggplot2)
require(sjstats)
require(gridExtra)
require(cowplot)


# MAIN EFFECT OF ADULT DENSITY

ef <- effect("BAc5", s_bio_ch5)
c <- as.data.frame(ef)

ef <- effect("BAh5", s_bio_ch5)
d <- as.data.frame(ef)

ef <- effect("BAc5", int_ch5.REML)
a <- as.data.frame(ef)

ef <- effect("BAh5", int_ch5.REML)
b <- as.data.frame(ef)


# PLOTTING SURVIVAL
s_c <- ggplot(c, aes(x=BAc5, y=fit)) +
  scale_y_continuous(limits=c(0.25, 1)) +
  scale_x_continuous(breaks=seq(0, 6, 2), name="\nConspecific basal area") +
  coord_cartesian(ylim=c(0.25, 1)) +
  coord_cartesian(xlim=c(-1, 6)) +
  
  geom_ribbon(inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAc5),
              alpha=0.2, linetype=1, size=1, color="gray10", fill="white") +
  geom_line(size=1, color="gray10") +

  scale_fill_manual(values="gray5") +
  theme_classic() +
  theme(
    axis.text.y=element_text(size=18),
    axis.title = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    plot.margin = unit(c(0,0,5,5.6), "mm"))


s_h <- ggplot(d, aes(x=BAh5, y=fit)) +
  scale_y_continuous(limits=c(0.25, 1)) +
  scale_x_continuous(breaks=seq(0, 6, 2), name="\nHeterospecific basal area") +
  coord_cartesian(ylim=c(0.25, 1)) +
  coord_cartesian(xlim=c(-1, 6)) +
  
  geom_ribbon(inherit.aes=FALSE,
              data=d, aes(ymin=lower, ymax=upper, x=BAh5),
              alpha=0.2, linetype=1, size=1, color="gray10", fill="white") +
  geom_line(size=1, color="gray10") +
  
  scale_fill_manual(values="gray5") +
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "gray90",
                                    colour = "gray90"),
    plot.margin = unit(c(0,5,5,3), "mm"))



# PLOTTING GROWTH
g_c <- ggplot(a, aes(x=BAc5, y=fit)) +
  scale_y_continuous(limits=c(-0.08, 0.15)) +
  scale_x_continuous(breaks=seq(0, 6, 2), name="\nConspecific basal area") +
  coord_cartesian(ylim=c(-0.08, 0.15)) +
  coord_cartesian(xlim=c(-1, 6)) +
  
  geom_ribbon(inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAc5),
              alpha=0.2, linetype=1, size=1, color="gray10", fill="white") +
  geom_line(size=1, color="gray10") +

  scale_fill_manual(values="gray5") +
  theme_classic() +
  theme(
    axis.text=element_text(size=18),
    axis.title.x=element_text(size=18),
    axis.title.y = element_blank(),
    plot.margin = unit(c(-6,0,0,0), "mm"))


g_h <- ggplot(b, aes(x=BAh5, y=fit)) +
  scale_y_continuous(limits=c(-0.08, 0.15), name="Log(Sapling growth rate) \n") +
  scale_x_continuous(breaks=seq(0, 6, 2), name="\nHeterospecific basal area") +
  coord_cartesian(ylim=c(-0.08, 0.15)) +
  coord_cartesian(xlim=c(-1, 6)) +
  
  geom_ribbon(inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAh5),
              alpha=0.2, linetype=1, size=1, color="gray10", fill="white") +
  geom_line(size=1, color="gray10") +
  
  scale_fill_manual(values="gray5") +
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "gray90",
                                    colour = "gray90"),
    plot.margin = unit(c(-6,5,0,3), "mm"))


plot_grid(s_c, s_h, g_c, g_h, ncol = 2, nrow = 2)





# DENSITY * SHADE TOLERANCE

ef <- effect("shade:BAc10", s_int_ch10)
x <- as.data.frame(ef)

ef <- effect("shade:BAh10", s_int_ch10)
y <- as.data.frame(ef)

ef <- effect("shade:BAc10", int_ch10)
e <- as.data.frame(ef)

ef <- effect("shade:BAh10", int_ch10)
f <- as.data.frame(ef)


# SURVIVAL PLOTS
s_c_sh <- ggplot(x, aes(x=BAc10, y=fit)) +
 scale_y_continuous(limits=c(0.25, 1)) +
 scale_x_continuous(breaks=seq(0, 6, 2)) +
  coord_cartesian(ylim=c(0.25, 1)) +
  coord_cartesian(xlim=c(-1, 6)) +
  
  geom_ribbon(data=x[x$shade==-2, ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAc10),
              alpha=0.2, linetype=1, size=1, color="gray30", fill="white") +
  geom_ribbon(data=x[x$shade==1, ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAc10),
              alpha=0.2, linetype=5, size=1, color="gray10", fill="white") +
  
  geom_line(data=x[x$shade==-2, ], size=1, color="gray30") +
  geom_line(data=x[x$shade==1, ], size=1, color="gray10") +

  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.title=element_blank(),
    plot.margin = unit(c(0,0,5,3.3), "mm"))



s_h_sh <- ggplot(y, aes(x=BAh10, y=fit)) +
  scale_y_continuous(limits=c(0.25, 1)) +
  scale_x_continuous(limits=c(-3, 4)) +
  coord_cartesian(ylim=c(0.25, 1)) +
  coord_cartesian(xlim=c(-3, 4)) +
  
  geom_ribbon(data=y[y$shade==-2, ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAh10),
              alpha=0.2, linetype=1, size=1, color="gray30", fill="white") +
  geom_ribbon(data=y[y$shade==1, ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAh10),
              alpha=0.2, linetype=5, size=1, color="gray10", fill="white") +
  
  geom_line(data=y[y$shade==-2, ], size=1, color="gray30") +
  geom_line(data=y[y$shade==1, ], size=1, color="gray10") +
  
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_blank(),
    axis.title=element_blank(),
    panel.background = element_rect(fill = "gray90",
                                    colour = "gray90"),
    plot.margin = unit(c(0,5,5,3), "mm"))




# GROWTH PLOTS

g_c_sh <- ggplot(e, aes(x=BAc10, y=fit)) +
  scale_y_continuous(limits=c(-0.08, 0.15)) +
  scale_x_continuous(breaks=seq(0, 6, 2), name = "\nConspecific basal area") +
  coord_cartesian(ylim=c(-0.08, 0.15)) +
  coord_cartesian(xlim=c(-1, 6)) +
  
  geom_ribbon(data=e[e$shade==-2, ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAc10),
              alpha=0.2, linetype=1, size=1, color="gray30", fill="white") +
  geom_ribbon(data=e[e$shade==1, ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAc10),
              alpha=0.2, linetype=5, size=1, color="gray10", fill="white") +
  
  geom_line(data=e[e$shade==-2, ], size=1, color="gray30") +
  geom_line(data=e[e$shade==1, ], size=1, color="gray10") +
  
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    axis.text.x = element_text(size=18),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=18),
    plot.margin = unit(c(-6,0,0,0.5), "mm"))



g_h_sh <- ggplot(f, aes(x=BAh10, y=fit)) +
  scale_y_continuous(limits=c(-0.08, 0.15)) +
  scale_x_continuous(limits=c(-3, 4), name = "\nHeterospecific basal area") +
  coord_cartesian(ylim=c(-0.08, 0.15)) +
  coord_cartesian(xlim=c(-3, 4)) +
  
  geom_ribbon(data=f[f$shade==-2, ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAh10),
              alpha=0.2, linetype=1, size=1, color="gray30", fill="white") +
  geom_ribbon(data=f[f$shade==1, ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAh10),
              alpha=0.2, linetype=5, size=1, color="gray10", fill="white") +
  
  geom_line(data=f[f$shade==-2, ], size=1, color="gray30") +
  geom_line(data=f[f$shade==1, ], size=1, color="gray10") +
  
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.title.y=element_blank(),
    axis.title.x = element_text(size=18),
    panel.background = element_rect(fill = "gray90",
                                    colour = "gray90"),
    plot.margin = unit(c(-6,5,0,3), "mm"))

plot_grid(s_c_sh, s_h_sh, g_c_sh, g_h_sh, ncol = 2, nrow = 2)




# DENSITY * MYC TYPE

ef <- effect("mycF:BAc5", s_int_ch5)
g <- as.data.frame(ef)

ef <- effect("mycF:BAh5", s_int_ch5)
h <- as.data.frame(ef)

ef <- effect("mycF:BAc5", int_ch5)
i <- as.data.frame(ef)

ef <- effect("mycF:BAh5", int_ch5)
j <- as.data.frame(ef)


# SURVIVAL PLOTS
s_c_myc <- ggplot(g, aes(x=BAc5, y=fit)) +
  scale_y_continuous(limits=c(0.25, 1)) +
  scale_x_continuous(breaks=seq(0, 6, 2)) +
  coord_cartesian(ylim=c(0.25, 1)) +
  coord_cartesian(xlim=c(-1, 6)) +
  
  geom_ribbon(data=g[g$mycF=="am", ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAc5),
              alpha=0.2, linetype=1, size=1, color="gray30", fill="white") +
  geom_ribbon(data=g[g$mycF=="ecm", ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAc5),
              alpha=0.2, linetype=5, size=1, color="gray10", fill="white") +
  geom_line(data=g[g$mycF=="am", ], size=1, color="gray30") + 
  geom_line(data=g[g$mycF=="ecm", ], size=1, color="gray10") + 

  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    plot.margin = unit(c(0,0,5,3.3), "mm"))



s_h_myc <- ggplot(h, aes(x=BAh5, y=fit)) +
  scale_y_continuous(limits=c(0.4, 1)) +
  scale_x_continuous(breaks=seq(0, 6, 2)) +
  coord_cartesian(ylim=c(0.4, 1)) +
  coord_cartesian(xlim=c(-1, 6)) +
  
  geom_ribbon(data=h[h$mycF=="am", ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAh5),
              alpha=0.2, linetype=1, size=1, color="gray30", fill="white") +
  geom_ribbon(data=h[h$mycF=="ecm", ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAh5),
              alpha=0.2, linetype=5, size=1, color="gray10", fill="white") +
  geom_line(data=h[h$mycF=="am", ], size=1, color="gray30") + 
  geom_line(data=h[h$mycF=="ecm", ], size=1, color="gray10") + 
  
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_blank(),
    axis.title=element_blank(),
    panel.background = element_rect(fill = "gray90",
                                    colour = "gray90"),
    plot.margin = unit(c(0,5,5,3), "mm"))




# GROWTH PLOTS

g_c_myc <- ggplot(i, aes(x=BAc5, y=fit)) +
  scale_y_continuous(limits=c(-0.08, 0.15)) +
  scale_x_continuous(breaks=seq(0, 6, 2), name = "\nConspecific basal area") +
  coord_cartesian(ylim=c(-0.08, 0.15)) +
  coord_cartesian(xlim=c(-1, 6)) +
  
  geom_ribbon(data=i[i$mycF=="am", ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAc5),
              alpha=0.2, linetype=1, size=1, color="gray30", fill="white") +
  geom_ribbon(data=i[i$mycF=="ecm", ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAc5),
              alpha=0.2, linetype=5, size=1, color="gray10", fill="white") +
  
  geom_line(data=i[i$mycF=="am", ], size=1, color="gray30") +
  geom_line(data=i[i$mycF=="ecm", ], size=1, color="gray10") +
  
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    axis.title.y=element_blank(),
    axis.text.x = element_text(size=18),
    axis.title.x=element_text(size=18),
    plot.margin = unit(c(-6,0,0,0.5), "mm"))



g_h_myc <- ggplot(j, aes(x=BAh5, y=fit)) +
  scale_y_continuous(limits=c(-0.08, 0.15)) +
  scale_x_continuous(breaks=seq(0, 6, 2), name = "\nHeterospecific basal area") +
  coord_cartesian(ylim=c(-0.08, 0.15)) +
  coord_cartesian(xlim=c(-1, 6)) +
  
  geom_ribbon(data=j[j$mycF=="am", ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAh5),
              alpha=0.2, linetype=1, size=1, color="gray30", fill="white") +
  geom_ribbon(data=j[j$mycF=="ecm", ], inherit.aes=FALSE,
              aes(ymin=lower, ymax=upper, x=BAh5),
              alpha=0.2, linetype=5, size=1, color="gray10", fill="white") +
  
  geom_line(data=j[j$mycF=="am", ], size=1, color="gray30") +
  geom_line(data=j[j$mycF=="ecm", ], size=1, color="gray10") +
  
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.title.y=element_blank(),
    axis.title.x = element_text(size=18),
    panel.background = element_rect(fill = "gray90",
                                    colour = "gray90"),
    plot.margin = unit(c(-6,5,0,3), "mm"))

plot_grid(s_c_myc, s_h_myc, g_c_myc, g_h_myc, ncol = 2, nrow = 2)



plot_grid(s_c, s_h, s_c_sh, s_h_sh, s_c_myc, s_h_myc, 
          g_c, g_h, g_c_sh, g_h_sh, g_c_myc, g_h_myc, ncol=6, nrow=2)



# PLOTTING MODEL COEFFICIENTS AS DOT AND WHISKERS
# require(sjstats)
# require(gridExtra)
# require(cowplot)

# int_ch5.REML <- lmer(log(growth+1) ~ diam1 + gap + shade*mycF +
#                        n_con + n_het + shade*BAc5 + shade*BAh5 + mycF*BAc5 + mycF*BAh5 +
#                        (1|plot),
#                      REML = TRUE,
#                      data = growth)
tabt <- std_beta(int_ch5.REML)

level_order <- data.frame(tabt$term)
level_order$order <- c(13, 12, 11, 10, 8, 7, 6, 5, 9, 4, 3, 2, 1)
level_order$order <- as.integer(level_order$order)
level_order <- level_order %>% arrange(order)
level_order <- level_order$tabt.term

gr <- ggplot(tabt, aes(x = factor(term, level = level_order), y = std.estimate)) +
  scale_y_continuous(name ="") +
  scale_x_discrete(name ="", labels=c("diam1" = "Diameter", "gap" = "Light", "shade" = "Shade tol",
                                      "mycFecm" = "Myc type (ECM)", "n_con" = "Sap count (con)",
                                      "n_het" = "Sap count (het)", "BAc5" = "Adult BA (con)",
                                      "BAh5" = "Adult BA (het)", "shade:mycFecm" = "Shade : Myc type",
                                      "shade:BAc5" = "Shade : Adult BA (con)",
                                      "shade:BAh5" = "Shade : Adult BA (het)",
                                      "mycFecm:BAc5" = "Myc : Adult BA (con)",
                                      "mycFecm:BAh5" = "Myc : Adult BA (het)")) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 16) + 
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), width = 0, size = 1) +
  theme_classic() + 
  theme(
    #    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size=18),
    axis.title = element_text(size=18))



# s_bio_ch5 <- glmer(surv ~ diam1 + gap + shade*mycF +
#                    n_con + n_het + BAc5 + BAh5 +
#                   (1|plot),
#                 family = binomial(link = "logit"),
#                 data = surv)tabt <- std_beta(int_ch5.REML)
tabs <- std_beta(s_bio_ch5)

level_order <- data.frame(tabs$term)
level_order$order <- c(1:4, 6:9, 5)
level_order$order <- as.integer(level_order$order)
level_order <- level_order %>% arrange(desc(order))
level_order <- level_order$tabs.term

su <- ggplot(tabs, aes(x = factor(term, level = level_order), y = std.estimate)) +
  scale_y_continuous(name ="") +
  scale_x_discrete(name ="", labels=c("diam1" = "Diameter", "gap" = "Light", "shade" = "Shade tol",
                                      "mycFecm" = "Myc type (ECM)", "n_con" = "Sap count (con)",
                                      "n_het" = "Sap count (het)", "BAc5" = "Adult BA (con)",
                                      "BAh5" = "Adult BA (het)", "shade:mycFecm" = "Shade : Myc type")) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 16) + 
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), width = 0, size = 1) +
  expand_limits(x = -3.5) +
  theme_classic() + 
  theme(
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=18),
    axis.title = element_text(size=18),
    panel.background = element_rect(fill = "gray90",
                                    colour = "gray90"))



plot_grid(gr, su, align = "h", nrow = 1, rel_widths = c(2, 1))




# 15m scale

# int_ch15.REML <- lmer(log(growth+1) ~ diam1 + gap + shade*mycF +
#                        n_con + n_het + shade*BAc15 + shade*BAh15 + mycF*BAc15 + mycF*BAh15 +
#                        (1|plot),
#                      REML = TRUE,
#                      data = growth)
tabt <- std_beta(int_ch15.REML)

level_order <- data.frame(tabt$term)
level_order$order <- c(13, 12, 11, 10, 8, 7, 6, 5, 9, 4, 3, 2, 1)
level_order$order <- as.integer(level_order$order)
level_order <- level_order %>% arrange(order)
level_order <- level_order$tabt.term

gr <- ggplot(tabt, aes(x = factor(term, level = level_order), y = std.estimate)) +
  scale_y_continuous(name ="") +
  scale_x_discrete(name ="", labels=c("diam1" = "Diameter", "gap" = "Light", "shade" = "Shade tol",
                                      "mycFecm" = "Myc type (ECM)", "n_con" = "Sap count (con)",
                                      "n_het" = "Sap count (het)", "BAc15" = "Adult BA (con)",
                                      "BAh15" = "Adult BA (het)", "shade:mycFecm" = "Shade : Myc type",
                                      "shade:BAc15" = "Shade : Adult BA (con)",
                                      "shade:BAh15" = "Shade : Adult BA (het)",
                                      "mycFecm:BAc15" = "Myc : Adult BA (con)",
                                      "mycFecm:BAh15" = "Myc : Adult BA (het)")) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 16) + 
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), width = 0, size = 1) +
  theme_classic() + 
  theme(
    #    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size=18),
    axis.title = element_text(size=18))



# s_int_ch15 <- glmer(surv ~ diam1 + gap + shade*mycF +
#                       n_con + n_het + shade*BAc15 + shade*BAh15 + mycF*BAc15 + mycF*BAh15 +
#                      (1|plot),
#                    family = binomial(link = "logit"),
#                    data = surv)
tabs <- std_beta(s_int_ch15)

level_order <- data.frame(tabs$term)
level_order$order <- c(13, 12, 11, 10, 8, 7, 6, 5, 9, 4, 3, 2, 1)
level_order$order <- as.integer(level_order$order)
level_order <- level_order %>% arrange(desc(order))
level_order <- level_order$tabs.term

su <- ggplot(tabs, aes(x = factor(term, level = level_order), y = std.estimate)) +
  scale_y_continuous(name ="", limits=c(-1, 1)) +
  scale_x_discrete(name ="", labels=c("diam1" = "Diameter", "gap" = "Light", "shade" = "Shade tol",
                                      "mycFecm" = "Myc type (ECM)", "n_con" = "Sap count (con)",
                                      "n_het" = "Sap count (het)", "BAc15" = "Adult BA (con)",
                                      "BAh15" = "Adult BA (het)", "shade:mycFecm" = "Shade : Myc type",
                                      "shade:BAc15" = "Shade : Adult BA (con)",
                                      "shade:BAh15" = "Shade : Adult BA (het)",
                                      "mycFecm:BAc15" = "Myc : Adult BA (con)",
                                      "mycFecm:BAh15" = "Myc : Adult BA (het)")) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 1) + 
  coord_flip() +
  geom_point(size = 4, shape = 16) + 
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), width = 0, size = 1) +
  theme_classic() + 
  theme(
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=18),
    axis.title = element_text(size=18),
    panel.background = element_rect(fill = "gray90",
                                    colour = "gray90"))



plot_grid(gr, su, align = "h", nrow = 1, rel_widths = c(2, 1))
