# First approach (aditional nodes)

# Define functions

# library(bnlearn)
library(gRain)
# library(dplyr) # for `rename`
library(ggplot2)
library(cowplot)
# library(grid)
# library(gridExtra)
# library(ggpubr)
# library(RColorBrewer)
library(reshape2)


bnl_lp1 = function(a,b,c,d,e,f,g,h,i,j,k) {
  #
  # a pr(A2 = 'F')
  # b pr(B0 = 'F')
  # c pr(B2 = 'F' | A2='F', B0='F')
  # d pr(B2 = 'F' | A2='F', B0='C')
  # e pr(B2 = 'F' | A2='C', B0='F')
  # f pr(B2 = 'F' | A2='C', B0='C')
  # g pr(C0 = 'F')
  # h pr(D2 = 'F' | B2='F', C0='F')
  # i pr(D2 = 'F' | B2='F', C0='C')
  # j pr(D2 = 'F' | B2='C', C0='F')
  # k pr(D2 = 'F' | B2='C', C0='C')
  #
  fc <- c("Failure","Correct")
  Astp = cptable(~A2, values=c(a,1-a),levels=fc)
  B0stp = cptable(~B0, values=c(b,1-b),levels=fc)
  Bstp = cptable(~B2+A2+B0, values=c(c,1-c,d,1-d,e,1-e,f,1-f),levels=fc)
  C0stp = cptable(~C0, values=c(g,1-g),levels=fc)
  Cstp = cptable(~C2+B2+C0, values=c(h,1-h,i,1-i,j,1-j,k,1-k),levels=fc)
  cpts = compileCPT(list(Astp,B0stp,Bstp,C0stp,Cstp))
  bnl.stp = grain(cpts)
  return(list('net'=bnl.stp, 'parms'=c(a,b,c,d,e,f,g,h,i,j,k)))
}


# Simulations
t =0.05
tbs=c(0.7, 0.8, 0.9, 0.95)
t1s=seq(0.01,1,by=0.02)

prb=c()
results_PB <- matrix(0,ncol=length(tbs),nrow=length(t1s))

# Element A (j=1) failing at level 'l'
idx <- 1
for (tb in tbs) {
  for (i in 1:length(t1s)) {
    bnlp1 = bnl_lp1((1-t1s[i]),t,tb,.5,.5,(1-tb),t,tb,.5,.5,(1-tb))[['net']]
    bnlp1c= compile(bnlp1)
    nodes=c('A2','B0', 'B2', 'C0', 'C2')
    def  =c('','','','','Correct')
    sev3 = setEvidence(object=bnlp1c,nodes=nodes,states=def)
    prb[i]=pEvidence(sev3)
  }
  results_PB[,idx] <- prb
  idx <- idx+1
}

data_1 <- as.data.frame(results_PB)
data_1$t1 <- t1s
data_plot_1 <- melt(data_1,id.var="t1")


# Element B (j=2) failing at level 'l'
idx <- 1
for (tb in tbs) {
  for (i in 1:length(t1s)) {
    bnlp1 = bnl_lp1(t,(1-t1s[i]),tb,.5,.5,(1-tb),t,tb,.5,.5,(1-tb))[['net']]
    bnlp1c= compile(bnlp1)
    nodes=c('A2','B0', 'B2', 'C0', 'C2')
    def  =c('','','','','Correct')
    sev3 = setEvidence(object=bnlp1c,nodes=nodes,states=def)
    prb[i]=pEvidence(sev3)
  }
  results_PB[,idx] <- prb
  idx <- idx+1
}

data_2 <- as.data.frame(results_PB)
data_2$t1 <- t1s
data_plot_2 <- melt(data_2,id.var="t1")


# Element C (j=3) failing at level 'l'
idx <- 1
for (tb in tbs) {
  for (i in 1:length(t1s)) {
    bnlp1 = bnl_lp1(t,t,tb,.5,.5,(1-tb),(1-t1s[i]),tb,.5,.5,(1-tb))[['net']]
    bnlp1c= compile(bnlp1)
    nodes=c('A2','B0', 'B2', 'C0', 'C2')
    def  =c('','','','','Correct')
    sev3 = setEvidence(object=bnlp1c,nodes=nodes,states=def)
    prb[i]=pEvidence(sev3)
  }
  results_PB[,idx] <- prb
  idx <- idx+1
}

data_3 <- as.data.frame(results_PB)
data_3$t1 <- t1s
data_plot_3 <- melt(data_3,id.var="t1")

# Elements A & B (j={1,2}) failing at level 'l'
idx <- 1
for (tb in tbs) {
  for (i in 1:length(t1s)) {
    bnlp1 = bnl_lp1((1-t1s[i]),(1-t1s[i]),tb,.5,.5,(1-tb),t,tb,.5,.5,(1-tb))[['net']]
    bnlp1c= compile(bnlp1)
    nodes=c('A2','B0', 'B2', 'C0', 'C2')
    def  =c('','','','','Correct')
    sev3 = setEvidence(object=bnlp1c,nodes=nodes,states=def)
    prb[i]=pEvidence(sev3)
  }
  results_PB[,idx] <- prb
  idx <- idx+1
}

data_12 <- as.data.frame(results_PB)
data_12$t1 <- t1s
data_plot_12 <- melt(data_12,id.var="t1")


# Elements A & C (j={1,3}) failing at level 'l'
idx <- 1
for (tb in tbs) {
  for (i in 1:length(t1s)) {
    bnlp1 = bnl_lp1((1-t1s[i]),t,tb,.5,.5,(1-tb),(1-t1s[i]),tb,.5,.5,(1-tb))[['net']]
    bnlp1c= compile(bnlp1)
    nodes=c('A2','B0', 'B2', 'C0', 'C2')
    def  =c('','','','','Correct')
    sev3 = setEvidence(object=bnlp1c,nodes=nodes,states=def)
    prb[i]=pEvidence(sev3)
  }
  results_PB[,idx] <- prb
  idx <- idx+1
}

data_13 <- as.data.frame(results_PB)
data_13$t1 <- t1s
data_plot_13 <- melt(data_13,id.var="t1")

# Elements B & C (j={2,3}) failing at level 'l'
idx <- 1
for (tb in tbs) {
  for (i in 1:length(t1s)) {
    bnlp1 = bnl_lp1(t,(1-t1s[i]),tb,.5,.5,(1-tb),(1-t1s[i]),tb,.5,.5,(1-tb))[['net']]
    bnlp1c= compile(bnlp1)
    nodes=c('A2','B0', 'B2', 'C0', 'C2')
    def  =c('','','','','Correct')
    sev3 = setEvidence(object=bnlp1c,nodes=nodes,states=def)
    prb[i]=pEvidence(sev3)
  }
  results_PB[,idx] <- prb
  idx <- idx+1
}

data_23 <- as.data.frame(results_PB)
data_23$t1 <- t1s
data_plot_23 <- melt(data_23,id.var="t1")


# Elements A, B & C (j={1,2,3}) failing at level 'l'
idx <- 1
for (tb in tbs) {
  for (i in 1:length(t1s)) {
    bnlp1 = bnl_lp1((1-t1s[i]),(1-t1s[i]),tb,.5,.5,(1-tb),(1-t1s[i]),tb,.5,.5,(1-tb))[['net']]
    bnlp1c= compile(bnlp1)
    nodes=c('A2','B0', 'B2', 'C0', 'C2')
    def  =c('','','','','Correct')
    sev3 = setEvidence(object=bnlp1c,nodes=nodes,states=def)
    prb[i]=pEvidence(sev3)
  }
  results_PB[,idx] <- prb
  idx <- idx+1
}

data_123 <- as.data.frame(results_PB)
data_123$t1 <- t1s
data_plot_123 <- melt(data_123,id.var="t1")

# Plot
ncols=3
nrows=3
id_plots <- vector(mode="list", length = ncols*nrows)

my_cols <- c(rgb(0.4, 0.7607843137254902, 0.6470588235294118), rgb(0.9882352941176471, 0.5529411764705883, 0.3843137254901961), rgb(0.5529411764705883, 0.6274509803921569, 0.796078431372549),rgb(0.9058823529411765, 0.5411764705882353, 0.76470588235294))
plot_ylim <- c(0,1)

plot_ylab <- expression("Values of "~italic(P)(~psi[alpha[paste(italic(j)~',3')]]^paste(italic(l)~'+1')==~0)~" at level "~ italic(l)+1)
plot_xlab <-expression("Values of "~italic(P)(~psi[alpha[paste(italic(j)~',2')]]^italic(l)==~0)~" at level "~ italic(l))


idplot <- 1
idx_title <- expression("One element "~(italic(j)==1)~" failing at level "~italic(l))
id_plots[[idplot]] <- ggplot(data=data_plot_1, aes(x=t1, y=value)) +
  geom_line(aes(colour=variable),size=0.9) +
  ggtitle(idx_title) +
  geom_text(x=0.09, y =0.928, label="(a)", size=5, fontface='plain') +
  scale_y_continuous(name=plot_ylab, limits = plot_ylim, expand = c(0, 0),breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_x_continuous(name=plot_xlab, expand = c(0, 0),breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_color_manual(values=my_cols) +
  theme(plot.margin=margin(0.3,0.1,0,0.1,"cm"),
        strip.text.x = element_text(size=12, face="bold",color="black"),
        strip.text.y = element_text(size=12, face="bold",color="black"),
        plot.title = element_text(size=12, face="bold",color="black",hjust=0.5, margin=margin(0,0,1,0)),
        # plot.background = element_rect(fill="white", color="gray"),
        strip.background = element_rect(fill="white", color="gray"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=10, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, color="black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.spacing.x = unit(0,"lines"),
        panel.spacing.y = unit(0,"lines"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank())

idplot <- 2
idx_title <- expression("One element "~(italic(j)==2)~" failing at level "~italic(l))
id_plots[[idplot]] <- ggplot(data=data_plot_2, aes(x=t1, y=value)) +
  geom_line(aes(colour=variable),size=0.9) +
  ggtitle(idx_title) +
  geom_text(x=0.09, y =0.92, label="(b)", size=5, fontface='plain') +
  scale_y_continuous(name=plot_ylab, limits = plot_ylim, expand = c(0, 0),breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_x_continuous(name=plot_xlab, expand = c(0, 0),breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_color_manual(values=my_cols) +
  theme(plot.margin=margin(0.3,0.1,0,0.1,"cm"),
        strip.text.x = element_text(size=12, face="bold",color="black"),
        strip.text.y = element_text(size=12, face="bold",color="black"),
        plot.title = element_text(size=12, face="bold",color="black",hjust=0.5, margin=margin(0,0,1,0)),
        # plot.background = element_rect(fill="white", color="gray"),
        strip.background = element_rect(fill="white", color="gray"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.spacing.x = unit(0,"lines"),
        panel.spacing.y = unit(0,"lines"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank())


idplot <- 3
idx_title <- expression("One element "~(italic(j)==3)~" failing at level "~italic(l))
id_plots[[idplot]] <- ggplot(data=data_plot_3, aes(x=t1, y=value)) +
  geom_line(aes(colour=variable),size=0.9) +
  ggtitle(idx_title) +
  geom_text(x=0.09, y =0.92, label="(c)", size=5, fontface='plain') +
  scale_y_continuous(name=plot_ylab, limits = plot_ylim, expand = c(0, 0),breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_x_continuous(name=plot_xlab, expand = c(0, 0),breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_color_manual(values=my_cols) +
  theme(plot.margin=margin(0.3,0.2,0,0.1,"cm"),
        strip.text.x = element_text(size=12, face="bold",color="black"),
        strip.text.y = element_text(size=12, face="bold",color="black"),
        plot.title = element_text(size=12, face="bold",color="black",hjust=0.5, margin=margin(0,0,1,0)),
        # plot.background = element_rect(fill="white", color="gray"),
        strip.background = element_rect(fill="white", color="gray"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.spacing.x = unit(0,"lines"),
        panel.spacing.y = unit(0,"lines"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank())

idplot <- 4
idx_title <- expression("Two elements "~(italic(j)%in%'{1,2}')~" failing at level "~italic(l))
id_plots[[idplot]] <- ggplot(data=data_plot_12, aes(x=t1, y=value)) +
  geom_line(aes(colour=variable),size=0.9) +
  ggtitle(idx_title) +
  geom_text(x=0.09, y =0.92, label="(d)", size=5, fontface='plain') +
  scale_y_continuous(name=plot_ylab, limits = plot_ylim, expand = c(0, 0),breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_x_continuous(name=plot_xlab, expand = c(0, 0),breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_color_manual(values=my_cols) +
  theme(plot.margin=margin(0.3,0.1,1.24,0.1,"cm"),
        strip.text.x = element_text(size=12, face="bold",color="black"),
        strip.text.y = element_text(size=12, face="bold",color="black"),
        plot.title = element_text(size=12, face="bold",color="black",hjust=0.5, margin=margin(0,0,1,0)),
        # plot.background = element_rect(fill="white", color="gray"),
        strip.background = element_rect(fill="white", color="gray"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=10, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, color="black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.spacing.x = unit(0,"lines"),
        panel.spacing.y = unit(0,"lines"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank())

idplot <- 5
idx_title <- expression("Two elements "~(italic(j)%in%'{1,3}')~" failing at level "~italic(l))
id_plots[[idplot]] <- ggplot(data=data_plot_13, aes(x=t1, y=value)) +
  geom_line(aes(colour=variable),size=0.9) +
  ggtitle(idx_title) +
  geom_text(x=0.09, y =0.92, label="(e)", size=5, fontface='plain') +
  scale_y_continuous(name=plot_ylab, limits = plot_ylim, expand = c(0, 0),breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_x_continuous(name=plot_xlab, expand = c(0, 0),breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_color_manual(values=my_cols) +
  theme(plot.margin=margin(0.3,0.1,0,0.1,"cm"),
        strip.text.x = element_text(size=12, face="bold",color="black"),
        strip.text.y = element_text(size=12, face="bold",color="black"),
        plot.title = element_text(size=12, face="bold",color="black",hjust=0.5, margin=margin(0,0,1,0)),
        # plot.background = element_rect(fill="white", color="gray"),
        strip.background = element_rect(fill="white", color="gray"),
        axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size=12, color="black"),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.spacing.x = unit(0,"lines"),
        panel.spacing.y = unit(0,"lines"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank())

idplot <- 6
idx_title <- expression("Two elements "~(italic(j)%in%'{2,3}')~" failing at level "~italic(l))
id_plots[[idplot]] <- ggplot(data=data_plot_23, aes(x=t1, y=value)) +
  geom_line(aes(colour=variable),size=0.9) +
  ggtitle(idx_title) +
  geom_text(x=0.09, y =0.92, label="(f)", size=5, fontface='plain') +
  scale_y_continuous(name=plot_ylab, limits = plot_ylim, expand = c(0, 0),breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_x_continuous(name=plot_xlab, expand = c(0, 0),breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_color_manual(values=my_cols) +
  theme(plot.margin=margin(0.3,0.2,0,0.1,"cm"),
        strip.text.x = element_text(size=12, face="bold",color="black"),
        strip.text.y = element_text(size=12, face="bold",color="black"),
        plot.title = element_text(size=12, face="bold",color="black",hjust=0.5, margin=margin(0,0,1,0)),
        # plot.background = element_rect(fill="white", color="gray"),
        strip.background = element_rect(fill="white", color="gray"),
        axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size=12, color="black"),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.spacing.x = unit(0,"lines"),
        panel.spacing.y = unit(0,"lines"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank())


idplot <- 7
idx_title <- expression("Three elements "~(italic(j)%in%'{1,2,3}')~" failing at level "~italic(l))
id_plots[[idplot]] <- ggplot(data=data_plot_123, aes(x=t1, y=value)) +
  geom_line(aes(colour=variable),size=0.9,show.legend = FALSE) +
  ggtitle(idx_title) +
  geom_text(x=0.09, y =0.92, label="(g)", size=5, fontface='plain') +
  scale_y_continuous(name=plot_ylab, limits = plot_ylim, expand = c(0, 0),breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_x_continuous(name=plot_xlab, expand = c(0, 0),breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_color_manual(values=my_cols) +
  theme(plot.margin=margin(-0.85,0.1,0.1,0.1,"cm"),
        strip.text.x = element_text(size=12, face="bold",color="black"),
        strip.text.y = element_text(size=12, face="bold",color="black"),
        plot.title = element_text(size=12, face="bold",color="black",hjust=0.5, margin=margin(0,0,1,0)),
        # plot.background = element_rect(fill="white", color="gray"),
        strip.background = element_rect(fill="white", color="gray"),
        axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=10, color="black"),
        axis.title.x = element_text(size=12, color="black"),
        axis.title.y = element_text(size=12, color="black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        # legend.position = "none",
        panel.spacing.x = unit(0,"lines"),
        panel.spacing.y = unit(0,"lines"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank())


# Legend labels
legend_11_07 <- expression(atop(italic(P)(~psi[alpha[paste(italic(j)~',3')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',3')]^'*']^paste(italic(l)~'+1')==~1~","~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~1)==italic(P)(~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',2')]^'*']^paste(italic(l)~'+1')==~1~","~psi[alpha[paste(italic(j)~',1')]]^paste(italic(l)~'+1')==~1)~"="~0.7~",",
                                italic(P)(~psi[alpha[paste(italic(j)~',3')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',3')]^'*']^paste(italic(l)~'+1')==~0~","~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~0)==italic(P)(~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',2')]^'*']^paste(italic(l)~'+1')==~0~","~psi[alpha[paste(italic(j)~',1')]]^paste(italic(l)~'+1')==~0)~"="~0.3))

legend_11_08 <- expression(atop(italic(P)(~psi[alpha[paste(italic(j)~',3')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',3')]^'*']^paste(italic(l)~'+1')==~1~","~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~1)==italic(P)(~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',2')]^'*']^paste(italic(l)~'+1')==~1~","~psi[alpha[paste(italic(j)~',1')]]^paste(italic(l)~'+1')==~1)~"="~0.8~",",
                                italic(P)(~psi[alpha[paste(italic(j)~',3')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',3')]^'*']^paste(italic(l)~'+1')==~0~","~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~0)==italic(P)(~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',2')]^'*']^paste(italic(l)~'+1')==~0~","~psi[alpha[paste(italic(j)~',1')]]^paste(italic(l)~'+1')==~0)~"="~0.2))

legend_11_09 <- expression(atop(italic(P)(~psi[alpha[paste(italic(j)~',3')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',3')]^'*']^paste(italic(l)~'+1')==~1~","~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~1)==italic(P)(~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',2')]^'*']^paste(italic(l)~'+1')==~1~","~psi[alpha[paste(italic(j)~',1')]]^paste(italic(l)~'+1')==~1)~"="~0.9~",",
                                italic(P)(~psi[alpha[paste(italic(j)~',3')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',3')]^'*']^paste(italic(l)~'+1')==~0~","~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~0)==italic(P)(~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',2')]^'*']^paste(italic(l)~'+1')==~0~","~psi[alpha[paste(italic(j)~',1')]]^paste(italic(l)~'+1')==~0)~"="~0.1))

legend_11_095 <- expression(atop(italic(P)(~psi[alpha[paste(italic(j)~',3')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',3')]^'*']^paste(italic(l)~'+1')==~1~","~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~1)==italic(P)(~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',2')]^'*']^paste(italic(l)~'+1')==~1~","~psi[alpha[paste(italic(j)~',1')]]^paste(italic(l)~'+1')==~1)~"="~0.95~",",
                                 italic(P)(~psi[alpha[paste(italic(j)~',3')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',3')]^'*']^paste(italic(l)~'+1')==~0~","~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~0)==italic(P)(~psi[alpha[paste(italic(j)~',2')]]^paste(italic(l)~'+1')==~1~"|"~psi[alpha[paste(italic(j)~',2')]^'*']^paste(italic(l)~'+1')==~0~","~psi[alpha[paste(italic(j)~',1')]]^paste(italic(l)~'+1')==~0)~"="~0.05))


plot_legend <- get_legend(ggplot(data=data_plot_123, aes(x=t1, y=value)) +
              geom_line(aes(colour=variable),size=0.9) +
              scale_color_manual(values=my_cols, 
              labels=c(legend_11_07,legend_11_08,legend_11_09,legend_11_095),
                "Conditional probabilities") + 
              theme(legend.title.align = 0.6,
                    legend.title = element_text(size=13),
                    legend.key = element_rect(fill="white"),
                    legend.key.width =  unit(1.8, "lines"),
                    legend.text = element_text(size=10.5, margin = margin(0,0,0,0, unit="pt")),
                    legend.text.align = 0,
                    legend.background = element_rect(linetype = 1),
                    legend.justification = c(0.93,0.65),
                    legend.spacing.y = unit(0.3,"cm"),
                    legend.box.margin = margin(0.05, 1.3, 0.02, 0.05, "cm"),
                    legend.box.spacing = unit(0, "cm"),
                    legend.box.background = element_rect(colour="grey")))


first_rows <- plot_grid(plotlist=id_plots[1:6], ncol=ncols, axis="tblr", rel_widths = c(1.15,1,1), rel_heights = c(1,1.13))
bottom_row <- plot_grid(plotlist=list(id_plots[[7]],plot_legend), ncol=2, axis="tblr", rel_widths = c(1.15,2))
figure <- plot_grid(first_rows, bottom_row, nrow=2, rel_heights = c(2,0.95))
ggsave(filename="BN_extra.pdf", device="pdf", plot=figure, width = 9.5, height = 10, scale=3, units="cm")
