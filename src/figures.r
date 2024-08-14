
require(ggplot2)
require(ggpubr)
require(reshape2)
require(dplyr)
### where the environmental perturbation ('delta')
### is set to a fraction of the critical delta as calculated analytically. This fraction
### is called delta_change in the output table

starting_nets <- data.frame(competition = c(0.0125, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15), 
                            delta = c(0.71, 0.49, 0.3, 0.21, 0.16, 0.12, 0.1),
                            tested = c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE))

original_sp_number <- 93

#### Figure 1 of the paper.
#### Panel (a):  DeltaC vs interspecific competition for the starting networks
a <- ggplot(starting_nets, aes(competition, delta)) +
  geom_point(size=6) +
  geom_line(aes(competition, delta), colour='black') +
  geom_hline(yintercept=c(.1), linetype="dashed", size=2) +
  # scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='') +
  # scale_color_manual(values=c('#000000','#ff7f0e'), name='') +
  theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
          plot.margin = unit(c(0.1, 0.15, 0.2, 0.1), "inches"),
          # axis.line = element_line(color="black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=25, colour ='black'),
          axis.text.x = element_text(size=25, colour ='black', angle=45, vjust = 0.5),
          axis.title = element_text(size=34),
          axis.title.x = element_text(size=34),
          strip.text = element_text(size = 30),
          plot.tag = element_text(size = 15),
          legend.position = 'none') +
  labs(x=expression('Interspecific competition'~(rho)), y=expression('Critical env. perturb. ('*Delta [c]*')')) +
  scale_x_continuous(breaks=c(0.0125, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15), labels=c('0.0125', '0.025', '0.05', '0.075', '0.1', '0.125', '0.15')) +
  scale_y_continuous(breaks=c(0.1, .3, .5, .7), labels=c('0.1', '0.3', '0.5', '0.7'))
#+
 # annotate("text", label = "A", x = .14, y = .66, size = 5) #+ scale_x_log10() + scale_y_log10()

ggsave('fig2b.pdf', width=7, height=7)


### Panel (b): final alpha diversity against rho for the simulations with constant environmental perturbations
### (constant environmental perturbation @ 0.105)

### To make the length of the simulation for constant environmental perturbation to match that of the 
### other simulations we look at the species richness stability for at least 350 steps

output_sce3 <- read.csv('./output-sce3-5-dec.csv', stringsAsFactors = FALSE)
#output_sce3 <- rbind(output_sce3, read.csv('./scenario-3/temp-output-new.csv', stringsAsFactors = FALSE))


output_sce3_final <- output_sce3 %>% 
  group_by(selection, competition, replicate, delta) %>% 
  filter(p == 0.5)

output_sce3_final <- output_sce3_final %>% 
  group_by(selection, competition, replicate, delta) %>% 
  filter(iteration == max(iteration))


# output_sce3_final <- read.csv('./output-final-sce3.csv', stringsAsFactors = FALSE)

b <- ggplot(subset(output_sce3_final, delta==0.1), aes(as.factor(competition), SA+SP, colour=as.factor(selection))) +
  geom_boxplot() + 
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
  theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=12, colour ='black'),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size=10, face='bold'),
        legend.text = element_text(size=10), 
        legend.position = c(0.85,.65)) +
  labs(x=expression('Interspecific competition'~(rho)), y=expression('final'~alpha*'-diversity')) +
  annotate("text", label = "B", x = 7, y = 71, size = 5) #+ scale_x_log10() + scale_y_log10()

ggsave('fig1b.pdf', width = 5, height = 5)



competition = c(0.0125, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15)
delta = c(0.71, 0.49, 0.3, 0.21, 0.16, 0.12, 0.1)


output_sce3_final$delta_crit <- starting_nets[match(output_sce3_final$competition, starting_nets$competition),]$delta
output_sce3_final$total_diversity <- (output_sce3_final$SP + output_sce3_final$SA)

data_sel <- subset(output_sce3_final, selection==TRUE & delta == 0.1)
lm_fit_sel <- lm(total_diversity ~ delta_crit , data=data_sel)
summary(lm_fit_sel)

predict(lm_fit_sel, newdata=data.frame(delta_crit=c(0)))

r_sel <- cor(data_sel$delta_crit, data_sel$total_diversity)

data_nosel <- subset(output_sce3_final, selection==FALSE & delta == 0.1)

### in case we want to fit on the means instead of all the dataset
data_nosel_model <- data_nosel %>% 
  group_by(delta_crit) %>% 
  summarise(total_diversity = mean(total_diversity) )

lm_fit_nosel <- lm(total_diversity ~ delta_crit, data=data_nosel)
summary(lm_fit_nosel)

predict(lm_fit_nosel, newdata=data.frame(delta_crit=c(0)))

r_nosel <- cor(data_nosel$delta_crit, data_nosel$total_diversity)

c <- ggplot(subset(output_sce3_final, delta == 0.1), aes((delta_crit), (SA+SP)/original_sp_number, colour=as.factor(selection))) +
  # geom_point() + 
  geom_pointrange(
    size = .3,
    stat = "summary",
    fun.min = function(z) {mean(z) - sd(z)},
    fun.max = function(z) {mean(z) + sd(z)},
    fun = mean) +
  geom_smooth(method = 'lm', se=FALSE, fullrange=FALSE) +
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
  # coord_cartesian(ylim = c(0.1,0.71)) +
  theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=12, colour ='black'),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size=10, face='bold'),
        legend.text = element_text(size=10), 
        legend.position = c(0.2,.65)) +
  labs(x=expression(Delta [cr]), y=expression('normalised final'~alpha*'-diversity')) +
  annotate("text", label = "C", y = .70, x = .65, size = 5) +
  annotate("text", label = paste0('slope = ', formatC(lm_fit_nosel$coefficients[2], digits=4)), y = .14, x = .6, size = 3.5) +
  annotate("text", label = paste0('r = ', formatC(r_nosel, digits=2)), y = .16, x = .6, size = 3.5) +
  annotate("text", label = paste0('slope = ', formatC(lm_fit_sel$coefficients[2], digits=4)), y = .50, x = .65, size = 3.5) +
  annotate("text", label = paste0('r = ', formatC(r_sel, digits=2)), y = .52, x = .65, size = 3.5) 

ggsave('fig1c.pdf', width=5, height = 5)

ggarrange(a,b,c, ncol=3, nrow=1)
ggsave('fig1all.pdf', width=15, height = 5)



d <- ggplot(output_sce3_final, aes(as.factor(competition), SA+SP, colour=as.factor(selection))) +
  facet_wrap(~delta, nrow=1, ncol=4) +
  geom_boxplot() + theme_bw() +
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
  theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text = element_text(size=25, colour ='black'),
        axis.text.x = element_text(size=25, angle = 45, vjust = 0.5),
        axis.title = element_text(size=30),
        strip.background = element_blank(),
        strip.text = element_text(size = 30),
        plot.tag = element_text(size = 15),
        legend.position = "none",
        panel.spacing = unit(2, "lines")) +
  labs(x=expression('Interspecific competition'~(rho)), y=expression('Final'~alpha*'-diversity'))

ggsave('fig2a.pdf', width = 19, height = 7)




e <- ggplot(output_sce3_final, aes((delta_crit), (SA+SP)/original_sp_number, colour=as.factor(selection))) +
  # geom_point() + 
  facet_wrap(~delta, nrow=1, ncol=4) +
  geom_pointrange(
    size = 1,
    stat = "summary",
    fun.min = function(z) {mean(z) - sd(z)},
    fun.max = function(z) {mean(z) + sd(z)},
    fun = mean) +
  geom_smooth(method = 'lm', se=FALSE, fullrange=FALSE) +
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
  # coord_cartesian(ylim = c(0.1,0.71)) +
  theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text = element_text(size=25, colour ='black'),
        axis.title = element_text(size=30),
        strip.background = element_blank(),
        strip.text = element_text(size = 30),
        plot.tag = element_text(size = 15),
        legend.position = "none",
        panel.spacing = unit(2, "lines")) +
  labs(x=expression('Critical environmental perturbation ('*Delta [c]*')'), y=expression('Normalised final'~alpha*'-diversity')) 

# +
#   annotate("text", label = "C", y = .70, x = .65, size = 5) +
#   annotate("text", label = paste0('slope = ', formatC(lm_fit_nosel$coefficients[2], digits=4)), y = .14, x = .6, size = 3.5) +
#   annotate("text", label = paste0('r = ', formatC(r_nosel, digits=2)), y = .16, x = .6, size = 3.5) +
#   annotate("text", label = paste0('slope = ', formatC(lm_fit_sel$coefficients[2], digits=4)), y = .50, x = .65, size = 3.5) +
#   annotate("text", label = paste0('r = ', formatC(r_sel, digits=2)), y = .52, x = .65, size = 3.5) 


ggsave('fig2c.pdf', width=19, height = 6.5)


ggarrange(d,e, ncol=1, nrow=2)
ggsave('final-diversity-and-preds.pdf', width=15, height = 7.5)





output_sce3_final$delta_crit <- starting_nets[match(output_sce3_final$competition, starting_nets$competition),]$delta
output_sce3_final$total_diversity <- output_sce3_final$SP + output_sce3_final$SA


selection_names <- c(
  `FALSE` = "Neutral",
  `TRUE` = "Selection"
)

f <- ggplot(output_sce3_final, aes((delta_crit), total_diversity, colour=as.factor(delta))) +
  # geom_point() + 
  facet_grid(~selection, labeller = as_labeller(selection_names)) +
  geom_pointrange(
    size = .3,
    stat = "summary",
    fun.min = function(z) {mean(z) - sd(z)},
    fun.max = function(z) {mean(z) + sd(z)},
    fun = mean) +
  stat_summary(fun = mean, geom = "line") +
  scale_color_brewer(palette = "Spectral", name = expression(Delta)) +
  theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=12, colour ='black'),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size=13, face='bold'),
        legend.text = element_text(size=10), 
        legend.position = 'top') +
  labs(x=expression(Delta [cr]), y=expression('final'~alpha*'-diversity'))


ggsave('fig2b.pdf', width = 7, height = 5)







## first we obtain normalised biodiversity
output_sce3_final$normalised_diversity <- output_sce3_final$total_diversity/original_sp_number

## we run linear regression for each value of delta change
slopes_delta <- NULL
for(s in unique(output_sce3_final$selection)){
  cur_data_sel <- subset(output_sce3_final, selection==s)
  for(dc in unique(cur_data_sel$delta)){
    cur_data <- cur_data_sel[which(cur_data_sel$delta == dc),]
    
    lm_fit_sel <- lm(normalised_diversity ~ delta_crit , data=cur_data)
    sum_mod <- summary(lm_fit_sel)
    slope <- sum_mod$coefficients[2,1]
    
    cur_out <- data.frame(dc, slope, sel = unique(cur_data$selection))
    if(is.null(slopes_delta)){
      slopes_delta <- cur_out
    }else{
      slopes_delta <- rbind(slopes_delta, cur_out)
    }
  }
}

g <- ggplot(slopes_delta, aes(dc, slope, colour = sel)) + 
  geom_point(size = 6) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
  # coord_cartesian(ylim = c(0.1,0.71)) +
  theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
        plot.margin = unit(c(0.1, 0.15, 0.2, 0.1), "inches"),
        # axis.line = element_line(color="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text = element_text(size=25, colour ='black'),
        axis.text.x = element_text(size=25, colour ='black', angle=45, vjust = 0.5),
        axis.title = element_text(size=34),
        axis.title.x = element_text(size=34),
        strip.text = element_text(size = 30),
        plot.tag = element_text(size = 15),
        legend.position = 'none') +
  labs(x=expression('Env. perturb. ('*Delta*')'), y=expression('Slope final'~alpha*'-div. vs'~Delta [c]))

ggsave('fig2d-new.pdf', width=7, height = 7)



##### new version of figure 2c. The same as before but with 

ggplot(output_sce3_final, aes(delta, delta*delta_crit)) + geom_point()

sp_mean <- aggregate(SP~selection+competition+delta, data=output_sce3_final, FUN='median')
sa_mean <- aggregate(SA~selection+competition+delta, data=output_sce3_final, FUN='median')

inter_sp <- interaction(sp_mean$selection, sp_mean$competition, sp_mean$delta)
inter_sa <- interaction(sa_mean$selection, sa_mean$competition, sa_mean$delta)

match(inter_sa, inter_sp)

s_means <- cbind(sp_mean, sa_mean[4])
s_means_true <- s_means[which(s_means$selection == TRUE),]
s_means_false <- s_means[which(s_means$selection == FALSE),]

inter_true <- interaction(s_means_true$competition, s_means_true$delta)
inter_false <- interaction(s_means_false$competition, s_means_false$delta)

if(length(which(inter_false %in% setdiff(inter_false, inter_true))) > 0){
  s_means_false <- s_means_false[-which(inter_false %in% setdiff(inter_false, inter_true)),]  
}

s_means_diff <- ((s_means_true$SP + s_means_true$SA) - (s_means_false$SP + s_means_false$SA)) / (s_means_false$SP + s_means_false$SA)

s_means_diff <- cbind(s_means_false[2:3], data.frame(s_diff = s_means_diff))


require(RColorBrewer)
getPalette = colorRampPalette(brewer.pal(7, "Set1"))


#### This is to obtain the point in x at which y attains it max value
myFmsy <- function(x, y){
  model <- loess(y ~ x)
  # 
  # yfit <- model$fitted
  # x[which(yfit == max(yfit))]
  newx <- data.frame(x = seq(0, max(x), .001))
  predicted <- predict(model, newx, se = TRUE)$fit
  newx[which(predicted == max(predicted)),]
  
}

x <- unique(s_means_diff[which(s_means_diff$competition == 0.15),]$delta)
y <- s_means_diff[which(s_means_diff$competition == 0.15),]$s_diff

model <- loess(y ~ x)
plot(predict(model, data.frame(x = seq(0, max(x), .001)), se = TRUE)$fit)

a <- predict(model, data.frame(x = seq(0, max(x), .001)), se = TRUE)$fit

seq(0, max(x), .001)[which(a == max(a))]

myFmsy(x,y)

intersects <- s_means_diff %>% 
  group_by(competition) %>% 
  summarize(intersect = myFmsy(delta, s_diff))

h <- ggplot(s_means_diff, aes(delta, s_diff, colour=as.factor(competition))) +
  geom_point() + geom_smooth(se = FALSE) +
  # geom_vline(data=intersects, aes(xintercept = intersect, colour=as.factor(competition))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text = element_text(size=15, colour ='black'),
        axis.title = element_text(size=18),
        legend.title = element_text(size=12, face='bold'),
        legend.text = element_text(size=10),
        legend.position = c(.2, .6)) +
  labs(x=expression(Delta), y=expression('Relative difference in final '*alpha*'-diversity')) +
  scale_color_manual(values=getPalette(7), name='Competition')


ggsave('fig2c-supp-new.pdf', width = 6, height = 5)



ggarrange(g,h, ncol=1, nrow=2)
ggsave('difference-in-final-diversity-detlta.pdf', width=7, height = 10)



##### Figure 3: A panel with biodiversity, structural stability and critical and effective competition.
##### To generate these figures we need to get the data from Alberto's output files and do some
##### pre-processing
deltac_all <- NULL
delta_values <- c(0, 0.1)
competitions <- c(0.15, 0.0125) 

for(d in delta_values){
  for(c in competitions){
    deltac <- read.table(paste0('./DeltaCritical_Delta',d,'-20-12/DeltaCmedian-Facultative_evol_trajectories_Delta',d,'_rho',c,'_rho',c, '-gammaExp_N_exp.txt'), sep=' ', header = TRUE, stringsAsFactors = FALSE)
    
    deltac <- deltac[-dim(deltac)[2]]
    metadata <- lapply(deltac$Network, function(x){ lapply(strsplit(x, '/'), strsplit, '-')})
    
    metadata <- unlist(metadata)
    
    # these indexes come from the string that is parsed from the delta and rho critical files
    if(d == 0){
      indexes <- c(3,5,7,10,13)
      gap <- 13
    }else if (d == 0.1){
      indexes <- c(2,4,6,9,12)
      gap <- 12
    }
    deltac$selection <- metadata[seq(indexes[1], length(metadata), gap)]
    deltac$competition <- as.numeric(metadata[seq(indexes[2], length(metadata), gap)])
    deltac$delta <- as.numeric(metadata[seq(indexes[3], length(metadata), gap)])
    deltac$replicate <- as.numeric(metadata[seq(indexes[4], length(metadata), gap)])
    deltac$iteration <- as.numeric(metadata[seq(indexes[5], length(metadata), gap)])
    
    #### Reading the rho critical files
    rho_crit <- read.table(paste0('./rhoCritical_Delta',d,'-20-12/rhoCritical_Matrices_evol_trajectories_Delta',d,'_rho',c,'.txt'), sep=' ', header = TRUE, stringsAsFactors = FALSE)
    rho_crit <- rho_crit[-dim(rho_crit)[2]]
    
    #### post-processing needed because of wrongly set values to NaN when the network is fully connected. Should be 1 in those cases
    if(length(which(is.nan(rho_crit$rhoCritA))) > 0) rho_crit[which(is.nan(rho_crit$rhoCritA)),]$rhoCritA <- 1
    if(length(which(is.nan(rho_crit$rhoCritP))) > 0) rho_crit[which(is.nan(rho_crit$rhoCritP)),]$rhoCritP <- 1
    
    #### This instruction is to ensure the data frames are aligned so data can be copied from one to another
    rho_crit <- rho_crit[match(deltac$Network, rho_crit$Network),]
    
    deltac$rhoCritA <- rho_crit$rhoCritA
    deltac$rhoCritP <- rho_crit$rhoCritP
    
    if(is.null(deltac_all)){
      deltac_all <- deltac  
    }else{
      deltac_all <- rbind(deltac_all, deltac)  
    }
  }
}

deltac <- deltac_all

deltac <- deltac[with(deltac, order(selection, delta, competition, replicate, iteration)),]
deltac$groups <- interaction(deltac$selection, deltac$competition, deltac$delta)

table(deltac$iteration)

if(length(which(deltac$rhoCritP < 0)) > 0) deltac[which(deltac$rhoCritP < 0),]$rhoCritP <- NA
if(length(which(deltac$rhoCritA < 0)) > 0) deltac[which(deltac$rhoCritA < 0),]$rhoCritA <- NA

total_output_plot <- subset(output_sce3, ((competition %in% unique(deltac$competition)) & p == 0.5))

deltac_subset <- NULL
total_output_plot_subset <- NULL

for(sel in unique(deltac$selection)){
  cur_sel_data <- deltac[which(deltac$selection == sel),]
  cur_sel_data_plot <- total_output_plot[which(total_output_plot$selection == sel),]
  for(d in unique(deltac$delta)){
    cur_del_data <- cur_sel_data[which(cur_sel_data$delta == d),]
    cur_del_data_plot <- cur_sel_data_plot[which(cur_sel_data_plot$delta == d),]

    for(comp in unique(deltac$competition)){
      cur_comp_data <- cur_del_data[which(cur_del_data$competition == comp),]
      target_iter <- max(as.numeric(names(which(table(cur_comp_data$iteration) >= 10))))
      subset_data <- cur_comp_data[which(cur_comp_data$iteration <= target_iter),]
      
      if(is.null(deltac_subset)){
        deltac_subset <- subset_data
      }else{
        deltac_subset <- rbind(deltac_subset, subset_data)
      }
      
      cur_comp_data <- cur_del_data_plot[which(cur_del_data_plot$competition == comp),]
      target_iter <- max(as.numeric(names(which(table(cur_comp_data$iteration) >= 10))))
      subset_data <- cur_comp_data[which(cur_comp_data$iteration <= target_iter),]
      
      if(is.null(total_output_plot_subset)){
        total_output_plot_subset <- subset_data
      }else{
        total_output_plot_subset <- rbind(total_output_plot_subset, subset_data)
      }
    }
  }
}


a <- as.character(interaction(total_output_plot_subset$selection, total_output_plot_subset$competition, total_output_plot_subset$replicate, total_output_plot_subset$iteration, total_output_plot_subset$delta))
b <- as.character(interaction(deltac_subset$selection, deltac_subset$competition, deltac_subset$replicate, deltac_subset$iteration, deltac_subset$delta))

deltac_subset$alpha <- total_output_plot_subset[match(b,a),]$SP + total_output_plot_subset[match(b,a),]$SA
deltac_subset$alphaP <- total_output_plot_subset[match(b,a),]$SP


for(delta_plot in unique(total_output_plot_subset$delta)){
  ##### Panel (a) : Alpha diversity through time
  df <- subset(total_output_plot_subset,  iteration <= 2000 & (iteration %% 25 == 0) & delta == delta_plot)
  i <- ggplot(df, aes(iteration/1000, SA+SP, colour=as.factor(selection))) + 
    facet_grid(~competition) +
    geom_pointrange(
      size = .3,
      stat = "summary",
      fun.min = function(z) {mean(z) - sd(z)},
      fun.max = function(z) {mean(z) + sd(z)},
      fun = mean) + 
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=30, colour ='black'),
          axis.title = element_text(size=35),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.tag = element_text(size = 15),
          legend.position = "none",
          panel.spacing = unit(2, "lines")) +
    coord_cartesian(ylim = c(55, 93)) +  # coord_cartesian(ylim = c(55,93)) + #for delta = 0.1
    labs(x='', y=expression(alpha*'-diversity')) 
  
  ggsave(paste0('biodiv-delta-',delta_plot,'.pdf'), width=11, height=6)
  
  ##### effective diversity
  j <- ggplot(df, aes(iteration/1000, evenness_plants+evenness_animals, colour=as.factor(selection))) + 
    facet_grid(~competition) +
    geom_pointrange(
      size = .3,
      stat = "summary",
      fun.min = function(z) {mean(z) - sd(z)},
      fun.max = function(z) {mean(z) + sd(z)},
      fun = mean) + 
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=30, colour ='black'),
          axis.title = element_text(size=35),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.tag = element_text(size = 15),
          legend.position = "none",
          panel.spacing = unit(2, "lines")) +
    coord_cartesian(ylim = c(55,93)) +  # coord_cartesian(ylim = c(55,93)) + #for delta = 0.1
    labs(x='', y='Effective diversity')
  
  
  ggsave(paste0('eff-div-delta-',delta_plot,'.pdf'), width=11, height=6)
  
  
  
  ##### abundances
  k <- ggplot(df, aes(iteration/1000, BiomassP+BiomassA, colour=as.factor(selection))) +
    facet_grid(~competition) +
    geom_pointrange(
      size = .3,
      stat = "summary",
      fun.min = function(z) {mean(z) - sd(z)},
      fun.max = function(z) {mean(z) + sd(z)},
      fun = mean) + 
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=30, colour ='black'),
          axis.title = element_text(size=35),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.tag = element_text(size = 15),
          legend.position = "none",
          panel.spacing = unit(2, "lines")) +
    labs(x='', y='Total biomass')
  
  
  ggsave(paste0('biomass-delta-',delta_plot,'.pdf'), width=11, height=6)
  
  
  
  ##### Connectance
  l <- ggplot(df, aes(iteration/1000, connectance, colour=as.factor(selection))) +
    facet_wrap(~competition) +
    geom_pointrange(
      size = .3,
      stat = "summary",
      fun.min = function(z) {mean(z) - sd(z)},
      fun.max = function(z) {mean(z) + sd(z)},
      fun = mean) +
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=30, colour ='black'),
          axis.title = element_text(size=35),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.tag = element_text(size = 15),
          legend.position = "none",
          panel.spacing = unit(2, "lines")) +
    labs(x='', y='Connectance')
  
  
  ggsave(paste0('connectance-delta-',delta_plot,'.pdf'), width=11, height=6)
  
  
  
  
  # nestedness NODFc
  m <- ggplot(df, aes(iteration/1000, nest_nodfmax, colour=as.factor(selection))) +
    facet_wrap(~competition) +
    geom_pointrange(
      size = .3,
      stat = "summary",
      fun.min = function(z) {mean(z) - sd(z)},
      fun.max = function(z) {mean(z) + sd(z)},
      fun = mean) +
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=30, colour ='black'),
          axis.title = element_text(size=35),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.tag = element_text(size = 15),
          legend.position = "none",
          panel.spacing = unit(2, "lines")) +
    labs(x='', y='NODFc')
  
  ggsave(paste0('nestedness-delta-',delta_plot,'.pdf'), width=11, height=6)
  
  
  
  
  # nestedness (ecological overlap)
  m1 <- ggplot(df, aes(iteration/1000, nestedness, colour=as.factor(selection))) +
    facet_wrap(~competition) +
    geom_pointrange(
      size = .3,
      stat = "summary",
      fun.min = function(z) {mean(z) - sd(z)},
      fun.max = function(z) {mean(z) + sd(z)},
      fun = mean) +
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=30, colour ='black'),
          axis.title = element_text(size=35),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.tag = element_text(size = 15),
          legend.position = "none",
          panel.spacing = unit(2, "lines")) +
    labs(x='', y='Ecological overlap')
  
  ggsave(paste0('ecological-overlap-delta-',delta_plot,'.pdf'), width=11, height=6)
  
  
  
  
  
  # compartments
  m2 <- ggplot(df, aes(iteration/1000, conn_comps, colour=as.factor(selection))) +
    facet_wrap(~competition) +
    geom_pointrange(
      size = .3,
      stat = "summary",
      fun.min = function(z) {mean(z) - sd(z)},
      fun.max = function(z) {mean(z) + sd(z)},
      fun = mean) +
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=30, colour ='black'),
          axis.title = element_text(size=35),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.tag = element_text(size = 15),
          legend.position = "none",
          panel.spacing = unit(2, "lines")) +
    labs(x='', y='Num. conn. comp.')
  
  ggsave(paste0('compartments-delta-',delta_plot,'.pdf'), width=11, height=6)
  
  
  
  if(length(which(deltac_subset$rhoCritA > 1)) > 0) deltac_subset[which(deltac_subset$rhoCritA > 1),]$rhoCritA <- NA
  df <- subset(deltac_subset, iteration <= 2000 & delta == delta_plot)
  
  # StdvDeg
  u <- ggplot(df, aes(iteration/1000, StdvDeg, colour=as.factor(selection))) +
    geom_pointrange(
      size = .5,
      stat = "summary",
      fun.min = function(z) {mean(z) - (sd(z) / sqrt(length(z)))},
      fun.max = function(z) {mean(z) + (sd(z) / sqrt(length(z)))},
      fun = mean) +
    facet_wrap(~competition) +
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=30, colour ='black'),
          axis.title = element_text(size=35),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.tag = element_text(size = 15),
          legend.position = "none",
          panel.spacing = unit(2, "lines")) +
    labs(x='', y=expression(sigma*'(degree)'))
  
  ggsave(paste0('sigmadeg-delta-',delta_plot,'.pdf'), width=11, height=6)
  
  
  ##### interspecific competition through time (critical and effective)
  
  # effective competition
  p <- ggplot(df, aes(iteration/1000, RhoEffA*10, colour=as.factor(selection))) +
    geom_pointrange(
      size = .5,
      stat = "summary",
      fun.min = function(z) {mean(z) - (sd(z) / sqrt(length(z)))},
      fun.max = function(z) {mean(z) + (sd(z) / sqrt(length(z)))},
      fun = mean) +
    facet_wrap(~competition, scales = 'free_y') +
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=30, colour ='black'),
          axis.title = element_text(size=35),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.tag = element_text(size = 15),
          legend.position = "none",
          panel.spacing = unit(.5, "lines")) +
    labs(x='', y=expression('Eff. competition ('*rho[eff]*')'))
  
  ggsave(paste0('eff-rho-delta-',delta_plot,'.pdf'), width=11, height=6)
  
  
  # critical competition
  o <- ggplot(df, aes(iteration/1000, rhoCritA*10, colour=as.factor(selection))) +
    geom_pointrange(
      size = .5,
      stat = "summary",
      fun.min = function(z) {mean(z) - (sd(z) / sqrt(length(z)))},
      fun.max = function(z) {mean(z) + (sd(z) / sqrt(length(z)))},
      fun = mean) +
    facet_wrap(~competition) +
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=30, colour ='black'),
          axis.title = element_text(size=35),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.tag = element_text(size = 15),
          legend.position = "none",
          panel.spacing = unit(2, "lines")) +
    labs(x='', y=expression('Crit. competition ('*rho[c]*')'))
  
  ggsave(paste0('rho-crit-delta-',delta_plot,'.pdf'), width=11, height=6)
  
  
  #### relative distance to critical competition
  
  q <- ggplot(df, aes(iteration/1000, ((rhoCritA - RhoEffA)/rhoCritA), colour=as.factor(selection))) +
    geom_pointrange(
      size = .5,
      stat = "summary",
      fun.min = function(z) {mean(z) - (sd(z) / sqrt(length(z)))},
      fun.max = function(z) {mean(z) + (sd(z) / sqrt(length(z)))},
      fun = mean) +
    facet_wrap(~competition, scales = 'free_y') +
    geom_hline(data = subset(df, competition == 0.15), aes(yintercept=0), linetype='dashed') +
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=30, colour ='black'),
          axis.title = element_text(size=35),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.tag = element_text(size = 15),
          legend.position = "none",
          panel.spacing = unit(.5, "lines")) +
    labs(x='', y=expression('Distance to '*rho[c]))
  
  ggsave(paste0('dist2rho-delta-',delta_plot,'.pdf'), width=11, height=6)
  
  
  
  
  # Assortativity
  v <- ggplot(df, aes(iteration/1000, Assort, colour=as.factor(selection))) +
    geom_pointrange(
      size = .5,
      stat = "summary",
      fun.min = function(z) {mean(z) - (sd(z) / sqrt(length(z)))},
      fun.max = function(z) {mean(z) + (sd(z) / sqrt(length(z)))},
      fun = mean) +
    facet_wrap(~competition) +
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=30, colour ='black'),
          axis.title = element_text(size=35),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.tag = element_text(size = 15),
          legend.position = "none",
          panel.spacing = unit(2, "lines")) +
    labs(x='', y='Assortativity')
  
  ggsave(paste0('assort-delta-',delta_plot,'.pdf'), width=11, height=6)
  
  
  
  ### critical delta
  n <- ggplot(df, aes(iteration/1000,  DeltaC, colour=as.factor(selection))) + 
    facet_wrap(~competition, scales = 'free_y') +
    geom_pointrange(
      size = .3,
      stat = "summary",
      fun.min = function(z) {mean(z) - (sd(z) / sqrt(length(z)))},
      fun.max = function(z) {mean(z) + (sd(z) / sqrt(length(z)))},
      fun = mean) + 
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=30, colour ='black'),
          axis.title = element_text(size=35),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.tag = element_text(size = 15),
          legend.position = "none",
          panel.spacing = unit(2, "lines")) +
    labs(x='', y=expression('Structural stability ('~Delta[c]~')'))
  
  ggsave(paste0('critical-delta-',delta_plot,'.pdf'), width=15, height=6)
  
 
  # ggsave('fig3d.pdf', width = 10, height = 5)
  
  # eta prime
  r <- ggplot(subset(df, EtaPrimeA <= 10), aes(iteration, EtaPrimeA, colour=as.factor(selection))) +
    geom_pointrange(
      size = .5,
      stat = "summary",
      fun.min = function(z) {mean(z) - (sd(z) / sqrt(length(z)))},
      fun.max = function(z) {mean(z) + (sd(z) / sqrt(length(z)))},
      fun = mean) +
    facet_wrap(~competition, scales = 'free_y') +
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=15, colour ='black'),
          axis.title = element_text(size=17),
          legend.title = element_text(size=10, face='bold'),
          legend.text = element_text(size=10),
          strip.text = element_text(size = 10),
          plot.tag = element_text(size = 15), legend.position = 'none') +
    labs(x='number of substitutions', y= 'Eta Prime', tag = 'J')
  
  # NDNSp
  s <- ggplot(df, aes(iteration, NDNSp, colour=as.factor(selection))) +
    geom_pointrange(
      size = .5,
      stat = "summary",
      fun.min = function(z) {mean(z) - (sd(z) / sqrt(length(z)))},
      fun.max = function(z) {mean(z) + (sd(z) / sqrt(length(z)))},
      fun = mean) +
    facet_wrap(~competition, scales = 'free_y') +
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=15, colour ='black'),
          axis.title = element_text(size=17),
          legend.title = element_text(size=10, face='bold'),
          legend.text = element_text(size=10),
          strip.text = element_text(size = 10),
          plot.tag = element_text(size = 15), legend.position = 'none') +
    labs(x='number of substitutions', y= 'NDNSp', tag = 'K')
  
  
  # NestOrder2P
  t <- ggplot(df, aes(iteration, NestOrder2P, colour=as.factor(selection))) +
    geom_pointrange(
      size = .5,
      stat = "summary",
      fun.min = function(z) {mean(z) - (sd(z) / sqrt(length(z)))},
      fun.max = function(z) {mean(z) + (sd(z) / sqrt(length(z)))},
      fun = mean) +
    facet_wrap(~competition, scales = 'free_y') +
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=15, colour ='black'),
          axis.title = element_text(size=17),
          legend.title = element_text(size=10, face='bold'),
          legend.text = element_text(size=10),
          strip.text = element_text(size = 10),
          plot.tag = element_text(size = 15), legend.position = 'none') +
    labs(x='number of substitutions', y= 'Nest Order 2P', tag = 'L')
  
  # Assort
  v <- ggplot(df, aes(iteration, Assort, colour=as.factor(selection))) +
    geom_pointrange(
      size = .5,
      stat = "summary",
      fun.min = function(z) {mean(z) - (sd(z) / sqrt(length(z)))},
      fun.max = function(z) {mean(z) + (sd(z) / sqrt(length(z)))},
      fun = mean) +
    facet_wrap(~competition, scales = 'free_y') +
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=15, colour ='black'),
          axis.title = element_text(size=17),
          legend.title = element_text(size=10, face='bold'),
          legend.text = element_text(size=10),
          strip.text = element_text(size = 10),
          plot.tag = element_text(size = 15), legend.position = 'none') +
    labs(x='number of substitutions', y= 'Assortativity', tag = 'N')
  
  
  
  # alphaP*RhoEffP / (1-RhoEffP)
  w <- ggplot(df, aes(iteration, (alphaP*(RhoEffP / (1-RhoEffP))), colour=as.factor(selection))) +
    geom_pointrange(
      size = .5,
      stat = "summary",
      fun.min = function(z) {mean(z) - (sd(z) / sqrt(length(z)))},
      fun.max = function(z) {mean(z) + (sd(z) / sqrt(length(z)))},
      fun = mean) +
    facet_wrap(~competition, scales = 'free_y') +
    stat_summary(fun = mean, geom = "line") +
    scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text = element_text(size=15, colour ='black'),
          axis.title = element_text(size=17),
          legend.title = element_text(size=10, face='bold'),
          legend.text = element_text(size=10),
          strip.text = element_text(size = 10),
          plot.tag = element_text(size = 15), legend.position = 'none') +
    labs(x='number of substitutions', y= 'alphaP*(RhoEffP / (1-RhoEffP))', tag = 'O')
  
  
  ggarrange(i,j,k,l,m,n,o,p,q,r,s,t,u,v,w, ncol=3, nrow=5)
  ggsave(paste0('fig3all-for-us-new-20-12-delta-',delta_plot,'.pdf'), width=30, height = 25)
} 




df <- subset(total_output_plot_subset, replicate %in% c(10:15) & delta == delta_plot & competition == 0.0125)
ggplot(df, aes(iteration, SA+SP, colour=as.factor(selection))) + 
  geom_point() +
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
  theme(panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.1, 0.15, 0.2, 0.1), "inches"),
        # axis.line = element_line(color="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text = element_text(size=15, colour ='black'),
        axis.title = element_text(size=20),
        axis.title.x = element_text(vjust = -2, size=20),
        legend.title = element_text(size=15, face='bold'),
        legend.text = element_text(size=15),
        strip.text = element_text(size = 10),
        plot.tag = element_text(size = 15), legend.position = c(0.8,0.8)) +
  labs(x='Number of substitutions', y=expression(alpha*'-diversity')) 

ggsave('sample-trajectories.pdf', width=6, height = 6)
















# output_diff_p <- read.csv('merged-results-diff-p-31-july-2023.csv')

output_diff_p <- read.csv('temp-output-350-sp-stab-p0.1.csv')
output_diff_p$p <- 0.1

temp_p <- read.csv('temp-output-350-sp-stab-p0.9.csv')
temp_p$p <- 0.9

output_diff_p <- rbind(output_diff_p, temp_p)

temp_p <- subset(output_sce3, (competition == 0.0125 & delta == 0.1 & p == 0.5))

output_diff_p <- rbind(output_diff_p[-c(1:2)], temp_p[-1])


output_diff_p_new <- output_diff_p[which(output_diff_p$competition %in% c(0.0125, 0.15)),]
output_diff_p_plot <- NULL

for(prob in sort(unique(output_diff_p_new$p))){
  print(prob)
  for(sel in sort(unique(output_diff_p_new$selection))){
    print(sel)
    for(comp in sort(unique(output_diff_p_new$competition))){
      print(comp)
      cur_data <- subset(output_diff_p_new, (selection == sel & competition == comp & p == prob))
      
      target_iter <- max(as.numeric(names(which(table(cur_data$iteration) >= 10))))
      subset_data <- cur_data[which(cur_data$iteration <= target_iter),]
      
      if(is.null(output_diff_p_plot)){
        output_diff_p_plot <- subset_data
      }else{
        output_diff_p_plot <- rbind(output_diff_p_plot, subset_data)
      }
    } 
  }
}

j <- ggplot(subset(output_diff_p_plot, iteration <= 2000 & (iteration %% 25 == 0)), aes(iteration/1000, SP+SA, colour=as.factor(selection))) + 
  facet_grid( ~ p) + 
  geom_pointrange(
    size = .3,
    stat = "summary",
    fun.min = function(z) {mean(z) - sd(z)},
    fun.max = function(z) {mean(z) + sd(z)},
    fun = mean) + 
  stat_summary(fun = mean, geom = "line") +
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
  theme(panel.background = element_rect(fill = "white", color = 'black', size = 1),
        title = element_text(size=30, colour ='black'),
        axis.text = element_text(size=25, colour ='black'),
        axis.title = element_text(size=40),
        legend.title = element_text(size=40, face='bold'),
        legend.text = element_text(size=40),
        strip.text = element_text(size = 40),
        plot.tag = element_text(size = 30),
        legend.position = 'top') +
  labs(x='', y=expression(alpha*'-diversity'), tag='a') 

ggsave('/Users/miguel/Downloads/diversity-diff-p.pdf', width = 17, height = 7)


#### Panel (b) : connectance
k <- ggplot(subset(output_diff_p_plot, iteration <= 2000 & (iteration %% 25 == 0)), aes(iteration/1000, connectance, colour=as.factor(selection))) + 
  facet_grid( ~ p) + 
  geom_pointrange(
    size = .3,
    stat = "summary",
    fun.min = function(z) {mean(z) - sd(z)},
    fun.max = function(z) {mean(z) + sd(z)},
    fun = mean) + 
  stat_summary(fun = mean, geom = "line") +
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
  theme(panel.background = element_rect(fill = "white", color = 'black', size = 1),
        title = element_text(size=30, colour ='black'),
        axis.text = element_text(size=25, colour ='black'),
        axis.title = element_text(size=40),
        legend.title = element_text(size=40, face='bold'),
        legend.text = element_text(size=40),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.tag = element_text(size = 30),
        legend.position = 'none') +
  labs(x='Number of substitutions (x1000)', y='Connectance', tag='b') 

# ggsave('connectance-diff-p.pdf', width = 20, height = 10)

ggsave('/Users/miguel/Downloads/connectance-diff-p.pdf', width = 17, height = 6)

ggarrange(j,k, ncol=1, nrow=2)
ggsave('/Users/miguel/Downloads/figure-diff-p.pdf', width=15, height = 15)




output_diff_p_new <- read.csv('merged-results-stab-350-sp-4-apr-2023.csv')

output_diff_p_plot <- NULL

for(prob in sort(unique(output_diff_p_new$delta_change))){
  print(prob)
  for(sel in sort(unique(output_diff_p_new$selection))){
    print(sel)
    for(comp in sort(unique(output_diff_p_new$competition))){
      print(comp)
      cur_data <- subset(output_diff_p_new, (selection == sel & competition == comp & delta_change == prob))
      
      target_iter <- max(as.numeric(names(which(table(cur_data$iteration) >= 10))))
      subset_data <- cur_data[which(cur_data$iteration <= target_iter),]
      
      if(is.null(output_diff_p_plot)){
        output_diff_p_plot <- subset_data
      }else{
        output_diff_p_plot <- rbind(output_diff_p_plot, subset_data)
      }
    } 
  }
}


# png(filename = 'biodiversity-time-3k-subs-new.png', width = 1500, height = 800)
l <- ggplot(subset(output_diff_p_plot, iteration <= 2000 & (iteration %% 25 == 0) & delta_change %in% c(0, 0.025, 0.05, 0.1, 0.25, 0.5)), aes(iteration/100, SA+SP, colour=as.factor(selection))) + 
  facet_grid(competition ~ delta_change) + 
  geom_pointrange(
    size = .3,
    stat = "summary",
    fun.min = function(z) {mean(z) - sd(z)},
    fun.max = function(z) {mean(z) + sd(z)},
    fun = mean) + 
  # stat_summary(fun = mean, geom = "line", size=3, colour='black') +
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
  theme(panel.background = element_rect(fill = "white", color = 'black', size = 1),
        title = element_text(size=30, colour ='black'),
        axis.text = element_text(size=20, colour ='black'),
        axis.title = element_text(size=40),
        legend.title = element_text(size=40, face='bold'),
        legend.text = element_text(size=40),
        strip.text = element_text(size = 40)) +
  labs(x='Number of substitutions (x100)', y=expression(alpha*'-diversity'),
       title = expression('Evolving diversity (rows = '~ rho ~'; columns = '~ delta ~') ')) 
# dev.off()
ggsave('figure-small-delta.pdf', width = 20, height = 10)


#### to plot matrices

###### here we try to create some 'trajectories' for the evolution of the network topology
selections <- c(TRUE, FALSE)
competitions <- c(0.0125, 0.15)
replicates <- 25
iterations <- c(0, seq(100, 1000, 50))

require(plot.matrix)
require(bipartite)
par(mar=c(.5,.5,.5,.5), mfrow=c(4,5))

for(s in selections){
  for(c in competitions){
    pdf(file = paste0('networks-selection-',s,'-comp-',c,'.pdf'), width=12, height=8)
    par(mar=c(1,1,1,1), mfrow=c(4,5))
    for(r in 1:1){
      for(i in iterations){
        
        if(c == 0.0125 & i > 300 & s == TRUE) break
        
        cur_data <- subset(output_diff_p, p == 0.1 & competition == c & selection == s & replicate == r & iteration == i)
        
        adj_matrix <- as.matrix(read.table(paste0('./evol-trajectories-p-0.1/selection-',s,'/competition-',c,'/output-replicate-',r,'/output-iter-',i,'/gammaOutA.out') , sep=","))
        
        adj_matrix <- matrix(adj_matrix, nrow=cur_data$SP, ncol = cur_data$SA)
        
        adj_matrix[adj_matrix != 0] <- 1
        
        bg_matrix <- matrix(0,47,46)
        
        bg_matrix[1:dim(adj_matrix)[1], 1:dim(adj_matrix)[2]] <- adj_matrix
        
        plot(sortweb(bg_matrix), xlab='', ylab = '', main = i, cex.axis=0.0001, key = NULL, axis.col=list(side=1, tick=FALSE), axis.row=list(side=2, tick=FALSE), col=c('white', 'black'))
        # visweb(bg_matrix, clear=FALSE)
        # text(20, 48, paste0("iter ",i),cex=1, font=2)
      }
    }
    dev.off()
  }
}


data <- read.csv('DeltaC_simulated_crit_thr_0.05.csv', sep='\t')
data$Delta_c <- round(data$Delta_c, digits=2)

data$selection <- as.logical(data$selection)
data$selection <- as.character(data$selection)

t <- deltac[which(deltac$competition == 0.15),]

ggplot(t, aes(x=as.factor(delta), y=DeltaC, colour=as.factor(selection))) +
  geom_boxplot() + theme_bw() + #coord_cartesian(y=c(0.6,1)) +
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process') 



ggplot(data, aes(competition, Delta_c, colour=as.factor(selection))) +
  facet_grid(~delta) +
  geom_pointrange(
    size = 1,
    stat = "summary",
    fun.min = function(z) {mean(z) - sd(z)},
    fun.max = function(z) {mean(z) + sd(z)},
    fun = mean) +
  #geom_line(aes(competition, Delta_c), colour='black') +
  #geom_hline(yintercept=c(.1), linetype="dashed", size=2) +
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='') +
  # scale_color_manual(values=c('#000000','#ff7f0e'), name='') +
  theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
        plot.margin = unit(c(0.1, 0.15, 0.2, 0.1), "inches"),
        # axis.line = element_line(color="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text = element_text(size=25, colour ='black'),
        axis.text.x = element_text(size=25, colour ='black', angle=45, vjust = 0.5),
        axis.title = element_text(size=34),
        axis.title.x = element_text(size=34),
        strip.text = element_text(size = 30),
        plot.tag = element_text(size = 15),
        legend.position = 'top') +
  labs(x=expression('Interspecific competition'~(rho)), y=expression('Critical env. perturb. ('*Delta [c]*')')) +
  scale_x_continuous(breaks=c(0.0125, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15), labels=c('0.0125', '0.025', '0.05', '0.075', '0.1', '0.125', '0.15')) +
  scale_y_continuous(breaks=c(0.1, .3, .5, .7), labels=c('0.1', '0.3', '0.5', '0.7'))



joint_data <- inner_join(data, deltac)

joint_data$delta_crit <- starting_nets[match(joint_data$competition, starting_nets$competition),]$delta

b <- ggplot(joint_data, aes((DeltaC), Delta_c, colour=as.factor(selection))) +
  # geom_point() + 
  facet_grid(competition~delta) +
  geom_pointrange(
    size = 1,
    stat = "summary",
    fun.min = function(z) {mean(z) - sd(z)},
    fun.max = function(z) {mean(z) + sd(z)},
    fun = mean) +
  #geom_smooth(method = 'lm', se=FALSE, fullrange=FALSE) +
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
  # coord_cartesian(ylim = c(0.1,0.71)) +
  theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text = element_text(size=25, colour ='black'),
        axis.title = element_text(size=30),
        strip.background = element_blank(),
        strip.text = element_text(size = 30),
        plot.tag = element_text(size = 15),
        legend.position = "top",
        panel.spacing = unit(2, "lines")) +
  labs(x=expression(Delta [c]*' predicted'), y=expression(Delta [c]*' simulated')) 

ggsave('delta-pred-delta-sim.pdf', width = 20, height = 10)



data_temp <- data[which(data$competition %in% c(0.0125, 0.15)),]

ggplot(data_temp, aes(iteration,  Delta_c, colour=as.factor(selection))) + 
  facet_wrap(delta~competition, scales='free_y') +
  geom_pointrange(
    size = .3,
    stat = "summary",
    fun.min = function(z) {mean(z) - (sd(z) / sqrt(length(z)))},
    fun.max = function(z) {mean(z) + (sd(z) / sqrt(length(z)))},
    fun = mean) + 
  stat_summary(fun = mean, geom = "line") +
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text = element_text(size=15, colour ='black'),
        axis.title = element_text(size=17),
        legend.title = element_text(size=10, face='bold'),
        legend.text = element_text(size=10),
        strip.text = element_text(size = 10),
        plot.tag = element_text(size = 15), legend.position = 'none') +
  labs(x='number of substitutions', y=expression('Critical perturbation'~Delta[c]), tag='F')



pred_summary <- summarySE(joint_data, 'DeltaC', c('selection', 'delta', 'competition', 'iteration'))
sim_summary <- summarySE(joint_data, 'Delta_c', c('selection', 'delta', 'competition', 'iteration'))

joint_data_2 <- inner_join(pred_summary, sim_summary, by=c('selection', 'delta', 'competition', 'iteration'))


b <- ggplot(joint_data_2, aes(DeltaC, Delta_c, colour=as.factor(selection))) +
  # geom_point() + 
  facet_grid2(delta~competition, scales='free', independent = 'all') +
  geom_point() +
  geom_errorbar(aes(ymin=Delta_c-se.y, ymax=Delta_c+se.y), width=0) +
  geom_errorbar(aes(xmin=DeltaC-se.x, xmax=DeltaC+se.x), width=0) +
  #geom_smooth(method = 'lm', se=FALSE, fullrange=FALSE) +
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
  # coord_cartesian(ylim = c(0.1,0.71)) +
  theme(panel.background = element_rect(fill = "white", color = 'black', size = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.2, 0.2, 0.1), "inches"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text = element_text(size=25, colour ='black'),
        axis.title = element_text(size=30),
        strip.background = element_blank(),
        strip.text = element_text(size = 30),
        plot.tag = element_text(size = 15),
        legend.position = "top",
        panel.spacing = unit(2, "lines")) +
  labs(x=expression(Delta [c]*' predicted'), y=expression(Delta [c]*' simulated')) 

ggsave('delta-pred-delta-sim.pdf', width = 10, height = 10)



ggplot(pred_summary, aes(iteration,  DeltaC, colour=as.factor(selection))) + 
  facet_wrap(~competition, scales = 'free_y') +
  geom_pointrange(
    size = .3,
    stat = "summary",
    fun.min = function(z) {mean(z) - (sd(z) / sqrt(length(z)))},
    fun.max = function(z) {mean(z) + (sd(z) / sqrt(length(z)))},
    fun = mean) + 
  stat_summary(fun = mean, geom = "line") +
  scale_color_manual(values=c('#1f77b4','#ff7f0e'), name='Evol. process', labels=c('Neutral', 'Selection')) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text = element_text(size=15, colour ='black'),
        axis.title = element_text(size=17),
        legend.title = element_text(size=10, face='bold'),
        legend.text = element_text(size=10),
        strip.text = element_text(size = 10),
        plot.tag = element_text(size = 15), legend.position = 'none') +
  labs(x='number of substitutions', y=expression('Critical perturbation'~Delta[c]), tag='F')



