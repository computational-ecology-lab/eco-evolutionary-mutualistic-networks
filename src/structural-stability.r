##########################################################################################################
####### This code is for the project on the evolution of mutualistic networks entitled:
####### Evolution of critical competition in mutualistic communities

####### It implements a wrapper for the application developed by Alberto Pascual-Garcia which
####### is used to evaluate the dynamics of a mutualistic communities based on his previous work

####### In this script we evaluate the structural stability of communities that resulted from the 
####### mutualistic evolution experiments

####### Ecological dynamics are obtained from the Alberto's script called from here

####### Author: Miguel Lurgi

####### The steps involved in the simulations are:
####### 1.- We take the networks that have resulted from evolutionary trajectories at specified timesteps. These are available
####### from the files rho(or Delta)Critical_Matrices_evol_trajectories_Delta0(or 0.1)_rho0.15(or 0.0125).txt that correspond
####### to the outcomes of evolutionary simulations performed with those parameter values combinations (rho in {0.0125, 0.15} and
####### Delta in {0, 0.1})

####### 2.- For each network we take abundances and growth rates at the fixed point (should be in the folder specified in the file)

####### 3.- For each of these networks we simulate dynamics for different values of delta from 0 to 1 in 0.05 intervals and 
####### check how many species go extinct 

####### 4.- We perform 50 replicates for each of these simulations and record the mean and std error of the number of species that went extinct
####### as well as the fraction of replicates with at least one extinction

####### Parameters are the same as in the original Beyond-MeanField.in file, except the gamma matrix and population abundances
####### which correspond to the output for the corresponding replicate (found within the given folder)

####### 5.- To see the results we plot delta against the fraction of replicates with at least one extinction.


deltas <- c(0.1)  #c(0, 0.1)
#rhos <- c(0.0125, 0.15)
#selections <- c(TRUE, FALSE)
exp_deltas <- seq(0.05, 1, 0.05)
output <- NULL

for(del in deltas){
  print(del)
  # for(rho in rhos){
    # print(rho)
  #comms <- read.table(paste0('./rhoCritical_Delta',del,'-20-12/rhoCritical_Matrices_evol_trajectories_Delta',del,'_rho',rho,'.txt'), sep=' ', header = T, stringsAsFactors = F)
  
  comms <- list.dirs(paste0('./scenario-3-big-delta-', del,'/code'))
  
  # for(i in 1:dim(comms)[1]){ 
    # cur_com <- comms[i,]$Network
    # 
    # if(del == 0){
    #   cur_com <- paste0('./scenario-3-big-delta-',del,'/code', substring(cur_com,2))
    # }else{
    #   cur_com <- paste0('./scenario-3-big-delta-',del,'/code/', cur_com)
    # }
    
  for(i in 1:length(comms)){ 
    cur_com <- comms[i]
      
    if( length(grep('iter', cur_com)) == 0  ) next
    
    selection <- strsplit(strsplit(cur_com, '-')[[1]][6], '/')[[1]][1]
    rho <- as.numeric(strsplit(strsplit(cur_com, '-')[[1]][7], '/')[[1]][1])
    original_rep <- as.numeric(strsplit(strsplit(cur_com, '-')[[1]][10], '/')[[1]][1])
    iteration <- as.numeric(strsplit(cur_com, '-')[[1]][12])
    
    cur_animals <- read.table(paste0(cur_com,'/animalsOut-Final.out'))
    cur_animals <- cur_animals[which(cur_animals$V7 == TRUE),]$V3
    
    cur_plants <- read.table(paste0(cur_com,'/plantsOut-Final.out'))
    cur_plants <- cur_plants[which(cur_plants$V7 == TRUE),]$V3
    
    cur_network <- scan(paste0(cur_com,'/gammaOutA.out'))
    cur_network <- matrix(cur_network, nrow = length(cur_plants), ncol = length(cur_animals))
    
    
    setwd('/home/miguel/Desktop/Documents/mutualistic-evolution/new-runs-Nov-2023/code')
    
    
    ####  writing the interaction strengths
    write.table(as.vector(t(cur_network)), 'gammaInP.dat', col.names = FALSE, row.names = FALSE, quote=FALSE)
    write.table(as.vector(cur_network), 'gammaInA.dat', col.names = FALSE, row.names = FALSE, quote=FALSE)
    
    
    #### and the network itself
    cur_network[cur_network != 0] <- 1
    write.table(cur_network, paste0('network.txt'), col.names = FALSE, row.names = FALSE, quote=FALSE)
    
    
    #### write initial abundances for next iteration
    write.table(cur_plants, file='plantsIn.dat', quote=F, row.names = F, col.names = F)
    write.table(cur_animals, file='animalsIn.dat', quote=F, row.names = F, col.names = F)
    
    # #### write the growth rates (alphas) for next iteration
    # write.table(cur_result_plant$Alpha, file='alphaInP.dat', quote=F, row.names = F, col.names = F)
    # write.table(cur_result_animal$Alpha, file='alphaInA.dat', quote=F, row.names = F, col.names = F)
    
    #### writing the params table
    params <- read.table('./original_files/Beyond-MeanField.in', stringsAsFactors = F)
    #### starting abundances should be read from file
    params[1,] <- 0
    params[3,] <- 0
    
    params[9,] <- rho
    params[10,] <- rho
    ## read interaction strengths from file
    params[11,] <- -1000000
    
    ## numbers of species in new network
    params[15,] <- length(cur_plants)   ## plants
    params[16,] <- length(cur_animals)   ## animals
    
    params[18,] <- paste0('network.txt')
    
    
    
    for(exp_del in exp_deltas){
      params[17,] <- exp_del # 0 = no perturbations, delta = perturbation according to the values of rho, C and Nest
      params[20,] <- runif(1)  
      write.table(params, file='Beyond-MeanField.in', quote=F, row.names = F, col.names = F)
      
      system('./integration.exe', ignore.stdout = T)
      
      cur_out <- read.table('Summary_Beyond-MeanField.tmp', stringsAsFactors = F)
      names(cur_out) <- c('var', 'value')
      
      ##### metrics characterising the system
      
      species <- cur_out[which(cur_out$var == 'Sp'),]$value + cur_out[which(cur_out$var == 'Sa'),]$value 
      extinct <- cur_out[which(cur_out$var == 'ExtinctP'),]$value + cur_out[which(cur_out$var == 'ExtinctA'),]$value
      
      
      cur_rep_out <- data.frame(selection, delta=del, competition=rho, replicate=original_rep, iteration, exp_delta=exp_del, species, extinct, fraction = extinct/species)
      
      if(is.null(output)){
        output <- cur_rep_out
      }else{
        output <- rbind(output, cur_rep_out)
      }
      
      system('rm *.out')
      system('rm *.in')
      system('rm *.tmp')
    
      
    }
    
    system('rm *.dat')
    system('rm *.txt')
    
    setwd('/home/miguel/Desktop/Documents/mutualistic-evolution/new-runs-Nov-2023')
  }
  # }
}

# write.csv(output, file='output-structural-stability.csv')


output <- read.csv("output-structural-stability.csv")

require(ggplot2)
require(Rmisc)

sum_out <- summarySE(output, measurevar="fraction", groupvars=c("delta", "competition", "iteration", "selection", "exp_delta"))

ggplot(sum_out, aes(iteration, fraction, colour=as.factor(exp_delta))) + theme_bw() + 
  facet_grid(selection~delta) + 
  geom_errorbar(aes(ymin=fraction-se, ymax=fraction+se), width=.03) +
  geom_line() +
  geom_point() 

ggplot(output, aes(delta, fraction)) + theme_bw() + geom_point(colour='orange')


out_frac_sims <- NULL

for(sel in unique(output$selection)){
  cur_sel <- output[which(output$selection == sel),]
  for(del in unique(cur_sel$delta)){
    cur_del <- cur_sel[which(cur_sel$delta == del),]
    for(rho in c(0.0125, 0.15)){
      cur_rho <- cur_del[which(cur_del$competition == rho),]
      for(exp_d in unique(cur_rho$exp_delta)){
        cur_expdel <- cur_rho[which(cur_rho$exp_delta == exp_d),]
        for(i in unique(cur_expdel$iteration)){
          if(i > 2000) next
          cur_iter <- cur_expdel[which(cur_expdel$iteration == i),]
          if(dim(cur_iter)[1] < 10) next
          print(dim(cur_iter)[1])
          cur_out_frac <- data.frame(selection=sel, delta=del, competition=rho, exp_delta=exp_d, iteration=i, fraction_sims=(length(which(cur_iter$extinct != 0))/dim(cur_iter)[1]))
          
          if(is.null(out_frac_sims)){
            out_frac_sims <- cur_out_frac
          }else{
            out_frac_sims <-rbind(out_frac_sims, cur_out_frac)
          }
        } 
      }
    }  
  }
}

# for(c in unique(output$competition)){
#   for(n in unique(output$network)){
#     for(d in unique(output$delta)){
#       for(i in unique(output$iteration)){ 
#         cur_net <- subset(output, (replicate == n & competition == c & delta == d & iteration == i))
#         
#         cur_out_frac <- data.frame(replicate=n, competition=c, delta=d, iteration=i, fraction_sims=(length(which(cur_net$extinct != 0))/dim(cur_net)[1]))
#         
#         if(is.null(out_frac_sims)){
#           out_frac_sims <- cur_out_frac
#         }else{
#           out_frac_sims <-rbind(out_frac_sims, cur_out_frac)
#         }
#       }
#     }
#   }
# }

# out_frac_sims <- out_frac_sims[with(out_frac_sims, order(rho, replicate, delta, iteration)),]

sum_out <- summarySE(out_frac_sims, measurevar="fraction_sims", groupvars=c("selection", "delta", "competition", "exp_delta", "iteration"))

temp <- out_frac_sims[which(as.character(out_frac_sims$exp_delta) %in% c('0.05', '0.2', '0.35', '0.5', '0.65', '0.8', '0.95')),]

a <- ggplot(temp, aes(iteration/1000, fraction_sims, colour=as.factor(exp_delta))) + 
  facet_grid(delta+competition~selection, 
             labeller = as_labeller(c('FALSE'='Neutral', 'TRUE'='Selection', '0'='0', '0.1'='0.1', '0.0125'='0.0125', '0.15'='0.15'))) +
  geom_point(size=1) +
  # geom_errorbar(aes(ymin=fraction_sims-se, ymax=fraction_sims+se), width=.03) +
  # geom_line() +
  geom_smooth(se=FALSE, size=1, method='loess', span=.4) +
  theme_bw() + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text = element_text(size=20, colour ='black'),
        axis.title = element_text(size=30),
        legend.title = element_text(size=18, face='bold'),
        legend.text = element_text(size=18),
        strip.text = element_text(size = 25),
        strip.background = element_rect(colour = "black", fill=NA, size=1),
        legend.position = 'top') +
  guides(colour = guide_legend(nrow = 1)) +
  labs(y='Fraction of simulations with at least 1 extinction', 
       x= 'Number of substitutions (x1000)', color='delta') +
  scale_color_brewer(name='Steady state perturbation', palette='Dark2')

ggsave('figure-structural-stability-new.pdf', width = 10, height = 10)





