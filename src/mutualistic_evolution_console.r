
##########################################################################################################
####### This code is for the project on the evolution of mutualistic networks entitled:
####### Evolution of critical competition in mutualistic communities

####### It implements a wrapper for the application developed by Alberto Pascual-Garcia which
####### is used to evaluate the dynamics of a mutualistic communities based on his previous work

####### In this script the evolution of this mutualistic interactions is implemented
####### Thus, ecological dynamics are obtained from the Alberto's software and 
####### evolutionary ones are implemeneted herein.

####### Author: Miguel Lurgi

require(igraph)
# 
# dir <- './macrodynamics/Mutualism/Multiple_Adaptive/test_run/'
# 
# inc_matrix <- read.table(paste0(dir,'Rmrz89.txt'))
# n_plants <- dim(inc_matrix)[1]
# n_animals <- dim(inc_matrix)[2]
# 
# row.names(inc_matrix) <- paste0('Plant-', 1:n_plants)
# colnames(inc_matrix) <- paste0('Animal-', 1:n_animals)
# 
# initial_net <- graph_from_incidence_matrix(inc_matrix, directed=T, mode='out')
# 
# par(mar=c(0,0,0,0))
# plot(initial_net, layout=layout.bipartite, vertex.size=5, vertex.label=NA)
# 
# setwd(dir)
# system('./integration.exe')
# 
# #### the interaction strengths for plants and animals respectively.
# #### for some reason the former are written by row and the latter by column
# inter_specific_plants <- matrix(read.table('gammaOutP.out')[,1], nrow=n_plants, ncol=n_animals, byrow=T)
# inter_specific_animals <- matrix(read.table('gammaOutA.out')[,1], nrow=n_plants, ncol=n_animals, byrow=F)
# 
# header <- c('species', 'BiomassIn', 'BiomassOut', 'Alpha', 'DegreeIn',  'DegreeOut', 'Alive')
# result_dynamics <- read.table('plantsOut-Final.out', stringsAsFactors = F)
# names(result_dynamics) <- header
# 
# params <- read.table('Beyond-MeanField.in', stringsAsFactors = F)
# write.table(params, file='Beyond-MeanField.in.new', quote=F, row.names = F, col.names = F)




################## till here are the tests on how to read and write the different files that are
################## written/read by the integration software

################## below we use that knowledge to implement the evolutionary simulations
start_time <- proc.time()
header <- c('species', 'BiomassIn', 'BiomassOut', 'Alpha', 'DegreeIn', 'DegreeOut', 'Alive')
setwd('/home/miguel/Desktop/Documents/mutualistic-evolution/macrodynamics/Mutualism/Multiple_Adaptive/test_run')
competition_regimes <- c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15) #c(0.025, 0.05, 0.075, 0.1, 0.125)   # 0.05, 0.107)
deltas <- c(0) # c(0.49, 0.3, 0.21, 0.16, 0.12)      ###### these are the original deltas
delta_change <- c(0) # c(0, .5, 0.105)  ### formerly 0.21

selection_level <- c('species') #, 'community')
selection_types <- c('biomass')  #c('biomass', 'persistence')
selection_regimes <- c(TRUE, FALSE) #c(TRUE, FALSE)

p <- 0.5
q <- 0.5
replicates <- 25
iterations <- 1000
output <- NULL

for(s_lev in selection_level){
  sel_lev_dir <- paste0('./selection-level-',s_lev)
  dir.create(sel_lev_dir, showWarnings = FALSE)
  
  print(s_lev)
  for(s_type in selection_types){
    
    # if(s_lev == 'species' & s_type == 'biomass') next;
    
    sel_type_dir <- paste0(sel_lev_dir,'/selection-type-',s_type)
    dir.create(sel_type_dir, showWarnings = FALSE)
    
    print(s_type)
    for(sel in selection_regimes){
      sel_dir <- paste0(sel_type_dir,'/selection-',sel)
      dir.create(sel_dir, showWarnings = FALSE)
      
      #if( (!sel) & (s_lev != 'species' | s_type != 'biomass')) next;
      
      print(sel)
      for(comp in competition_regimes){
        
        #if(s_type == 'biomass' & s_lev == 'species' & sel & comp < 0.075) next
        
        print(comp)
        comp_dir <- paste0(sel_dir,'/competition-', comp)
        dir.create(comp_dir, showWarnings = FALSE)
        delta_original <- 0 #deltas[which(competition_regimes == comp)]
        # delta <- 0 #delta - (delta*.5)       ##### we make delta half of its original value
        
        for(delta_c in delta_change){
          
          # if(s_lev == 'species' & s_type == 'persistence' & sel & comp == 0.075 & delta_c == 0) next;
          
          if(delta_c != .5){
            delta <- delta_c
          }else{
            delta <- delta_original - (delta_original*.5)
          }
          
          print(delta)
          dest_dir <- paste0(comp_dir,'/delta-', delta_c)
          dir.create(dest_dir, showWarnings = FALSE)
          
          for(r in 1:replicates){
            
            # if(s_type == 'biomass' & s_lev == 'species' & sel & comp == 0.075 & delta_c == 0 & r < 59) next
            
            print(r)
            #### for each competition regime we make sure we set the competition parameter to the right value
            #### in the input parameters file
            if(r == 1){
              params <- read.table('./original_files/Beyond-MeanField.in', stringsAsFactors = F)
              params[9,] <- comp
              params[10,] <- comp
              params[17,] <- delta
              write.table(params, file='./original_files/Beyond-MeanField.in', quote=F, row.names = F, col.names = F)
              
              system('rm *.in')
              system(paste0('cp ./original_files/Beyond-MeanField.in .'))
            }
            
            if(sel){
              evolution_failed <- FALSE
            }
            conn <- 0
            # i <- 0
            # while(conn < .95){
            #   i <- i+1
            ended_before <- FALSE
            
            change_attempts <- 1
            for(i in 1:iterations){
              #print(paste0('selection = ', sel, ' competition = ', comp, ' replicate = ', r,' iteration = ',i))
              
              if( (!sel) | (sel & !evolution_failed)){
                params <- read.table('Beyond-MeanField.in', stringsAsFactors = F)
                system('./integration.exe', ignore.stdout = T)
                
                ###### read the dynamics output to keep track of what's going on:
                cur_out <- read.table('Summary_Beyond-MeanField.tmp', stringsAsFactors = F)
                names(cur_out) <- c('var', 'value')
                
                ##### metrics characterising the system
                indeg <- cur_out[which(cur_out$var == 'AvDegreeInA'),]$value
                plants <- cur_out[which(cur_out$var == 'Sp'),]$value - cur_out[which(cur_out$var == 'ExtinctP'),]$value
                animals <- cur_out[which(cur_out$var == 'Sa'),]$value - cur_out[which(cur_out$var == 'ExtinctA'),]$value
                plants_biomass <- cur_out[which(cur_out$var == 'BiomassP'),]$value
                animals_biomass <- cur_out[which(cur_out$var == 'BiomassA'),]$value
                nestedness <- cur_out[which(cur_out$var == 'NestednessTin'),]$value
                conn <- cur_out[which(cur_out$var == 'ConnectanceIn'),]$value
                evenness_plants <- cur_out[which(cur_out$var == 'EntropyP'),]$value
                evenness_animals <- cur_out[which(cur_out$var == 'EntropyA'),]$value
                
                
                cur_result_plant <- read.table('plantsOut-Final.out', stringsAsFactors = F)
                names(cur_result_plant) <- header
                
                cur_result_animal <- read.table('animalsOut-Final.out', stringsAsFactors = F)
                names(cur_result_animal) <- header
                
                cv_alpha_p <- sd(cur_result_plant$Alpha)/mean(cur_result_plant$Alpha)
                cv_alpha_a <- sd(cur_result_animal$Alpha)/mean(cur_result_animal$Alpha)
                
                cur_line <- data.frame(sel_level=s_lev, sel_type=s_type, selection=sel, competition=comp, delta=delta, delta_change=delta_c, replicate=r, iteration=i, change_attempts=change_attempts, evolution_failed=evolution_failed, indegree=indeg, SP=plants, SA=animals, BiomassP=plants_biomass, BiomassA=animals_biomass, nestedness=nestedness, connectance=conn, evenness_plants=evenness_plants, evenness_animals=evenness_animals, cv_alpha_p=cv_alpha_p, cv_alpha_a=cv_alpha_a)
                
                if(is.null(output)){
                  output <- cur_line
                }else{
                  output <- rbind(output, cur_line)
                }
                
                #### reseet the counter of change attempts
                change_attempts <- 1
                
                if(animals < 2 | plants < 2){
                  print('There are not enough plants or animals left --- Replicate terminated')
                  ended_before <- TRUE
                  break
                }
                
                inc_matrix <- matrix(read.table('gammaOutP.out')[,1], nrow=max(cur_result_plant$species), ncol=max(cur_result_animal$species), byrow=T)
                
                dead_plants <- which(!cur_result_plant$Alive) # | cur_result_plant$DegreeOut == 0)
                if(length(dead_plants) > 0) cur_result_plant <- cur_result_plant[-dead_plants,]
                
                dead_animals <- which(!cur_result_animal$Alive) #  | cur_result_animal$DegreeOut == 0)
                if(length(dead_animals) > 0) cur_result_animal <- cur_result_animal[-dead_animals,]
                
                if(length(dead_plants) > 0) inc_matrix <- inc_matrix[-dead_plants,]
                if(length(dead_animals) > 0) inc_matrix <- inc_matrix[,-dead_animals]
                
                n_plants <- dim(inc_matrix)[1]
                n_animals <- dim(inc_matrix)[2]
                
                row.names(inc_matrix) <- paste0('Plant-', 1:n_plants)
                colnames(inc_matrix) <- paste0('Animal-', 1:n_animals)
                
                cur_net <- graph_from_incidence_matrix(inc_matrix, directed=T, mode='out', weighted=TRUE)
                original_net <- cur_net
                # we remove disconnected vertices that might have gotten extinct during ecological dynamics
                #original_net <- cur_net <- delete.vertices(cur_net, degree(cur_net, mode='all') == 0)
                
              }
              
              #### when evolution fails we still need to keep track of the network properties,
              #### we keep track of that by keeping the properties of the previous network
              if(sel & evolution_failed & !is.null(output)){
                cur_line$iteration <- i
                cur_line$change_attempts <- change_attempts
                cur_line$evolution_failed <- evolution_failed
                output <- rbind(output, cur_line)
              }
              
              #### this is the species that will be changed
              cur_net <- original_net
              species_to_change <- sample(V(cur_net), 1)
              sp_change_name <- species_to_change$name
              
              #print(paste0('species to change = ', sp_change_name))
              
              if(p >= runif(1)){
                #print('interaction swap!')
                # interaction swap
                
                #### first we select the interaction partner that we are going to change
                #### and the new interaction partner
                neighs <- neighbors(cur_net, species_to_change, mode = 'all')
                
                
                if(length(setdiff(V(cur_net)[which(V(cur_net)$type != species_to_change$type)], neighs)) == 0){
                  print(paste0('Species ', species_to_change$name, ' is fully connected and therefore cannot swap connections'))
                  next
                }
                
                ### if the species doesn't have interactions the only way to go is to create one...
                if(length(neighs) == 0){
                  #print(paste0('species with no interaction - creating new link- iteration: ', i, ' - competition: ', comp, ' selection: ', sel))
                  possible_partners <- V(cur_net)[which(V(cur_net)$type != species_to_change$type)]
                  new_partner <- sample(possible_partners, 1)
                  if(species_to_change$type){
                    cur_net <- cur_net + edge(new_partner, species_to_change, weight=rnorm(1, 0.15, 0.05))
                  }else{
                    cur_net <- cur_net + edge(species_to_change, new_partner, weight=rnorm(1, 0.15, 0.05))
                  }
                }else{
                  
                  if(length(neighs) > 1){
                    cur_partner <- sample(neighs, 1)
                  }else{
                    cur_partner <- neighs
                  }
                  possible_partners <- V(cur_net)[which(V(cur_net)$type != species_to_change$type)]
                  if(length(possible_partners[-which(possible_partners == cur_partner)]) > 1){
                    new_partner <- sample(possible_partners[-which(possible_partners == cur_partner)], 1)  
                  }else if(length(possible_partners[-which(possible_partners == cur_partner)] == 1)){
                    new_partner <- possible_partners[-which(possible_partners == cur_partner)]
                  }
                  
                  
                  #### then, based on whether the new partner was a partner already we decide whether
                  #### to create and remove links or just swap interaction strengths
                  cur_edge <- E(cur_net)[species_to_change %--% cur_partner]
                  cur_weight <- cur_edge$weight
                  if(new_partner %in% neighs){
                    E(cur_net)[species_to_change %--% cur_partner]$weight <- E(cur_net)[species_to_change %--% new_partner]$weight
                    E(cur_net)[species_to_change %--% new_partner]$weight <- cur_weight
                    
                  }else{
                    if(species_to_change$type){
                      cur_net <- cur_net - cur_edge + edge(new_partner, species_to_change, weight=cur_weight) 
                    }else{
                      cur_net <- cur_net - cur_edge + edge(species_to_change, new_partner, weight=cur_weight)
                    }
                  }
                }
              }else{
                
                # loss or creation of an interaction
                if(runif(1) < q | (degree(cur_net, species_to_change, mode = 'all') == 0)){
                  #print(paste0('create link! - iteration: ', i, ' - competition: ', comp, ' selection: ', sel))
                  # create
                  
                  neighs <- neighbors(cur_net, species_to_change, mode = 'all')
                  if(length(setdiff(V(cur_net)[which(V(cur_net)$type != species_to_change$type)], neighs)) == 0){
                    print(paste0('Species ', species_to_change$name, ' is fully connected and therefore cannot create new connections'))
                    next
                  }
                  possible_partners <- V(cur_net)[which(V(cur_net)$type != species_to_change$type)]
                  
                  
                  if(length(which(possible_partners %in% neighs)) == 0){
                    new_partner <- sample(possible_partners, 1)
                  }else if(length(which(!(possible_partners %in% neighs))) == 1){
                    new_partner <- possible_partners[-which(possible_partners %in% neighs)]
                  }else{
                    new_partner <- sample(possible_partners[-which(possible_partners %in% neighs)], 1)
                  }
                  
                  
                  if(species_to_change$type){
                    cur_net <- cur_net + edge(new_partner, species_to_change, weight=rnorm(1, 0.15, 0.05))
                  }else{
                    cur_net <- cur_net + edge(species_to_change, new_partner, weight=rnorm(1, 0.15, 0.05))
                  }
                  
                }else{
                  #print('loose link!')
                  # loose
                  if(length(incident_edges(cur_net, species_to_change, mode='all')[[1]]) > 1){
                    cur_net <- cur_net - sample(incident_edges(cur_net, species_to_change, mode='all')[[1]],1)
                  }
                  
                  #### this part of the code was commented because we shouldn't allow for species to remain unconnected
                  else{
                    cur_net <- cur_net - incident_edges(cur_net, species_to_change, mode='all')[[1]]
                  }
                }
              }
              
              # we remove disconnected vertices
              # cur_net <- delete.vertices(cur_net, degree(cur_net, mode='all') == 0)
              
              # if(sum(degree(cur_net, mode='all') == 0)){
              #   print('There are unconnected species')
              # }
              
              #### if selection is enforced (i.e., we only accept changes if beneficial for the species
              #### subject to change), then we need to check this constraint is met before saving all the changes
              if(sel){
                if(evolution_failed){
                  evolution_failed <- FALSE
                }
                
                if(s_lev == 'species'){
                  sp_index <- as.numeric(strsplit(sp_change_name, '-')[[1]][2])
                  if(grepl('Plant', sp_change_name)){
                    cur_biomass_sp <- cur_result_plant[sp_index,]$BiomassOut 
                  }else{
                    cur_biomass_sp <- cur_result_animal[sp_index,]$BiomassOut 
                  }
                }else if(s_lev == 'community'){
                  cur_live_sps <- sum(as.numeric(as.logical(cur_result_plant$Alive))) + sum(as.numeric(as.logical(cur_result_animal$Alive)))
                }
                
                #### we write everything to the temp folder and run a temp simulation to figure out whether the mutation was beneficial for the species
                setwd('/home/miguel/Desktop/Documents/mutualistic-evolution/macrodynamics/Mutualism/Multiple_Adaptive/test_run/temp')
                
                #### writing the network
                write.table(as_incidence_matrix(cur_net), paste0('network-',i,'.txt'), col.names = FALSE, row.names = FALSE, quote=FALSE)
                
                #### and the strengths
                write.table(as.vector(t(as_incidence_matrix(cur_net, attr = 'weight'))), 'gammaInP.dat', col.names = FALSE, row.names = FALSE, quote=FALSE)
                write.table(as.vector(as_incidence_matrix(cur_net, attr = 'weight')), 'gammaInA.dat', col.names = FALSE, row.names = FALSE, quote=FALSE)
                
                #### write initial abundances for next iteration
                write.table(cur_result_plant$BiomassOut, file='plantsIn.dat', quote=F, row.names = F, col.names = F)
                write.table(cur_result_animal$BiomassOut, file='animalsIn.dat', quote=F, row.names = F, col.names = F)
                
                #### write the growth rates (alphas) for next iteration
                write.table(cur_result_plant$Alpha, file='alphaInP.dat', quote=F, row.names = F, col.names = F)
                write.table(cur_result_animal$Alpha, file='alphaInA.dat', quote=F, row.names = F, col.names = F)
                
                #### writing the params table
                #### starting abundances should be read from file
                params_test <- params
                params_test[1,] <- 0
                params_test[3,] <- 0
                #### make sure alpha's (growth rates) are read from files
                params_test[5,] <- 0
                
                ## read interaction strengths from file
                params_test[11,] <- -1000000
                
                ## numbers of species in new network
                params_test[15,] <- length(which(!V(cur_net)$type))   ## plants
                params_test[16,] <- length(which(V(cur_net)$type))   ## animals
                
                params_test[17,] <- delta # 0 = no perturbations, delta = perturbation according to the values of rho, C and Nest
                params_test[18,] <- paste0('network-',i,'.txt')
                params_test[20,] <- params[20,]
                write.table(params_test, file='Beyond-MeanField.in', quote=F, row.names = F, col.names = F)
                
                system('./integration.exe', ignore.stdout = T)
                
                
                if(s_lev == 'species'){
                  if(grepl('Plant', sp_change_name)){
                    new_result_plant <- read.table('plantsOut-Final.out', stringsAsFactors = F)
                    names(new_result_plant) <- header
                    
                    new_biomass_sp <- new_result_plant[sp_index,]$BiomassOut
                    new_alive_sp <- new_result_plant[sp_index,]$Alive
                  }else{
                    new_result_animal <- read.table('animalsOut-Final.out', stringsAsFactors = F)
                    names(new_result_animal) <- header
                    
                    new_biomass_sp <- new_result_animal[sp_index,]$BiomassOut 
                    new_alive_sp <- new_result_animal[sp_index,]$Alive
                  }
                  
                  if(s_type == 'biomass'){
                    if(new_biomass_sp < cur_biomass_sp){
                      evolution_failed <- TRUE
                    }  
                  }else if(s_type == 'persistence'){
                    if(!new_alive_sp){
                      evolution_failed <- TRUE
                    }  
                  }  
                }else if(s_lev == 'community'){
                  
                  new_result_plant <- read.table('plantsOut-Final.out', stringsAsFactors = F)
                  new_result_animal <- read.table('animalsOut-Final.out', stringsAsFactors = F)
                  names(new_result_plant) <- header
                  names(new_result_animal) <- header
                  
                  cur_biomass_com <- sum(new_result_plant$BiomassIn) + sum(new_result_animal$BiomassIn)
                  
                  new_live_sps <- sum(as.numeric(as.logical(new_result_plant$Alive))) + sum(as.numeric(as.logical(new_result_animal$Alive)))
                  new_biomass_com <- sum(new_result_plant$BiomassOut) + sum(new_result_animal$BiomassOut)
                  
                  if(s_type == 'biomass'){
                    if(new_biomass_com < cur_biomass_com){
                      evolution_failed <- TRUE
                    }  
                  }else if(s_type == 'persistence'){
                    if(new_live_sps < cur_live_sps){
                      evolution_failed <- TRUE
                    }  
                  }  
                }
                
                system(paste0('rm *.dat'))
                system(paste0('rm *.out'))
                system(paste0('rm *.tmp'))
                system(paste0('rm *.txt'))
                system(paste0('rm *.in'))
                
                setwd('/home/miguel/Desktop/Documents/mutualistic-evolution/macrodynamics/Mutualism/Multiple_Adaptive/test_run')
              }
              
              if(sel & evolution_failed){
                change_attempts <- change_attempts + 1
                # print('evolution failed!')
                # print(cur_biomass_sp)
                # print(new_biomass_sp)
                # 
                # if(i == iterations){
                #   print("let's see what happens next... ")
                #   print(r)
                # }
                
                # if( (i==1) | (i%%10 == 0) ){
                new_dir <- paste0('./output-iter-',i)
                if(  ((i==1) | (i%%25 == 0)) & (!file.exists(new_dir)) & (r <= 5) ){
                  #### we move the output files of this iteration to a different folder
                  dir.create(new_dir, showWarnings = FALSE)
                  system(paste0('cp ', params[18,], ' ', new_dir))
                  system(paste0('cp *.dat ', new_dir))
                  system(paste0('cp *.out ', new_dir))
                  system(paste0('cp *.tmp ', new_dir))
                }
                next
              }
              # else{
              #   change_attempts <- 1
              # }
              
              new_dir <- paste0('./output-iter-',i)
              if(  ((i==1) | (i%%25 == 0)) & (!file.exists(new_dir)) & (r <= 5) ){
                # if( !file.exists(new_dir) ){
                #### we move the output files of this iteration to a different folder
                
                dir.create(new_dir, showWarnings = FALSE)
                system(paste0('mv ', params[18,], ' ', new_dir))
                system(paste0('mv *.dat ', new_dir))
                system(paste0('mv *.out ', new_dir))
                system(paste0('mv *.tmp ', new_dir))
              }else{
                system(paste0('rm ', params[18,]))
                system('rm *.dat')
                system('rm *.out')
                system('rm *.tmp')
              }
              
              #### and then we create the new input files based on the current state of the system
              #### so to prepare things for the next iteration
              #### writing the network
              write.table(as_incidence_matrix(cur_net), paste0('network-',i,'.txt'), col.names = FALSE, row.names = FALSE, quote=FALSE)
              
              #### and the strengths
              write.table(as.vector(t(as_incidence_matrix(cur_net, attr = 'weight'))), 'gammaInP.dat', col.names = FALSE, row.names = FALSE, quote=FALSE)
              write.table(as.vector(as_incidence_matrix(cur_net, attr = 'weight')), 'gammaInA.dat', col.names = FALSE, row.names = FALSE, quote=FALSE)
              
              #### write initial abundances for next iteration
              write.table(cur_result_plant$BiomassOut, file='plantsIn.dat', quote=F, row.names = F, col.names = F)
              write.table(cur_result_animal$BiomassOut, file='animalsIn.dat', quote=F, row.names = F, col.names = F)
              
              #### write the growth rates (alphas) for next iteration
              write.table(cur_result_plant$Alpha, file='alphaInP.dat', quote=F, row.names = F, col.names = F)
              write.table(cur_result_animal$Alpha, file='alphaInA.dat', quote=F, row.names = F, col.names = F)
              
              #### writing the params table
              #### starting abundances should be read from file
              params[1,] <- 0
              params[3,] <- 0
              #### make sure alpha's (growth rates) are read from files
              params[5,] <- 0
              
              ## read interaction strengths from file
              params[11,] <- -1000000
              
              ## numbers of species in new network
              params[15,] <- length(which(!V(cur_net)$type))   ## plants
              params[16,] <- length(which(V(cur_net)$type))   ## animals
              
              params[17,] <- delta # 0 = no perturbations, delta = perturbation according to the values of rho, C and Nest
              params[18,] <- paste0('network-',i,'.txt')
              params[20,] <- runif(1)
              write.table(params, file='Beyond-MeanField.in', quote=F, row.names = F, col.names = F)
              
            }
            
            #### here we need to do some housekeeping after a replicate has finished so the next one can start
            print('I will do the housekeeping')
            
            if(r <= 5){
              #### move every remaining file to the next-iter directory to continue runs if necessary
              new_dir <- paste0('./next-iter')
              dir.create(new_dir, showWarnings = FALSE)
              system(paste0('mv ', params[18,], ' ', new_dir))
              system(paste0('mv *.dat ', new_dir))
              
              new_dir <- paste0(dest_dir,'/output-replicate-',r)
              dir.create(new_dir, showWarnings = FALSE)
              
              system(paste0('mv *-iter* ', new_dir))
            }else{
              system('rm *.dat')
              system('rm *.txt')
              system('rm -rf *-iter*')
            }
            ### in case the last iteration didn't end well (i.e., evolution failed)
            ### we also need to remove the remaining output files to make sure the
            ### next iteration runs properly
            if((sel & evolution_failed) | ended_before){
              system('rm *.out')
              system('rm *.tmp')
              system('rm *.in')
              system('rm *.txt')
            }
            
            ### copy original starting files to current execution directory
            system(paste0('cp ./original_files/Beyond-MeanField.in .'))
            system(paste0('cp ./original_files/Rmrz89.txt .'))
            system(paste0('cp ./original_files/*.dat .'))
            
            ##### just to keep track of progress along the way in case the routine crashes...
            write.csv(output, file='temp-output-new.csv')
          }
          
        }
      }
    }
  }
}



stop_time <- proc.time()
print((stop_time - start_time))
