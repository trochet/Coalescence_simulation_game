#Initialization parameters
#This is for a game that involves simulating population structure (2 populations)

#Number of chromosomes (half the number of people)
n_chrom<-18

#Haplotype length
hap_length<-120

#Number of possible lineage sites per population (will be assigned letters of the alphabet, so don't put more than 26)
n_lin<-9

#Number of lineage sites that induce mutation per population
n_mut_sites<-2

#Number of lineage sites that induce recombination per population
n_recomb_sites<-2

#Number of lineage sites that induce mutation and recombination per population
n_mut_recomb_sites<-1

#Number of lineage sites that allow migration per population
n_mig_sites<-1

#Length of the p arm
p_arm_limit<-40

###########################
#  Creates data matrices  #
###########################

#Creates names of lineage sites
sites<-LETTERS[1:(n_lin*2)]
sites_pop1<-LETTERS[1:n_lin]
sites_pop2<-LETTERS[(n_lin+1):(n_lin*2)]

#Creates the player names
players<-rep(NA,2*n_chrom)
for(i in 1:n_chrom){
    players[(2*i)-1]<-paste0("p_",i)
    players[(2*i)]<-paste0("q_",i)
}

#Matrix of haplotypes
haplotypes<-matrix(0,nrow=n_chrom,ncol=hap_length)

#Matrix of lineages, indicating which one had which players
rounds<-as.data.frame(matrix(NA,nrow=1,ncol=2*n_lin))
names(rounds)<-sites

#Matrix of lineage associated with each player in each round
player_lin<-as.data.frame(matrix(NA,nrow=1,ncol=2*n_chrom))
names(player_lin)<-players

#Random chooses lineage sites for mutation, recombination, or both
mut_sites_pop1<-sample(sites_pop1,n_mut_sites,replace=FALSE,prob=rep(1/n_lin,n_lin))
mut_sites_pop2<-sample(sites_pop2,n_mut_sites,replace=FALSE,prob=rep(1/n_lin,n_lin))
mut_sites<-union(mut_sites_pop1,mut_sites_pop2)

recomb_sites_pop1<-sample(setdiff(sites_pop1,mut_sites_pop1),n_recomb_sites,replace=FALSE,prob=rep(1/(n_lin-n_mut_sites),n_lin-n_mut_sites))
recomb_sites_pop2<-sample(setdiff(sites_pop2,mut_sites_pop2),n_recomb_sites,replace=FALSE,prob=rep(1/(n_lin-n_mut_sites),n_lin-n_mut_sites))
recomb_sites<-union(recomb_sites_pop1,recomb_sites_pop2)

mut_recomb_sites_pop1<-sample(setdiff(sites_pop1,union(mut_sites_pop1,recomb_sites_pop1)),n_mut_recomb_sites,replace=FALSE,prob=rep(1/(n_lin-n_mut_sites-n_recomb_sites),n_lin-n_mut_sites-n_recomb_sites))
mut_recomb_sites_pop2<-sample(setdiff(sites_pop2,union(mut_sites_pop2,recomb_sites_pop2)),n_mut_recomb_sites,replace=FALSE,prob=rep(1/(n_lin-n_mut_sites-n_recomb_sites),n_lin-n_mut_sites-n_recomb_sites))
mut_recomb_sites<-union(mut_recomb_sites_pop1,mut_recomb_sites_pop2)

mig_sites_pop1<-sample(setdiff(sites_pop1,union(mut_recomb_sites_pop1,union(mut_sites_pop1,recomb_sites_pop1))),n_mig_sites,replace=FALSE,prob=rep(1/n_lin,n_lin))
mig_sites_pop2<-sample(setdiff(sites_pop2,union(mut_recomb_sites_pop2,union(mut_sites_pop2,recomb_sites_pop2))),n_mig_sites,replace=FALSE,prob=rep(1/n_lin,n_lin))
mig_sites<-union(mig_sites_pop1,mig_sites_pop2)


#######################
#     Initializes     #
#######################
#Random start
#init<-sample(union(sites_pop1,sites_pop2),n_chrom,replace=FALSE,prob=rep(1/2*n_lin,2*n_lin))

#Start with equal numbers (if possible) in both populations
init<-union(sample(sites_pop1,ceiling(n_chrom/2),replace=FALSE,prob=rep(1/n_lin,n_lin)),sample(sites_pop2,floor(n_chrom/2),replace=FALSE,prob=rep(1/n_lin,n_lin)))

for(i in 1:n_chrom){
    rounds[1,which(sites==init[i])]<-paste0("p_",i,",q_",i)
    player_lin[1,which(names(player_lin)==names(player_lin)[grep(paste0("_",i,"$"),names(player_lin),perl=TRUE)])]<-init[i]
}





####################################
# Function to find stop conditions #
####################################
count_players<-function(v){
    #v is a row of from "rounds"
    v<-as.character(v)
    counts_p<-rep(0,length(v))
    counts_q<-rep(0,length(v))
    for(i in 1:length(v)){
        if(v[i]=="NA" || is.na(v[i])){
            next
        }
        counts_p[i]<-length(grep("^p_",unlist(strsplit(v[i],","))))
        counts_q[i]<-length(grep("^q_",unlist(strsplit(v[i],","))))
    }
    return(max(max(counts_p),max(counts_q)))
}

######################
#  Playing the game  #
######################

#Stops when all of the short arms or the long arms coalesce
while(max(count_players(rounds[nrow(rounds),]))<n_chrom){
    #Mutations
    all_mut_sites<-union(mut_sites,mut_recomb_sites)
    for(s in all_mut_sites){
        if(is.na(rounds[nrow(rounds),which(names(rounds)==s)])){
            next
        } else {
            site_players<-unlist(strsplit(rounds[nrow(rounds),which(names(rounds)==s)],","))
            
            mut_site<-sample(1:hap_length,1,prob=rep(1/hap_length,hap_length))
            if(mut_site<=p_arm_limit){
                if(length(grep("^p_",site_players,perl=TRUE))>0){
                    p_site_players<-site_players[grep("^p_",site_players,perl=TRUE)]
                    haplotypes[setdiff(as.numeric(unlist(strsplit(p_site_players,"^p_"))),NA),mut_site]<-1
                }
            } else {
                if(length(grep("^q_",site_players,perl=TRUE))>0){
                    q_site_players<-site_players[grep("^q_",site_players,perl=TRUE)]
                    haplotypes[setdiff(as.numeric(unlist(strsplit(q_site_players,"^q_"))),NA),mut_site]<-1
                }
            }
        }
    }
    #Deciding where to send players
    rounds[nrow(rounds)+1,]<-NA
    player_lin[nrow(player_lin)+1,]<-NA
    
    #Recombinations
    all_recomb_sites<-union(recomb_sites,mut_recomb_sites)
    for(s in all_recomb_sites){
        if(is.na(rounds[nrow(rounds)-1,which(names(rounds)==s)])){
            next
        } else {
            #Destinations for the separate arms
            p_dest<-sample(sites,1,prob=rep(1/(2*n_lin),2*n_lin))
            q_dest<-sample(sites,1,prob=rep(1/(2*n_lin),2*n_lin))
            if(s %in% sites_pop1){
                if(p_dest %in% setdiff(sites_pop2,mig_sites_pop2)){
                    while(p_dest %in% setdiff(sites_pop2,mig_sites_pop2)){
                        p_dest<-sample(sites,1,prob=rep(1/(2*n_lin),2*n_lin))
                    }
                }
                if(q_dest %in% setdiff(sites_pop2,mig_sites_pop2)){
                    while(q_dest %in% setdiff(sites_pop2,mig_sites_pop2)){
                        q_dest<-sample(sites,1,prob=rep(1/(2*n_lin),2*n_lin))
                    }
                }
            } else {
                if(p_dest %in% setdiff(sites_pop1,mig_sites_pop1)){
                    while(p_dest %in% setdiff(sites_pop1,mig_sites_pop1)){
                        p_dest<-sample(sites,1,prob=rep(1/(2*n_lin),2*n_lin))
                    }
                }
                if(q_dest %in% setdiff(sites_pop1,mig_sites_pop1)){
                    while(q_dest %in% setdiff(sites_pop1,mig_sites_pop1)){
                        q_dest<-sample(sites,1,prob=rep(1/(2*n_lin),2*n_lin))
                    }
                }
            }
            site_players<-unlist(strsplit(rounds[nrow(rounds)-1,which(names(rounds)==s)],","))
            if(length(grep("^p_",site_players,perl=TRUE))>0){
                p_site_players<-site_players[grep("^p_",site_players,perl=TRUE)]
                if(is.na(rounds[nrow(rounds),which(sites==p_dest)])){
                    rounds[nrow(rounds),which(sites==p_dest)]<-paste(p_site_players,collapse=",")
                } else {
                    rounds[nrow(rounds),which(sites==p_dest)]<-paste(c(rounds[nrow(rounds),which(sites==p_dest)],p_site_players),collapse=",")
                }
                player_lin[nrow(player_lin),which(players %in% p_site_players)]<-p_dest
            }
            if(length(grep("^q_",site_players,perl=TRUE))>0){
                q_site_players<-site_players[grep("^q_",site_players,perl=TRUE)]
                if(is.na(rounds[nrow(rounds),which(sites==q_dest)])){
                    rounds[nrow(rounds),which(sites==q_dest)]<-paste(q_site_players,collapse=",")
                } else {
                    rounds[nrow(rounds),which(sites==q_dest)]<-paste(c(rounds[nrow(rounds),which(sites==q_dest)],q_site_players),collapse=",")
                }
                player_lin[nrow(player_lin),which(players %in% q_site_players)]<-q_dest
            }
        }
    }
    #Everyone else
    non_recomb_sites<-setdiff(sites,all_recomb_sites)
    for(s in non_recomb_sites){
        if(is.na(rounds[nrow(rounds)-1,which(names(rounds)==s)])){
            next
        } else {
            dest<-sample(sites,1,prob=rep(1/(2*n_lin),2*n_lin))
            if(s %in% sites_pop1){
                if(dest %in% setdiff(sites_pop2,mig_sites_pop2)){
                    while(dest %in% setdiff(sites_pop2,mig_sites_pop2)){
                        dest<-sample(sites,1,prob=rep(1/(2*n_lin),(2*n_lin)))
                    }
                }
            } else {
                if(dest %in% setdiff(sites_pop1,mig_sites_pop1)){
                    while(dest %in% setdiff(sites_pop1,mig_sites_pop1)){
                        dest<-sample(sites,1,prob=rep(1/(2*n_lin),(2*n_lin)))
                    }
                }
            }
            site_players<-unlist(strsplit(rounds[nrow(rounds)-1,which(names(rounds)==s)],","))
            if(is.na(rounds[nrow(rounds),which(sites==dest)])){
                rounds[nrow(rounds),which(sites==dest)]<-paste(site_players,collapse=",")
            } else {
                rounds[nrow(rounds),which(sites==dest)]<-paste(c(rounds[nrow(rounds),which(sites==dest)],site_players),collapse=",")
            }
            player_lin[nrow(player_lin),which(players %in% site_players)]<-dest
        }
    }
}
