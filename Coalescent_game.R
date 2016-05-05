#Initialization parameters

#Number of chromosomes (half the number of people)
n_chrom<-18

#Haplotype length
hap_length<-120

#Number of possible lineage sites (will be assigned letters of the alphabet)
n_lin<-18

#Number of lineage sites that induce mutation
n_mut_sites<-10

#Number of lineage sites that induce recombination
n_recomb_sites<-2

#Number of lineage sites that induce mutation and recombination
n_mut_recomb_sites<-2

#Length of the p arm
p_arm_limit<-40

###########################
#  Creates data matrices  #
###########################

#Creates names of lineage sites
sites<-LETTERS[1:n_lin]

#Creates the player names
players<-rep(NA,2*n_chrom)
for(i in 1:n_chrom){
    players[(2*i)-1]<-paste0("p_",i)
    players[(2*i)]<-paste0("q_",i)
}

#Matrix of haplotypes
haplotypes<-matrix(0,nrow=n_chrom,ncol=hap_length)

#Matrix of lineages, indicating which one had which players
rounds<-as.data.frame(matrix(NA,nrow=1,ncol=n_lin))
names(rounds)<-sites

#Matrix of lineage associated with each player in each round
player_lin<-as.data.frame(matrix(NA,nrow=1,ncol=2*n_chrom))
names(player_lin)<-players

#Random chooses lineage sites for mutation, recombination, or both
mut_sites<-sample(sites,n_mut_sites,replace=FALSE,prob=rep(1/n_lin,n_lin))
recomb_sites<-sample(setdiff(sites,mut_sites),n_recomb_sites,replace=FALSE,prob=rep(1/(n_lin-n_mut_sites),n_lin-n_mut_sites))
mut_recomb_sites<-sample(setdiff(sites,union(mut_sites,recomb_sites)),n_mut_recomb_sites,replace=FALSE,prob=rep(1/(n_lin-n_mut_sites-n_recomb_sites),n_lin-n_mut_sites-n_recomb_sites))


#######################
#     Initializes     #
#######################
init<-sample(sites,n_chrom,replace=FALSE,prob=rep(1/n_lin,n_lin))
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
            p_dest<-sample(sites,1,prob=rep(1/n_lin,n_lin))
            q_dest<-sample(sites,1,prob=rep(1/n_lin,n_lin))
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
            dest<-sample(sites,1,prob=rep(1/n_lin,n_lin))
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
