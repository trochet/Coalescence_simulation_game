library("diagram")
t<-seq(0,2*pi,length=((2*n_lin)+1))[1:(2*n_lin)]
coords<-as.data.frame(cbind(sin(t),cos(t)))
lab_coords<-cbind(sin(t)*1.07,cos(t)*1.07)
player_lab<-as.character(sort(rep(1:n_chrom,2)))
site_cols<-rep("black",n_lin*2)
site_cols[which(sites %in% sites_pop2)]<-"gray"
site_cols[which(sites %in% mut_sites_pop1)]<-"red"
site_cols[which(sites %in% mut_sites_pop2)]<-"coral1"
site_cols[which(sites %in% recomb_sites_pop1)]<-"blue"
site_cols[which(sites %in% recomb_sites_pop2)]<-"deepskyblue"
site_cols[which(sites %in% mut_recomb_sites_pop1)]<-"purple"
site_cols[which(sites %in% mut_recomb_sites_pop2)]<-"mediumpurple1"
site_cols[which(sites %in% mig_sites_pop1)]<-"green3"
site_cols[which(sites %in% mig_sites_pop2)]<-"green4"
player_col<-rep(NA,2*n_chrom)
for(i in 1:n_chrom){
    player_col[(2*i)-1]<-rainbow(n_chrom,alpha=0.4)[i]
    player_col[2*i]<-rainbow(n_chrom,alpha=0.4)[i]
}
line_widths<-rep(c(2,5),n_chrom)
line_types<-rep(c(1,2),n_chrom)

plot(coords[,c(1:2)],xlim=c(min(lab_coords[,1]),max(lab_coords[,1])),ylim=c(min(lab_coords[,2]),max(lab_coords[,2])),pch=16,cex=1.2,col=site_cols,xlab="",ylab="",axes=FALSE,main="Round 1")
text(lab_coords,sites)
legend(-1.4,1.45,pch=16,col=c("black","gray","red","coral1","blue","deepskyblue","purple","mediumpurple1","green3","green4"),c(rep("Ordinary site",2),rep("Mutation",2),rep("Recombination",2),rep("Both",2),rep("Migration",2)),cex=0.85)

m1<-match(player_lin[1,],sites)
m2<-match(player_lin[2,],sites)
arrows(x0=coords[m1[which(m1!=m2)],1],y0=coords[m1[which(m1!=m2)],2],x1=coords[m2[which(m1!=m2)],1],y1=coords[m2[which(m1!=m2)],2],col=player_col[which(m1!=m2)],lwd=line_widths[which(m1!=m2)],lty=line_types[which(m1!=m2)],length=0.2,angle=30,code=2)
if(length(which(m1==m2))>0){
    for(j in 1:length(which(m1==m2))){
        selfarrow(as.numeric(coords[m1[which(m1==m2)[j]],c(1,2)]),lcol=player_col[which(m1==m2)[j]],lwd=line_widths[which(m1==m2)[j]],lty=line_types[which(m1==m2)[j]],arr.pos=0.5,curve=c(0.08,0.08))
    }
}



Sys.sleep(2)

for(i in 2:(nrow(player_lin)-1)){
    m1<-m2
    m2<-match(player_lin[i+1,],sites)
    plot(coords[,c(1:2)],xlim=c(min(lab_coords[,1]),max(lab_coords[,1])),ylim=c(min(lab_coords[,2]),max(lab_coords[,2])),pch=16,cex=1.2,col=site_cols,xlab="",ylab="",axes=FALSE,main=paste0("Round ",i))
    text(lab_coords,sites)
    legend(-1.4,1.45,pch=16,col=c("black","gray","red","coral1","blue","deepskyblue","purple","mediumpurple1","green3","green4"),c(rep("Ordinary site",2),rep("Mutation",2),rep("Recombination",2),rep("Both",2),rep("Migration",2)),cex=0.85)
    
    arrows(x0=coords[m1[which(m1!=m2)],1],y0=coords[m1[which(m1!=m2)],2],x1=coords[m2[which(m1!=m2)],1],y1=coords[m2[which(m1!=m2)],2],col=player_col[which(m1!=m2)],lwd=line_widths[which(m1!=m2)],lty=line_types[which(m1!=m2)],length=0.2,angle=30,code=2)
    if(length(which(m1==m2))>0){
        for(j in 1:length(which(m1==m2))){
            selfarrow(as.numeric(coords[m1[which(m1==m2)[j]],c(1,2)]),lcol=player_col[which(m1==m2)[j]],lwd=line_widths[which(m1==m2)[j]],lty=line_types[which(m1==m2)[j]],arr.pos=0.5,curve=c(0.08,0.08))
        }
    }
    Sys.sleep(2)
}