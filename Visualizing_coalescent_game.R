library("diagram")
t<-seq(0,2*pi,length=(n_lin+1))[1:n_lin]
coords<-as.data.frame(cbind(sin(t),cos(t)))
lab_coords<-cbind(sin(t)*1.07,cos(t)*1.07)
player_lab<-as.character(sort(rep(1:20,2)))
site_cols<-rep("black",n_lin)
site_cols[which(sites %in% mut_sites)]<-"red"
site_cols[which(sites %in% recomb_sites)]<-"blue"
site_cols[which(sites %in% mut_recomb_sites)]<-"purple"
player_col<-rep(NA,2*n_chrom)
for(i in 1:n_chrom){
    player_col[(2*i)-1]<-rainbow(n_chrom,alpha=0.4)[i]
    player_col[2*i]<-rainbow(n_chrom,alpha=0.4)[i]
}
line_widths<-rep(c(2,5),n_chrom)
line_types<-rep(c(1,2),n_chrom)

par(xpd=TRUE)
plot(coords[,c(1:2)],xlim=c(min(lab_coords[,1]),max(lab_coords[,1])),ylim=c(min(lab_coords[,2]),max(lab_coords[,2])),pch=16,cex=1.2,col=site_cols,xlab="",ylab="",axes=FALSE,main="Round 1")
text(lab_coords,sites)
legend(-1.3,1.4,pch=16,col=c("black","red","blue","purple"),c("Ordinary site","Mutation","Recombination","Both"),cex=0.9)

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
    legend(-1.3,1.4,pch=16,col=c("black","red","blue","purple"),c("Ordinary site","Mutation","Recombination","Both"),cex=0.9)

    
    arrows(x0=coords[m1[which(m1!=m2)],1],y0=coords[m1[which(m1!=m2)],2],x1=coords[m2[which(m1!=m2)],1],y1=coords[m2[which(m1!=m2)],2],col=player_col[which(m1!=m2)],lwd=line_widths[which(m1!=m2)],lty=line_types[which(m1!=m2)],length=0.2,angle=30,code=2)
    if(length(which(m1==m2))>0){
        for(j in 1:length(which(m1==m2))){
            selfarrow(as.numeric(coords[m1[which(m1==m2)[j]],c(1,2)]),lcol=player_col[which(m1==m2)[j]],lwd=line_widths[which(m1==m2)[j]],lty=line_types[which(m1==m2)[j]],arr.pos=0.5,curve=c(0.08,0.08))
        }
    }
    Sys.sleep(2)
}