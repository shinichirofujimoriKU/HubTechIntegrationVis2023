if(nrow(XX)>0){      
  miny <- min(XX$Y,XX2$Y) 
  na.omit(XX$Value)
  ylab1 <- paste0(areamappara$Var[areamappara$Class==AreaItem], " (", areamappara$Unit[areamappara$Class==AreaItem], ")")
  xlab1 <- areamappara$Var[areamappara$Class==AreaItem]
  
  areapaletteArea <- filter(areapaletteload,V0==AreaItem & V1 %in% unique(XX$Ind))$V2
  names(areapaletteArea) <- filter(areapaletteload,V0==AreaItem & V1 %in% unique(XX$Ind))$V1
  colorpal <- areapaletteArea 
  numitem <- length(as.vector(unique(Data4plot$Region))) #Get number of items
  
  plot2 <- ggplot() + 
    geom_area(data=filter(XX,Y<=maxy),aes(x=Y, y = Value , fill=reorder(Ind,-order)), stat="identity") + 
    ylab(ylab1) + xlab(xlab1) +labs(fill="")+ guides(fill=guide_legend(reverse=TRUE)) + MyThemeLine +
    theme(legend.position="bottom", text=element_text(size=12),  
          axis.text.x=element_text(angle=45, vjust=0.9, hjust=1, size = 12)) +
    guides(fill=guide_legend(ncol=5)) + scale_x_continuous(breaks=seq(miny,maxy,10)) +  ggtitle(paste(AreaItem,sep=" "))+
    facet_wrap( ~ Region,scales="free_y",ncol=mergecolnum) + scale_fill_manual(values=colorpal) + 
    annotate("segment",x=miny,xend=maxy,y=0,yend=0,linetype="solid",color="grey") + theme(legend.position='bottom')+
    geom_line(data=filter(XX3,Y<=maxy),aes(x=Y, y = Value ), color="black",linetype="dashed",size=1.2)
  if(nrow(XX2)>=1){
    plot3 <- plot2 +    geom_area(data=XX2,aes(x=Y, y = Value , fill=reorder(Ind,-order)), stat="identity", alpha=0.7)
  }else{
    plot3 <- plot2
  }
