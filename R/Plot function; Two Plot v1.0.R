


plot.two=function(x1,y1,x2,y2,xlim=NULL,ylim1=NULL,ylim2=NULL,type1="points",type2="points",col1="red",col2="blue",cex=1,cex.lab=2, cex.axis = cex.lab,pch1=16,pch2=16,xlab="",ylab1="",ylab2="",add=FALSE,xpos=1,mtext.line=2.9){

  if(is.null(xlim)){
    xlim=range(pretty(c(min(x1,x2)-1e-3,max(x1,x2)+1e-3)))
  }

  if(is.null(ylim1)){
    ylim1=range(pretty(c(min(y1)-1e-3,max(y1)+1e-3)))
  }
  if(is.null(ylim2)){
    ylim2=range(pretty(c(min(y2)-1e-3,max(y2)+1e-3)))
  }


  rescale.y1=(y1-ylim1[1])/(ylim1[2]-ylim1[1])
  rescale.y2=(y2-ylim2[1])/(ylim2[2]-ylim2[1])

  if(add==FALSE){
    plot(0,0,col=0,xlim=xlim,ylim=c(0,1),axes=F,xaxs="i",yaxs="i",ylab="",xlab="")
  }



  if(type1=="points"){
    points(x1,rescale.y1,col=col1,cex=cex,pch=pch1)
    print("hi")
  } else if(type1=="connected"){
    order(x1)
    points(sort(x1),rescale.y1[order(x1)],col=col1,cex=cex,pch=pch1)
    lines(sort(x1),rescale.y1[order(x1)],col=col1,cex=cex)
  }

  if(type2=="points"){
    points(x2,rescale.y2,col=col2,cex=cex,pch=pch2)
  } else if(type2=="connected"){
    points(sort(x2),rescale.y2[order(x2)],col=col2,cex=cex,pch=pch2)
    lines(x2,rescale.y2[order(x2)],col=col2,cex=cex)
  }

  ylabels1=pretty(ylim1)
  ylabels2=pretty(ylim2)




  box()
  if(is.null(xlab)==FALSE){
    if(xpos==1){
      axis(1,pretty(c(x1,x2)),cex.axis=cex.axis)
      mtext(xlab,  side = 1, line = mtext.line, cex = cex.lab)
    } else{
      axis(3,pretty(c(x1,x2)),cex.axis=cex.axis)
      mtext(xlab,  side = 3, line = mtext.line, cex = cex.lab)
    }
  }

  axis(2,pos=xlim[1],label=pretty(ylim1),at=0:(length(pretty(ylim1))-1)/(length(pretty(ylim1))-1),col=col1,col.axis=col1,cex.axis=cex.axis)
  mtext(ylab1, side = 2, line = mtext.line, cex = cex.lab, col=col1)

  axis(4,pos=xlim[2],label=pretty(ylim2),at=0:(length(pretty(ylim2))-1)/(length(pretty(ylim2))-1),col=col2,col.axis=col2,cex.axis=cex.axis)
  mtext(ylab2, side = 4, line = mtext.line, cex = cex.lab, col=col2)


}
