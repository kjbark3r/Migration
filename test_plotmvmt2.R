require(migrateR)
plot.mvmt <- function(mvmt, new = F, omit = NA, ranked = T, 
    xlim = c(0,365), ...){		
      fam <- attr(mvmt, "family")
      dates <- attr(mvmt, "dates")
      stdt.chr <- paste(dates["styr"],dates["stdt"],sep = "-")
      stdt <- strptime(stdt.chr,"%Y-%m-%d") + 24*60*60*as.numeric(dates["dday"])
      p <- data.frame(
       mod = c("disperser","migrant","mixmig","nomad","resident"),
       col = c("purple", "blue", "darkgreen", "orange", "red"),
        lty = c(2,1,3:5),
        stringsAsFactors = F
      )
      p <- p[which(p$mod%in%names(mvmt@models)),]
cat("\tmodels set:", paste(p$mod, collapse =", "), "\n")
      if(new == T) dev.new()
      opar <- par(mar = c(0,0,0,0), mgp = c(3,1,0), xpd=F)
      par(mar=c(5, 4.1, 4.1, 9.1),mgp = c(2.5,0.5,0))

      mdata <- mvmt@data 
	    x1 <- mdata$decday[!mdata$cut]      
	    y1 <- mvmt@data[,fam]

     if (max(na.omit(c(dates["rloc"], 1)))>1)
        fam = "rnsd"

      ylab <- c(nsd = expression("NSD " (Km^2)),
        rnsd =expression("rNSD " (Km^2)),
        elev = "Elevation (m)")[fam]
      
      plot(seq(1,365,length.out = length(y1)), y1, typ = "n",
        xlab = "Days", ylab = ylab, xaxt = "n", main = attr(mvmt,"burst"), 
        xlim = xlim, # changed 2017-4-14
        las=1, ...)    
      migrateR:::mvmtAxis(stdt)
      
      if(sum(mdata$cut) > 0 ){
        x2 <- mdata$decday[mdata$cut]      
        y2 <- y1[mdata$cut]
        points(x2, y2, pch = 0, cex = .5, col = "darkgrey") #, xlim = xlim	
      } 
      
      y1 <- y1[!mdata$cut]            
	    points(x1, y1, cex = .5, col = "grey", xlim = xlim)
cat("\tpoints added\n")

      # Omit Models From Plot (?)
      omit <- match(omit,names(mvmt@models))
cat("\tomit models(?):", omit, "\n")      
	    if(!is.na(omit[1])){
		    mvmt@models <- mvmt@models[-omit]
		    p <- p[-omit,]
	    }
	    modn <- names(mvmt@models)
cat("\tmodels retained:", paste(modn, collapse =", ", "\n"))
      # Models
	    y1 <- mapply(fitted, mvmt@models)
cat("\tdimensions:", nrow(y1), "x", ncol(y1), "\n")	    
	    matplot(x1, y1, add = T, typ = "l", 
	    lty = p$lty, lwd = 2, col = p$col, xlim = xlim)
cat("\tlines added\n")
	    
      # Legend
      par(xpd=T)	
	    aicres <- sapply(mvmt@models, AIC)
	    daic <- aicres - min(aicres)	
	    yaxp <- par("yaxp")
	    yleg <- yaxp[1] + 0.75 * (yaxp[2] - yaxp[1])

      # Rank Legend on AIC?
	    if(ranked){
        aico   <- order(daic)
        aicres <- aicres[aico]
        p$col   <- p$col[aico]
        modn   <- modn[aico]
        daic   <- daic[aico]
        p$lty   <- p$lty[aico]
	    }
cat("\tlegend assembled\n")      	    
	    legend(390, yleg, title = as.expression(
        substitute(A~B, list(A = as.name("Delta"),
          B = as.name("AIC")))
        ), legend = paste(round(daic, 0), modn), lty = p$lty,
          col = p$col, lwd = 2
      )
cat("\tlegend added\n")      
	    par(opar)
cat("\trun successfully complete\n")	    
      invisible(attr(mvmt,"burst"))
    }