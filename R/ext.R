#' Imported from plotrix.
#' https://github.com/plotrix/plotrix/blob/master/R/twoord.plot.R
#' @noRd
color.axis<-function(side=1,at=NULL,labels=TRUE,axlab=NA,axlab.at=NA,
                     col=par("fg"),cex.axis=par("cex.axis"),cex=par("cex")) {

  xylim<-par("usr")
  if(side %% 2) {
    at.ok<-at >= xylim[1] & at <= xylim[2]
    if(sum(at.ok) < length(at)) {
      at<-at[at.ok]
      labels<-labels[at.ok]
    }
  }
  else {
    at.ok<-at >= xylim[3] & at <= xylim[4]
    if(sum(at.ok) < length(at)) {
      at<-at[at.ok]
      labels<-labels[at.ok]
    }
  }
  axis(side=side,at=at,labels=rep("",length(at)),col=col)
  if(labels[1] == TRUE && length(labels) == 1) labels<-at
  mtext(labels,side=side,at=at,line=0.7,col=col,cex=cex.axis)
  if(!is.na(axlab)) {
    if(is.na(axlab.at))
      axlab.at<-ifelse(side%%2,sum(xylim[1:2])/2,sum(xylim[3:4])/2)
    mtext(axlab,side=side,at=axlab.at,line=2,col=col,cex=cex)
  }
  if(side == 1) abline(h=xylim[3],col=col)
  if(side == 2) abline(v=xylim[1],col=col)
  if(side == 3) abline(h=xylim[4],col=col)
  if(side == 4) abline(v=xylim[2],col=col)
}

#' Imported from plotrix.
#' Fix the left-axis label issue.
#' https://github.com/plotrix/plotrix/blob/master/R/twoord.plot.R
#' @import graphics
#' @noRd
twoord.plot<-function(lx,ly,rx,ry,data=NULL,main="",xlim=NULL,lylim=NULL,
                      rylim=NULL,mar=c(5,4,4,4),lcol=1,rcol=2,xlab="",
                      lytickpos=NA,ylab="",ylab.at=NA,rytickpos=NA,rylab="",rylab.at=NA,
                      lpch=1,rpch=2,type="l",xtickpos=NULL,xticklab=NULL,halfwidth=0.4,
                      axislab.cex=1,do.first=NULL,xaxt="s",...) {

  if(!is.null(data)) {
    ly<-unlist(data[ly])
    ry<-unlist(data[ry])
    if(missing(lx)) lx<-1:length(ly)
    else lx<-unlist(data[lx])
    if(missing(rx)) rx <- 1:length(ry)
    else rx<-unlist(data[rx])
  }
  if(missing(lx)) lx<-1:length(ly)
  if(missing(ry)) {
    if(missing(rx)) {
      rx<-1:length(ry)
      ry<-ly
      ly<-lx
      lx<-1:length(ly)
    }
    else {
      ry<-rx
      rx<-1:length(ry)
    }
  }
  oldmar<-par("mar")
  par(mar=mar)
  if(is.null(xlim)) xlim<-range(c(lx,rx))
  if(missing(lx)) lx<-1:length(ly)
  if(is.null(lylim)) {
    lylim<-range(ly,na.rm=TRUE)
    lyspan<-diff(lylim)
    if(lyspan == 0) lyspan<-lylim[1]
    lylim[2]<-lylim[2]+lyspan*0.04
    if(lylim[1] != 0) lylim[1]<-lylim[1]-lyspan*0.04
  }
  if(length(type) < 2) type<-rep(type,2)
  # first display the "left" plot
  if(match(type[1],"bar",0)) {
    oldcex<-par(cex=axislab.cex)
    plot(lx,ly,xlim=xlim,ylim=lylim,xlab=xlab,ylab="",yaxs="i",type="n",
         main="",axes=FALSE,...)
    par(oldcex)
    if(!is.null(do.first)) eval(parse(text=do.first))
    ybottom<-par("usr")[3]
    if (lylim[1] < 0) abline(h=0,lty=2)
    rect(lx-halfwidth,ifelse(ly<0,ly,ybottom),lx+halfwidth,
         ifelse(ly>0,ly,0),col=lcol)
  }
  else {
    oldcex<-par(cex=axislab.cex)
    plot(lx,ly,xlim=xlim,ylim=lylim,xlab=xlab,ylab="",yaxs="i",type="n",
         main="",axes=FALSE,...)
    par(oldcex)
    if(!is.null(do.first)) eval(parse(text=do.first))
    points(lx,ly,col=lcol,pch=lpch,type=type[1],...)
  }
  title(main=main)
  xylim<-par("usr")
  #mtext(ylab,2,2,col=lcol,cex=axislab.cex)
  box()
  if(xaxt != "n") {
    # display the X axis
    if(inherits(lx,"POSIXt")) axis.POSIXct(1)
    else {
      if(is.null(xticklab)) axis(1,cex.axis=axislab.cex)
      else {
        if(is.null(xtickpos)) xtickpos<-1:length(xticklab)
        axis(1,at=xtickpos,labels=xticklab,cex.axis=axislab.cex)
      }
    }
  }
  # display the left axis
  if(is.na(lytickpos[1])) lytickpos<-pretty(ly)
  # BUG: missing left-axis labels
  lylabels = lytickpos
  if(is.na(ylab.at)) ylab.at<-mean(lytickpos)
  color.axis(2,at=lytickpos, labels = lylabels, axlab=ylab,axlab.at=ylab.at,
             col=ifelse(is.na(lcol),1,lcol),cex.axis=axislab.cex,cex = axislab.cex)
  # get the "right" y limits
  if(is.null(rylim)) {
    rylim<-range(ry,na.rm=TRUE)
    ryspan<-diff(rylim)
    if(ryspan == 0) ryspan<-rylim[1]
    rylim[2]<-rylim[2]+ryspan*0.04
    if(rylim[1] != 0) rylim[1]<-rylim[1]-ryspan*0.04
  }
  # multiplier for the "right" y values
  ymult<-diff(lylim)/diff(rylim)
  # offset for the "right" y values
  yoff<-lylim[1]-rylim[1]*ymult
  if(match(type[2],"bar",0)) {
    if(rylim[1] < 0) abline(h=0,lty=2)
    rect(rx-halfwidth,ifelse(ry<0,ry,rylim[1]*ymult+yoff),rx+halfwidth,
         ifelse(ry>0,ry*ymult+yoff,0),col=rcol)
  }
  else points(rx,ry*ymult+yoff,col=rcol,pch=rpch,type=type[2],...)
  if(is.na(rytickpos[1])) rylabels<-pretty(rylim)
  else rylabels<-rytickpos
  if(min(rylabels) < rylim[1]) rylabels<-rylabels[rylabels >= rylim[1]]
  if(max(rylabels) > rylim[2]) rylabels<-rylabels[rylabels <= rylim[2]]
  axat<-rylabels*ymult+yoff
  if(is.na(rylab.at)) rylab.at<-mean(rytickpos)
  if(!is.na(rylab.at)) rylab.at<-rylab.at*ymult+yoff
  # display the right axis
  color.axis(4,at=axat,labels=rylabels,axlab=rylab,
             axlab.at=rylab.at,col=ifelse(is.na(rcol),1,rcol),
             cex.axis=axislab.cex,cex=axislab.cex)
  par(mar=oldmar,new=FALSE,col.axis="black")
}

#' Imported from zoo
#' https://github.com/cran/zoo/blob/master/R/plot.zoo.R
#' @noRd
make.par.list <- function(nams, x, n, m, def, recycle = sum(unnamed) > 0) {
  if (!is.list(x)) x <- if (m == 1) list(x) else as.list(x)
  y <- vector(mode = "list", length = length(nams))
  names(y) <- nams
  in.x <- nams %in% names(x)
  unnamed <- if (is.null(names(x))) rep(TRUE, length(x)) else names(x) == ""
  if (!recycle) y[] <- def
  y[in.x] <- x[nams[in.x]]
  if (recycle) {
    stopifnot(sum(unnamed) > 0)
    y[!in.x] <- rep(x[unnamed], length.out = sum(!in.x)) ## CHECK, this was: x[unnamed]
  } else {
    y[which(!in.x)[seq_len(sum(unnamed))]] <- x[unnamed]
  }
  lapply(y, function(y) if (length(y)==1) y else rep(y, length.out = n))
}

#' Imported from zoo
#' https://github.com/cran/zoo/blob/master/R/plot.zoo.R
#' Original function ignores cex.lab. Fixed by adding cex in mtext.
#' @importFrom grDevices xy.coords
#' @noRd
plot.zoo <- function(x, y = NULL, screens, plot.type, panel = lines,
                     xlab = "Index", ylab = NULL, main = NULL, xlim = NULL, ylim = NULL,
                     xy.labels = FALSE, xy.lines = NULL, yax.flip = FALSE,
                     oma = c(6, 0, 5, 0), mar = c(0, 5.1, 0, if(yax.flip) 5.1 else 2.1),
                     col = 1, lty = 1, lwd = 1, pch = 1, type = "l", log = "",
                     nc, widths = 1, heights = 1, cex.lab = NA, ...)
{
  ## if y supplied: scatter plot y ~ x
  if(!is.null(y)) {
    if(NCOL(x) > 1 || NCOL(y) > 1) stop("scatter plots only for univariate zoo series")
    xyzoo <- merge.zoo(x, y, all = FALSE)
    xy <- coredata(xyzoo)
    xy <- xy.coords(xy[,1], xy[,2])

    xlab <- if(missing(xlab)) deparse(substitute(x)) else xlab
    ylab <- if(missing(ylab)) deparse(substitute(y)) else ylab
    xlim <- if(is.null(xlim)) range(xy$x[is.finite(xy$x)]) else xlim
    ylim <- if(is.null(ylim)) range(xy$y[is.finite(xy$y)]) else ylim
    if(is.null(main)) main <- ""
    do.lab <- if(is.logical(xy.labels)) xy.labels else TRUE
    if(is.null(xy.lines)) xy.lines <- do.lab
    ptype <- if(do.lab) "n" else if(missing(type)) "p" else type

    plot.default(xy, type = ptype,col = col, pch = pch, main = main,
                 xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, log = log, ...)
    if(do.lab) text(xy, col = col,
                    labels = if(!is.logical(xy.labels)) xy.labels else index2char(index(xyzoo)), ...)
    if(xy.lines) lines(xy, col = col, lty = lty, lwd = lwd, type = if(do.lab) "c" else "l", ...)

    return(invisible(xyzoo))
  }
  ## Else : no y, only x

  recycle <- function(a, len, nser)
    rep(lapply(as.list(a), rep, length.out = len), length.out = nser)
  # same as range except it passes pairs through
  range2 <- function(x, ...) if (length(x) == 2) x else range(x, ...)
  if (missing(plot.type)) {
    plot.type <- if (missing(screens)) "multiple"
    else if (length(unique(screens) == 1)) "single"
    else "multiple"
  }
  plot.type <- match.arg(plot.type, c("multiple", "single"))
  nser <- NCOL(x)
  if (missing(screens)) {
    screens <- if (plot.type == "single") 1 else seq_len(nser)
  }
  dots <- list(...)
  x.index <- index(x)
  if(is.ts(x.index)) x.index <- as.vector(x.index)
  cn <- if (is.null(colnames(x))) paste("V", seq_len(nser), sep = "")
  else colnames(x)

  screens <- make.par.list(cn, screens, NROW(x), nser, 1)
  screens <- as.factor(unlist(screens))[drop = TRUE]
  ngraph <- length(levels(screens))
  if(nser > 1 && (plot.type == "multiple" || ngraph > 1)) {
    if (ngraph == 1) {
      screens <- as.factor(seq(nser))
      ngraph <- nser
    }
    if(is.null(main)) main <- deparse(substitute(x))
    main.outer <- TRUE
    if(is.null(ylab)) ylab <- colnames(x)[!duplicated(screens)]
    if(is.null(ylab)) ylab <- paste("Series", which(!duplicated(screens)))
    if(is.call(ylab)) ylab <- as.expression(ylab)
    ylab <- rep(ylab, length.out = ngraph)
    if(!is.list(ylab)) ylab <- as.list(ylab)
    lty <- rep(lty, length.out = nser)
    lwd <- rep(lwd, length.out = nser)
    col <- make.par.list(cn, col, NROW(x), nser, 1)
    pch <- make.par.list(cn, pch, NROW(x), nser, par("pch"))
    type <- make.par.list(cn, type, NROW(x), nser, "l")
    if (!is.null(ylim)) {
      if (is.list(ylim)) ylim <- lapply(ylim, range2, na.rm = TRUE)
      else ylim <- list(range2(ylim, na.rm = TRUE))
      ylim <- lapply(make.par.list(cn, ylim, 2, nser, NULL), function(x)
        if (is.null(x) || length(na.omit(x)) == 0) NULL
        else range2(x, na.rm = TRUE))
    }
    panel <- match.fun(panel)
    if(missing(nc)) nc <- if(ngraph >  4) 2 else 1
    oldpar <- par(no.readonly = TRUE)
    on.exit({ par(oldpar) })
    nr <- ceiling(ngraph / nc)
    layout(matrix(seq(nr*nc), nr), widths = widths, heights = heights)
    par(mar = mar, oma = oma)
    # TRUE if all elements of L are the same -- else FALSE
    allsame <- function(L) {
      f <- function(x, y) if (identical(x, y)) x
      !is.null(Reduce(f, L))
    }
    # idx is vector of indices into ylim.
    # If the entries indexed by it are all the same then use that common value;
    # otherwise, if the ylim are specified use the range of the ylim values;
    # otherwise, use the range of the data
    f <- function(idx) if (allsame(ylim)) ylim[idx][[1]]
    else if (!is.null(ylim) && length(idx) > 0 &&
             length(unlist(ylim[idx])) > 0) range(ylim[idx], finite = TRUE)
    else range(x[, idx], na.rm = TRUE)
    # ranges is indexed by screen
    ranges <- tapply(1:ncol(x), screens, f)
    for(j in seq_along(levels(screens))) {
      panel.number <- j
      y.side <- if (j %% 2 || !yax.flip) 2 else 4
      range. <- rep(ranges[[j]], length.out = length(time(x)))
      if(j%%nr==0 || j == length(levels(screens))) {
        args <- list(x.index, range., xlab = "", ylab = "", yaxt = "n",
                     xlim = xlim, ylim = ylim[[j]], log = log, ...)
        args$type <- "n"
        do.call("plot", args)
        mtext(xlab, side = 1, line = 3, cex = cex.lab)
      } else {
        args <- list(x.index, range., xaxt = "n", yaxt = "n", xlab = "",
                     ylab = "", xlim = xlim, ylim = ylim[[j]], log = log, ...)
        args$type <- "n"
        do.call("plot", args)
        if ("bty" %in% names(args) && args$bty == "n") {} else box()
      }
      do.call("axis", c(list(side = y.side, xpd = NA), dots))
      mtext(ylab[[j]], y.side, line = 3, cex = cex.lab)

      for(i in which(screens == levels(screens)[j])) {
        ## for potential usage in panel function
        series.number <- i
        series.within.screen <- ave(seq_along(screens), screens, FUN = seq_along)[series.number]

        ## draw individual lines/points with panel function
        panel(x.index, x[, i], col = col[[i]], pch = pch[[i]], lty = lty[i], lwd = lwd[i], type = type[[i]], ...)
      }
    }
  } else {
    if(is.null(ylab)) ylab <- deparse(substitute(x))
    if(is.call(ylab)) ylab <- as.expression(ylab)
    if(is.null(main)) main <- ""
    main.outer <- FALSE
    if(is.null(ylim)) ylim <- range(x, na.rm = TRUE)
    else ylim <- range2(c(ylim, recursive = TRUE), na.rm = TRUE)

    lty <- rep(lty, length.out = nser)
    lwd <- rep(lwd, length.out = nser)
    col <- make.par.list(cn, col, NROW(x), nser, 1)
    pch <- make.par.list(cn, pch, NROW(x), nser, par("pch"))
    type <- make.par.list(cn, type, NROW(x), nser, "l")

    dummy <- rep(range(x, na.rm = TRUE),
                 length.out = length(index(x)))

    args <- list(x.index, dummy, xlab = xlab, ylab = ylab[1], ylim = ylim, xlim = xlim, log = log, ...)
    args$type <- "n"
    do.call("plot", args)
    if ("bty" %in% names(args) && args$bty == "n") {} else box()
    y <- as.matrix(x)
    for(i in 1:nser) {
      panel(x.index, y[, i], col = col[[i]], pch = pch[[i]], lty = lty[i],
            lwd = lwd[i], type = type[[i]], ...)
    }
  }
  dots <- list(...)
  title.args <- c(list(main = main, outer = main.outer),
                  dots[grep("[.]main$", names(dots))])
  do.call("title", title.args)
  return(invisible(x))
}
