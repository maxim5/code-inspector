mylatex <- function (object, title = first.word(deparse(substitute(object))), 
                     file = paste(title, ".tex", sep = ""), append = FALSE, label = title, 
                     rowlabel = title, rowlabel.just = "l", cgroup = NULL, n.cgroup = NULL, 
                     rgroup = NULL, n.rgroup = NULL, 
                     cgroupTexCmd = NULL,  rgroupTexCmd = NULL, rownamesTexCmd = NULL, 
                     colnamesTexCmd = NULL,  cellTexCmds = NULL, rowname, 
                     cgroup.just = rep("c", sum(n.cgroup)-1),  colheads = NULL, 
                     extracolheads = NULL, extracolsize = "scriptsize", 
                     dcolumn = FALSE, numeric.dollar = !dcolumn, cdot = FALSE, 
                     longtable = FALSE, draft.longtable = TRUE, ctable = FALSE, 
                     booktabs = FALSE, table.env = TRUE, here = FALSE, lines.page = 40, 
                     caption = NULL, caption.lot = NULL, caption.loc = c("top","bottom"), 
                     double.slash = FALSE, vbar = FALSE, collabel.just = rep("c", nc),                                    na.blank = TRUE, insert.bottom = NULL, 
                     do.begin=TRUE, do.end=TRUE, first.hline.double = !(booktabs | ctable), 
                     where = "!tbp", size = NULL,  center = c("center", "centering", "none"), 
                     landscape = FALSE, multicol = TRUE,  math.row.names = FALSE, math.col.names = FALSE, rowcolors=NULL, ...) 
{
  center <- match.arg(center)
  caption.loc <- match.arg(caption.loc)
  #cx <- format.df(object, dcolumn = dcolumn, na.blank = na.blank, 
  #                numeric.dollar = numeric.dollar, cdot = cdot, math.row.names = math.row.names, 
  #                math.col.names = math.col.names, double.slash = double.slash, 
  #                ...)
  cx <- object
  if (missing(rowname)) 
    rowname <- dimnames(cx)[[1]]
  if (is.null(colheads)) 
    colheads <- dimnames(cx)[[2]]
  col.just <- attr(cx, "col.just")
  nc <- ncol(cx)
  nr <- nrow(cx)
  if (length(cgroup)) {
    k <- length(cgroup)
    if (!length(n.cgroup)) 
      n.cgroup <- rep(nc/k, k)
    if (sum(n.cgroup) != nc) 
      stop("sum of n.cgroup must equal number of columns")
    if (length(n.cgroup) != length(cgroup)) 
      stop("cgroup and n.cgroup must have same lengths")
  }
  if (!length(rowname)) 
    rgroup <- NULL
  if (!length(n.rgroup) && length(rgroup)) 
    n.rgroup <- rep(nr/length(rgroup), length(rgroup))
  if (length(n.rgroup) && sum(n.rgroup) != nr) 
    stop("sum of n.rgroup must equal number of rows in object")
  if (length(rgroup) && length(n.rgroup) && (length(rgroup) != 
    length(n.rgroup))) 
    stop("lengths of rgroup and n.rgroup must match")
  if (length(rgroup) && rowlabel.just == "l") 
    rowname <- paste("~~", rowname, sep = "")
  sl <- ifelse(double.slash, "\\\\", "\\")
  if (ctable) {
    eol <- paste(sl, "NN\n", sep = "")
    eog <- ""
  }
  else if (longtable && length(n.rgroup)) {
    eol <- paste(sl, "tabularnewline*\n", sep = "")
    eog <- paste(sl, "tabularnewline\n", sep = "")
  }
  else {
    eol <- paste(sl, "tabularnewline\n", sep = "")
    eog <- paste(sl, "tabularnewline\n", sep = "")
  }
  if (booktabs) {
    toprule <- paste(sl, "toprule\n", sep = "")
    midrule <- paste(sl, "midrule\n", sep = "")
    bottomrule <- paste(sl, "bottomrule\n", sep = "")
  }
  else if (ctable) {
    toprule <- paste(sl, "FL\n", sep = "")
    midrule <- paste(sl, "ML\n", sep = "")
    bottomrule <- paste(sl, "LL\n", sep = "")
  }
  else {
    toprule <- if (first.hline.double) 
      paste(sl, "hline", sl, "hline\n", sep = "")
    else paste(sl, "hline\n", sep = "")
    midrule <- bottomrule <- paste(sl, "hline\n", sep = "")
  }
  if (!is.null(cellTexCmds) & !(all(dim(cx) == dim(cellTexCmds)) & 
    length(dim(cx)) == length(dim(cellTexCmds)))) {
    msg <- "The dimensions of cellTexCmds must be:"
    msg1 <- paste(dim(cx), collapse = " x ")
    msg <- paste(msg, msg1)
    msg <- paste(msg, ", but you gave me: ")
    msg1 <- paste(dim(cellTexCmds), collapse = " x ")
    msg <- paste(msg, msg1, sep = "")
    stop(msg)
  }
  if (length(cgroup) & !is.null(cellTexCmds)) {
    my.index <- split(1:NCOL(cellTexCmds), rep(cumsum(n.cgroup), 
                                               times = n.cgroup))
    new.index <- NULL
    new.col <- dim(cx)[2] + 1
    for (i in my.index) new.index <- c(new.index, i, new.col)
    new.index <- new.index[-length(new.index)]
    cellTexCmds <- cbind(cellTexCmds, "")[, new.index]
  }
  if (!is.null(cellTexCmds) | !is.null(rownamesTexCmd)) {
    if (is.null(rownamesTexCmd) & !is.null(rowname)) 
      rownamesTexCmd <- rep("", nr)
    if (is.null(cellTexCmds)) {
      cellTexCmds <- rep("", dim(cx)[1] * dim(cx)[2])
      dim(cellTexCmds) <- dim(cx)
    }
    rcellTexCmds <- cbind(rownamesTexCmd, cellTexCmds)
    thisDim <- dim(rcellTexCmds)
    rcellTexCmds <- paste(rcellTexCmds, sep = "")
    rcellTexCmds[rcellTexCmds == sl] <- ""
    dim(rcellTexCmds) <- thisDim
  }
  else {
    rcellTexCmds <- NULL
  }
  if (length(cgroup)) {
    last.col <- cumsum(n.cgroup)
    first.col <- c(1, 1 + last.col[-length(last.col)])
    cgroup.cols <- cbind(first.col, last.col)
    col.subs <- split(seq(length.out = nc), rep.int(seq_along(n.cgroup), 
                                                    times = n.cgroup))
    cxi <- list()
    for (i in seq(along = col.subs)) cxi[[i]] <- cx[, col.subs[[i]], 
                                                    drop = FALSE]
    cxx <- cxi[[1]]
    col.justxx <- col.just[col.subs[[1]]]
    collabel.justxx <- collabel.just[col.subs[[1]]]
    colheadsxx <- colheads[col.subs[[1]]]
    extracolheadsxx <- extracolheads[col.subs[[1]]]
    cgroupxx <- cgroup[1]
    n.cgroupxx <- n.cgroup[1]
    for (i in seq(along = col.subs)[-1]) {
      cxx <- cbind(cxx, "", cxi[[i]])
      col.justxx <- c(col.justxx, "c", col.just[col.subs[[i]]])
      collabel.justxx <- c(collabel.justxx, "c", collabel.just[col.subs[[i]]])
      cgroupxx <- c(cgroupxx, "", cgroup[i])
      n.cgroupxx <- c(n.cgroupxx, 1, n.cgroup[i])
      colheadsxx <- c(colheadsxx, "", colheads[col.subs[[i]]])
      if (!is.null(extracolheads)) {
        extracolheadsxx <- c(extracolheadsxx, "", extracolheads[col.subs[[i]]])
      }
    }
    cgroup.colsxx <- cgroup.cols + 0:(nrow(cgroup.cols) - 
      1)
    cx <- cxx
    col.just <- col.justxx
    collabel.just <- collabel.justxx
    n.cgroup <- n.cgroupxx
    cgroup.cols <- cgroup.colsxx[cgroup != "", , drop = FALSE]
    cgroup <- cgroupxx
    colheads <- colheadsxx
    extracolheads <- extracolheadsxx
    nc <- ncol(cx)
  }
  cline <- NULL
  if (length(rowname)) {
    cx <- cbind(rowname, cx)
    col.just <- c(rowlabel.just, col.just)
    if (length(extracolheads)) 
      extracolheads <- c("", extracolheads)
    collabel.just <- c(rowlabel.just, collabel.just)
    if (length(cgroup) == 0L) 
      colheads <- c(rowlabel, colheads)
    else {
      colheads <- c("", colheads)
      cgroup <- c(rowlabel, cgroup)
      rlj <- ifelse(rowlabel.just == "l", "l", "c")
      cgroup.just <- c(rlj, cgroup.just)
      n.cgroup <- c(1, n.cgroup)
      cgroup.cols <- 1 + cgroup.cols
      cline <- paste(sl, "cline{", cgroup.cols[, 1], "-", 
                     cgroup.cols[, 2], "}", sep = "", collapse = " ")
    }
    nc <- 1 + nc
  }
  vbar <- ifelse(vbar, "|", "")
  if (!append) 
    cat("", file = file)
  cat("%", deparse(sys.call()), "\n%\n", file = file, append = file != 
    "")
  if (dcolumn) {
    if(do.begin){
      decimal.point <- ifelse(cdot, paste(sl, "cdot", sep = ""), 
                              ".")
      cat(sl, "newcolumntype{.}{D{.}{", decimal.point, "}{-1}}\n", 
          sep = "", file = file, append = file != "")
    }
  }
{
  tabular.cols <- paste(vbar, col.just, sep = "")
  if (!length(n.cgroup)) 
    tabular.cols <- c(tabular.cols, vbar)
  else {
    vv2 <- cumsum(n.cgroup)
    tabular.cols[vv2] <- paste(tabular.cols[vv2], vbar, 
                               sep = "")
  }
  tabular.cols <- paste(tabular.cols, collapse = "")
}
  if (length(caption) && !ctable) {
    caption <- paste(sl, "caption", if (length(caption.lot)) 
      paste("[", caption.lot, "]", sep = ""), "{", caption, 
                     if (!longtable) 
                       paste(sl, "label{", label, "}", sep = ""), "}", 
                     sep = "")
    table.env <- TRUE
  }
  if (ctable) {
    latex.begin <- paste(if (length(size)) 
      paste("{", sl, size, sep = ""), paste(sl, "ctable[", 
                                            sep = ""), if (length(caption) && caption.loc == 
                                              "bottom") 
                                              "botcap,", if (length(caption)) 
                                                paste("caption={", caption, "},", sep = ""), if (length(caption.lot)) 
                                                  paste("cap={", caption.lot, "},", sep = ""), paste("label=", 
                                                                                                     label, ",", sep = ""), if (!landscape) 
                                                                                                       paste("pos=", where, ",", sep = ""), if (landscape) 
                                                                                                         "sideways", paste("]{", tabular.cols, "}", sep = ""), 
                         if (length(insert.bottom)) 
                           paste("{", paste(sl, "tnote[]{", sedit(insert.bottom, 
                                                                  "\\\\", " "), "}", sep = "", collapse = ""), 
                                 "}", sep = "")
                         else "{}", paste("{", toprule, sep = ""), sep = "")
    latex.end <- paste("}", if (length(size)) 
      "}", sep = "")
  }
  else if (!longtable) {
    latex.begin <- paste(if (table.env) 
        paste(sl, if (landscape) "begin{sidewaystable}" else "begin{table}", if (here) 
          "[H]"
              else paste("[", where, "]", sep = ""), "\n", sep = ""), 
                         if (length(size)) 
                           paste(sl, size, "\n", sep = ""), if (caption.loc == 
                             "top" && !missing(caption)) 
                             paste(caption, "\n"), if (center == "center") 
                               paste(sl, "begin{center}\n", if(!is.null(rowcolors)) paste(sl, rowcolors, "\n", sep='') else paste('\n'), sep = "")
                         else {
                           if (center == "centering") 
                             paste(sl, "centering\n", sep = "")
                         },                          
                         paste(sl, "begin{tabular}{", tabular.cols, "}\n", 
                                  toprule, sep = ""), sep = "")
    latex.end <- paste(paste(sl, "end{tabular}\n", sep = ""), 
                       if (center == "center") 
                         paste(sl, "end{center}\n", sep = ""), if (caption.loc == 
                           "bottom" && !missing(caption)) 
                           paste(caption, "\n"), if (length(insert.bottom)) 
                             paste(insert.bottom, collapse = "\\\\"), if (table.env) 
                               paste(sl, if(landscape) "end{sidewaystable}\n" else "end{table}", sep = ""),  sep = "")
  }
  else {
    latex.begin <- paste(paste(if (!draft.longtable) 
      paste(sl, "let", sl, "LTmulticolumn=", sl, "multicolumn", 
            sep = ""), paste(sl, "setlongtables", sep = ""), 
                               if (landscape) 
                                 paste(sl, "begin{landscape}", sep = ""), if (length(size)) 
                                   paste("{", sl, size, "\n", sep = ""), paste(sl, 
                                                                               "begin{longtable}{", tabular.cols, "}", sep = ""), 
                               sep = "\n"), if (caption.loc == "top" && !missing(caption)) 
                                 paste(caption, eog)
                         else "\n", toprule, sep = "")
    latex.end <- paste(if (caption.loc == "bottom" && !missing(caption)) 
      paste(caption, eog), paste(sl, "end{longtable}\n", 
                                 sep = ""), if (length(size)) 
                                   "}\n", if (landscape) 
                                     paste(sl, "end{landscape}\n", sep = ""), sep = "")
  }
  if(do.begin){
    cat(latex.begin, file = file, append = file != "")  
  }
  
  if (length(cgroup)) {
    cvbar <- paste(cgroup.just, vbar, sep = "")
    cvbar[1] <- paste(vbar, cvbar[1], sep = "")
    cvbar[-length(cvbar)] <- paste(cvbar[-length(cvbar)], 
                                   vbar, sep = "")
    slmc <- paste(sl, "multicolumn{", sep = "")    
    if (!is.null(cgroupTexCmd)) 
      labs <- paste(sl, cgroupTexCmd, " ", cgroup, sep = "")
    else
      labs <- paste(" ", cgroup, sep = "")
    if (multicol)       
      labs <- paste(slmc, n.cgroup, "}{", cvbar, "}{", 
                    labs, "}", sep = "")
    cat(labs, file = file, sep = "&\n", append = file != 
      "")
    if (!length(cline)) {
      inr <- as.numeric(length(rowname))
      cline <- paste(sl, "cline{", 1 + inr, "-", nc, "}", 
                     sep = "")
    }
    cat(eol, cline, "\n", sep = "", file = file, append = file != 
      "")
  }
{
  cvbar <- paste(collabel.just, vbar, sep = "")
  cvbar[1] <- paste(vbar, cvbar[1], sep = "")
  if (length(n.cgroup)) {
    vv2 <- cumsum(n.cgroup[-length(n.cgroup)])
    cvbar[vv2] <- paste(cvbar[vv2], vbar, sep = "")
  }
  slmc1 <- paste(sl, "multicolumn{1}{", sep = "")
  labs <- colheads
  if (!is.null(colnamesTexCmd)) 
    labs <- paste(sl, colnamesTexCmd, " ", labs, sep = "")
  header <- NULL
  if (length(labs)) {
    if (!length(extracolheads)) {
      heads <- get2rowHeads(labs)
      colheads <- heads[[1]]
      if (any(heads[[2]] != "")) 
        extracolheads <- heads[[2]]
    }
    if (multicol) 
      colheads <- paste(slmc1, cvbar, "}{", colheads, 
                        "}", sep = "")
    header <- paste(colheads, collapse = "&")
    if (length(extracolheads)) {
      extracolheads <- ifelse(extracolheads == "" | 
        extracolsize == "", extracolheads, paste("{", 
                                                 sl, extracolsize, " ", extracolheads, "}", 
                                                 sep = ""))
      if (multicol) 
        extracolheads <- ifelse(extracolheads == "", 
                                extracolheads, paste(slmc1, cvbar, "}{", 
                                                     extracolheads, "}", sep = ""))
      else extracolheads <- ifelse(extracolheads == 
        "", extracolheads, paste(extracolheads, sep = ""))
      header <- paste(header, eol, paste(extracolheads, 
                                         collapse = "&"), sep = "")
  }
    cat(header, eog, file = file, sep = "", append = file != 
      "")
    if (ctable) 
      cat(midrule, file = file, append = file != "")
    else cat(midrule, file = file, append = file != "")
}
}
  if (longtable) {
    if (missing(caption)) 
      cat(sl, "endhead\n", midrule, sl, "endfoot\n", sep = "", 
          file = file, append = file != "")
    else {
      cat(sl, "endfirsthead", sep = "", file = file, append = file != 
        "")
      cat(sl, "caption[]{\\em (continued)} ", eol, sep = "", 
          file = file, append = file != "")
      cat(midrule, sep = "", file = file, append = file != 
        "")
      cat(header, file = file, sep = "&", append = file != 
        "")
      cat(eog, midrule, sl, "endhead", "\n", midrule, sep = "", 
          file = file, append = file != "")
      if (length(insert.bottom)) {
        cat(paste(sl, "multicolumn{", nc, "}{", "p{", 
                  sl, "linewidth}}{", insert.bottom, "}", eol, 
                  sep = "", collapse = "\n"), sep = "", file = file, 
            append = file != "")
      }
      cat(sl, "endfoot\n", sep = "", file = file, append = file != 
        "")
      cat(sl, "label{", label, "}\n", sep = "", file = file, 
          append = file != "")
    }
  }
{
  if (length(n.rgroup)) {
    rg.end <- cumsum(n.rgroup)
    rg.start <- rg.end - n.rgroup + 1
    if (!length(rgroup)) {
      rgroup <- rep("", length(n.rgroup))
    }
    else {
      if (!is.null(rgroupTexCmd)) {
        rgroup <- paste("{", sl, rgroupTexCmd, " ", 
                        rgroup, "}", sep = "")
      }
      else {
        rgroup <- paste("{", rgroup, "}", sep = "")
      }
    }
    seq.rgroup <- seq(along = n.rgroup)
  }
  else {
    seq.rgroup <- 1
    rg.end <- nr
    rg.start <- 1
  }
  linecnt <- 0
  for (j in seq.rgroup) {
    if (length(n.rgroup)) {
      if (longtable && linecnt > 0 && (linecnt + n.rgroup[j] + 
        (n.rgroup[j] > 1)) > lines.page) {
        cat(sl, "newpage\n", sep = "", file = file, 
            append = file != "")
        linecnt <- 0
      }
      cat(rgroup[j], rep("", nc - 1), sep = "&", file = file, 
          append = file != "")
      cat(eol, sep = "", file = file, append = file != 
        "")
      linecnt <- linecnt + 1
    }
    for (i in rg.start[j]:rg.end[j]) {
      if (!length(n.rgroup)) {
        if (longtable && linecnt > 0 && (linecnt + 
          1 > lines.page)) {
          cat(sl, "newpage\n", sep = "", file = file, 
              append = file != "")
          linecnt <- 0
        }
      }
      if (!is.null(rcellTexCmds)) {
        num.cols <- ncol(cx)
        for (colNum in 1:num.cols) {
          cat(sl, rcellTexCmds[i, colNum], " ", cx[i, colNum],  
              file = file, append = file != "", sep='')
          if (colNum < num.cols) 
            cat("&", file = file, append = file != 
              "")
        }
      }
      else {
        cat(cx[i, ], file = file, sep = "&", append = file != 
          "")
      }
      cat(if (i == rg.end[j] || (!ctable && !length(n.rgroup))) 
        eog
          else if (i < rg.end[j]) 
            eol, sep = "", file = file, append = file != 
              "")
      linecnt <- linecnt + 1
    }
    if (length(n.rgroup) > j) 
      cat(midrule, sep = "", file = file, append = file != 
        "")
    else cat(bottomrule, sep = "", file = file, append = file != 
      "")
  }
}
  if(do.end){
    cat(latex.end, file = file, sep = "\n", append = file != "")  
  }
  
  sty <- c("longtable"[longtable], "here"[here], "dcolumn"[dcolumn], 
           "ctable"[ctable], "booktabs"[booktabs], if (landscape && 
             !ctable) "lscape")
  structure(list(file = file, style = sty), class = "latex")
  }


beautify.out.matrix <- function(z, dec = dec){
  ## First: add parenthesis to standard errors
  x <- z$out.matrix  
  x <- x.old <- format.df(x, dec = dec, dcolumn=TRUE, na.blank=TRUE)
  s <- z$stars  
  outrows <- nrow(x)
  seqq <- seq(2,outrows, 2)
  len <- max(nchar(x))
  subs <- apply(x[seqq,], 2, FUN=function(u) {
    idx <- unlist(regexpr("~", u))
    ifelse(idx>0, paste(sub("~+", "(", u), ")", sep=""),
           paste("(",u,")",sep=""))})
  subs[subs=="()"] <- ""
  
  x[seqq,] <- subs
  
  stars <- matrix("", outrows, ncol(x))
  stars[(seqq-1),] <- z$stars
  z$out.matrix <- matrix(paste(x, stars, sep=''), outrows, ncol(x))
  rownames(z$out.matrix) <- rownames(x)
  colnames(z$out.matrix) <- colnames(x)
  attr(z$out.matrix, "col.just") <- attr(x.old, "col.just")
  z
}

##' Function to arrange regression outputs into an well-formatted
##' LaTeX illustrative tables of multiple statistical models. 
##'
##' \code{outreg2} borrows heavily from the \code{apsrtable}
##' package. It preserves its flexibility in that many statistical
##' models can be easily considered. It extends it by allowing finer
##' control on the LaTeX output. This flexibility is obtained by using
##' a modified version of the finction \code{latex} of the
##' \code{Hmisc} package. 
##' @title Arrange regression outputs into an illustrative table
##' @param ... the statistical models.
##' @param model.list the statistical models as a list. If model list is
##' missing, then the models are assumed to be passed by \code{...}. 
##' @param model.names Optional vector of names to use as column
##' headings in the table. If more models than names are supplied,
##' unnamed models are numbered (starting at one more than the number
##' of names).
##' @param order Determines the order in which terms (rows) are
##' included in the output when more than one model (column) is
##' present. “lr” and “rl” take the order of terms from the first or
##' last (leftmost or rightmost) model and appends new terms as they
##' are encountered. “longest” uses the order of terms in the model
##' with the most \code{terms.Default = “lr”}.
##' @param omitcoef An optional integer or character vector of
##' coefficient indices, or an expression involving coefnames that
##' evaluates to integer or character, of rows to exclude from the
##' output. See details.
##' @param omit.model An optional integer or character vector of model
##' indices (in the order they are entred) to be excluded from the
##' output. Useful if \code{order="largest"}, and the largest model is
##' not to be shown. See details.
##' @param coef.names An optional vector of names for coefficients. It
##' is recommended to establish the \code{omitcoef} and \code{order}
##' settings with automatic symbolic naming before supplying a vector
##' of “pretty” variable names. If automatic symbolic naming is used,
##' names are taken from the variables in the models and “sanitized”
##' for latex. If \code{coef.names} are supplied, they must be valid
##' latex, with double-backslash escape characters. 
##' @param cons.below 
##' @param stars how statistical significance “stars”, either "stata",
##' "default", or 1. "stata" is based on the stata default and gives
##' three stars.  "default" uses the \code{R} default,  not to be
##' confused with the function's (perhaps confusing) Default="stata"
##' @param lev When \code{stars=1}, what level should be used for the test to
##' reject statistical insignificance and bestow the glittering star?
##' Disable decoration entirely by specifying \code{lev=0}. \code{Default=.05}.
##' @param additional.rows A matrix with number of column equal to the
##' number of included models for additional annotation of the
##' table. Useful for fixed effects, country effects,
##' etc. specifications. 
##' @param title 
##' @param file name of the file to create. The default is not to
##' create a file and to have the generated LaTeX code just printed to
##' standard output. This is especially useful when running under
##' Sweave in R using its results=tex tag, to save having to manage
##' many small external files. When file="", latex keeps track of
##' LaTeX styles that are called for by creating or modifying an
##' object \code{latexStyles} (in \code{.GlobalTemp} in R or in frame
##' 0 in S-Plus). \code{latexStyles} is a vector containing the base
##' names of all the unique LaTeX styles called for so far in the
##' current session. See the end of the examples section for a way to
##' use this object to good effect. For dvips, file is the name of an
##' output postscript file.
##' @param append defaults to FALSE. Set to TRUE to append output to
##' an existing file.
##' @param label a text string representing a symbolic label for the
##' table for referencing in the LaTeX \code{\\label} and \code{\\ref}
##' commands. \code{label} is only used if caption is given.
##' @param rowlabel 
##' @param rowlabel.just 
##' @param cgroup 
##' @param n.cgroup 
##' @param rgroup 
##' @param n.rgroup 
##' @param cgroupTexCmd 
##' @param rgroupTexCmd 
##' @param rownamesTexCmd 
##' @param colnamesTexCmd 
##' @param cellTexCmds 
##' @param rowname 
##' @param cgroup.just 
##' @param colheads 
##' @param extracolheads 
##' @param extracolsize 
##' @param dcolumn 
##' @param tight 
##' @param numeric.dollar 
##' @param cdot 
##' @param longtable 
##' @param draft.longtable 
##' @param ctable 
##' @param booktabs 
##' @param table.env 
##' @param here 
##' @param lines.page 
##' @param caption 
##' @param caption.lot 
##' @param caption.loc 
##' @param double.slash 
##' @param vbar 
##' @param collabel.just 
##' @param na.blank 
##' @param insert.bottom 
##' @param do.begin Should the latex code at the beginning of table be printed? (Default: TRUE)
##' @param do.end Should the latex code at the end of table be printed? (Default: TRUE)
##' @param first.hline.double 
##' @param where 
##' @param size 
##' @param center 
##' @param landscape 
##' @param multicol 
##' @param math.row.names 
##' @param math.col.names 
##' @param rowcolors This command is useful for inserting color command. For
##' instance, if \code{rowcolors=rowcolors{1}{green}{pink}}, then the rows are 
##' subsequently coloured in green and in pink. 
##' @return A character vector containing the LaTeX table.
##' @author Giuseppe Ragusa
##' @export
outreg2 <- function(..., model.list, model.names=NULL, order=c("lr","rl","longest"), 
                   omitcoef=NULL, omit.model=NULL,
                   coef.names=NULL, cons.below=TRUE,                       
                   stars='stata',lev=.05,
                   additional.rows=NULL,
                   title=first.word(deparse(substitute(object))),
                   file="",
                   append=FALSE, label=title,
                   rowlabel="", rowlabel.just="l",
                   cgroup=NULL, n.cgroup=NULL,
                   rgroup=NULL, n.rgroup=NULL,
                   cgroupTexCmd=NULL,
                   rgroupTexCmd=NULL,
                   rownamesTexCmd=NULL,
                   colnamesTexCmd=NULL,
                   cellTexCmds=NULL,
                   rowname, 
                   cgroup.just=rep("c",sum(n.cgroup)),
                   colheads=NULL,
                   extracolheads=NULL, extracolsize='scriptsize',
                   dcolumn=TRUE, tight=TRUE, numeric.dollar=!dcolumn, cdot=FALSE,
                   longtable=FALSE, draft.longtable=TRUE, ctable=FALSE, booktabs=FALSE,
                   table.env=TRUE, here=FALSE, lines.page=40,
                   caption=NULL, caption.lot=NULL, caption.loc='bottom',
                   double.slash=FALSE,
                   vbar=FALSE, collabel.just=rep("c",nc), na.blank=TRUE,
                   insert.bottom=NULL, do.begin=TRUE, do.end=TRUE,
                   first.hline.double=!(booktabs | ctable),
                   where='!tbp', size=NULL,
                   center=c('center','centering','none'),
                   landscape=FALSE,
                   multicol=TRUE,
                   math.row.names=TRUE, math.col.names=FALSE, rowcolors=NULL,
                   dec = 3)
{
  ## Step 1. Send everything to apsrtable2
  if(missing(model.list))
    ml <- list(...)
  else     
    ml <- model.list

  coef.table <- apsrtable2(ml, 
                           model.names=model.names, 
                           order=order, 
                           omitcoef=omitcoef, 
                           omit.model=omit.model,
                           coef.names=coef.names,                       
                           stars=stars,lev=lev)
  
  table <- beautify.out.matrix(coef.table, dec = dec)
  if(tight & dcolumn){
    old.col.just <- attr(table$out.matrix, 'col.just')
    attr(table$out.matrix, 'col.just') <- sub("-1", "1", old.col.just)
  }
  x1 <- table$out.matrix
  outrows <- nrow(x1)

  ## FIXME: should cons.belo be passed to apsrtable2? Probably not. For now
  ##        cons.below is disable. The intercept goes at the end.
  ## match("Constant", rownames(x1))
  ## if(cons.below & any(!is.na(match(c("Constant","(Intercept)"), rownames(x1))))){
  ##   x1 <- rbind(x1, "", x1[1:2,])[-c(1,2),]  
  ## }
  
  x2 <- table$out.info
  x3 <- additional.rows
  if(is.null(x3)){
    final.table <- rbind(rbind(rbind(rbind(x1, " "), x2[,-1]), " "), x3)
    rownames(final.table) <- c(rownames(x1), "", x2[,1], "") ## no need
  }      
  else{
    final.table <- rbind(rbind(rbind(rbind(x1, " "), x2[,-1]), " "), x3)
    rownames(final.table) <- c(rownames(x1), "", x2[,1], "", rownames(x3))
  }
  attr(final.table, 'col.just') <- attr(table$out.matrix, 'col.just')
  nc <- ncol(final.table)
  mylatex(final.table, 
          title=title,
          file=file,
          append=append, 
          label=label,
          rowlabel=rowlabel, 
          rowlabel.just=rowlabel.just,
          cgroup=cgroup, 
          n.cgroup=n.cgroup,
          rgroup=rgroup, 
          n.rgroup=n.rgroup,
          cgroupTexCmd=cgroupTexCmd,
          rgroupTexCmd=rgroupTexCmd,
          rownamesTexCmd=rownamesTexCmd,
          colnamesTexCmd=colnamesTexCmd,
          cellTexCmds=cellTexCmds,          
          cgroup.just=cgroup.just,
          colheads=colheads,
          extracolheads=extracolheads, 
          extracolsize=extracolsize,
          dcolumn=dcolumn, 
          numeric.dollar=FALSE, 
          cdot=cdot,
          longtable=longtable, 
          draft.longtable=draft.longtable, 
          ctable=ctable, 
          booktabs=booktabs,
          table.env=table.env, 
          here=here, 
          lines.page=lines.page,
          caption=caption, 
          caption.lot=caption.lot, 
          caption.loc=caption.loc,
          double.slash=double.slash,
          vbar=vbar, 
          collabel.just=collabel.just, 
          na.blank=TRUE,
          insert.bottom=insert.bottom, 
          first.hline.double=first.hline.double,
          where=where, 
          size=size,
          center=center,
          landscape=landscape,
          multicol=multicol,
          math.row.names=math.row.names, 
          math.col.names=math.col.names,
          rowcolors=rowcolors, cdec = cdec)
          
}

apsrtable2 <- function (modelList, model.names=NULL, order=c("lr","rl","longest"), 
                        omitcoef=NULL, omit.model=NULL,
                        coef.names=NULL,                       
                        stars='stata',lev=.05, digits=2,
                        se=c("robust","vcov","both","pval"),                       
                       model.counter=1)                                                                     
{
  x <- list()
  signif.stars <- TRUE
  order <- match.arg(order,c("lr","rl","longest"))
  opts <- match.call(expand.dots=FALSE)
  se <- match.arg(se,c("robust","vcov","both","pval"))
  
  models <- modelList
  nmodels <- length(models)
  
  ## used to multiply later for column counts
  coef.cols <- 1
  coef.rows <-  2
  
  ## get the summaries for the objects
  model.summaries <- lapply(models,
                            ## If an apsrtableSummary exists, use it
                            ## Otherwise, use summary.
                            function(x) {
                              s <- try(apsrtableSummary(x), silent=TRUE) 
                              if (inherits(s, "try-error")) {
                                s <- summary(x)
                              }
                              if(!is.null(x$se) && se != "vcov") {
                                est <- coef(x)
                                if(class(x$se) == "matrix") {
                                  x$se <- sqrt(diag(x$se))
                                } 
                                s$coefficients[,3] <- tval <- est / x$se
                                e <- try(s$coefficients[,4] <-
                                  2 * pt(abs(tval),
                                         length(x$residuals) - x$rank,
                                         lower.tail=FALSE),silent=TRUE)
                                if(inherits(e,"try-error")){
                                  s$coefficients[, 4] <-
                                    2*pnorm(abs(tval),lower.tail=FALSE)
                                }
                                s$se <- x$se }
                              if(se == "pval") {
                                s$coefficients[,2] <- s$coefficients[,4]
                                
                              }
                              return(s)
                            } )
  
  ## Quietly switch the se.note to the pval.note as needed
  if(se=="pval") { se.note <- pval.note }
  
  ## Set up the model names
  ## If there's a vector of names, use that, or as many as there are
  ## and either all or the remainder.
  ## Optionally, model.number.start allows you to resetcounter
  ## TO DO: allow model "name" attribute to be used
  ##        but overridden by vector here.
  if (is.null(model.names)) {
    m.first = model.counter; m.last=m.first+(nmodels-1)
    model.names=paste("Model", m.first:m.last)
  } else if (!is.null(model.names) && (length(model.names) < nmodels) ) {
    m.first = length(model.names)+1
    model.names=c(model.names, paste( "Model", m.first:nmodels))
  }
  
  ## get and order the coefficient names from all models
  coefnames <- orderCoef(model.summaries, order=order)
  
  ## mark those to omit from the output
  incl <- rep(TRUE,length(coefnames))
  names(incl) <- coefnames
  if(!is.null(omitcoef)) {
    ## Boris Shor <boris@bshor.com> asked how to omitcoef by regex
    ##  this line enables omitcoef=expression() 2010-03-17
    ##  OR if you want to mix modes or provide multiple expr
    ##  you can supply a list() eg list(expression(grep), 15) 
    omitcoef <- unlist(sapply(omitcoef, eval.parent, n=2 ))
    #print(omitcoef)
    incl[omitcoef] <- FALSE
  }
  ## now figure out position of each coef in each model
  model.summaries <- coefPosition(model.summaries, coefnames)
  
  ## Now that the coef name matching is done, switch to pretty names
  ## if they are supplied. 
  if(!is.null(coef.names)) {
    if(length(coef.names) != sum(incl)) {
      warning("Supplied coef.names not the same length as output. Check automatic names before supplying 'pretty' names.\n")
    }
    coefnames[incl] <- coef.names
  } else {
    coefnames[incl] <- sanitize(coefnames[incl])
  }
  
  star.out <- lapply(model.summaries, function(x){  
    var.pos <- attr(x,"var.pos")
    model.out <- model.se.out <- star.out <- rep(NA,length(coefnames))
    model.out[var.pos] <- x$coefficients[,1]
    star.out[var.pos] <- apsrStars(x$coefficients,stars=stars,lev=lev,signif.stars=TRUE)
    star.out[incl]
  })
  
  out.table <- lapply(model.summaries, function(x){  
    var.pos <- attr(x,"var.pos")
    model.out <- model.se.out <- star.out <- rep(NA,length(coefnames))
    model.out[var.pos] <- x$coefficients[,1]
    star.out[var.pos] <- apsrStars(x$coefficients,stars=stars,lev=lev,signif.stars=TRUE)
    model.out <- ifelse(!is.na(model.out),
                        model.out,                        
                        NA)
    
    model.se.out[var.pos] <- x$coefficients[,2]
    if( !is.null(x$se) & se %in% c("robust","both") ) {
      model.se.out[var.pos] <- x$se
    }
    
    model.se.out <- ifelse(!is.na(model.se.out),
                           model.se.out,                           
                           NA)
      ## Create two side by side columns and mesh them together
      model.out <- rep(model.out[incl], each=2)
      model.se.out <- rep(model.se.out[incl], each=2)
      pos.se <- (1:length(model.out))[(1:length(model.out) %% 2==0)]
      model.out[pos.se] <- model.se.out[pos.se]
      ## Add a new model info attribute to the model's output entry
      ## To change modelInfo for a given model, change the method for it
      ## see ?modelInfo, it is reasonably well documented.
     attr(model.out,"model.info") <- modelInfo(x)  
      return(model.out)
  })  
  out.matrix <- matrix(unlist(out.table),
                       length(coefnames[incl])*coef.rows,
                       nmodels*coef.cols)
  
  rownames(out.matrix) <- rep(coefnames[incl],each=coef.rows)
  
  out.info <- lapply(out.table, attr, "model.info")
  info.names <- orderCoef(out.info)
  out.info <- coefPosition( out.info, orderCoef(out.info) )
  out.info <- lapply(out.info, function(x) {
    var.pos <- attr(x,"var.pos")
    model.out <- rep("",length(info.names))
    model.out[var.pos] <- coef(x)
    return(model.out)
  } )
  
  out.info <- matrix(unlist(out.info), length(info.names), nmodels)
  out.info <- cbind(as.character(info.names), out.info)
  outrows <- nrow(out.matrix)
  rownames(out.matrix)[seq(2,outrows,2)]   <- ""  
  star.out <- matrix(unlist(star.out), (outrows/2), nmodels)
  
  if(rownames(out.matrix)[1]=="(Intercept)"){
    if(outrows>2) {
      rownames(out.matrix)[1] <- "Constant"    
      out.matrix <- out.matrix[c(3:outrows,1:2),]
      star.out <- star.out[c(2:(outrows/2),1),]
    }
  }
  
  star.out[is.na(star.out)] <- ""
  ##star.out <- apply(apply(star.out, 2, paste, "", sep=""),2, function(u) paste("", u, sep=""))
  if(!is.null(omit.model)){
    out.matrix <- out.matrix[,omit.model, drop=FALSE]
    star.out   <- star.matrix[,omit.model, drop=FALSE]
  }
    
  colnames(out.matrix) <- model.names
  list(out.matrix=out.matrix, out.info=out.info, stars = star.out, outrows=outrows)
}

apsrStars <- function (x, digits = max(3, getOption("digits") - 2),
                       signif.stars = getOption("show.signif.stars"), 
                       signif.legend = signif.stars,
                       dig.tst = max(1, min(5, digits - 1)), cs.ind = 1:k,
                       tst.ind = k + 1, zap.ind = integer(0), 
                       P.values = NULL,
                       has.Pvalue = nc >= 3 && # used to be 4
                       substr(colnames(x)[nc],
                                      1, 3) == "Pr(" ||
                       grep("z",colnames(x)[nc]) == TRUE,
                       eps.Pvalue = .Machine$double.eps, na.print = "NA",
                       stars="default",lev=.05,
    ...) 
{
    if (is.null(d <- dim(x)) || length(d) != 2) 
        stop("'x' must be coefficient matrix/data frame")
    nc <- d[2]
    if (is.null(P.values)) {
        scp <- getOption("show.coef.Pvalues")
        if (!is.logical(scp) || is.na(scp)) {
            warning("option \"show.coef.Pvalues\" is invalid: assuming TRUE")
            scp <- TRUE
        }
        P.values <- has.Pvalue && scp
    }
    else if (P.values && !has.Pvalue) 
        stop("'P.values' is TRUE, but 'has.Pvalue' is not")
    if (has.Pvalue && !P.values) {
        d <- dim(xm <- data.matrix(x[, -nc, drop = FALSE]))
        nc <- nc - 1
        has.Pvalue <- FALSE
    }
    else xm <- data.matrix(x)
    k <- nc - has.Pvalue - (if (missing(tst.ind)) 
        1
    else length(tst.ind))
    if (!missing(cs.ind) && length(cs.ind) > k) 
        stop("wrong k / cs.ind")
    Cf <- array("", dim = d, dimnames = dimnames(xm))
    ok <- !(ina <- is.na(xm))
    if (length(cs.ind) > 0) {
        acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
        if (any(is.finite(acs))) {
            digmin <- 1 + floor(log10(range(acs[acs != 0], na.rm = TRUE)))
            Cf[, cs.ind] <- format(round(coef.se, max(1, digits - 
                digmin)), digits = digits)
        }
    }
    if (length(tst.ind) > 0) 
        Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst), 
            digits = digits)
    if (length(zap.ind) > 0) 
        Cf[, zap.ind] <- format(zapsmall(xm[, zap.ind], digits = digits), 
            digits = digits)
    if (any(r.ind <- !((1:nc) %in% c(cs.ind, tst.ind, zap.ind, 
        if (has.Pvalue) nc)))) 
        Cf[, r.ind] <- format(xm[, r.ind], digits = digits)
    okP <- if (has.Pvalue) 
        ok[, -nc]
    else ok
    x1 <- Cf[okP]
    dec <- getOption("OutDec")
    if (dec != ".") 
        x1 <- chartr(dec, ".", x1)
    x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
    if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
        Cf[okP][not.both.0] <- format(xm[okP][not.both.0], digits = max(1, 
            digits - 1))
    }
    if (any(ina)) 
        Cf[ina] <- na.print
    
    if (P.values) {
        if (!is.logical(signif.stars) || is.na(signif.stars)) {
            warning("option \"show.signif.stars\" is invalid: assuming TRUE")
            signif.stars <- TRUE
        }
        
        if (any(okP <- ok[, nc])) {
          pv <- as.vector(xm[, nc])
          Cf[okP, nc] <- base::format.pval(pv[okP], digits = dig.tst, 
                                     eps = eps.Pvalue)
          signif.stars <- signif.stars && any(pv[okP] < 0.1)
          Signif <- ""
          if (signif.stars && stars=="default") {
            Signif <- symnum(pv, corr = FALSE, na = FALSE, 
                             cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                             symbols = c("^{***}", "^{**}", "^*", "^\\dagger\ ", " "))
            Cf <- cbind(Cf, format(Signif))
          }
          else if (signif.stars && stars==1) {
           Signif <- symnum(pv, corr = FALSE, na = FALSE, 
                             cutpoints = c(0,lev,1), 
                             symbols = c("^*"," "))
          }
          else if (signif.stars && stars=="stata") {
              Signif <- symnum(pv, corr = FALSE, na = FALSE, 
                               cutpoints = c(0, 0.01, 0.05, 0.1, 1), 
                               symbols = c("^{***}", "^{**}", "^*", " "))
              Cf <- cbind(Cf, format(Signif))
          }
          
          return(Signif)
        }
      }

    return()
  }

#'  summary methods used by \code{"outreg2"}. 
#'
#' @param ... a model passed to \code{outreg2}.
#'
#' @return A \code{model.info} object. 
#' 
#' @seealso \code{\link{outreg2}} and \code{\link{apsrtable}}.
#' 
#' @export
#' @docType methods
#' @rdname modelInfo
setGeneric("modelInfo", function(x) standardGeneric("modelInfo") )

##' @rdname modelInfo
##' @aliases modelInfo.summary.reg
##' @export
modelInfo.summary.reg <- function(x) {
    env <- sys.parent()
    digits <- evalq(digits, envir=env)
    model.info <- list(    
    "$R^2$"=formatC(x$r.squared,format="f",digits=digits),
    "adj. $R^2$"=formatC(x$adj.r.squared,format="f",digits=digits),
    "$N$"=formatC(sum(x$df[1:2]),format="d"))    
    class(model.info) <- "model.info"
    invisible(model.info) 
}

##' @rdname modelInfo
##' @aliases modelInfo.summary.lm
##' @export
modelInfo.summary.lm <- function(x) {
    env <- sys.parent()
    digits <- evalq(digits, envir=env)
    model.info <- list(
                       "$N$"=formatC(sum(x$df[1:2]),format="d"),
                       "$R^2$"=formatC(x$r.squared,format="f",digits=digits),
                       "adj. $R^2$"=formatC(x$adj.r.squared,format="f",digits=digits),
                       "Resid. sd" = formatC(x$sigma,format="f",digits=digits))
    class(model.info) <- "model.info"
    invisible(model.info) 
}


##' @rdname modelInfo
##' @aliases modelInfo.summary.glm
##' @export
modelInfo.summary.glm <- function(x) {
  env <- sys.parent()
  digits <- evalq(digits, envir=env)
  model.info <- list(
                       "$N$"=formatC(sum(x$df[1:2]),format="d"),
                       
                       AIC=formatC(x$aic,format="f",digits=digits),
                       BIC= formatC(
                         ( (x$aic - 2*(length(x$coef)) ) +
                             log(sum(x$df[1:2]))*
                             length(coef(x)) ),
                         format="f",digits=digits),
                       "$\\log L$"=formatC( ((x$aic - 2*(length(x$coef))) / -2),
                         format="f",digits=digits))
  class(model.info) <- "model.info"
  invisible(model.info)
}



## 2009-02-25 mjm
## modelInfo request from Antonio Ramos for AER Tobit function
## Should be similar for 'survreg' objects, but without (necessarily)
## censoring info..
##' @rdname modelInfo
##' @aliases modelInfo.summary.tobit
##' @export
"modelInfo.summary.tobit" <- function(x) {
 env <- sys.parent()
 digits <- evalq(digits, envir=env)
 model.info <- list(
                    "Total $N$"=formatC(as.integer(x$n[1]),format="d"),
                    "Censored $N$"=formatC(sum(x$n[c(2,4)]),format="d"),
                    "$\\log L$" =
formatC(x$loglik[2],format="f",digits=digits),
                    "p(Wald)"=formatC(pchisq(x$wald,
                      sum(x$df) - x$idf,
                      lower.tail = FALSE),
                      digits=digits,format="f")
                    )
 class(model.info) <- "model.info"
 return(model.info)
}
##' @rdname modelInfo
##' @aliases modelInfo.summary.gee
##' @export
"modelInfo.summary.gee" <- function(x) {
 env <- sys.parent()
 digits <- evalq(digits, envir=env)
 model.info <- list(" " = ""
                    )
 class(model.info) <- "model.info"
 return(model.info)
}

##' @rdname modelInfo
##' @aliases modelInfo.summary.coxph
##' @export
"modelInfo.summary.coxph" <- function (x) {
       env <- sys.parent()
       digits <- evalq(digits, envir=env)
       model.info <- list()
       model.info[["$N$"]] <- x$n
       pv <- formatC(x$waldtest["pvalue"], format="f", digits=digits)
       rsq <- formatC(x$rsq["rsq"], format="f", digits=digits)
       maxrsq <- formatC(x$rsq["maxrsq"], format="f", digits=digits)
       model.info$Wald <- sprintf("%.0f on %.0f df, p = %s",
                                  x$waldtest["test"], x$waldtest["df"],
                                  pv)
       model.info[["$R^2$"]] <- sprintf("%s (Max %s)", rsq, maxrsq)
       class(model.info) <- "model.info"
       invisible(model.info)
}


## Req by Solomon Messing. This fxn is based on print.lrm, which seems
## to contain everything needed for table and modelinfo.
apsrtableSummary.lrm <- function (x) {
  digits <- 4
  strata.coefs <- FALSE
  sg <- function(x, d) {
    oldopt <- options(digits = d)
    on.exit(options(oldopt))
    format(x)
  }
  rn <- function(x, d) format(round(as.single(x), d))

  ##cat("\n")
    if (x$fail) {
      cat("Model Did Not Converge\n")
      return()
    }
  ##cat("Logistic Regression Model\n\n")
  ##sdput(x$call)
  ##cat("\n\nFrequencies of Responses\n")
  ##print(x$freq)
  if (length(x$sumwty)) {
    ##cat("\n\nSum of Weights by Response Category\n")
    ##print(x$sumwty)
  }
  ##cat("\n")
  if (!is.null(x$nmiss)) {
    ##cat("Frequencies of Missing Values Due to Each Variable\n")
    ##print(x$nmiss)
    ##cat("\n")
  }
  else if (!is.null(x$na.action)) 
    ##naprint(x$na.action)
  ns <- x$non.slopes
  nstrata <- x$nstrata
  if (!length(nstrata)) 
    nstrata <- 1
  pm <- x$penalty.matrix
  if (length(pm)) {
    psc <- if (length(pm) == 1) 
      sqrt(pm)
    else sqrt(diag(pm))
    penalty.scale <- c(rep(0, ns), psc)
    cof <- matrix(x$coef[-(1:ns)], ncol = 1)
    ##cat("Penalty factors:\n\n")
    ##print(as.data.frame(x$penalty, row.names = ""))
    ##cat("\nFinal penalty on -2 log L:", rn(t(cof) %*% pm %*% 
    ##    cof, 2), "\n\n")
  }
  vv <- diag(x$var)
    cof <- x$coef
    if (strata.coefs) {
        cof <- c(cof, x$strata.coef)
        vv <- c(vv, x$Varcov(x, which = "strata.var.diag"))
        if (length(pm)) 
            penalty.scale <- c(penalty.scale, rep(NA, x$nstrat - 
                1))
    }
    score.there <- nstrata == 1 && (length(x$est) < length(x$coef) - 
        ns)
    stats <- x$stats
    stats[2] <- signif(stats[2], 1)
    stats[3] <- round(stats[3], 2)
    stats[4] <- round(stats[4], 2)
    stats[5] <- round(stats[5], 4)
    stats[6] <- round(stats[6], 3)
    stats[7] <- round(stats[7], 3)
    if (nstrata == 1) {
        stats[8] <- round(stats[8], 3)
        stats[9] <- round(stats[9], 3)
        stats[10] <- round(stats[10], 3)
        if (length(stats) > 10) {
            stats[11] <- round(stats[11], 3)
            if (length(x$weights)) 
                stats[12] <- round(stats[12], 3)
        }
    }
    else stats <- c(stats, Strata = x$nstrat)

    res <- list()
    res$modelinfo <- stats

    z <- cof/sqrt(vv)
    stats <- cbind(cof,vv,cof/sqrt(vv) )
    stats <- cbind(stats, (1 - pchisq(z^2, 1)))
  ugh <- names(cof)
  names(cof) <- sub("Intercept","(Intercept)",ugh)
    dimnames(stats) <- list(names(cof), c("Coef", "S.E.", "Wald Z", 
        "Pr(z)"))
    if (length(pm)) 
        stats <- cbind(stats, `Penalty Scale` = penalty.scale)
    ##print(stats, quote = FALSE)
    ##cat("\n")
    if (score.there) {
        q <- (1:length(cof))[-est.exp]
        if (length(q) == 1) 
            vv <- x$var[q, q]
        else vv <- diag(x$var[q, q])
        z <- x$u[q]/sqrt(vv)
        stats <- cbind(z, (1 - pchisq(z^2, 1)))
        dimnames(stats) <- list(names(cof[q]), c("Score Z", "P"))
        ##printd(stats, quote = FALSE)
        ##cat("\n")
    }
  res$coefficients <- stats
  class(res) <- "summary.lrm"
  invisible(res)
}

##' @rdname modelInfo
##' @aliases modelInfo.summary.lrm
##' @export
"modelInfo.summary.lrm" <- function(x) {
  env <- sys.parent()
  digits<- evalq(digits, envir=env)
  x <- as.numeric(x$modelinfo)
  ##         number of observations
  ##         used in the fit, maximum absolute value of first derivative
  ##         of log likelihood, model likelihood ratio chi-square, d.f.,
  ##         P-value, c index (area under ROC curve), Somers' D_{xy},
  ##         Goodman-Kruskal gamma, Kendall's tau-a rank correlations
  ##         between predicted probabilities and observed response, the
  ##         Nagelkerke R^2 index, and the Brier score
  model.info <- list(
                     "$N$"=formatC(x[1],format="d"),
                     "Max.Deriv."=formatC(x[2],format="f",digits=digits),
                     "LR $\\chi^2$"=formatC(x[3],format="f",digits=digits),
                     "d.f."=formatC(x[4],format="d"),
                     "$P$"=formatC(x[5],format="f",digits=digits),
                     "C-index"=formatC(x[6],format="f",digits=digits),
                     "Somers $D_{xy}$"=formatC(x[7],format="f",digits=digits),
                     ##"$\\gamma$"=formatC(x[8],format="f",digits=digits),
                     ##"Kendall's tau-a"=formatC(x[9],format="f",digits=digits),
                     "Nagelkerke $R^2$"=formatC(x[10],format="f",digits=digits),
                     "Brier" = formatC(x[11],format="f",digits=digits))
  class(model.info) <- "model.info"
  invisible(model.info) 
}

setGeneric("modelInfo", def=function(x){standardGeneric("modelInfo")})

setOldClass("summary.lm")

setOldClass("summary.glm")
setOldClass("summary.tobit")
setOldClass("summary.gee")
setOldClass("summary.coxph")
setOldClass("summary.negbin")
#' "summary.lrm" class
#'
#' @name summary.lrm
#' @family summary.lrm
#'
#' @exportClass summary.lrm
setOldClass("summary.lrm")
#' "summary.lrm" class
#'
#' @name summary.reg
#' @family summary.reg
#'
#' @exportClass summary.reg
setOldClass("summary.reg")


setMethod("modelInfo", "summary.lm", modelInfo.summary.lm )
setMethod("modelInfo", "summary.reg", modelInfo.summary.reg )
setMethod("modelInfo","summary.glm", modelInfo.summary.glm )
setMethod("modelInfo","summary.tobit", modelInfo.summary.tobit)
setMethod("modelInfo","summary.gee",modelInfo.summary.gee)
setMethod("modelInfo","summary.coxph",modelInfo.summary.coxph)
setMethod("modelInfo","summary.negbin",modelInfo.summary.glm)
setMethod("modelInfo", "summary.lrm", modelInfo.summary.lrm)

"coef.model.info" <- function(object,...) {
  x <- as.matrix(unlist(object)); invisible(x)
} 

## RULES: All according to longest model,
##        then left to right
## RESULT: union of all models' coefficient names in requested order.
orderCoef <- function(model.summaries,order="lr") {
  nmodels <- length(model.summaries)
  mlength <- sapply(model.summaries, function(x) length(coef(x)) )
  longest <- which.max(mlength) # longest model
  if(order=="rl") {
    modelorder <- nmodels:1 } else {
      modelorder <- 1:nmodels }
  if(order=="longest") {
    coefnames <-  rownames(coef(model.summaries[[longest]]))
  } else {
    coefnames <- rownames(coef(model.summaries[[modelorder[1]]])) }
  
  for(i in seq_along(model.summaries)) {
    matched <- match(rownames(coef(model.summaries[[i]])), coefnames, nomatch=0)
    unmatched <- which(is.na(matched) | matched==0)
    coefnames <- c(coefnames,
                   rownames(coef(model.summaries[[i]]))[unmatched]
                   )
  }
  return(coefnames)
}
## Given a list of model summaries (or anything with a coef method),
## and a master (unioned) list of coef names,
##
## Append an attribute to each element containing its coefs' position in the
## master coefficient list
"coefPosition" <- function(model.summaries, coefnames) {
  model.summaries <- lapply(model.summaries, function(x) {
    pos <- match(rownames(coef(x)), coefnames)
    attr(x,"var.pos") <- pos
    return(x)
  })
return(model.summaries)
}

"se.note" <- function(env) {
  note <- paste(ifelse( evalq(se,envir=env) != "vcov","Robust s","S"),
                "tandard errors in parentheses",
                ifelse(evalq(se,envir=env)=="both",
                       paste("\\\\\n\\multicolumn{",
                             evalq(nmodels,envir=env)+1,"}{l}{",
                             'Na\\"ive standard errors in brackets',
                             collapse="",sep=""),
                       "" ) ,sep="")
  return(note)
}

## Added pval support
"pval.note" <- function(env) {
  note <- paste(ifelse(evalq(se,envir=env) != "vcov", "Robust ", ""),
                "$p$ values in parentheses",sep="")
  return(note)
}
"stars.note" <- function(env) {
  paste(ifelse(evalq(stars,envir=env)=="default",
               paste("$^\\dagger$ significant at $p<.10$; $^* p<.05$; $^{**} p<.01$; $^{***} p<.001$"),
               paste("$^*$ indicates significance at $p<",evalq(lev,envir=env),"$")))
}

## apsrtableSummary enables easy S3 method masking of summary functions
## where the model-package provides a summary not suitable for apsrtable,
## such as z scores instead of pnorms.

"apsrtableSummary" <- function(x) {
  UseMethod("apsrtableSummary") }

"apsrtableSummary.gee" <- function(x) {
  s <- summary(x)
  newCoef <- coef(s)
  ## which columns have z scores? (two of them in robust case)
  zcols <- grep("z",colnames(newCoef))
  newCoef[,zcols] <- pnorm(abs(newCoef[,zcols]), lower.tail=FALSE)
  colnames(newCoef)[zcols] <- "Pr(z)"
  s$coefficients <- newCoef
  ## put the robust se in $se so that notefunction works automatically
  ## the se checker will overwrite [,4] with pt, but this doesn't matter
  ## because the last column Pr(z) is used by apsrstars() anyway
  ## and the se are pulled from $se.
  if( class(x)[1] == "gee.robust") {
    s$se <- coef(s)[,4]
  }
  return(s)
}

"apsrtableSummary.clogit" <- apsrtableSummary.coxph <- function (x) {
       s <- summary(x)
       if("robust se" %in% colnames(coef(s))) s$se <- coef(s)[,"robust se"]
       s$coefficients <- coef(s)[,c("coef","se(coef)", "Pr(>|z|)")]
       return(s)
}
"apsrtableSummary.negbin" <- function (x) {
       s <- summary(x)
       coefs <- coef(s)
       theta <- matrix(c(s$theta, s$SE.theta,NA,NA),1,4)
       theta[,3] <- theta[,1]/theta[,2] ;
       theta[,4] <- pnorm(abs(theta[,3]),lower.tail=FALSE)
       rownames(theta) <- "$\\theta$"
       s$coefficients <- rbind(coefs,theta)
       return(s)
}


"print.apsrtable" <- function(x,...) {
  cat(paste(x))
}

## ADM <admartin@wustl.edu> requested sanitizing coefnames 2010-03-10
## this function taken from print.xtable v1.5-6.
sanitize <- function(str) {
  result <- str
  result <- gsub("\\\\","SANITIZE.BACKSLASH",result)
  result <- gsub("$","\\$",result,fixed=TRUE)
  result <- gsub(">","$>$",result,fixed=TRUE)
  result <- gsub("<","$<$",result,fixed=TRUE)
  result <- gsub("|","$|$",result,fixed=TRUE)
  result <- gsub("{","\\{",result,fixed=TRUE)
  result <- gsub("}","\\}",result,fixed=TRUE)
  result <- gsub("%","\\%",result,fixed=TRUE)
  result <- gsub("&","\\&",result,fixed=TRUE)
  result <- gsub("_","\\_",result,fixed=TRUE)
  result <- gsub("#","\\#",result,fixed=TRUE)
  result <- gsub("^","\\verb|^|",result,fixed=TRUE)
  result <- gsub("~","\\~{}",result,fixed=TRUE)
  result <- gsub("SANITIZE.BACKSLASH","$\\backslash$",result,fixed=TRUE)
  return(result)
}
       
fround <- function (x, digits) {
    format (round (x, digits), nsmall=digits)
}
  
pfround <- function (x, digits) {
    print (fround (x, digits), quote=FALSE)
}
 


## A couple of test calls here for random features
## library(apsrtable);example(apsrtable);apsrtable(lm.D90, lm.D9, glm.D9, digits=1, align="center", stars="default", model.counter=0, order="rl", omitcoef="(Intercept)")
## library(apsrtable);example(apsrtable);apsrtable(lm.D90,coef.names=c("\\#1","\\#0"))
