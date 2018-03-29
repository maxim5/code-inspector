#' reconstruct Filters which is available in Windows.
#'
#' An nx2 matrix of file description & filters, different types within
#' each filter being separated by a \code{;}.
#' @keywords internal
#' @family ui
#' @export
Filters <- matrix(c(
  "R or S files (*.R, *.q, *.ssc, *.S)", "*.R;*.q;*.ssc;*.S",
  "Enhanced metafiles (*.emf)",      "*.emf",
  "Postscript files (*.ps)",         "*.ps",
  "PDF files (*.pdf)",               "*.pdf",
  "Png files (*.png)",               "*.png",
  "Windows bitmap files (*.bmp)",    "*.bmp",
  "Jpeg files (*.jpeg, *.jpg)",       "*.jpeg;*.jpg",
  "Text files (*.txt)",              "*.txt",
  "R images (*.RData, *.rda)",        "*.RData;*.rda",
  "Zip files (*.zip)",               "*.zip",
  "All files (*.*)",                 "*.*"),
  byrow=T, ncol=2, dimnames=list(c('R', 'emf', 'ps', 'pdf', 'png', 'bmp', 'jpeg',
                                   'txt', 'Rdata', 'zip', 'All'), NULL)
)


#' cross-platform file selection interface
#'
#' This is a cross-platform function to allow the user to choose a file
#' interactively.
#' If \code{graphics=TRUE}, \code{choose.files} is used on Windows (file
#' dialog), and \code{tcltk::tk_choose.files} otherwise (& if supported),
#' and an interactive text interface if all else fails.
#'
#' @param startDir start directory (default current directory)
#' @param title title for dialog
#' @param filter a 2-column matrix with file description & filter in each
#' column, with multiple filters for a single file type being separated by
#' semicolons (doesn't work in text interface yet).
#' @param multiple whether to allow selection of multiple items
#' @param graphics whether to present a graphics interface (if possible)
#'
#' @return the file(s) chosen (character vector). If \code{is.empty} is
#' \code{TRUE}, the user selected 'Cancel'.
#' @examples \dontrun{
#' # choose .R or .q files (or All files):
#' fList <- gui.file.choose(filter=matrix(c('R files', '*.q;*.R',
#'                                          'All files', '*.*'),
#'                                          ncol=2, byrow=T))
#' }
#' @family ui
#' @export
gui.file.choose <- function(startDir=getwd(), title='Choose file(s)',
                            multiple=FALSE, filter=NULL, graphics=TRUE) {
    out <- NULL
    if (graphics) {
        if (.Platform$OS.type == 'windows') {
            filter[, 1] <- sprintf('%s (%s)', filter[, 1], filter[, 2])
            out <- choose.files(caption=title, multi=multiple, filters=filter)
        } else {
            if (capabilities('tcltk') && suppressWarnings(tcltk:::.TkUp)) {
                out <- tcltk::tk_choose.files(caption=title,
                                              multi=multiple,
                                              filters=filter)
            }
        }
    } else {
       out <- file.browse(root=startDir,
                          title=paste(title, '(0 to quit) in folder', startDir),
                          multiple=multiple)
    }
    if (is.empty(out) || nchar(out) == 0) {
        out <- NULL
    }
    return(out)
}



#' cross-platform directory selection interface
#'
#' This is a cross-platform function to allow the user to choose a directory
#' interactively.
#' If \code{graphics=TRUE}, \code{choose.dir} is used on Windows (file
#' dialog), and \code{tcltk::tk_choose.dir} otherwise (& if supported),
#' and an interactive text interface if all else fails.
#'
#' @inheritParams gui.file.choose
#' @return the dir(s) chosen (character vector). If \code{is.empty} is
#' \code{TRUE}, the user selected 'Cancel'.
#' @family ui
#' @export
gui.dir.choose <- function(startDir=getwd(), title='choose a directory',
                           multiple=FALSE, graphics=TRUE) {
    out <- NULL
    if (graphics) {
        if (.Platform$OS.type == 'windows') {
            out <- choose.dir(caption=title)
        } else {
            if (capabilities('tcltk') && suppressWarnings(tcltk:::.TkUp)) {
                out <- tcltk::tk_choose.dir(caption=title)
            }
        }
    } else {
        out <- dir.browse(root=startDir, multiple=multiple,
                          title=paste(title, '(0 to quit)'),
                          .select=FALSE)
    }
    return(out)
}

#' cross-platform item selection interface
#'
#' This is a cross-platform function to allow the user to choose an item
#' from a list of items interactively.
#' If \code{graphics=TRUE}, \code{select.list} is used on Windows (similar
#' to the "Select CRAN repository..." when you \code{install.packages}),
#' and \code{tcltk::tk_select.list} otherwise (& if supported),
#' and an interactive text interface (\code{menu(..., graphics=F)})
#' if all else fails.
#'
#' @inheritParams gui.file.choose
#' @param choices vector of choices to select from
#' @param index.return whether to return the selected item from \code{choices}, or
#' the index of the selected item in \code{choices}
#' @param mono whether to try display in monospace font (uses tcltk
#' interface).
#'
#' @return the item(s) chosen (vector), either indices or values, depending
#' on \code{index.return}.
#' If \code{is.empty} is \code{TRUE}, the user selected 'Cancel'.
#' @family ui
#' @export
gui.menu <- function(choices, title=NULL, multiple=FALSE,
                     index.return=FALSE, graphics=TRUE,
                     mono=FALSE) {
    if ((.Platform$OS.type != 'windows' || mono) &&
        graphics && capabilities('tcltk') && suppressWarnings(tcltk:::.TkUp)) {
        # tk_select.list
        # note: if I just use tk_select.list will it use this version or
        # tcltk's version?
        choice <- utilitiesR:::tk_select.list(choices, title=title,
                                              multi=multiple, mono=mono)
    } else if (graphics && .Platform$OS.type == 'windows') {
        choice <- select.list(choices, title=title, graphics=TRUE, multiple=multiple)
    } else {
        choice <- select.list(choices, title=title, graphics=FALSE, multiple=multiple)
    }
    if (index.return) {
        # ARGH: select.list truncates what it returns to 100 characters!
        # Help file didn't mention that...
        return(which(substr(choices, 1, nchar(choice)) %in% choice))
    }
    return(choice)
}

#' cross-platform user input interface
#'
#' This is a cross-platform function to allow the user to type some input
#' interactively.
#'
#' If \code{graphics=TRUE}, \code{winDialogString} is used on Windows,
#' and \code{\link{tk_entry.dialog}} otherwise (& if supported),
#' and an interactive text interface (\code{readline})
#' if all else fails.
#'
#' @inheritParams gui.file.choose
#' @param message message asked to the user
#' @param default default value
#'
#' @return character string of what the user typed.
#' @family ui
#' @export
gui.readline <- function(message, default='', graphics=TRUE) {
    if (graphics) {
        if (.Platform$OS.type == 'windows') {
            return(winDialogString(message, default=default))
        } else {
            if (capabilities('tcltk') && suppressWarnings(tcltk:::.TkUp)) {
                return(tk_entry.dialog(title='Enter value', message=message,
                                       default=default))
            }
        }
    }
    return(readline(message))
}

#' cross-platform yes/no interface
#'
#' This is a cross-platform function to allow the user to answer a yes/no
#' question interactively
#'
#' If \code{graphics=TRUE}, \code{winDialog} is used on Windows,
#' and \code{tk_messageBox} otherwise (& if supported),
#' and an interactive text interface
#' if all else fails.
#'
#' @inheritParams gui.readline
#'
#' @return a BOOLEAN, whether they selected 'YES' (TRUE) or 'NO' (FALSE)
#' @family ui
#' @export
gui.yesno <- function(message, graphics=TRUE) {
    if (graphics) {
        if (.Platform$OS.type == 'windows') {
            return(tolower(winDialog(type='yesno', message))=='yes')
        } else {
            if (capabilities('tcltk') && suppressWarnings(tcltk:::.TkUp)) {
                response <- tolower(as.character(tcltk::tk_messageBox(message=message, type='yesno')))
                return(response == 'yes')
            }
        }
    }
    return(verifyInput(message,
                       yes=, y=TRUE,
                       no=, n=FALSE,
                       case.sensitive=FALSE))
}

#' tcltk modal dialog to get user input
#' @param title title of the dialog
#' @param message message put to the user
#' @param default default value of the dialog
#' @param entryWidth ????
#' @return whatever the user typed in as a character string.
#' @note from here:
#' \url{http://bioinf.wehi.edu.au/~wettenhall/RTclTkExamples/modalDialog.html}
#' @family ui
#' @export
tk_entry.dialog <- function(title, message, default='', entryWidth=20) {
    require(tcltk)
    dlg <- tktoplevel()
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    tkwm.title(dlg, title)
    textEntryVarTcl <- tclVar(paste(default))
    textEntryWidget <- tkentry(dlg, width=paste(entryWidth), textvariable=textEntryVarTcl)
    tkgrid(tklabel(dlg, text="       "))
    tkgrid(tklabel(dlg, text=message), textEntryWidget)
    tkgrid(tklabel(dlg, text="       "))
    ReturnVal <- NA #returnValOnCancel
    onOK <- function() {
        ReturnVal <<- tclvalue(textEntryVarTcl)
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    #  onCancel <- function() {
    #    ReturnVal <<- returnValOnCancel
    #    tkgrab.release(dlg)
    #    tkdestroy(dlg)
    #    tkfocus(ttMain)
    #  }
    OK.but     <- tkbutton(dlg, text="   OK   ", command=onOK)
    #Cancel.but <- tkbutton(dlg, text=" Cancel ", command=onCancel)
    tkgrid(OK.but)#, Cancel.but)
    tkgrid(tklabel(dlg, text="    "))

    tkfocus(dlg)
    tkbind(dlg, "<Destroy>", function() tkgrab.release(dlg))#;tkfocus(ttMain)})
    tkbind(textEntryWidget, "<Return>", onOK)
    tkwait.window(dlg)

    return(ReturnVal)
}


#' Text-based interactive file selection.
#' @param root the root directory to explore
#'             (default current working directory)
#' @param multiple boolean specifying whether to allow
#'                 multiple files to be selected
#' @param title what gets printed before the menu.
#' @return character vector of selected files.
#' @examples
#' \dontrun{
#' fileList <- file.browse()
#' }
#' @family ui
#' @seealso \code{\link{dir.browse}} for a similar functionality for
#' directories.
#' @export
file.browse <- function (root=getwd(), multiple=FALSE,
                         title=sprintf('Select file(s) (0 to quit) in folder %s', root)) {
    # .. and list.files(root)
    x <- c(dirname(normalizePath(root)), list.files(root, full.names=TRUE))
    isdir <- file.info(x)$isdir
    obj <- sort(isdir, index.return=TRUE, decreasing=TRUE)
    isdir <- obj$x
    x <- x[obj$ix]
    lbls <- sprintf('%s%s', basename(x), ifelse(isdir, '/', ''))
    lbls[1] <- sprintf('../ (%s)', basename(x[1]))

    files <- c()
    sel <- -1
    while (TRUE) {
        sel <- menu(lbls, title=title)
        if (sel == 0) break
        if (isdir[sel]) {
            # directory, browse further
            files <- c(files, file.browse(x[sel], multiple))
            break
        } else {
            # file, add to list
            files <- c(files, x[sel])
            if (!multiple) break
            # remove selected file from choices
            lbls <- lbls[-sel]
            x <- x[-sel]
            isdir <- isdir[-sel]
        }
    }
    return(files)
}

#' Text-based interactive directory selection.
#' @param root the root directory to explore
#'             (default current working directory)
#' @param multiple boolean specifying whether to allow
#'                 multiple directories to be selected
#' @param title what gets printed before the menu.
#' @param .select *internal* argument, how the function works out whether
#'                you're trying to select a directory or browse it.
#' @return character vector of selected directories.
#' @note This could be a bit buggy, due to the fact that it's a little
#' hard to tell whether a user wishes to browse into a directory, or
#' select it.
#' @examples
#' \dontrun{
#' d <- dir.browse()
#' }
#' @family ui
#' @seealso \code{\link{file.browse}} for a similar functionality for
#' directories.
#' @export
dir.browse <- function(root=getwd(), multiple=FALSE,
                       title=sprintf('Select a directory'), .select=FALSE) {
    # .. and list.files(root)
    x <- c(dirname(normalizePath(root)), list.dirs(root, full.names=TRUE))
    lbls <- sprintf('%s/', basename(x))
    lbls[1] <- sprintf('../ (%s)', basename(x[1]))
    cd <- which(x == '.')
    if (length(cd) > 0) lbls[cd] <- sprintf('./ (%s)', basename(x[cd]))
    n <- length(lbls) + 1
    lbls[n] <- ifelse(.select,
                      'Go back to browsing mode',
                      'Add a directory')
    dirs <- c()
    sel <- -1
    while (TRUE) {
        sel <- menu(lbls, title=title)
        if (sel == 0) break
        # switch between browsing and adding
        if (sel == n) {
            .select <- !.select
            next
        }

        # otherwise add to list OR browse to that dir
        if (.select && sel != 1) {
            # add to list, unless they chose .. because that's too hard!
            dirs <- c(dirs, x[sel])
            if (!multiple) break

            # remove selected dir from choices
            # Urgh no, this will end up
            # lbls <- lbls[-sel]
            # x <- x[-sel]
        } else {
            # they're browsing, navigate there.
            dirs <- c(dirs,
                      dir.browse(x[sel], multiple, title, .select=FALSE))
            break
        }
    }
    return(unique(dirs))
}


if (!exists('list.dirs', mode='function')) {
    #' lists all directories
    #' @param ... See \code{\link[base]{list.files}}
    #' @return vector of directories
    #' @seealso \code{\link[base]{list.files}}
    #' @family file
    #' @export
    list.dirs <- function(...) {
        tmp <- list.files(...)
        return(tmp[file.info(tmp)$isdir])
    }
}

#' Function almost identical to tcltk::tk_select.list
#'
#' This function is identical to \code{tcltk::tk_select.list}, *except*
#' that it adds in an additional \code{tkwait.visibility} to the code prior
#' to calling \code{tkgrab.set}.
#'
#' This gets around the problem that sometimes occurs (on Linux? Unable to
#' reliably reproduce), where one uses \code{tcltk::tk_select.list}, and
#' gets the error \code{[tcl] grab failed: window not viewable}.
#'
#' A bit of googling into tcltk suggests that this occurs when one attempts
#' to set grab on a window that is not yet mapped to the screen (i.e.
#' visible).
#'
#' The solution is to add a \code{tkwait.visibility(dlg)} *before* calling
#' \code{tkgrab.set(dlg)}. That is what this function does.
#' @inheritParams tcltk::tk_select.list
#' @inheritParams gui.menu
#' @keywords internal
#' @family ui
# DON'T EXPORT.
tk_select.list <- function (choices, preselect=NULL, multiple=FALSE,
                            title=NULL, mono=FALSE) {
    require(tcltk)
    if (mono) {
        tcl('option', 'add', '*Listbox.font', 'courier 10')
    }
    have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
    if (!have_ttk) {
        ttkbutton <- tkbutton
    }
    lvar <- tclVar()
    tclObj(lvar) <- choices
    oldmode <- tclServiceMode(FALSE)
    dlg <- tktoplevel()
    tkwm.title(dlg, title)
    tkwm.deiconify(dlg)
    # ONLY CHANGE:
    tkwait.visibility(dlg)
    # /ONLY CHANGE
    tkgrab.set(dlg)
    tkfocus(dlg)
    if (!is.null(title) && nzchar(title)) {
        lab <- if (have_ttk)
            ttklabel(dlg, text=title, foreground="blue")
        else tklabel(dlg, text=title, fg="blue")
        tkpack(lab, side="top")
    }
    onOK <- function() {
        res <- 1L + as.integer(tkcurselection(box))
        ans.select_list <<- choices[res]
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    onCancel <- function() {
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    buttons <- tkframe(dlg)
    tkpack(buttons, side="bottom")
    OK <- ttkbutton(buttons, text=gettext("OK"), width=6,
                    command=onOK)
    Cancel <- ttkbutton(buttons, text=gettext("Cancel"), command=onCancel)
    tkpack(OK, Cancel, side="left", fill="x", padx="2m")
    scht <- as.numeric(tclvalue(tkwinfo("screenheight", dlg))) - 200L
    ht <- min(length(choices), scht %/% 20)
    box <- tklistbox(dlg, height=ht, listvariable=lvar, bg="white",
                     setgrid=1,
                     selectmode=ifelse(multiple, "multiple", "single"))
    tmp <- tcl("font", "metrics", tkcget(box, font=NULL))
    tmp <- as.numeric(sub(".*linespace ([0-9]+) .*", "\\1", tclvalue(tmp))) + 3
    ht <- min(length(choices), scht %/% tmp)
    tkdestroy(box)
    if (ht < length(choices)) {
        scr <- if (have_ttk)
            ttkscrollbar(dlg, command=function(...) tkyview(box, ...))
        else tkscrollbar(dlg, repeatinterval=5,
                         command=function(...) tkyview(box, ...))
        box <- tklistbox(dlg, height=ht, width=0, listvariable=lvar,
                         bg="white", setgrid=1,
                         selectmode=ifelse(multiple, "multiple", "single"),
                         yscrollcommand=function(...) tkset(scr, ...))
        tkpack(box, side="left", fill="both", expand=TRUE)
        tkpack(scr, side="right", fill="y")
    }  else {
        box <- tklistbox(dlg, height=ht, width=0, listvariable=lvar,
                         bg="white",
                         selectmode=ifelse(multiple, "multiple", "single"))
        tkpack(box, side="left", fill="both")
    }
    preselect <- match(preselect, choices)
    preselect <- preselect[preselect > 0L] - 1L
    if (length(preselect)) {
        for (i in preselect) tkselection.set(box, i)
        tkyview(box, preselect[1L])
    }
    ans.select_list <- character()
    tkbind(dlg, "<Destroy>", onCancel)
    tkbind(dlg, "<Double-ButtonPress-1>", onOK)
    tkfocus(box)
    tclServiceMode(oldmode)
    tkwait.window(dlg)
    Sys.sleep(0.1)
    if (!multiple && !length(ans.select_list)) {
        ans.select_list <- ""
    }
    ans.select_list
}


#' Get names of all toplevel tk windows
#' @param root string giving parent window : i.e., get names of all
#' toplevel tk windows that are descendants of \code{root}.
#' @return a character array of all toplevel tk windows that are
#' descendants of \code{root}, or \code{character(0)} if there are none.
#' @details
#' Taken from algorithm in \url{http://wiki.tcl.tk/1461}:
#' \preformatted{
#' proc toplist {{W .}} {
#'     set list {}
#'     if { [string equal [winfo toplevel $W] $W] } {
#'         lappend list $W
#'     }
#'     foreach w [winfo children $W] {
#'         set list [concat $list [toplist $w]]
#'     }
#'     return $list
#' }
#' }
#' @keywords internal
# DON'T EXPORT
tk_toplist <- function(root='.') {
    winlist <- NULL
    if (try(tclvalue(tkwinfo('toplevel', root)), silent=TRUE) == root) {
        winlist <- as.character(tkwinfo('children', root))
    }
    return(winlist)
}
