#' Renders server output.
#'
#' @param serverValues traditional Shiny inputs, as well as user-created values (reactive values), to be used in rendering output
#' @param session traditional Shiny server session value
#' @param output traditional shiny output
#' @param serv_out_list a named list of functions that render output. Each function is of the form function(calc, session), where calc is a named list containing the traditional Shiny input and reactive values that have calculated values derived from input, and session is the traditional Shiny server session value. It returns the results of a Shiny render function. The name of each function corresponds to its output label.
#' @return traditional Shiny output argument
serverFunct <- function(serverValues, session, output, serv_out_list){
  # go through each output rendering and render
  if (length(serv_out_list) > 0){
    for (v in 1:length(serv_out_list)){
      # check for errors related to numbers of inputs
      tryCatch({
        # renderoutput and assign to output
        output[[names(serv_out_list)[v]]] <- serv_out_list[[v]](serverValues, session)
      }, error = function(e){
        if (length(grep("unused argument ", as.character(e)[1], fixed = T))>0 |
            length(grep("is missing, with no default", as.character(e)[1], fixed = T))>0){
          e$message <- paste("Argument", v, "of serv_out's functions does not have 2 arguments")
        }
        stop(e)
      })
    }
  }
  return(output)
}
 
#' Renders user interface for all mwshiny windows.
#'
#' @param ui_list named list of shiny UI pages. The name of each entry in the UI page list corresponds to its window title. No windows can be named 'WindowSelector', titles must be uniquely named, and titles cannot have spaces.
#' @return ui: user interfaces for all windows
mwsUI <- function(ui_list) {
  
  # window titles
  win_titles <- names(ui_list)
  win_select <- ""
  for (w in win_titles){
    # check if titles have spaces
    if (grepl(" ", w)){
      stop(paste("Window titles cannot have spaces. Please remove space in window title:", w))
    }
    
    win_select <- paste0(win_select,
                         '<h2><a href="?',w,'">',w,'</a></h2>')
  }
  
  other_win <- list()
  if (length(ui_list) > 0){
    for (u in 1:length(ui_list)){
      other_win[[length(other_win)+1]] <- tags$div(ui_list[[u]], class = paste0(win_titles[u], " Window"))
    }
  }
  
  # check if there is a html tag -- stop
  if (grepl("</html>", other_win, fixed= T)[1]){
    stop("The <html> tag is reserved for the main page. Please remove any occurences of the <html> tag in your UIs.")
  }
  
  # check if there is a body tag -- suggest change to div with a warning
  if (grepl("</body>", other_win, fixed= T)[1]){
    warning("The <body> tag is reserved for the main page. We suggest you change your <body> tags to <div> tags in your UIs, as errors may occur.")
  }
  
  ui <- shiny::shinyUI(shiny::bootstrapPage(
    shiny::HTML('<script type="text/javascript">
                $(function() {
                $("div.Window").hide();
                var tokens = window.location.href.split("?");
                if (tokens.length > 1) {
                var shown_window = tokens[1];
                $("div."+shown_window).show();
                } else {
                $("div.WindowSelector").show();
                }
                });
                </script>'),
    shiny::div(class="WindowSelector Window",
               shiny::HTML(win_select),
               style='position: absolute;
               top: 50%; left: 50%;
               margin-right: -50%;
               transform: translate(-50%, -50%)'
    ),
    other_win
    ))
  
  return(ui)
}
 
#' Renders user interface for all mwshiny windows.
#'
#' @param win_titles vector of uniquely named strings, corresponding to window titles. Must be same length as ui_win, and titles must be same index as corresponding ui page in ui_win. No windows can be named 'WindowSelector', and titles cannot have spaces.
#' @param ui_list list of shiny ui pages. Must be same length as win_titles, and ui page must be same index as corresponding title in win_titles.
#' @param depend deprecated; previously was a way to declare HTML dependencies, but now they are inferred from elements of \code{ui_list}.
#' @return ui: user interfaces for all windows
mwsUI <- function(win_titles, ui_list, depend = NULL) {
  force(win_titles)
  force(ui_list)
  
  if (!is.null(depend)) {
    warning(call. = FALSE, "The 'mwsUI' function's 'depend' parameter is no longer used")
  }
  
  function(req) {
    qs <- parseQueryString(req$QUERY_STRING)
    
    qs <- req$QUERY_STRING
    mw_win <- substr(qs, 2, nchar(qs))
    
    # mw_win <- qs$mw_win
    # mw_win <- if (!is.null(mw_win) && length(mw_win) == 1 && grepl("^\\d+$", mw_win, perl = TRUE)) {
    mw_win <- if (!is.null(mw_win) && nchar(mw_win) > 0) {
      # as.integer(mw_win)
      mw_win
    } else {
      NULL
    }
    
    if (is.null(mw_win)) {
      mswSelectorPage(win_titles)
      # } else if (mw_win %in% seq_along(ui_list)) {
    } else if (mw_win %in% names(ui_list)) {
      mswPage(ui_list[[mw_win]])
    } else {
      NULL
    }
  }
}
 
mswSelectorPage <- function(win_titles) {
  win_select <- lapply(seq_along(win_titles), function(i) {
    win_title <- win_titles[[i]]
    tags$h2(
      tags$a(href = paste0("?",win_title), #mw_win=", i),
             win_title
      )
    )
  })
  
  shiny::bootstrapPage(
    shiny::div(class = "Window",
               shiny::div(
                 style = htmltools::css(
                   position = "absolute", top = "50%", left = "50%",
                   margin_right = "-50%", transform = "translate(-50%, -50%)"),
                 win_select
               )
    )
  )
}

mswPage <- function(ui) {
  shiny::bootstrapPage(ui)
}

#' Runs Shiny app in multiple specified windows.
#'
#' @param ui_win named list of shiny UI pages. The name of each entry in the UI page list corresponds to its window title. No windows can be named 'WindowSelector', titles must be uniquely named, and titles cannot have spaces.
#' @param serv_calc a named list of functions that calculate variables derived from user input, to be used in rendering output. Each function is of the form function(calc, session), where calc is a named list containing the traditional Shiny input and user-created reactive values, and session is the traditional Shiny server session value. All calculated variables that are needed to render output should be added, named, to the calc list. When using reactive functions such as observeEvent(), each should be contained in a separate function, and variables dependent on these reactions should be added to calc. Note that these functions follow all Shiny conventions (reactive values must be accessed in a reactive context, etc.).
#' @param serv_out a named list of functions that render output. Each function is of the form function(calc, session), where calc is a named list containing the traditional Shiny input and reactive values that have calculated values derived from input, and session is the traditional Shiny server session value. It returns the results of a Shiny render function. The name of each function corresponds to its output label. Note that these functions follow all Shiny conventions (reactive values must be accessed in a reactive context, etc.).
#' @return Shiny app object (i.e., it runs the app)
#' @export
#' @import shiny
#' @examples
#' if(interactive()){
#' # Run a simple 2-window app, initially bringing up the window selector window:
#' ui_win <- list()
#' ui_win[["clickinput"]] <- fluidPage(numericInput(inputId = "click", label = "a", value = 1))
#' ui_win[["clickoutput"]] <- fluidPage(plotOutput("clickplot"))
#' serv_out <- list()
#' serv_out[["clickplot"]] <- function(calc, session){
#'   renderPlot({
#'       plot(1:calc$click,1:calc$click)
#'   })
#' }
#' mwsApp(ui_win, list(), serv_out)
#' }
mwsApp <- function(ui_win=list(), serv_calc=list(), serv_out=list()){
  # safeguards for improper arguments
  win_titles <- names(ui_win)
  if ("WindowSelector" %in% win_titles){
    stop("Argument win_titles contains the reserved window name 'WindowSelector'")
  }
  
  if (length(unique(win_titles)) != length(win_titles)){
    stop("Argument win_titles contains duplicate window titles.")
  }
  
  if (typeof(ui_win)!="list"){
    stop("Argument ui_win is not a list")
  }
  
  if (typeof(serv_calc)!="list"){
    stop("Argument serv_calc is not a list")
  }
  
  if (typeof(serv_out)!="list"){
    stop("Argument serv_out is not a list")
  }
  
  if (is.null(names(serv_out)) & length(serv_out) > 0){
    stop("Argument serv_out is unnamed")
  }
  
  if (is.null(names(ui_win)) & length(ui_win) > 0){
    stop("Argument depend is unnamed")
  }
  
  # compute ui
  ui <- mwsUI(names(ui_win),ui_win)
  
  # preallocate serverValues
  serverValues <- shiny::reactiveValues()
  
  
  # create server, getting output
  mws_server <- (function(input,output,session){ #shiny::shinyServer
    observe({
      for (inputId in names(input)) {
        serverValues[[inputId]] <- input[[inputId]]
      }
    })
    
    # run each of the server calculation functions
    if (length(serv_calc) > 0){
      for (s in 1:length(serv_calc)){
        
        # check for errors related to numbers of inputs
        tryCatch({
          serv_calc[[s]](serverValues, session)
        }, error = function(e){
          if (length(grep("unused argument ", as.character(e)[1], fixed = T))>0 |
              length(grep("is missing, with no default", as.character(e)[1], fixed = T))>0){
            e$message <- paste("Argument", s, "of serv_calc's functions does not have 2 arguments")
          }
          stop(e)
        })
      }
    }
    
    # then allocate each of these to output
    serverFunct(serverValues, session, output, serv_out)
    
  })
  
  # run the app!
  shiny::shinyApp(ui, mws_server)
}
