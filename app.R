# styler::style_dir() #nolint

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

clean_para <- function(true_paras) {
  clean_paras <- sapply(true_paras, function(para) {
    para <- trimws(tolower(para))
    if (substr(para, 1, 3) == "the") {
      para <- trimws(substr(para, 4, 1e5))
    }

    # space clean
    para_words <- strsplit(para, " ")[[1]]
    para_words <- sapply(para_words, function(word) {
      if (word == "") {
        word <- NULL
      } else if (!word %in% c("the", "of", "and", "for", "under")) {
        word <- tools::toTitleCase(word)
      }
      return(word)
    })
    para <- paste0(trimws(para_words), collapse = " ")

    # comma clean
    para_words <- strsplit(para, ",")[[1]]
    if (length(para_words) == 2) {
      para_words[2] <- tools::toTitleCase(para_words[2])
    }
    para <- paste0(trimws(para_words), collapse = ", ")

    # paren clean
    para_words <- strsplit(para, "\\(")[[1]]
    if (length(para_words) == 2) {
      para_words[2] <- toupper(para_words[2])
    }
    para <- paste0(trimws(para_words), collapse = " (")

    return(para)
  })
  return(clean_paras)
}

clean_action <- function(true_actions) {
  clean_actions <- sort(unique(tolower(unlist(strsplit(true_actions, "\\/")))))
  return(clean_actions)
}

starter_text <- "<br><br><span style=\"color:red;\">Note: "
merger_text <- paste0(
  starter_text,
  "Mergers usually lead to consolidation of staff.</span>"
)
subsumed_text <- paste0(
  starter_text,
  "This may lead to consolidation of staff.</span>"
)
private_text <- paste0(
  starter_text,
  "This may lead to mass layoffs.</span>"
)
# merger_text <- subsumed_text <- private_text <- "" #nolint

action_interpreter <- function(
    action, para, comment_1 = NULL, comment_2 = NULL) {
  action <- tolower(trimws(action))
  comment_1 <- trimws(comment_1)
  text <- ""
  if (action == "") {
    text <- paste0("There is no recommendation for the ", para, ".")
  } else if (grepl("merged", action)) {
    text <- paste0(
      "The <b>", para, "</b> will be <b>merged</b> with the <b>",
      comment_1, "</b>"
    )
    if (comment_2 != "") {
      text <- paste0(
        text, " to form the <b>", comment_2, "</b>"
      )
    }
    text <- paste0(text, ".", merger_text)
  } else if (grepl("transformed", action)) {
    text <- paste0(
      "The <b>", para,
      "</b> will be <b>transformed</b> to an ",
      "<b>extra-ministerial department in the ",
      comment_1, "</b>."
    )
    text <- paste0(text, subsumed_text)
  } else if (grepl("abolished/", action)) {
    text <- paste0("The <b>", para, "</b> will be <b>abolished</b>, and ")
    if (grepl("transferred", action)) {
      text <- paste0(
        text,
        "its operations will be <b>transferred</b> to the <b>",
        comment_1, "</b>."
      )
    } else if (grepl("subsumed", action)) {
      text <- paste0(
        text,
        "its operations will be <b>subsumed</b> under the <b>",
        comment_1, "</b>."
      )
    }
    text <- paste0(text, private_text)
  } else if (grepl("transferred", action)) {
    text <- paste0(
      "The operations of the <b>", para, "</b> will be ",
      "<b>transferred</b> to the <b>",
      comment_1, "</b>."
    )
    text <- paste0(text, subsumed_text)
  } else if (grepl("subsumed", action)) {
    text <- paste0(
      "The <b>", para, "</b> will be <b>subsumed</b> under the <b>",
      comment_1, "</b>."
    )
    text <- paste0(text, subsumed_text)
  } else if (action == "cease funding") {
    text <- paste0("The <b>", para, "</b> will <b>cease to be funded</b>.")
    text <- paste0(text, private_text)
  } else if (action == "amended") {
    text <- paste0("The operations of the <b>", para, "</b> will be amended.")
  } else if (action == "abolished") {
    text <- paste0("The <b>", para, "</b> will be <b>abolished</b>.")
    text <- paste0(text, private_text)
  } else if (action == "self funding") {
    text <- paste0("The <b>", para, "</b> will become <b>self-funding</b>.")
    text <- paste0(text, private_text)
  } else if (action == "privatized") {
    text <- paste0("The <b>", para, "</b> will be <b>privatized</b>.")
    text <- paste0(text, private_text)
  } else if (grepl("commercial", action)) {
    text <- paste0(
      "The operations of the <b>", para, "</b> will be <b>commercialized</b>."
    )
    text <- paste0(text, private_text)
  } else if (grepl("liquidated", action)) {
    text <- paste0("The <b>", para, "</b> will be <b>liquidated</b>.")
  }

  return(text)
}

parastatal_selection_fun <- function(
    selected_para, true_para_list, clean_para_list, dat) {
  parastatal <- NULL

  if (selected_para == "NA") {
    return("")
  }

  true_para <- true_para_list[which(clean_para_list == selected_para)]
  print(true_para)
  sub_dat <- dat[parastatal == true_para]

  result_text <- action_interpreter(
    sub_dat$action, selected_para, sub_dat$comment_1, sub_dat$comment_2
  )
  return(paste0("<br>", result_text))
}

action_selection_fun <- function(
    selected_action, dat) {
  action <- NULL

  if (length(selected_action) == 1) {
    if (selected_action == "") {
      return("")
    }
  }

  para_list <- c()
  for (sel in selected_action) {
    para_list <- c(para_list, dat[grepl(sel, action)]$parastatal)
  }

  para_list <- clean_para(sort(unique(para_list)))

  result_text <- paste0(para_list, collapse = "\n")
  return(result_text)
}

library(data.table)
library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(

  tags$style(HTML("
    .sl_link {
      text-align: center;
      font-size: 18px;
      margin-bottom: 50px;
    }
    .sl_dl {
      text-align: center;
    }
    .sl_img {
      max-width: 80%;
      display: block;
      margin-left: auto;
      margin-right: auto;
      margin-bottom: 0px;
      height: auto;
    }
    @media only screen and (max-width: 600px) {
      .sl_img {
        max-width: 30%;
        padding-bottom: 0px;
      }
    }
  ")),

  # Application title
  titlePanel("Steve Oronsaye Report"),
  fluidRow(
    column(
      5,
      pickerInput(
        "parastatal_select",
        "Search by Government Agency",
        choices = NA,
        options = pickerOptions(
          liveSearch = TRUE,
          actionsBox = TRUE,
          size = "auto"
        )
      )
    )
  ),
  fluidRow(
    column(
      10,
      htmlOutput("parastatal_text"),
      offset = 1
    )
  ),
  hr(),
  fluidRow(
    column(
      6,
      awesomeCheckboxGroup(
        "action_select", "Search by Proposed Action",
        choices = NA, width = "100%"
      )
    )
  ),
  fluidRow(
    column(
      10,
      verbatimTextOutput("action_text"),
      offset = 1
    )
  ),
  hr(),
  fluidRow(
    column(
      1,
      tags$div(
        id = "sl_dl",
        class = "sl_dl",
        downloadButton(
          "report_link",
          "Download the full report"
        )
      ),
      offset = 1
    )
  ),
  hr(),
  fluidRow(
    column(
      3,
      tags$div(
        id = "sl_link",
        class = "sl_link",
        uiOutput("sl_web")
      )
    ),
    column(
      1,
      tags$div(
        id = "sl_img",
        class = "sl_img",
        imageOutput("sl_logo")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  parastatal <- NULL
  action <- NULL
  dat <- data.table::fread("oronsaye_data.csv")

  true_para_list <- sort(dat[, .N, parastatal]$parastatal)
  clean_para_list <- clean_para(true_para_list)

  true_action_list <- sort(dat[, .N, action]$action)
  clean_action_list <- clean_action(true_action_list)

  updatePickerInput(
    session, "parastatal_select",
    choices = clean_para_list
  )

  updateAwesomeCheckboxGroup(
    session, "action_select",
    choices = clean_action_list,
    inline = TRUE, status = "danger"
  )

  output$parastatal_text <- renderText({
    return(parastatal_selection_fun(
      input$parastatal_select,
      true_para_list,
      clean_para_list,
      dat
    ))
  })

  output$report_link <- downloadHandler(
    filename = function() "Oronsaye Report.pdf",
    content = function(file) file.copy("./Oronsaye_Report.pdf", file),
    contentType = "application/pdf"
  )

  output$action_text <- renderText({
    return(action_selection_fun(input$action_select, dat))
  })

  output$sl_logo <- renderImage({
    list(
      src = "./VN.png",
      width = "100%"
    )
  }, deleteFile = FALSE)

  sl_url <- a(
    "SocialistLabour.com.ng",
    href = "https://socialistlabour.com.ng/",
    target = "_blank"
  )
  output$sl_web <- renderUI({
    tagList(sl_url)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
