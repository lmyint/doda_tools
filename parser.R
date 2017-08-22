library(markdown)

parseExperimentRmd <- function(file) {
    lines <- readLines(file)
    ## Remove empty lines and lines for scratch work
    lines <- lines[lines != ""]
    indexStop <- grep("PARSER IGNORE BELOW", lines)
    lines <- lines[seq_len(indexStop-1)]
    ## Separate lines by section
    linesBySection <- separateBy(lines, pattern = "^# ")
    ## Each element of uiList is a single UI (if that section is not randomized)
    uiListBySection <- lapply(linesBySection, parseSection)
    ## Extract tools and skills
    bool_skills <- uiListBySection=="skills"
    if (any(bool_skills)) {
        skills <- attr(uiListBySection[[which(bool_skills)]], "skills")
    } else {
        skills <- c()
    }
    bool_tools <- uiListBySection=="tools"
    if (any(bool_tools)) {
        tools <- attr(uiListBySection[[which(bool_tools)]], "tools")
    } else {
        tools <- c()
    }
    uiListBySection <- uiListBySection[!bool_skills & !bool_tools]
    ## Create a list where each element is the full UI for one variation of the experiment
    combos <- expand.grid(lapply(lengths(uiListBySection), seq_len))
    uiList <- lapply(seq_len(nrow(combos)), function(i) {
        tl <- lapply(seq_len(ncol(combos)), function(j) {
            index <- combos[i,j]
            uiListBySection[[j]][[index]]
        })
        tagList(tl, actionButton("submit_experiment", "Submit"))
    })
    attr(uiList, "skills") <- skills
    attr(uiList, "tools") <- tools
    return(uiList)
}

parseSection <- function(lines) {
    is_intro <- length(grep("Introductory text", lines[1], ignore.case = TRUE)) > 0
    is_question <- length(grep("Question", lines[1], ignore.case = TRUE)) > 0
    is_skills <- length(grep("Skills", lines[1], ignore.case = TRUE)) > 0
    is_tools <- length(grep("Tools", lines[1], ignore.case = TRUE)) > 0
    if (is_intro) {
        linesInPrompts <- separateBy(lines, pattern = "# Prompt")
        ## List of HTML strings: one list element per version
        ui <- lapply(linesInPrompts, function(promptLines) {
            tags$div(HTML(markdownToHTML(text = tail(promptLines, -1), fragment.only = TRUE)))
        })
    } else if (is_question) {
        ## If only 1 version, separate lines by level 2 headings
        linesBySection <- separateBy(lines, pattern = "^## ")
        ui <- parseQuestion(linesBySection)
    } else if (is_skills) {
        skills <- strsplit(lines[2], ",")[[1]]
        ui <- "skills"
        attr(ui, "skills") <- skills
    } else if (is_tools) {
        tools <- strsplit(lines[2], ",")[[1]]
        ui <- "tools"
        attr(ui, "tools") <- tools
    }
    return(ui)
}

parseQuestion <- function(lineList) {
    widthPercAssign <- "70%"
    tag <- type <- prompt <- c()
    choices <- list()
    extractQuestionComponents <- function(ll) {
        for (i in seq_along(ll)) {
            lines <- ll[[i]]
            if (length(grep("tag", lines[1], ignore.case = TRUE)) > 0) {
                tag <<- tail(lines, -1)
            } else if (length(grep("type", lines[1], ignore.case = TRUE)) > 0) {
                type <<- tail(lines, -1)
            } else if (length(grep("prompt", lines[1], ignore.case = TRUE)) > 0) {
                prompt <<- c(prompt, paste(tail(lines, -1), collapse = "\n"))
            } else if (length(grep("choice", lines[1], ignore.case = TRUE)) > 0) {
                choices[[length(choices)+1]] <<- tail(lines, -1)
            } else if (length(grep("variation", lines[1], ignore.case = TRUE)) > 0) {
                ## Split these lines by level 3 headers and recurse
                linesByLevel3 <- separateBy(lines, pattern = "^### ")
                extractQuestionComponents(linesByLevel3)
            }
        }
    }
    extractQuestionComponents(lineList)
    ## Format checking
    if (length(tag) > 1) stop("Only one tag should be supplied per question")
    if (length(type) > 1) stop("Only one question type should be supplied")
    if (length(tag)==0) stop("Must supply a tag")
    if (length(type)==0) stop("Must supply a question type")
    if (any(prompt=="")) stop("Supplied question prompts cannot be empty")
    if (any(duplicated(choices))) stop("Choices must be unique")
    ## Create UI based on type
    if (length(choices) > 0) {
        combos <- expand.grid(prompt = seq_along(prompt), choices = seq_along(choices))
    } else {
        combos <- expand.grid(prompt = seq_along(prompt))
    }
    qid <- paste0("assign_", tag)
    uiQuestion <- lapply(seq_len(nrow(combos)), function(i) {
        indexPrompt <- combos$prompt[i]
        questionPrompt <- tags$span(HTML(markdownToHTML(text = prompt[indexPrompt], fragment.only = TRUE)))
        if (length(grep("choose one", type, ignore.case = TRUE)) > 0) {
            indexChoices <- combos$choices[i]
            # choicesList <- lapply(choices[[indexChoices]], function(answerChoice) {
            #     markdownToHTML(text = answerChoice, fragment.only = TRUE)
            # })
            choicesList <- as.list(seq_along(choices[[indexChoices]]))
            names(choicesList) <- choices[[indexChoices]]
            # names(choicesList) <- LETTERS[seq_along(choicesList)]
            radioButtons(qid,
                label = questionPrompt,
                choices = choicesList,
                selected = NA,
                width = widthPercAssign)
        } else if (length(grep("choose many", type, ignore.case = TRUE)) > 0) {
            indexChoices <- combos$choices[i]
            # choicesList <- lapply(choices[[indexChoices]], function(answerChoice) {
            #     markdownToHTML(text = answerChoice, fragment.only = TRUE)
            # })
            choicesList <- as.list(seq_along(choices[[indexChoices]]))
            names(choicesList) <- choices[[indexChoices]]
            # names(choicesList) <- LETTERS[seq_along(choicesList)]
            checkboxGroupInput(qid,
                label = questionPrompt,
                choices = choicesList,
                selected = NA,
                width = widthPercAssign)
        } else if (length(grep("short", type, ignore.case = TRUE)) > 0) {
            textInput(qid,
                label = questionPrompt,
                value = "",
                width = widthPercAssign)
        } else if (length(grep("long", type, ignore.case = TRUE)) > 0) {
            textAreaInput(qid,
                label = questionPrompt,
                value = "",
                width = "400px",
                height = "100px")
        } else if (length(grep("numeric", type, ignore.case = TRUE)) > 0) {
            numericInput(qid,
                label = questionPrompt,
                value = 0,
                width = widthPercAssign)
        } else if (length(grep("file", type, ignore.case = TRUE)) > 0) {
            fileInput(qid,
                label = questionPrompt,
                width = widthPercAssign)
        }
    })
    return(uiQuestion)
}

separateBy <- function(lines, pattern) {
    idxSections <- c(grep(pattern, lines, ignore.case = TRUE), length(lines)+1)
    lapply(seq_len(length(idxSections)-1), function(i) {
        idx <- idxSections[i]:(idxSections[i+1]-1)
        lines[idx]
    })
}