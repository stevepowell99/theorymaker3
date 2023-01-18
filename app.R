library(stringr)
library(fun)
library(markdown)
library("shinyBS")
library("shinyjs")
library("rmarkdown")

# source("extras/t2a.r") # the main script that generates the diagrams
eval(parse("extras/t2a.r", encoding="UTF-8")) #this is a replacement for source because source can't read a utf8 file

## helper functions

SreadLines = function(x)
  suppressWarnings(readLines(encoding = "UTF-8", x))

unicodeify=function(x)x


writeLog = function(x) {
  xx = c(Sys.time() %>% as.character,
         x[1],
         x[2] %>% str_sub(1, 4),
         x[-(1:2)],
         "</br>")
  write.table(
    x = paste0(xx, collapse = " | "),
    file = "www/log.html",
    append = TRUE,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t"
  )
  
} # the function that adds sessions to the /log.html file


currentURL = ""
makeReactiveBinding("currentURL")


jsCode <- "shinyjs.pageCol = function jumpto(anchor){
window.location.href = '#'+anchor;
}"


# Define UI
ui <- tagList(
  useShinyjs(),
  inlineCSS(list(.red = "background: snow")),
  
  extendShinyjs(text = jsCode, functions = "pageCol"),
  # source("extras/tooltips.R"),
  
  # tooltips
  bsTooltip(
    id = "myText",
    title = "The variables are listed here, one on each line. If you start a line with one more space than the one above it, Theory Maker will link this line to the one above with an arrow",
    placement = "top",
    trigger = "hover"
  )
  #tooltips----
  ,
  bsTooltip(
    id = "gallery2",
    title = "Click on an example to view and edit it",
    placement = "right",
    trigger = "hover"
  )
  ,
  bsTooltip(
    id = "titl",
    title = "If you click 'Save a version', this title will be used for the link.",
    placement = "top",
    trigger = "hover"
  )
  ,
  bsTooltip(
    id = "savebut",
    title = "Click to save a version and get a link which you can return to if you want to view and edit your diagram later.",
    placement = "top",
    trigger = "hover"
  )
  ,
  # bsTooltip(
  #   id = "downloadGraph",
  #   title = "If you click 'Save a version', this title will be used for the link.",
  #   placement = "top",
  #   trigger = "hover"
  # )
  # ,
  # bsTooltip(
  #   id = "youremail",
  #   title = "Please add your email so we can occasionally tell you about Theory Maker updates.",
  #   placement = "top",
  #   trigger = "hover"
  # )
  # ,
  bsTooltip(
    id = "publiccheck",
    title = "Leave this box checked if you would like to submit your diagram for display in the Examples section.",
    placement = "top",
    trigger = "hover"
  )
  ,
  bsTooltip(
    id = "showex",
    title = "Leave this box checked if you would like to submit your diagram for display in the Examples section.",
    placement = "top",
    trigger = "hover"
  )
  ,
  bsTooltip(
    id = "propslider",
    title = "Drag the slider to the right to make your diagram taller and thinner.",
    placement = "right",
    trigger = "hover"
  )
  ,
  bsTooltip(
    id = "direct",
    title = "Choose which direction you would like the arrows to go in.",
    placement = "right",
    trigger = "hover"
  )
  ,
  
  
  
  ## layout starts
  # tags$head(
  #   HTML(
  #     '<script type="text/javascript">
  #     $(document).ready(function() {
  #     // creates a handler for our special message type
  #     Shiny.addCustomMessageHandler("api_url", function(message) {
  #     // set up the the submit URL of the form
  #     $("#form1").attr("action", "/" + message.url);
  #     $("#submitbtn").click(function() { $("#form1").submit(); });
  #     });
  #     })
  #     </script>'
  #   ),
  #   
  #   
  # #   ## include an external script for a chat window
  # #   includeScript("chat.js")
  # ),
  # 
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "mystyles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "//cdn-images.mailchimp.com/embedcode/classic-10_7.css"),
    
    tags$style(type = "text/css", "select { width: 100px; }"),
    # defines width of dropdown panel
    tags$style(type = 'text/css', ".span4 { max-width: 150px; }")  # defines width of panel containing  dropdown panel
    
  ),
  
  navbarPage(
    #theme = shinytheme("cerulean"),
    #
    #     googleAuthUI("example1"),
    # p("Logged in as: ", textOutput("user_name")),
    
    id = "main",
    inverse = F,
    collapsible = T,
    
    windowTitle =  "Theory Maker",
    title = div(
      img(src = "img/logo-small.gif", height = "20px"),
      a("Theorymaker", href = ".")
    ),
    
    
    
    tabPanel(
      title = "Text App",
      value = "app",
      icon = icon("list")
      ,
      
      
      
      sidebarLayout(
        sidebarPanel(
          bsAlert("bsmsg"),
          bsAlert("found"),
          bsAlert("notfound"),
          
  
          
                  
          div(
          
            tags$div(
              p("Early test version of Theorymaker3! You cannot make groups by starting a line with '-'"), 
              p("Put one more spaces at the start of a line to get an arrow to the line above."),
              style = "display:inline-block;margin-top:10px"
            )
            ,
            tags$div(
            style = "display:inline-block",
            actionLink("tabBut", "Help and tips."),
            tags$a("Watch videos.", href = "https://www.youtube.com/channel/UCGZu-7IM7JF3rHdO6xpchOA")
            ),
            
            bsModal(
              "cheatModal",
              "Theory Maker",
              "tabBut",
              size = "large",
              tags$div(includeHTML('extras/pages/cheat.html'))#, class = "col3")
            )


          ),

          
          
          
          
          
          tags$style(
            type = "text/css",
            "textarea {width:100%;font-family:'Courier';font-weight:bold} "
          ),
          
          
          tags$textarea(
            id = "myText",
            rows = 26,
            cols = 60,
"Happiness !heart
 Personality
 Luck; colour=red3
 Effort
  Personality"
            
          ),
          div(list(
            div(textInput(
              "titl", "Title", value = "", width = "100%"
            ), style = "display:inline-block;width=100%"),
            div(checkboxInput("publicc", "Public", value = T), style =
                  "display:inline-block;margin-left:10px") ,
            div(style = "display:inline-block", uiOutput("savebut"))
          )),
          uiOutput("titlvalid"),
          uiOutput("versions"),
          # textInput("urll","lab"),
          
          
          div(
            
            downloadButton(outputId = 'downloadGraph', label = "Download picture"),
            downloadButton(outputId = 'downloadGraph2', label = "Download PDF"),
            downloadButton(outputId = 'downloadGraph3', label = "Download for Office")
          )
          
          
        ),
        
        
        # Show a live output of the diagram
        mainPanel(
          #h5(tags$b("Live Output")),
          
          # div(id="pagetop"),
          
          
#           bsModal(id = 'startupModal', 
#                   title = NULL, trigger = '',
#             size = 'large', list(
#             p("Free online apps to visualise your Theory of Change"),
#            div(
#             h2(list("Theorymaker",tags$i("text"))),
#             p("The classic Theorymaker - type something and watch your diagram appear."),
#             # p("Best for diagrams with a simple, perhaps hierarchical structure."),
#             # a(img(src='img/text.PNG',height="100px"),href="."),
#             HTML('<button type="button" class="btn btn-default" data-dismiss="modal"><img src="img/text.PNG",height="100px",style="height:100px"></button>')
#             ,
#             tags$br()
#             ,
#             div(style="background-color:#1e5722;padding:10px;border-radius:5px",
#               HTML('<button type="button" class="btn btn-default" data-dismiss="modal"><b>Start!</b></button>')
#             ,
#             a(div("About",class="btn btn-default"),href="?tab=about")
#             ,
#             a(div("Help",class="btn btn-default"),href="?help=yes")
#             ,
#             a(div("Videos",class="btn btn-default"),href="https://www.youtube.com/channel/UCGZu-7IM7JF3rHdO6xpchOA")
#             )
#             )
# 
# 
#             ,
#             div(
#             
#             div(
#             h2("About")
#             ,
#             tags$ul(
#               tags$li("completely free & open-source"),
#               tags$li("let you save diagrams online & export as pictures"),
#               tags$li("help you organise your diagrams using 'grouping boxes'"),
#               tags$li("both are based on the same symbols and ideas")
#               )
#             ,
#             div(style="background-color:#1e5722;padding:10px;border-radius:5px",
#             a(div("Introductory slides",class="btn btn-default"),href="http://www.pogol.net/public/slides.html",target="_new")
#             ,
#             a(div("Key to symbols",class="btn btn-default"),href="http://www.pogol.net/public/slides.html#/overview-basics",target="_new")
#             ,
#             a(div("Mailing list",class="btn btn-default"),href="http://eepurl.com/b4O3Kj",target="_new")            )
#             )
#             )
#               
#               
#               
#                       )
# )         ,
          
          div(
            id = "pagetop",
            # tags$a(imageOutput("imagegraph",
            #             width = "100%",
            #             height = "auto",
            #             inline = F)
            #        ,href="#imagegraph"
            #        )
            imageOutput(
              "imagegraph",
              width = "100%",
              height = "auto",
              inline = F
            )
            ,
            href = "#imagegraph"
            
          ),
          
          uiOutput("blog"),
          
   
          div(
            style = "background-color:whitesmoke;padding:15px",
            div(style = "background-color:whitesmoke;width=100%",
                div(actionButton(
                  "showexb", div(hidden(div(id = "s1", "Show Examples")), div(id = "s2", "Hide Examples"))
                ), style = "display:inline-block")),
            div(
              id = "gall",
              h1("Examples"),
              div("Click one to make your own version.", id = "clickone"),
              # tags$h2("Examples",style="margin-left:12px;display:inline-block",id="gallery2"),
              div(textInput(
                "search", label = "Search", width = "290px"
              ), id = "gallerySearch"),
              uiOutput("message2"),
              uiOutput("gallery")
              # )
            )
          )
          
        )
        
        
        )
      
      
      
      ),
    
    

# tabPanel(HTML("</li><li><a href=\"http://theorymaker.info/?help=yes\" >Help")),
tabPanel(HTML("</li><li><a href=\"http://www.pogol.net/public/slides.html\" target=\"_blank\">Slides")),
tabPanel(HTML("</li><li><a href=\"http://www.pogol.net/public/slides.html#/overview-basics\" target=\"_blank\">Symbols")),
tabPanel(HTML("</li><li><a href=\"https://www.youtube.com/channel/UCGZu-7IM7JF3rHdO6xpchOA\" target=\"_blank\">Videos")),
tabPanel(HTML("</li><li><a href=\"http://eepurl.com/b4O3Kj\" target=\"_blank\">Mailing list")),

    
    navbarMenu(
      "More ...",
      
      tabPanel(
        "About Theory Maker",
        value = "about",
        includeMarkdown('extras/pages/about.md'),
        style = 'width: 600px;margin: auto'
      ),
      
      tabPanel(
        "What is new in Theory Maker 2?",
        value = "new",
        includeMarkdown('extras/pages/new-in-tm-2.md'),
        style = 'width: 600px;margin: auto'
      ),
      
      tabPanel(
        "Features",
        value = "features",
        includeMarkdown('extras/pages/features.md'),
        style = 'width: 600px;margin: auto'
      ),
      tabPanel(
        "FAQ",
        value = "faq",
        includeMarkdown('extras/pages/FAQ.md'),
        style = 'width: 600px;margin: auto'
      ),
      tabPanel(
        title =   tags$a('Blog', href = 'http://www.pogol.net/tagged/theorymaker')
      )
      
      
    )
    
)
)


# Define server logic

server <- function(input, output, session) {
  
  # access_token <- callModule(googleAuth, "example1")
  
  
  
  
  
          observe({
          query <- parseQueryString(session$clientData$url_search)
          qe = query[['tab']] 
          # browser()
            # if(is.null(qe) & is.null(query[['help']]) & is.null(query[['text']]) & is.null(query[['tag']]) & is.null(query[['example']]) & is.null(query[['permalink']]) )toggleModal(session, "startupModal", toggle = "open")
            
          
        })
  
##update page from tab
  
          observe({
          if(input$main=="app")toggleClass("savemsg","red")
            
          
        })
  
  
##update page from tab
  
          observe({
          query <- parseQueryString(session$clientData$url_search)
          qe = query[['tab']] 
          # browser()
            if(!is.null(qe))updateNavbarPage(session, "main", selected =  as.character(qe))
            
          
        })
  
  
  
  
  # delay(500,  closeAlert(session, "bsmsg"))
  
  loaded=F
  makeReactiveBinding("loaded")
  
  first=T
  makeReactiveBinding("first")
  
  
  
  # fileexists=F
  # makeReactiveBinding("fileexists")
  issaved=F
  makeReactiveBinding("issaved")
  inputtitl=""
  makeReactiveBinding("inputtitl")
  
  observeEvent(input$showexb, {
 toggle("gall",anim = T,animType = "fade")
    toggle("s1")
    toggle("s2")
})
#   observeEvent(input$showvideos, {
#  toggle("videos",anim = T,animType = "fade")
#     toggle("vs1")
#     toggle("vs2")
# })

  # update from permalink or example url; also set buttons and messages ----
  observe({
    
    first=F
    
    
    # browser()
    query <- isolate(parseQueryString(session$clientData$url_search))
    
    
    
    
    qh = query[['help']]
    if(!is.null(qh)) {
      if(qh=="yes")
        
                  toggleModal(session,modalId = 
                  "cheatModal",toggle=open
                  )
        
    } else {
    
    
    ql = query[['permalink']]
    qe = query[['example']]
    
    if(!is.null(qe)) ql=qe
    # ql=tolower(ql)
    
    # if(!loaded)currentURL<<-ql
    
    if (!is.null(ql)) {
      filename=""
      ql=gsub("-public","",ql)
      ql=gsub("-submitted","",ql)
      
      fp=paste0("www/", ql, "-public.txt");
      fs=paste0("www/", ql, "-submitted.txt");
      fo=paste0("www/", ql, ".txt");
      fpn=gsub(" ","-",fp)
      fsn=gsub(" ","-",fs)
      fso=gsub(" ","-",fo)
      
      for(c in list(fp,fs,fpn,fsn,fso,fo)){
        # c=tolower(c)
        if(file.exists(c))filename=c
      }
# browser()
            if(filename!="")
        fromfile2 = SreadLines(filename) %>% paste0(collapse = "\n")
      else
      {fromfile2 = ""
        createAlert(session, "notfound",  title = "Sorry, couldn't find that link.",content="Let me know if you need help: steve@pogol.net", append = FALSE)      
        }
      if (input$myText != fromfile2 & !loaded &fromfile2 != "") {
        updateTextInput(session, "myText", value = fromfile2)
        updateTextInput(session, "titl", value = ql)
        updateCheckboxInput(session, "publicc", value = grepl("-public",filename))
        writeLog(c("from url:",session$token,ql))
        
        if(!grepl("-public",filename) & file.info(filename)$mtime %>% as.Date<as.Date("2016-10-16")) createAlert(session, "found",  title = "Link found",content="Diagrams created in the old version of Theory Maker may look different in Theory Maker 2. Let me know if you have any problems: steve@pogol.net", append = FALSE)
        # browser()
      }
      # if(!loaded)updateTextInput(session, "urll", value = gsub("\\.txt$","",ql))
      
      loaded<<-T
    }
    
    # update from text------------
    
    qt = query[['text']]
    
    if (!is.null(qt)) {
      
      
      if (!loaded) {
        updateTextInput(session, "myText", value = URLdecode(query[['text']]))
        
        # output$links = renderUI(tags$p(paste0("Loaded diagram ", input$title, "")))
      }
      loaded<<-T
    }
    }
    
  })
  
  observe({
    b=input$myText 
    bb=str_split(b,"\\n")[[1]]
    blogstart=which(grepl("^===",bb))
    # browser()
    if(length(blogstart)>0)blogtext=bb[(blogstart+1):length(bb)] %>% paste0(collapse="\n\n") else blogtext=""
    if(is.na(blogtext))blogtext=""
    
    output$blog = renderText(if(blogtext=="")"" else markdownToHTML(text=blogtext,fragment.only = T))
  })
  
  
  
  observe({
    
    output$versions=renderUI(if(!issaved) div() else div(id="savemsg","Saved to this permanent link: ",tags$a(paste0(currentURL),href=paste0("?permalink=",currentURL))))
  })
  
  
  observe({
    
    # myTextVec <- reactive({str_split(input$myText,"\\n")[[1]]})
    
    
    
    # output$blog = renderText(markdownToHTML(text="# My blog",fragment.only = T))
    
    output$savebut=renderUI(div(actionButton("saveb", "Save a version")))
    
  })
  
  
  output$titlvalid=renderUI({
    validate(errorClass="warn",
      need(!grepl("[^[[:alnum:]|-]]*",input$titl ), "Please only type a-z, -, and numbers; no spaces")
    )
  })
  
  ##gallery---------------
  output$gallery <-
    renderUI({
      
      searchterm=((input$search)) 
      # browser()#%>% as.character()
      if(is.null(searchterm))searchterm=""
      
      # browser()
      permlist<-list.files("www",".*\\.txt$")
      permlist=permlist[order(tolower(permlist))]
      
      permlistpub<<-permlist[grepl("-public",permlist)]
      for(i in permlistpub){
        txtpath=paste0("www/",i)
        imgpath=gsub("txt$","svg",txtpath)
        nopath=gsub("\\.txt$","",i)
        if(!file.exists(imgpath)){
          makeToC2(tex = readLines(encoding="UTF-8",txtpath) %>% paste0(collapse="\n"),sess=paste0(nopath,""))
          
        }
        
        
      }
      list(
        lapply(permlistpub,function(k){
          # if(currentURL!="")browser()
          
          kk= gsub("-public\\.txt","",k) 
            
            kkk=gsub("^[0-9]*","",kk) %>% gsub("-"," ",.)
          if(searchterm!="")content=paste0(readLines(encoding="UTF-8",paste0("www/",k)),collapse=" ") else content=""
          if(searchterm==""|nchar(searchterm)<3 |grepl(tolower(searchterm),tolower(content)))         actionLink(k, HTML(
            paste0(
              "<div class='galouter'>",
              
              paste0(
              
              "<div class='",if(currentURL==kk)"foundtext" else "text","' width='50px'>"  #this doesnt work, dont know why. classes not appearing at all in inspector
                ,
                kkk,
                "</div>",
                "<img class='galimage' src='",
                URLencode(gsub("txt$","svg",k))  ,
                "' max-height='300px' max-width='200px'/>"
              ),
              "</div>"
            )
          ), class = "gallink") else NULL
        }))
      
      
    }
    )
  
  # render from gallery click----
  observe({
    
    permlistpub=list.files("www/",".*-public\\.txt")
    
    lapply((permlistpub), function(x) {
      observeEvent(input[[x]], {
        updateTextInput(session, "myText", value = readLines(encoding="UTF-8",paste0("www/",x)) %>% paste0(collapse="\n"))
        x=gsub("-public\\.txt","",x)
        updateTextInput(session, "titl", value = gsub("\\.txt$","",x))
        # browser()
        
                  writeLog(x = c("gallclick",session$token,x))
                  
                  
              js$pageCol("pagetop")

        currentURL<<-x
      })
    })
  })
  
  # savebut-----
  
  observeEvent(input$saveb, {
    
    # permlist<-list.files("www",".*\\.txt$") 
    
    # browser()
    if (""!=(input$titl)) {
      # inputtitl<<-iconv(input$titl,"","ASCII",sub="") %>% gsub("[^[[:alnum:]]]*","",.)
      inputtitl<<-gsub("[^[[:alnum:]|-]]*","",input$titl)
      currentURL<<-inputtitl
      
      
      # if(grepl(".*-delete$",inputtitl)) file.remove()  
      
# browser()
      filen=paste0("www/",inputtitl,ifelse(input$publicc & !grepl("*-public$",inputtitl),"-submitted",""),".txt")
      
      writeLines(unicodeify(input$myText),filen,useBytes = T)
      issaved<<-T
      file.copy(paste0("www/",session$token,".svg"), paste0("www/",gsub("","",inputtitl),".svg"),overwrite = T)
          # output$versions=renderUI(if(!issaved) div() else div("Saved to this permanent link: ",tags$a(paste0(currentURL),href=paste0("?permalink=",currentURL))))
      # info(paste0(tags$a(paste0(currentURL),href=paste0("?permalink=",currentURL))))
      # inlineCSS("#savemsg {fontsize:22px}")
      toggleClass("savemsg","red")
      delay(500,toggleClass("savemsg","red"))
      # g=0
      
      # makeToC2(tex = input$myText, sess=currentURL, ext = "png",ratio = input$proportion,rankdir=input$direction)
      # makeToC2(tex = input$myText, sess=currentURL, ext = "emf",ratio = input$proportion,rankdir=input$direction)
      
      writeLog(c("Save di: ",session$token,paste0("<a href='",currentURL,".png'>",currentURL,"</a>"),paste0("<a href='",paste0(".?permalink=",currentURL),"'>link</a>")))
    }
  }
  )
  
  
  # Send a pre-rendered image, and don't delete the image after sending it
  output$imagegraph <- renderImage({
    
    validate(
      need(input$myText != "", "Please type something in the window on the left")
      # ,
    )
    
    
    
    # browser()
    
    
    
    #make both png and svg
    makeToC2(tex = input$myText, sess=session$token, ext = "svg",ratio = input$proportion,rankdir=input$direction)
    
    textStore<<-input$myText

    # myt2=gsub("^( *) ([[:alnum:]].*)","\\1x\\2",input$myText)
    # updateTextInput(session,"myText",value=myt2)  #not easy to do because cursor jumps to end each time
    
    
    #Get size of "imagegraph" container from the UI elements
    wid  <- session$clientData$output_imagegraph_width
    hei <- session$clientData$output_imagegraph_height
    

    
    # Return a list containing the filename and alt text
    sesspath=paste0("www/",session$token)
    writeLines(input$myText,paste0(sesspath,".txt"))
    list(
      src = paste0(sesspath, ".svg"),
      alt = "Theory Maker",
      align = "top",
      width = wid
      
    )
    
  }, deleteFile = FALSE)
  
  #make a download handler for the downloadGraph button
  output$downloadGraph <- downloadHandler(
    filename = function(){
      paste0("Theory Maker - ",currentURL,".png")
    }, 
    #make a copy of the file on server to download
    content = function(file){
            writeLog(c("Save png: ",session$token,paste0("<a href='",currentURL,".png'>",currentURL,"</a> downloaded")))
      makeToC2(tex = input$myText, sess=session$token, ext = "png",ratio = input$proportion,rankdir=input$direction)
      file.copy(paste0("www/",session$token,".png"), file)
    }
  )
  #make a download handler for the downloadGraph button
  output$downloadGraph2 <- downloadHandler(
    filename = function(){
      paste0("Theory Maker - ",currentURL,".pdf")
    }, 
    #make a copy of the file on server to download
    content = function(file){
            writeLog(c("Save pdf: ",session$token,paste0("<a href='",currentURL,".pdf'>",currentURL,"</a> downloaded")))

      makeToC2(tex = input$myText, sess=session$token, ext = "pdf",ratio = input$proportion,rankdir=input$direction)
      file.copy(paste0("www/",session$token,".pdf"), file)
    }
  )
  
  output$downloadGraph3 <- downloadHandler(
    filename = function(){
      paste0("Theory Maker - ",currentURL,".emf")
    }, 
    #make a copy of the file on server to download
    content = function(file){
            writeLog(c("Save emf: ",session$token,paste0("<a href='",currentURL,".emf'>",currentURL,"</a> downloaded")))

      makeToC2(tex = input$myText, sess=session$token, ext = "emf",ratio = input$proportion,rankdir=input$direction)
      file.copy(paste0("www/",session$token,".emf"), file)
    }
  )
  
          users_data <- data.frame(START = Sys.time() ,SESS=session$token)

output$twitdiv=renderText(paste0(HTML("<div style=''>
                       <a href='https://twitter.com/share' 
                     class='twitter-share-button' 
                     align='middle' 
                     data-url='theorymaker.info' 
                     data-text='","  @stevepowell99 #evaluation' 
                     data-size='small'>
                       </a>
                       <script>!function(d,s,id){
                         var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';
                         if(!d.getElementById(id)){
                           js=d.createElement(s);
                           js.id=id;
                           js.src=p+'://platform.twitter.com/widgets.js';
                           fjs.parentNode.insertBefore(js,fjs);
                         }
                       }(document, 'script', 'twitter-wjs');
                     </script>
                       <a href='https://twitter.com/stevepowell99' class='twitter-follow-button' data-show-count='false'>Follow @stevepowell99</a>
                       <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>
                       </div>)")))          
          
          
  session$onSessionEnded(function() {
    file.remove(paste0("www/",session$token, ".dot"))
    file.remove(paste0("www/",session$token, ".svg"))
    file.remove(paste0("www/",session$token, ".txt"))
              users_data$END <- Sys.time() 

              users_data$minutes <- ((users_data$END %>% as.numeric)-(users_data$START %>% as.numeric)) %>% (function(x)x/60) %>% as.numeric %>% round(2)
          # Write a file in your working directory
                                writeLog(x = c("logoff:  ",session$token,users_data$minutes,str_sub(textStore,1,122),paste0("<a href='",paste0(".?text=",URLencode(textStore)),"'>link</a>")))

          # write.table(x = users_data, file = "www/00userdata.txt",
          #             append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t")
    # file.remove(paste0("www/",session$token, ".png"))
    # file.remove(paste0("www/",session$token, ".pdf"))
  })  
  

  api_url <- session$registerDataObj( 
    name   = 'api', # an arbitrary but unique name for the data object
    data   = list(), # you can bind some data here, which is the data argument for the
    # filter function below.
    filter = function(data, req) {
      # print(ls(req))  # you can inspect what variables are encapsulated in this req
      # environment
      if (req$REQUEST_METHOD == "GET") {
        # handle GET requests
        query <- parseQueryString(req$QUERY_STRING)
        # say:
        # name <- query$name
        # etc...
      } 
      
      if (req$REQUEST_METHOD == "POST") {
        # handle POST requests here
        # writeLines("asdf",con="000sdf.txt")
        # browser()
        reqInput <- req$rook.input
        
        # read a chuck of size 2^16 bytes, should suffice for our test
        buf <- reqInput$read(2^16) 
        bufr=rawToChar(buf)
        
        perm=str_match(bufr,'xxpermalink\"\r\n\r\n(.*)\r')[1,2]
        graph=str_match(bufr,'xxgraph\"\r\n\r\n(.*)\r')[1,2]
        
        # b=paste0(buf,collapse="-")
        # h <- sapply(seq(1, nchar(b), by=2), function(x) substr(b, x, x+1))
        # r=rawToChar(as.raw(strtoi(h, 16L)))
        # simply dump the HTTP request (input) stream back to client
        s=shiny:::httpResponse(
          200, 'text/plain', buf
        )
        # browser()
        writeLines(graph,con=paste0("www/permalinks/",perm,".txt"))
      }          
    }
  )
  # api_url()
  
  session$sendCustomMessage("api_url", list(url=api_url))
  
    
}

shinyApp(ui = ui, server = server)

