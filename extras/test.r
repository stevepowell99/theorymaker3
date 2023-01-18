#TODO
# themes
#  line by line defaults 
# speed up app.r   
# second use of edge defaults doest work. can't set themes e.g. nodes at bottom of text.
# maybe treat first element in x=y;colour=red specially, i.e. always as a label.

#  
### little bug
# makeToC2("a::x=y;color=red
# + ",ext="pdf")
#  edge inline styles not working; crash if spaces and nothing above

library(htmltools)
library(stringr)
library(digest)
library(RColorBrewer)
library(stringi)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(purrr)

#→►֎

tfun=function(x){
  str_replace_all(x,c(
    "y"="▣"
  ))  
}


# Sys.setlocale("LC_ALL", 'C')

makeToC2 = function(tex = NULL,
                    # isarticle= getwd()=="C:/Users/User/Google Drive/Projects/articles" | getwd()=="C:/Users/User/Google Drive/Projects/ukes-pearl-slides",
                    # ispres=getwd()=="C:/Users/User/Google Drive/Projects/Theorymaker-pres"|getwd()=="C:/Users/User/Google Drive/Projects/ukes-pearl-slides" ,
                    # isbook=getwd()=="C:/Users/User/Google Drive/Projects/book-bookdown" | ispres | isarticle,
                    # simple=!isbook | ispres |getwd()!="C:/Users/User/Google Drive/Projects/ukes-pearl-slides",
                    render=F,
                    
                    isbasictheme=grepl("articles",getwd()),
                    
                    domark=!grepl("Shiny",getwd()),
                    dolink=grepl("pres",getwd()),
                    
                    ext =ifelse(grepl("articles|ukes|pres",getwd()),"svg","png"),
                    
                    file = NULL,
                    sess=digest(tex),
                    path = paste0("www/",sess),

                    boxlevels= c(9,0),
                    displayIncludes = T,
                    title = "",
                    caption = "",
                    ranksep = .7,
                    ordering ="none",
                    ranks="",
                    boxes = NULL,
                    nodesep = .2,
                    rowfontsize = "10",
                    rankdir = "LR",
                    ratio = "",
                    rev=F,
                    headport = '',
                    wrap = 30,
                    # small = F,
                    # addID = F,
                    # show_decimal = T,
                    ...) {

  


    
    
    ndf <-
      create_node_df(
        n = 1,
        label = tfun(tex))
    graph <-
      create_graph(nodes_df = ndf)
    
    export_graph(graph,
                 file_name = "kill.png")
}
