#TODO
# themes
#  line by line defaults 
# speed up app.r   
# maybe treat first element in x=y;colour=red specially, i.e. always as a label.

#  
### little bug
# makeToC2("a::x=y;color=red
# + ",ext="pdf")
# edge attributes not working; edge inline styles not working; crash if spaces and nothing above

library(htmltools)
library(stringr)
library(digest)
library(RColorBrewer)
library(stringi)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(purrr)



Sys.setlocale("LC_ALL", 'C')


xc = function(str)
  stringr::str_split(str, ' ')[[1]]


niceattify=function(x){
str_replace_all(x,c(
  "colour"="color",
  "width"="penwidth",
  "direction"="rankdir"
  ))  
}

niceattify2=function(x){
  x=str_replace_all(x,c(
    "grey"="gray" 
    ,"white([0-9])"="#ffffff\\10"
    ,"black([0-9])"="#000000\\10"
    ,"(redblue)([1-9])"="/\\rdbu9/\\2"
    ,"(purpleblue)([1-9])"="/\\pubu9/\\2"
    ,"(orangered)([1-9])"="/\\orrd9/\\2"
    ,"(greenblue)([1-9])"="/\\gnbu9/\\2"
    
    
  ))  
  str_replace_all(x,paste0("([1-9]*)(",collist,")([1-9])"),"/\\2s9/\\3")
}




listify=function(lis){
  # browser()
  main=lis %>%
    str_split(";") %>% .[[1]]
  
  #what if all the parts include =. then assume the first one is a label
  
  
  if(str_detect(main,"=") %>% all){
    elements=main[1]
    attrs=main[-1]
  } else {
  elements=main[!grepl("=",main)]
  attrs=   main[grepl("=",main)]
  }
  bits=str_split(attrs,"=") %>% data.frame(stringsAsFactors = F)
  attrs=niceattify2(bits[2,])
  attrs=attrs %>% str_trim %>% as.list 
  names(attrs)=(bits[1,] %>% str_trim %>% niceattify )
  return(list(elements,attrs))
  

}#makes a list of named attrs


collist="blue|orange|red|grey|green|purple"
collist2="transparent|aliceblue|antiquewhite|aqua|aquamarine|azure|beige|bisque|black|blanchedalmond|blue|blueviolet|brown|burlywood|cadetblue|chartreuse|chocolate|coral|cornflowerblue|cornsilk|crimson|cyan|darkblue|darkcyan|darkgoldenrod|darkgray|darkgrey|darkgreen|darkkhaki|darkmagenta|darkolivegreen|darkorange|darkorchid|darkred|darksalmon|darkseagreen|darkslateblue|darkslategray|darkslategrey|darkturquoise|darkviolet|deeppink|deepskyblue|dimgray|dimgrey|dodgerblue|firebrick|floralwhite|forestgreen|fuchsia|gainsboro|ghostwhite|gold|goldenrod|gray|grey|green|greenyellow|honeydew|hotpink|indianred |indigo |ivory|khaki|lavender|lavenderblush|lawngreen|lemonchiffon|lightblue|lightcoral|lightcyan|lightgoldenrodyellow|lightgray|lightgrey|lightgreen|lightpink|lightsalmon|lightseagreen|lightskyblue|lightslategray|lightslategrey|lightsteelblue|lightyellow|lime|limegreen|linen|magenta|maroon|mediumaquamarine|mediumblue|mediumorchid|mediumpurple|mediumseagreen|mediumslateblue|mediumspringgreen|mediumturquoise|mediumvioletred|midnightblue|mintcream|mistyrose|moccasin|navajowhite|navy|oldlace|olive|olivedrab|orange|orangered|orchid|palegoldenrod|palegreen|paleturquoise|paboxlevioletred|papayawhip|peachpuff|peru|pink|plum|powderblue|purple|rebeccapurple|red|rosybrown|royalblue|saddlebrown|salmon|sandybrown|seagreen|seashell|sienna|silver|skyblue|slateblue|slategray|slategrey|snow|springgreen|steelblue|tan|teal|thistle|tomato|turquoise|violet|wheat|white|whitesmoke|yellow|yellowgreen"

# just takes a toc text specification and produces a markdown output like ![kill](kill.svg); important byproduct is the dot and graphics file
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

  
  if (!is.null(tex)){  {




    plaintex=tex
    
    ## strip off blogtext
    tex=gsub("\n====.*","",tex)
    
    

    

    ### cleaning---------

    tex=gsub("\"","'",tex)

    tex=gsub("\n( )+\n|$","\n",tex) #strip lines with just spaces
    tex=str_replace_all(tex,"\n+","\n")

    ## aliases
    
    tex=str_replace_all(tex,"\\:\\:",";label=")
    
    ## globals
    
    # browser()
    tex=str_replace_all(tex,"\n *(proportion|ratio|layout|direction|rankdir|theme) *=","\ngraph;\\1=")
    # tex=str_replace_all(tex,"\n *direction *=","\ngraph;direction=")
    
    
    
    ### unicode replacements
    
    tex=str_replace_all(tex,c(
      
     "!frown"="\u2639"
    ,'!factual '= '\u2714'
    ,'!tick'= '\u2714'
    ,'!cross'= '\u2718'
    ,'!heart'= '\u2665'
    ,'!noyes'= '\u25E8'
    ,'!lohi'= '\u25EA'
    ,'!spark'= '\u26A1'
    ,'!n0'= '\u24ea;'
    ,'!n1'= '\u2460;'
    ,'!n2'= '\u2461;'
    ,'!n3'= '\u2462;'
    ,'!n4'= '\u2463;'
    ,'!n5'= '\u2464;'
    ,'!n6'= '\u2465;'
    ,'!n7'= '\u2466;'
    ,'!n8'= '\u2467;'
    ,'!n9'= '\u2468;'
    ,'!b0'= '\u24ff;'
    ,'!b1'= '\u2776;'
    ,'!b2'= '\u2777;'
    ,'!b3'= '\u2778;'
    ,'!b4'= '\u2779;'
    ,'!b5'= '\u277a;'
    ,'!b6'= '\u277b;'
    ,'!b7'= '\u277c;'
    ,'!b8'= '\u277d;'
    ,'!b9'= '\u277e;'
      ))

    
    ## split string
    
    texlist=str_split(tex,pattern="\n")[[1]]
    texlist=texlist[sapply(texlist,str_trim)!=""]



    
    
dotchar=" "
graph=create_graph()

graph=add_global_graph_attrs(graph,"layout","dot","graph")
graph=add_global_graph_attrs(graph,"rankdir","LR","graph")
# graph=add_global_graph_attrs(graph,"shape","rectangle","node")
# graph=add_global_graph_attrs(graph,"height","0","node")
# graph=add_global_graph_attrs(graph,"fixedsize","false","node")

theme_node=c(
  "shape"="rectangle",
  "height"="0",
  "fixedsize"="false"
)

theme_edge=c(
  "color"="black",
  "style"="solid"
)

##  - loop through whole list----
targs=0

    for(i in seq_along(texlist)){
      l=texlist[i]

      t=str_match(l,paste0(" *?(graph|diagram|node|edge|subgraph|variable|arrow|group) *; *(.*)"))
      
      if(!is.na(t[1,2])){
          # browser()
        globs=listify(paste0("dummy;",t[1,3]))
        globs=globs[[2]]
        
        
        for(r in seq_along(globs)){
      graph=add_global_graph_attrs(graph,names(globs)[r],globs[r],t[1,2])
      
      if(t[1,2]=="node"){
        comb=list(globs,theme_node) %>% unlist
        theme_node=comb[unique(names(comb))]
      }
      
      if(t[1,2]=="edge"){
        comb=list(globs,theme_edge) %>% unlist
        theme_edge=comb[unique(names(comb))]
      }
      
      # browser()
      }
      } else {
      
      t=str_match(l,paste0("(-*)( *)(.*?)$"))
      
      
      
      isgroup=t[1,2]!=""
      
      # strip spaces if necessary
      spaces=nchar(t[1,3])
      if(i>1)spaces=min(max(get_node_df(graph)$spaces)+1,spaces)
      
      
      rest=(t[1,4])
      
      rest2=str_match(rest,"(\\((.*)\\))?(.*)")
      
      # browser()
      rest=rest2[1,4]
      edgeattrs=rest2[1,3]
      
      if(!isgroup){
        
      ##### add nodes  
      
        lr=listify(rest)
        allatrs=c(lr[[2]],spaces=spaces,line=i,lastTargetForLevel=spaces,theme_node)
        allatrs=allatrs[unique(names(allatrs))] #because there might be doubled attributes
        
        #in case there are any aliases:
        names(allatrs)=names(allatrs) %>% str_replace_all("label","otherlabel")
        # browser()
        labs=lr[[1]]
        llabs=length(labs)
        
        # browser()
        
        
      graph=add_n_nodes(graph, llabs, label = labs, node_aes = allatrs, node_data = NULL)
        
      #### set target for edges to come to this node
      
        if(i>1){
      which=get_node_attrs(graph,"line")!=i
      levs=get_node_attrs(graph,"lastTargetForLevel")
      tmp=levs==(spaces)
      levs[tmp&which]=99
      graph=set_node_attrs(graph,"lastTargetForLevel",values = levs)
        }
      
      
      ## add edges
      
      
      if(spaces>0 & i>1){
      last=nrow(get_node_info(graph))
        fromvec=(last-(llabs-1)):last
        # browser()
        # targs=(get_node_attrs(graph,"spaces")  <spaces) %>% which %>% max
        targs=(1:last)[(get_node_attrs(graph,"lastTargetForLevel")  ==spaces-1)] #%>% which %>% max
        
        combs=expand.grid(fromvec,targs)
        
        
        edf=create_edge_df(
          from = combs[,1],
          to = combs[,2]
          # ,
          # style="dashed"
          )
        
      graph=add_edge_df(graph, edf)
      
      ## Add any edge attrs 
      
        browser()
      if(!is.na(edgeattrs)){
        e3=listify(edgeattrs)
        
        if(length(e3[[2]])==0){
          elab=NA
          edgeattrs2=e3[[1]]
          
        } else {
        
        elab=(e3)[[1]][1] #the first label
        edgeattrs2=e3[[2]]
        }
          df=data.frame(from=combs[,1],to=combs[,2],theme_edge,stringsAsFactors = F)
          if(length(edgeattrs2)>0)df=cbind(df,edgeattrs2,stringsAsFactors=F)
          if(!is.na(elab)) df=cbind(df,label=elab,stringsAsFactors=F)
          graph=graph %>% 
            join_edge_attrs(df)
          
      }
      
      
      
      }
      }
      
      

      
      # t=str_match(l,paste0("(-*)((graph|diagram|node|edge|subgraph|variable|arrow|group);)*(",dotchar,"*)(\\(\\(.*?\\)\\))?(=*\\((.*?)\\))*(.*?)( *)$"))
      

    }
    }


## consolidate - transfer edges - of duplicated labels. Doesn't transfer any node attrs though.
deleteVec=NULL

for(e in 1:nrow(get_node_df(graph))){
  ed=get_edge_df(graph)#have to do this in each loop because arrows are constantly repositioned
  en=get_node_df(graph)
  el=get_node_attrs(graph,"label")
  elm=((el==el[e]) %>% which %>% min)
  if(elm<e){
    # browser()
    #so there is an earlier node with same label, need to transfer edges
    # efrom=ed$from
    # eto  =ed$to
    
    # make the recode
    recoder=paste0(e," -> ",elm)
    
    graph <-
      graph %>%
      recode_edge_attrs(
        edge_attr_from = from,
        recoder
      )
    
    graph <-
      graph %>%
      recode_edge_attrs(
        edge_attr_from = to,
        recoder
      )
    
    deleteVec=c(deleteVec,e)
    # browser()
    oldnodes=graph$nodes_df[e,]
    graph$nodes_df[elm,]=ifelse(is.na(graph$nodes_df[elm,]),oldnodes,graph$nodes_df[elm,])
  }
  
}
# browser()

for(e in deleteVec){

graph <-
  graph %>%
  delete_node(
    e
  )
}
      

  }}
  # browser()
  
  # in case there are any aliases
  if("otherlabel" %in% (graph$nodes_df %>% names))
    graph$nodes_df$label=ifelse(is.na(graph$nodes_df$otherlabel),graph$nodes_df$label,graph$nodes_df$otherlabel)
  # 
  # graph <-
  #   graph %>%
  #   recode_node_attrs(
  #     node_attr_from = shape,
  #     "square -> red",
  #     otherwise = "green",
  #     node_attr_to = color)
  
  writeLines(generate_dot(graph),"dot.dot")
    export_graph(
graph,      file_name = paste0(path,".",ext))#,title = "asd2f")
  
# if(render)render_graph(graph)

   if(domark) cat(paste0("<div class=dualoutput><div class=tinylink>",
      if(dolink & nchar(plaintex)<2000)tags$a(class="editlink",target="_new",href=paste0("http://theorymaker.info?text=",URLencode(plaintex,reserved = T))," Clone"),
      "\n\n</div>",
    "\n",
    "![",caption,"](", path, ".",ext, ")",  

    paste0("\n\n</div>")
    , sep = "")) 
      
      #else cat("![",caption,"](", path, ".",ext, ")")#else stop("not isbook")

  # system(paste0('dot "','dot','.dot" -T','png','  -o "','dot','.','png','"',sep=''))
}
