---
title: "Help and tips for Theory Maker 2"
output:
  html_document:
    self_contained: false
    toc: true
    toc_depth: 5
---

### Getting started!

(To close this window, click anywhere outside it.)

Try typing something in the text window to the left.  
 
Or get some inspiration from the Examples at the bottom of the main page. Click on one you like and try editing it. Don't worry, you aren't editing the original.

Or try copying the examples below in this help window and pasting them into the text window.


Click `Download` to get your diagram as a picture or a PDF.

Remember to save your text somewhere, or type a Title and click `Save a version` to get a bookmark you can return to later.


### Variable names

Theory Maker understands most of the lines you will type as the names of "Variables" - the basic building blocks which you will use to build up your theory. 

    This is a variable name. 

You can type almost any text for the names of your variables. Most languages are fine. Each line becomes a variable in the diagram. Be careful with numbers at the beginning of lines, they may get used for auto decimal numbering, e.g. a line starting 1.1 will get joined to a line starting with 1.


#### Line breaks

Theory Maker breaks lines at around 30 characters, but you can force a line break if you want, by using two slashes// or two spaces, like this: 

    This ridiculously long line // will be broken into two. 

If you use *three* slashes or *three* spaces, a line-break will be inserted and also, Theory Maker's normal automatic line-breaking will be switched off for that line. 

    This line will break   here

or

    This line will break///here

So in particular, if you want to a line not to break at all, do this:

    This line will not be broken even if it is very long///


To change the automatic line-breaking, type a line like this:

    wrap=60


Also, if you use ((double brackets)) to specify the Levels of a Variable, like this:

    Percentage of people voting ((0-100))

the part in double brackets will go on its own line, and automatic line-breaking will be switched off for that line.


### Formatting variables

#### Colours

Type the name of the colour you want after the name of the variable  like this:


    My variable;  color=blue

- You can use all the [HTML colour names](http://www.w3schools.com/colors/colors_names.asp) like `black`, `white`, `skyblue` etc. You can use `transparent` too.

- These colours: `red`, `orange`, `blue`, `green`, `grey`, `purple` are special because if you put a number from 1 to 9 straight after these names, like `orange3`, you get nice shades which all fit together. 

- ... whereas white and black can be made variously transparent in the same way, by adding a number from 1 to 9. So `white1` is a very transparent white, `black7` is a fairly opaque black.

If you want a  colour which fades from left to right, try something like blue2:red3 or white:snow. Please don't even try anything like red:blue. You have been warned.

You can even use HTML colours:

    color=#eeefff

... and you can add two additional digits for transparency:

    color=#eeefff50


#### Fills and borders

`color=` refers to the colour of the whole shape (variable or grouping box) - the main body as well as the edge. If you want to have the main body coloured differently from the edges, you have to put this:

    color=red; fillcolor=green

... and you can adjust the thickness of the "pen" which draws the border with something like `penwidth=4`. 


#### Other formatting

You can tweak the appearance in other ways too like this:

    My variable;  fontcolor=green2;  fontname=Courier;  fontsize=22

Some possibilities:

    shape=oval              # also none, circle, diamond, square
    style=dotted,filled     # also dashed,filled etc
    height=.2;fontsize=8    # height and width can be larger but not 
                            # smaller than required by the text, unless you 
                            # set fixedsize=true
    style=striped,filled;fillcolor=red2:red3:red4   #amazing!                

##### Fontnames    

You can use any fontnames installed on your computer, but they might not look the same on another computer. 

###### Fonts which work everywhere:

AvantGarde-Book AvantGarde-BookOblique AvantGarde-Demi
AvantGarde-DemiOblique Bookman-Demi Bookman-DemiItalic
Bookman-Light Bookman-LightItalic Courier Courier-Bold
Courier-BoldOblique Courier-Oblique Helvetica
Helvetica-Bold Helvetica-BoldOblique Helvetica-Narrow
Helvetica-Narrow-Bold Helvetica-Narrow-BoldOblique
Helvetica-Narrow-Oblique Helvetica-Oblique NewCenturySchlbk-Bold
NewCenturySchlbk-BoldItalic NewCenturySchlbk-Italic
NewCenturySchlbk-Roman Palatino-Bold Palatino-BoldItalic
Palatino-Italic Palatino-Roman Symbol Times-Bold Times-BoldItalic
Times-Italic Times-Roman ZapfChancery-MediumItalic ZapfDingbats

###### Other fonts which work nearly everywhere include these:

- Arial
- Verdana
- Impact
- Comic sans MS


### Arrows 

#### Making arrows by indenting lines with spaces

Indent lines to show what contributes to what.

    A, gets the arrowhead
     B, indented, so contributes to A
      C, contributes to B
      D, also contributes to B


#### Listing several variables at once with "; "

You can make links between lists of variables. 

This links X to A and B:

    A; B
     X

This links X and Y to A:

    A
     X; Y

And this links X, Y and Z to A and B:

    A; B
     X; Y; Z

You can also format several variables at once with the same trick:

    A; B; C; colour=orange

#### Making arrows with decimal numbering    

    1 Outcome
    1.a gets arrow to 1
    1.b also gets an arrow to 1

To use decimal numbering, Theory Maker looks at the first "word" in the line. If one line has, say, `G.12.a` at the start and another line has `G.12`, Theory Maker will draw a line from `G.12.a` to `G.12`. If you also have a line beginning `G`, Theory Maker will also draw a line to it from `G.12`.  

#### Repeats

You can repeat a variable. This pattern makes an arrow from B to A and back to B:

    Outcome A
     Outcome B
      Outcome A

If you want to draw a large network, you can try to do it all at once,  like this:

    Outcome A
     Outcome B
      Outcome C
       Sub-outcome C1
       Sub-outcome C2

... or you can break it up into pieces, like this:

    Outcome A
     Outcome B

    Outcome B
     Outcome C

    Outcome C
     Sub-outcome C1
     Sub-outcome C2

Theory Maker will treat this exactly the same as the previous example.


### Aliases

If you have long variable names but want to repeat the variable like in the examples above, you can use labels like this to save you typing:

    A
     B
      C

    A;  label= Some variable with a very very long name.


    
### Grouping your variables

Use one or more hyphens `-` to group your variables, i.e. to put a grouping box around them. Start with just one `-` for the first group or groups, then put `--` for groups inside them, and so on.

(If there are no variables inside a group, the grouping box does not appear).

    -This is group A
    A variable which is just in group A
    --This is B, inside A
    A variable inside both groups
    Another variable inside both groups

You can "stop" a group by using a row with just hyphens.

    -Box A
    This variable is inside group A
    -
    This variable is outside Box A


#### Formatting groups: almost the same as formatting variables


    -My group;  fontcolor=green2;  fontname=Courier;  fontsize=22
    Some variable inside the box

Groups can have coloured insides too. `color` affects the insides as well as the border;  if you add `fillcolor=` as well, you can colour the insides differently:

    -My group;  color=green2;  fillcolor=red2;  penwidth=4
    Some variable inside the box

And groups can have styles:

    -My group;  style=dashed
    Some variable inside the box

    `style` can = `rounded`, `dashed`, `dotted`, `filled` and combinations of these, as well as `invisible`.
    
    labelloc=t          # `t` to put label at the top, `b` at the bottom
    
    labeljust=l         # `l` to put label at the left, `r` at the right


### Formatting arrows: put the label and the styling inside brackets.

If you want to prettify your arrows, put the information inside brackets just in front of the variable name where the arrow *begins*, and *after* the space or spaces: 

    Variable B
     (A label on the arrow from A to B) Variable A

You can style your arrows too:


    Variable B
     (My arrow;  width=8;  color=blue;  style=dotted)Variable A

You can also try these:

    direction=both  (back, forwards, both or none)

    font=Georgia

    fontsize=7

    taillabel= my tail label

    headlabel= my head label

    constraint=false              #very useful; tells Theory Maker to ignore  
                                  this arrow when calculating the layout

Telling the arrow what side of the variable to attach itself:

    attach=n (i.e. north;  can also be s, e, w, nw, ne, se, sw) 

... and the same for the tail of the arrow

    tailattach=n 


#### Global formatting for variables, groups and arrows 

Put a line like this at the end of the diagram to change all the variables at once:

    variable;  colour=red;  font=Courier; shape=diamond

... and the same for arrows and groups    
    
    group;  colour=white;  fillcolour=green; labelloc=bottom; labeljust= right
    
    arrow;  colour=blue; arrowsize=2

Any formatting you can apply to an individual variable, group or arrow can also be applied globally with lines like these.


### Other tweaks to the whole diagram

#### Standard graphviz tweaks

Lines like these below only apply to the diagram itself, like the background colour etc. You can precede them with `diagram;` like this:

    diagram;  colour=white

or you can do without, as in these examples:

    colour=white    # whole diagram background colour. Alias for `bgcolor`.

    proportion=1     # tall and thin or short and fat diagrams. `.1` is very short, `2` is very tall.    Alias for `ratio`.   

    direction=TB     # top-down. Also, BT, LR or RL

    label=My title   # adds a title to the whole diagram

    nodesep=.1       # moves variables closer or further apart

    ranksep=2        # try it and see!

    orientation=L    # flips the diagram 90 degrees

    splines=false    # keeps the arrows straight. Also try: `polyline`

    layout=neato     # if you don't want a hierarchical diagram, try this!  
                       Also: circo dot fdp neato nop nop1 nop2 osage patchwork sfdp twopi

#### Hardcore

If you really want, you can tweak any part of your diagram with almost the entire range of [Graphviz attributes](http://www.graphviz.org/doc/info/attrs.html) from label positioning to URLs. 


#### Special Theory Maker tweaks


    wrap=20          # changes the line wrapping in labels
 
    rowfontsize=12   # change the font size of the info rows
 
##### Themes

To apply a theme to the whole diagram, try:

    theme=winter     

You can also try theme=night, summer, comic, and drama. More themes are coming! You can still continue to add formatting as usual, e.g. adding `fontsize=10` will change the font and leave the rest of the theme the same.



##### Getting things in the right order



    rank=same;  Variable A;  Variable B


This line lines up the listed variables  (Variable A and Variable B) and also puts them in the order you specify. So if you want your variables X, Y and Z for some reason in the order X, Z, Y, write

    rank=same; X; Z, Y

(So if you are a Graphviz expert, note that rank=same does a bit more in Theory Maker than it does in Graphviz.) 

### Improving readability

#### Proportion



Drag the `Proportion` slider to the left or right. If you want to make the changes permanent, e.g. because you want to save a permanent link to the diagram, type a line like:



    proportion=.5 





#### Direction



Use the `Direction` button to choose top-to-bottom, left-to-right etc. Or write a line like:



    direction=TB



#### Variable separation



Moves variables further apart: `nodesep=2`. Closer together: `nodesep=.01`.


### Features

Often your variables and groups of variables have special information like "Indicator" or "Timepoint" which need to be specified. You can easily add them like this:

    -Group A;  timepoint==endline
    Teacher satisfaction,  instrument==questionnaire,  target == score above 5
    Student satisfaction,  instrument==interview

Theory Maker will add corresponding rows and coordinate the colours automatically. Try it and see!

If you just want notes, try this:

    Teacher satisfaction,  note==should be improved

If you happen to have a variable which has an `==` in its name, you have to be careful that Theory Maker doesn't think you are using the `==` to specify formatting or info rows as mentioned above. In this case, make sure that you specify the variable name before anything else in the line, like this:

    Performance = improved



### Shortcuts 

There are a few shortcuts and aliases to save your typing fingers and to  enable compatibility with earlier versions of Theory Maker.


    !5 ! 4 !3 !2 !1

... makes these blocks:  ▇ ▆ ▅ ▃ ▂. 

Also, `!321` is the same as  `!3!2!1` (but quicker to type).


    A variable [blue3]


This is the same as:

    A variable;  colour=blue3

...

    a::A variable with an alias 

This is the same as:

    a; label=A variable with an alias

This ...

    !do

adds an intervention arrow. !control works similarly.

This ...

    A
     (.)B
     (.)C

makes dotted lines


This ...

    A
     (-)B
     (-)C

makes dashed lines


This ...

    A !def
     B
     C


Or this ...

    A; Definition=some definition
     B
     C

makes dashed lines, indicating that A is *defined* by B and C.


This ...

    A !incomplete
     B
     C

Adds a half-circle to say A is *not completely* defined or influenced by B and C.