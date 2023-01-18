
# Theory Maker Features (& Known Bugs)


## Main features

- Human-readable syntax.
- Two different ways to "draw" lines between variables, so you can use whatever is most convenient.
- Recognises and makes auto links for standard decimal naming conventions beginning with a number. 
- Boxes for putting variables in groups, for example to mark off different phases, places or participants.
- Format your variables and boxes with colours, fonts etc.
- Easy to change diagram direction (left-to-right, top-to-bottom, etc.) and diagram proportions (wider, taller etc.).
- Easy to add cross links even in mainly hierarchical theories, e.g. from one sub-Outcome to more than one Outcome
- You can create aliases/labels for your variables to save tiresome retyping
- Add notes, "calendar bars" and many other bells and whistles
- Lots of live templates and suggestions for your theory of change, ready for you to edit (don't worry, you aren't editing the originals).
- Each new diagram gets a unique permanent link  which you can bookmark to return to later or send to a friend. Anyone can save changes to this link. 

The rules for typing diagrams in Theory Maker are [here](?help=yes).

## What it doesn't do

### No beautifying

Theory Maker doesn't give you much control over the actual layout (like whether something appears at the top or bottom). This tool tries to keep the layout simple, but that doesn't necessarily mean things are where you expect them to be. Don't bother trying to get the nodes and boxes to move about: If you want to tweak a diagram further, you can download the .svg version of it and manipulate it further in Inkscape, Libreoffice Draw or Illustrator. 

### Remember to save your text

There is no registration or log-in. You just type text. You can type a title for your diagram and click to  save a version and bookmark the link. If you don't, don't forget to copy and paste (somewhere safe) the text you typed, because it won't be there next time you visit the site. 

---

## Comments, suggestions, bug reports: 

[Here](http://stevepowell.blot.im/quicktoc#disqus_thread).

---

# Below this line are technical details of interest mostly to myself... 


## Bugs

Sometimes crashes when call to /www/x.png from Word?

-a
x
-a
x

crashes because newrank=true when two clusters have exactly the same contents. This is just a Graphviz bug, don't know what to do about it. 

# Missing features to add soonest

Maybe complete rewrite in python using `pydot`, which supports nested subgraphs? (`Rgraphviz` doesn't because `graph` doesn't support them either.)

Really important to ensure that 

a;sex=m
a;sex=f

resolve to DIFFERENT Variables. 

but this doesnt work with aliases. -- fixed this.
Have a look at line 627.


Move email list button higher up.

- Arrows to boxes is possible like this:

-y
a
-
 (lhead=cluster_y)b

Easy alias - just take first matching letters. This would work for decimals as well. But it would have to be a separate loop after the main processing.
- Wrap at node level, not only global?
- Auto-save if add e.g. "autosave" on URL
- Conditional formatting and/or fromnow;arrow;width=3 etc
- add link to live .png
- !- as shortcut for //!rwedge


## Possible features (Roadmap)

- Release as R package on github.
- Upload a logframe as Excel 
- Longer text as footnotes. Maybe everything after first sentence. Or just put first sentence in bold
- Calendar spreadsheet output
- Easy sliders for wrap, nodesep, theme....
- Option to download zip of different versions of diagram, with and without different boxes
- Timeline - when a date is given, introduce a bar of dates joined by hidden arrows, using rank=same
- Automatic legend 
    - rank=sink. http://stackoverflow.com/questions/6149834/rank-attribute-is-confusing-to-me
    - or just as text, with autoreplace of colours
- Easy to tweet a link with picture and text (to original version). Use https://dev.twitter.com/cards/types/summary-large-image; also https://dev.twitter.com/cards/overview. Doesn't work at the moment because of timeouts.
- Print permalink on diagram / make it a hyperlink


## Bugs to be fixed

- Shiny crashes on live embed .png in Word - look at console output
- when I update tm, check that <U+25FC> block comes out ok as pdf and png
- Ensure makeToC2 chunk label gets transferred to figure, so we can link to it with \@ref(fig:joineddia)
- delete query string when clicking on gallery etc
- if name contains "=" it gets interpreted as a global att, so either have to list all atts to check if it exists, or insist that graph-level globals are of form diagram;x=y
- text-align left is difficult, becaue even if you manage to get <BR align="left"/> into the label, it indents the first line as if it is a paragraph.

## Posts to write 

[Flexible theories of change](http://www.odi.org/sites/odi.org.uk/files/resource-documents/working_with_loose_toc_-_final.pdf)

[Simon Hearn on Impact](http://betterevaluation.org/blog/what_is_impact?utm_source=feedburner&utm_medium=email&utm_campaign=Feed%3A+EvalCentral+%28Eval+Central%29)

