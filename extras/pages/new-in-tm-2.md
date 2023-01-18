# Theory Maker 2!

## What is new in Theory Maker 2.3

- You can use triple spaces (as well as triple ///) to force a line break
- You can use double // to force a line break but not switch off auto line-breaking for the rest of the Variable. 
- Custom Features like `sex=male` do not now need a preceding `;`
- A [useful notation](http://theorymaker.info/book/0C0320ImageAndDifferences.html#soft-arithmetic-with-differences) to show factual (and counterfactual) Levels of Variables which helps you distinguish between the names of the Variables and the Difference you hope to make.
- New shortcuts like !hand !uu !ud !stop !end !start !rich !look !memory !Sigma !binary !lohi !circ2 !curvedown !curveup.


## What is new in Theory Maker 2

Theory Maker 2 is a complete rewrite of Theory Maker. 
- It runs about twice as fast (the code is about a quarter the length of the old code) and should be a lot more stable and easier to maintain. 
- Works with other languages: You can now use a whole range of non-English Unicode characters with accents and symbols.
- You can apply themes just by adding a line like `theme=winter`, `theme=summer`, `theme=drama`, `theme=comic` or `theme=night`. More coming!
- The syntax is simpler and you can tweak your diagram with almost the entire range of [Graphviz attributes](http://www.graphviz.org/doc/info/attrs.html) from label positioning to URLs.
- You can set 'engine=circle' to get non-hierarchical, more circular layouts

I am also running it on a more powerful server.

[Here](?help=yes) are the new, somewhat simpler rules for writing Theory Maker diagrams. Or just click on the link "Help and tips" above the text window.

## If you saved a diagram in a previous version of Theory Maker

Some things will work fine, but if your diagram had:
- arrows between groups ("boxes") of variables, 
- "sentences in variable names. Like. this."
- dots to draw arrows, 
- `>>` to draw arrows, 

it may not look right now. Sorry for any inconvenience. You will need to "redraw" your diagram just using spaces to create arrows. Or just write to me at steve@pogol.net and I will fix it for you. 

So if you had this:

    apple
    .banana
    .peach

you can rewrite it as

    banana; peach
     apple

and if you had this:

    apple>>banana

you should rewrite it as

    banana
     apple


If you had this:

    A variable name. A sentence to appear as a note. Another sentence.

you can rewrite it like this:

    A variable name; note=A sentence to appear as a note///Another sentence.///


Look [here](?help=yes) for more help.

