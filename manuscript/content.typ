#import "template.typ": *

#import "@preview/wordometer:0.1.4": word-count, total-words
#show: word-count

/*
Set up title page
*/
#align(center)[
  #v(15em)
  //#line(length: 100%, stroke: 0.5pt)
  //#v(1em)
  *Title*
  //#v(1em)
  //#line(length: 100%, stroke: 0.5pt)
  #v(3em)
  Jan Luca Schnatz
  #v(0em)
  Student ID: 7516898
  #v(0em)
  Department of Psychology, Goethe University Frankfurt
  #v(0em)
  #emph("PsyMSc3B/C"): Bayesian Computational Modeling of Learning: A Hands-On Introduction
  #v(0em)
   Prof. Dr. Garvin Brod
  #v(0em)
  March XX, 2026
  #pagebreak()
]

= Translational Abstract

#v(1em)
#par(first-line-indent: 0em)[
  _Keywords_: 
  _Word count_: #total-words words
]


#pagebreak()

#pagebreak()

#bibliography("references.bib", style: "apa")

#pagebreak()

= Statutory Declaration

I herewith declare that I have composed the present manuscript myself and without use of any other than the cited sources and aids. Sentences or parts of sentences quoted literally are marked as such; other references with regard to the statement and scope are indicated by full details of the publications concerned. The manuscript in the same or similar form has not been submitted to any examination body and has not been published. This manuscript was not yet, even in part, used in another examination or as a course performance.


#set par(justify: true, first-line-indent: (amount: 0em, all: true), leading: 1.25em, spacing: 1em)

#v(4em)
#line(length: 40%, stroke: 0.5pt)
Jan Luca Schnatz
#v(0em)
Darmstadt, August 31st, 2025