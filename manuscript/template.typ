// Set document properties

#let template = doc => {

set text(font: "TeX Gyre Pagella", size: 11pt, spacing: 100%)
set par(justify: true, first-line-indent: (amount: 2.5em, all: true), leading: 1.25em, spacing: 1em)
set page(
  margin: 1in, 
  paper: "a4", 
  numbering: "1", 
  number-align: top + right, 
  //header: [#align(left)[#smallcaps([Preregistration for Replicable, Reproducible Science])]]
  )
show heading.where(level: 1): it => {
  set text(size: 11pt)
  set align(center)
  set block(below: 1.25em)
  it
}
show heading.where(level: 2): it => {
  set text(size: 11pt)
  set align(left)
  set block(below: 1.25em)

  it
}

show heading: set block(above: 1.5em, below: 1.5em, spacing: 1em)
show quote: set pad(x: 3em)

show outline.entry.where(level: 1): it => {
  //set text(weight: "bold")
  it
}
show outline.entry: set outline.entry(fill: [])


doc

}

#let pre-cite(..args, prefix: none) = {
  if prefix == none {
    cite(..args)
  } else {
    show "(": [(#prefix]
    cite(..args)
  }
}

#let cite-eg = pre-cite.with(prefix: "eg., ")