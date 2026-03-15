// Set document properties
#let apa-figure-numbering(n) = {
  let header-counter = counter(heading).get().first()
  let queried-heading = query(selector(heading).before(here())).last().numbering
  if queried-heading == none {
    queried-heading = "A"
  }
  if header-counter == 0 {
    numbering("1.1", n)
  } else {
    numbering(queried-heading + "1", header-counter, n)
  }
}

#let apa-figure(body, numbering: n => apa-figure-numbering(n), ..args, note: none, specific-note: none, probability-note: none) = {
  figure([
    #set par(first-line-indent: 1em, leading: 0em, spacing: 1em)
    #set block(below: 0.5em, breakable: false, above: 0em)
    #set align(center)
    #body

    #set align(left)
    #set text(size: 0.8em)
    #set par(leading: 1em)

    #if note != none [
      #v(0.6em)
      #emph([Note.]) #note
      #parbreak()
    ]

    #if specific-note != none [
      #v(1em)
      #specific-note
      #parbreak()
    ]

    #if probability-note != none [
      #probability-note
    ]

    #v(1em)
  ], numbering: numbering, gap: 1em, ..args)
}

#let template = doc => {
  set math.equation(numbering: "(1.1)")
  show math.equation: set text(font: "TeX Gyre Pagella Math")
  set text(font: "TeX Gyre Pagella", size: 11pt, spacing: 100%)
  set par(justify: true, first-line-indent: (amount: 2.5em, all: true), leading: 1em, spacing: 1em)
  set page(margin: 1in, paper: "a4", numbering: "1", number-align: top + right, columns: 1)

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
  set heading(numbering: "1.1")
  show quote: set pad(x: 3em)

  show outline.entry.where(level: 1): it => {
    set text(weight: "bold")
    it
  }
  show outline.entry: set outline.entry(fill: [])

  //* Figure caption formatting
  show figure: set align(start)
  set figure.caption(position: top)
  show figure.caption: set par(first-line-indent: 0em)
  show figure: set block(width: 100%, above: 2em)
  set figure.caption(separator: [ \ ])
  show figure.caption: it => {
    [
      #strong[#it.supplement #context it.counter.display(it.numbering)]
      #it.separator
      #emph[#it.body]
    ]
  }
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