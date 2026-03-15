#import "template.typ": *
#show: template
#import "@preview/wordometer:0.1.4": total-words, word-count
#show: word-count
#import "@preview/pillar:0.3.3"
#import "@preview/fletcher:0.5.8" as fletcher: diagram, edge, node
#show math.equation.where(block: true): it => {
  show regex("[∫-∳]|[⨋-⨜]"): math.stretch.with(size: 2.4em)
  set block(spacing: 1.5em)
  it
}
#show table.cell: set par(leading: 0.5em)
#show table.cell: set align(horizon)
#show ref: it => {
  if it.element == none {
    return it
  }
  let elem = it.element
  if elem.func() != heading {
    return it
  }
  if elem.numbering != none {
    return it
  }
  underline(link(it.target, [#elem.body]))
}

/*
Set up title page
*/

#show link: it => underline(text(fill: navy)[#it])
#let title = [*Reverse-Engineering Inductive Bias: \ A Hierarchical Bayesian Account of Overhypotheses*]

#align(
  center,
)[
  #v(15em)
  #title
  #v(2em)
  #text(size: 11pt, [Jan Luca Schnatz])
  #v(0em)
  Department of Psychology,
  Goethe University Frankfurt
  #v(0em)
  #emph("PsyMSc3B/C") Bayesian Computational Modeling of Learning
  #v(0em)
  Prof. Dr. Garvin Brod
  #v(0em)
  March 15, 2026

  #place(
    bottom + left,
    [The corresponding presentation for this article can be accessed at #link("https://jlschnatz.github.io/hbm/"). Open materials, including the code to reproduce the results of the paper and presentation are available at #link("https://github.com/jlschnatz/hbm").],
  )
  #pagebreak()
]

#heading([Abstract], outlined: false, numbering: none)

Humans possess a remarkable ability to form rich generalizations from sparse data. This efficiency is driven by inductive biases that restrict the hypothesis space during the learning process. This article describes how hierarchical Bayesian models provide a computational, reverse-engineering account of this cognitive capacity. By operating across multiple levels of abstraction, these models formalize how learners exploit the hierarchical structure of their environment to form overhypotheses, which are higher-level expectations that systematically guide subsequent, lower-level inferences. To build intuition, a hierarchical Beta-Binomial model is first used to demonstrate how population-level regularities, such as homogeneous marble colours within bags, enable precise predictions from sparse observations in a simplified marble colour-guessing game. This framework is then extended to a multidimensional Dirichlet-Multinomial model to formalize the acquisition of the shape bias in infant category learning. By closely aligning with empirical data from toddlers in a noun generalization study, the model illustrates how overhypotheses can be acquired through experience. In this way, hierarchical Bayesian models offer a nuanced perspective between nativist and empiricist views of cognition, showing how learners can generalize efficiently from experience when constrained by plausibly innate priors. Importantly, while these models primarily operate at the computational level of analysis, bridging the gap to algorithmic and implementation levels remains important for a comprehensive understanding of human cognition.
#v(1em)
#par(first-line-indent: 0em)[
  _Keywords_: Hierarchical Bayesian Model, Inductive Bias, Overhypothesis, Category Learning
]

#pagebreak()
#outline()
#pagebreak()

#heading(title, outlined: false, numbering: none)

Humans are remarkably efficient learners. Even when exposed to very little data, we can form rich expectations about our environment and make accurate predictions. For example, upon hearing a novel word applied to a single unfamiliar object, a toddler effortlessly learns to extend that new label to other objects of the same kind. This capability is even more remarkable when considering that there are nearly infinitely many possibilities regarding which aspects of the object define the new entity, such as its color, shape, material, or even its spatial location. In the most extreme case, this may even manifest as _one-shot learning_, the ability to make precise inferences about future data after observing only a single instance of a new concept.

To navigate this vast space of possibilities, human cognition fundamentally relies on the notion of _abstraction_ #cite(<griffiths2024>) #cite(<kemp2008>). Knowledge is not represented as a flat collection of isolated facts, but is instead organized across different levels of a hierarchy. This concept of hierarchical structure is ubiquitous throughout human cognition. One classic example is the syntactical organization of language, where continuous, low-level speech signals are organized into phonemes, which combine to form words, and are ultimately structured into phrases and sentences governed by syntactic grammatical rules #cite(<griffiths2024>).

Within this hierarchical structure, learning operates through simultaneous inferences across multiple levels, continuously integrating bottom-up data processing with top-down expectations #cite(<kemp2008>). What makes this system so efficient is the _inductive bias_ it affords. For any meaningful generalization to occur from sparse data, learners must rely on this abstract background knowledge to restrict the hypothesis space they consider #cite(<griffiths2024>). In concrete terms, this constraining mechanism is realized through an _overhypothesis_ – a piece of abstract knowledge or assumptions that define and structure the available hypothesis space at a lower, less abstract level #cite(<kemp2006>) #cite(<kemp2007>).

While the theoretical necessity of overhypotheses is clear, understanding their underlying mechanism requires a formal framework. For a reverse-engineering account of cognition, the key challenge is to explain the nature, function, and origin of this abstract knowledge in precise computational terms #cite(<griffiths2024>). This approach seeks to understand the mind by first identifying the core computational problems it faces, such as the problem of induction, and then demonstrating how observed human behavior naturally arises from solving those problems mathematically. Drawing on the theoretical framework by #cite(<griffiths2024>, form: "prose"), this article describes how hierarchical Bayesian models formalize the concepts of abstraction, inductive bias, and overhypotheses. These models provide a reverse-engineering account of human cognition by demonstrating how abstract prior knowledge is acquired through experience, and how the resulting overhypotheses efficiently guide future learning. To build a mathematical intution, the article first lays out the simplest possible example, a hierarchical _Beta-Binomial model_ illustrating how overhypotheses about color distribution of bags of marbles are formed during a marble guessing game. Next, this framework is generalized to a reald-world empirical application, demonstrating how a multidimensional _Dirichlet-Multinomial model_ explains the acquisition of the shape bias in early human category learning.

= The Hierarchical Beta-Binomial Model <sec-hbbm>

== Building an Intuition for Inductive Bias <sec-intuition>

Imagine a fictitious scenario in which two children play a color guessing game with marbles from different bags. One child draws marbles sequentially from each bag, while the other tries to predict their color (either black or white). As shown in @fig-marble-composition, 100 marbles are drawn from each of seven different bags, revealing their respective color distributions. The task of the child is now to predict the color of the second marble drawn from the eighth bag after the first one has been revealed, given the observed distributions of the previous seven bags.

#apa-figure(
  image("../figures/dotplot_mu0.5_phi0.8.svg", width: 100%),
  caption: [Color Distribution of Bags of Marbles in the Example],
  note: [Each bag in this example contains 100 marbles that are either black or white. The proportion of black marbles varies between bags. The task is to predict the color of the next marble in bag eight after observing the first marble and all the other seven bags. Figure generated using the _ggplot2_ #cite(<wickham2026>) R-package.],
) <fig-marble-composition>

At first glance, a single black marble contains very little information about the color of future marbles. And yet, the guessing child feels surprisingly confident. Based on the patterns observed in the previous bags and the first draw from the eighth bag, it intuitively seems far more likely than chance that the second marble will also be black. What gives rise to this remarkably accurate intuition?

Initially, a naive explanation might suggest that the child can make such accurate predictions simply because they were informed by prior data. However, merely observing the seven bags of marbles does not, on its own, allow for precise inferences. To exemplify, imagine that all marbles were poured into a single giant container and all but the last marble were drawn. Since the overall color distribution across bags is roughly equal, the container would be filled half with white and half with black marbles and thus the child’s prediction would be no better than chance. The key difference between these two examples lies in the _hierarchical structure_ of the data. As introduced earlier, human learning operates by making inferences across multiple levels of abstraction. Here, marbles are grouped within bags, and each bag has its own color distribution. By recognizing that each bag has a high probability of containing mostly one color, the child is able to leverage this higher-level information to make more accurate predictions about individual marbles at the lower level.

While this conceptual explanation accounts for the child's intuition, building a reverse-engineering account requires a mathematical formalization of the problem. The following section translates the marble game into a probabilistic model to show how the observed behavior naturally arises from solving the underlying problem of induction.

== Formalization of the Marble Game <sec-formalization-hbbm>

The formalization begins at the lowest level and then progresses up the hierarchy, ultimately constructing a probabilistic model that reflects both marble-level outcomes and bag-level structure #pre-cite(<griffiths2024>, prefix: [see ]). The lowest-level of the model starts with the data itself (i.e., marbles drawn from one or more bags). For each bag $i$, $y_i$ black marbles are independently drawn from $n_i$ total marbles, which can be represented as the set $d_i$.

$
  d_i: lr({y_i, n_i}, size: #125%)
$ <eq-data>

To formalize the data-generating process, probability distributions are used to describe how these observations arise. In the literal marble example, marbles are drawn without replacement, which would imply a changing probability for a black across draws, which would formally correspond to a Hypergeometric distribution. However, the hierarchical Bayesian model adopts a more abstract representation. Each bag is assumed to be characterized by an underlying probability of "producing" a black marble, and draws are treated as conditionally independent given this parameter. This corresponds either to sampling with replacement or to an infinitely large bag. Under this representation, the number of black marbles observed in $n_i$ draws follows a _Binomial_ distribution.

$
  y_i | n_i tilde.op "Binom"(theta_i)
$ <eq-binom>

The parameter $theta_i$ represents the probability of drawing a black marble from bag $i$. Importantly, the subscript $i$ implies that this probability may vary across bags. At this level in the hierarchy, a Bayesian model would estimate and quantify uncertainty of each $theta_i$ separately. Consequently, information about the color distribution in one bag would not inform inference for another bag. In the example of the two children, predictions for the eighth bag would therefore rely exclusively on the single observed marble from that bag. Such a model would ignore the hierarchical structure present in the data and would lead to highly uncertain predictions.

To account for this structure, it is assumed that the individual parameters $theta_i$ are themselves drawn from a broader population distribution #cite(<griffiths2024>). This higher-level distribution represents the general characteristics of bags in terms of their color composition and allows information to be shared across bags. Because this distribution is defined over a probability parameter $theta_i in lr([0, space.sixth 1], size: #125%)$, a natural choice is the _Beta_ distribution. The Beta family is defined on the unit interval and is sufficiently flexible to represent a wide range of beliefs about how likely bags are to contain black marbles (see @fig-beta). Formally, this second level of the hierarchy can then be written as

$
  theta_i tilde.op "Beta"(alpha, space.sixth beta)
$ <eq-beta>

The Beta distribution in its standard parameterization is defined by the two real-positive valued ($RR^+$) shape parameters $alpha$ and $beta$. These parameters can be interpreted as prior pseudo-counts. More specifically, before observing the current data, the model behaves as if it had previously observed $alpha - 1$ black marbles and $beta - 1$ white marbles. In this way, they encode both prior expectations and the strength of prior belief. When $alpha = beta = 1$ the Beta distribution reduces to the Uniform distribution, corresponding to complete uncertainty regarding $theta_i$ (see @fig-beta). As $alpha$ and $beta$ increase, the distribution becomes more concentrated around its mean, reflecting greater prior certainty. Depending on which parameter is greater relative to the other, the skewness of the distribution shifts corresponding to varying beliefs in $theta_i$. When $alpha + beta < 2$, the distribution is U-shaped, placing more mass near 0 and 1 (see @fig-beta).

#let beta-dist = image("../figures/beta-dist.svg")
#apa-figure(
  beta-dist,
  caption: [Shape of the Beta Distribution for Different Parameter Values],
  note: [$alpha$ and $beta$: shape parameters, $mu = alpha / (alpha + beta)$: mean parameter, $kappa = alpha + beta$: concentration parameter. The black line indicates the mean of each distribution. Figure generated using the _ggplot2_ #cite(<wickham2026>) and _ggdist_ #cite(<kay2025>) R-packages.],
) <fig-beta>

Because this parametrization is not completely intuitive in the marble game example, the distribution can be reparametrized in terms of a prior mean $mu = alpha / (alpha + beta)$ and a concentration parameter $kappa = alpha + beta$. The former parameter represents the literal mean of the distribution (black line in @fig-beta) and the concentration parameter how strongly the prior concentrates around the mean (i.e., the effective prior sample size). This means that the precision parameters is inversly related to the variance of the Beta distribution (exact relationship $sigma^2 = (mu (1 - mu)) / (1 + kappa)$).

Speaking in terms of the example, the mean parameter $mu$ can be interpreted as the average probability of drawing a black marble across the entire population of bags. The precision parameter $kappa$ describes how homogeneous or heterogeneous the color distribution of the marbles is within each bag. When $alpha + beta < 2$, there is a high probability that the colors will be homogenous in color within bags, meaning they will be either almost exclusively white or black (like in @fig-marble-composition). In contrast, when the precision parameter $kappa$ is large the distribution is tightly centered around its mean $mu$, and the bags are likely to contain a mixture of black and white marbles with proportions close to $mu$.

The final layer of the model quantifies uncertainty of the bag-level parameters $mu$ and $kappa$ by placing hyperprior distributions on each parameter. In this example, a Uniform distribution is placed over the mean $mu$ and an Exponential distribution over the precision $kappa$ #cite(<griffiths2024>).

$
  mu ~    & "Uniform"(0, space.sixth 1) \
  kappa ~ & "Exponential"(1)
$ <eq-hyper>

A uniform prior on $mu$ reflects complete prior uncertainty about the average probability of drawing a black marble across bags. Consequently, every admissible values of $mu$ (between 0 and 1) is conceived as equally likely. In comparison to the prior on $mu$, the exponential prior with rate $lambda = 1$ on $kappa$ is _informative_, because it biases the expectation towards smaller precision values. Therefore, it places more prior certainty on scenarios with low variability within bags and high variability between bags, including near-homogeneous bags when $kappa$ is very small. In summary, before seeing any data, the children consider all expected proportions of black marbles across all bags equally plausible, while anticipating substantial variability in the bag-specific probabilities of drawing a black marble.

Importantly, the rate parameter $lambda$ of the hyperprior distribution is assumed to be known in advance. In principle, however, additional layers can be added to hierarchical models by once again placing prior distributions over these parameters. This may be especially relevant for learning processes that require more complex hierarchies, for example, language comprehension #cite(<griffiths2024>) #cite(<kemp2008>). To ensure the model is identifiable, it is necessary to assume that the hyperparameters in the highest levels of the hierarchy are a priori known.

Posterior inference of the parameters at any level of the hierarchical model, conditional on the observed data, can be obtained by combining the prior distributions with the data likelihood using Bayes’ rule #cite(<gelman2014>):

$
  P(bold(theta), alpha, beta | bold(y)) prop
  underbracket(P(alpha, beta), "Hyperprior") dot
  overbracket(P(bold(theta) | alpha, beta), "Conditional prior") dot
  underbracket(P(bold(y) | bold(theta), alpha, beta), "Likelihood")
$ <eq-bayes1>

Inferences for any bag-level parameter $theta_i$ can be obtained by integrating out the hyperparameters $alpha$ and $beta$. Because the resulting integral is typically analytically intractable, numerical integration via Markov Chain Monte Carlo (MCMC) algorithms is used as an approximation #cite(<griffiths2024>).

$
  P(theta_i | d_1, dots, d_n) = integral.double P(theta_i | alpha, beta, d_i)space.sixth P(alpha, beta | d_1, dots d_n) space.quarter d alpha space.sixth d beta
$ <eq-bayes2>

== Model Application and Posterior Inference <sec-application-hbbm>

@fig-hbbm1 visualizes the estimated posterior distributions for the bag-level parameters $theta_i$ as well as the shared population-level parameters $mu$ and $kappa$, after observing the data shown in @fig-marble-composition and applying the model described in @eq-binom – @eq-hyper. Most importantly, the posterior distribution for $theta_8$ demonstrates that the model’s predictions align with the intuition of the children. Although only a single black marble has been observed in bag 8, the model is highly confident that $theta_8$ is close to one.

How does the model achieve such precise predictions? The key lies in the highest level of the hierarchy. The posterior distribution of $kappa$ places high certainty on low values of the parameter, inducing a U-shaped population-level distribution over $theta_i$. This implies that the bags tend to be internally homogeneous in color. By capturing this population-level regularity, the model mathematically formalizes the acquisition of an overhypothesis. This learned overhypothesis acts as a highly informative prior. It severely restricts the hypothesis space, allowing the model to leverage a single low-level observation to make a precise, confident prediction for a new bag.

#apa-figure(
  image("../figures/panel_mu0.5_phi0.8.svg"),
  caption: [Posterior Estimates Showing How Bag-Level Homogeneity Enables Accurate Predictions for a New Bag],
  note: [Gray ribbon represents the kernel density estimate from the posterior samples (methodological detail see section @appendix. Black solid lines indicate the posterior median. $theta_i$: probability of drawing a black marble. Inspired from #cite(<griffiths2024>, form: "prose"). Figure generated using the _ggdist_ R package #cite(<kay2025>).],
) <fig-hbbm1>

To contrast these results with a scenario in which precise predictions are not possible, @fig-hbbm2 shows the posterior distributions obtained from data that do not support strong inferences about the probability of drawing a black marble in bag 8. The posterior distribution for $theta_8$ (second-to-bottom panel) illustrates this very clearly. The median value, indicated by the solid black line, suggests that the model’s prediction is no better than random chance, and the high uncertainty is reflected in the heavy tails of the distribution.

Examining the data together with the population-level distribution of bags reveals the reasons for this uncertainty. Overall, the bags contain a lower proportion of black marbles, and within each bag, the colors are more evenly mixed. At the level of the model parameters, two important changes are evident. First, the population-level mean $mu$ is smaller, reflecting the lower average proportion of black marbles across all bags. Second, the precision parameter $kappa$ is higher than in the previous example, indicating that the model has learned that the bags are more heterogeneous in their color composition. Because the model has inferred that bags are heterogeneous, a single observation in bag 8 provides very little information about the bag-specific probability $theta_8$. As a result, the posterior for $theta_8$ remains diffuse, and the model cannot make a precise prediction. Because the data lacks strong population-level regularities, the model cannot form an overhypothesis to meaningfully reduce the hypothesis space regarding the probability of a black marble at the lower level. This example illustrates an important property of hierarchical models that also applies to human cognition: the formation of an overhypothesis can only occur when strong population-level regularities exist at higher levels of abstraction. In their absence, predictions for new, sparsely observed bags remain uncertain.

#apa-figure(
  image("../figures/panel_mu0.2_phi20.svg"),
  caption: [Posterior Estimates Showing How Heterogeneous Bags Lead to Uncertain Predictions for a New Bag],
  note: [Gray ribbon represents the kernel density estimate from the posterior samples (methodological detail see section @appendix. Black solid lines indicate the posterior median. $theta_i$: probability of drawing a black marble. Inspired from #cite(<griffiths2024>, form: "prose"). Figure generated using the _ggplot2_ #cite(<wickham2026>) and _ggdist_ #cite(<kay2025>) R-packages.],
) <fig-hbbm2>

= Multidimensional Extension for Category Learning <sec-hdmm>

The marble game example has conceptually illustrated how hierarchical structure in data is exploited by humans to form multiple layers of abstraction and how Bayesian models capture this process in a probabilistic framework. Additionally, the example has illustrated how Bayesian models formalize the idea of an overhypothesis when higher-level regularities exist, which in turn can support precise predictions at lower levels even in one-shot learning scenarios. While the marble game is a simplified example, the same principles apply to more complex, real-world learning tasks. In particular, hierarchical Bayesian models provide a natural framework for understanding the acquisition of shape bias that guides category learning in children.

== The Shape Bias Phenomenon <sec-shape-bias>

To exemplify, consider a scenario involving a dyadic interaction between a mother and her child. When the mother points to a novel object on a counter that she refers to as a _pen_, the child faces an immediate inductive challenge: Upon which characteristics should the concept of _pen_ be generalized to identify future instances? While the child could theoretically extend the label to other objects sharing the same material, color, texture, or spatial location, empirical evidence suggests that children predominantly extend the label to objects that share the same shape #cite-eg(<smith2002>). This phenomenon is known as the _shape bias_ #cite(<griffiths2024>), which can be conceptualized as a learned overhypothesis defined by a higher-level abstract expectation that members of a category tend to share a similar shape. By forming this overhypothesis, the child drastically reduces the hypothesis space upon which perceptual characteristics an unknown category may be generalized.

== Formalizing the Shape Bias <sec-formalization-hdmm>

To formally model and explain the shape bias phenomenon from a Bayesian perspective, the hierarchical beta–binomial model introduced in the section @sec-formalization-hbbm must be extended in several respects. @tbl-model-extension provides an overview of the modified components. At the lowest level of the hierarchy, the observed data consist of counts of object exemplars (e.g., a pen lying on the counter). The grouping variable that defines the hierarchical structure is the object category to which a given exemplar belongs (e.g., pen). In contrast to the marble example, however, object exemplars are not characterized by a single binary feature dimension (e.g., black vs. white). Instead, they vary along multiple feature dimensions, such as shape, color, texture, and size. Moreover, each of these dimensions typically comprises more than two and arguably infinitely many possible levels (e.g., a small red plastic pen vs. a large aluminum pen).

#let table1 = pillar.table(
  cols: "c[0.7fr]" + "c[0.8fr]" + "c[0.8fr]" + "c[1.2fr]" + "c[1fr]" + "c[1fr]",
  table.hline(stroke: 0.75pt),
  table.cell(align: left)[*Example*],
  [*Hierarchy*],
  [*Data*],
  [*Dimensionality*],
  [*Type*],
  [*Values*],
  table.hline(stroke: 0.75pt),
  [Marble \ game],
  [Bags],
  [Marbles],
  [Unidimensional],
  [Color],
  [Binary],
  [Shape \ bias],
  [Object \ category],
  [Exemplars of objects],
  [Multidimensional],
  [Shape, color, \ texture, size],
  [Multinomial],
  table.hline(stroke: 0.75pt),
)

#apa-figure(table1, caption: [Model Adaption of the Marble Game Example to the Shape Bias in Category Learning]) <tbl-model-extension>

More formally, let there be $m$ feature dimensions (shape, color texture, size, etc.) and $i$ object categories. Within each feature dimension $m$ there are $F_m$ possible levels #cite(<glassen2016>). For instance, in the category _pen_ ($i = 1$) along the color dimension ($m = 2$), if there are $F_2 = 3$ possible colors, a child may observe that out of 10 exemplars, 5 were black, 3 white and 2 silver. Rather than a binomial distribution, these counts follow its generalization, the _Multinomial_ distribution.

$
  bold(y)_(i m) ~ & "Multinomial"(bold(theta)_(i m))
$ <eq-multi>

The parameter vector $bold(theta)_(i m)$ is a _simplex_ (i.e., a vector summing to 1) representing the proportions of feature levels. In the example case, the empirical proportions of colors would be $theta_(11) = lr([0.5, 0.3, 0.2], size: #130%)$ for each color, respectively. The conjugate prior distribution of the Multinomial is a _Dirichlet_ distribution with parameters $alpha_m$ and $bold(beta)_m$, the multivariate generalization of the Beta distribution from the marble game example.

$
  bold(theta)_(i m) ~ & "Dirichlet"(alpha_m space.sixth bold(beta)_m)
$ <eq-dir>

Intuitively, $alpha_m$ captures the extent to which exemplars within a category are uniform in their feature values, while $bold(beta)_m$ represents the expected distribution of feature values across the entire category #cite(<kemp2006>) #cite(<kemp2007>). In other words, $bold(beta)_m$ encodes the population-level expectation, and $alpha_m$ determines how strongly individual exemplars conform to that expectation. Linking this back to the marble example, $bold(alpha)_m$ and $beta_m$ can be thought of as a generalization of the mean $mu$ and precision $kappa$, respectively. The ternary plot in @fig-tern shows the Dirichlet distribution for three possible feature values. In the left column, the three colors (black, white, silver) are equally likely across categories (see $bold(alpha)_m$), but the small value of $beta$ (analogous to $kappa$) implies that object exemplars within categories are mostly uniform in color. In the second column, the overall color expectation remains the same, but a larger $beta$ increases within-category variability, making exemplars more heterogeneous. The two rightmost columns illustrate the same concept for non-uniform overall color expectations (i.e., different category probabilities for colors).

#apa-figure(
  image("../figures/tern.svg", width: 21cm - 2in),
  caption: [Visualization of the Dirichlet Distribution for Three Feature Values of the Color Dimension],
  note: [Shading indicates the probability density over the simplex; lighter regions correspond to more likely combinations of the three color probabilities. w: white, b: black, s: silver (see main text). Figure generated using the _ggplot2_ #cite(<wickham2026>) and _ggtern_ #cite(<hamilton2018>) R-packages.],
) <fig-tern>

As in the marble game example, prior distributions are required for $bold(alpha)_m$ and $beta_m$. Specifically, the concentration parameter $alpha_m$ is assigned an exponential prior with a fixed rate of $lambda_m = 1$ and the population-level feature proportions $bold(beta)_m$ follow a Dirichlet prior with uniform weights (i.e., multivariate uniform distribution).

$
  alpha_m ~      & "Exponential"(1) \
  bold(beta)_m ~ & "Dirichlet"(bold(1)_F_m)
$ <eq-dir-prior>

The multivariate uniform prior reflects complete prior uncertainty about feature-value proportions for each dimension, while the exponential prior with rate $lambda = 1$ favors relatively homogenous features distributions within categories. This completes the extension of the marble game framework to multiple feature dimensions, yielding the _Dirichlet–Multinomial model_ #cite(<griffiths2024>).

How does this formalization help to explain the shape bias in human category learning? To answer this, the model described in @eq-multi – @eq-dir-prior is instantiated independently for each feature dimension $m$. @fig-shape-bias-model illustrates a simplified learning scenario involving two feature dimensions: shape ($m = 1$) and color ($m = 2$). Here, the learner observes three established object categories, each containing three exemplars, alongside a single exemplar ($y^"new"$) belonging to a novel category. Each dimension possesses three possible feature values: circle, square, or triangle for shape, and white, gray, or black for color. Crucially, the training data reveal a structural regularity: exemplars within the same object category consistently share the same shape (i.e., the shape bias), but they vary in color. For the color dimension (right panel), the presence of multiple colors within each category indicates high within-category heterogeneity. Consequently, the model infers a relatively large value for $alpha_2$ (conceptually analogous to the broader distribution in the second column of @fig-tern). In contrast, for the shape dimension (left panel), objects within a category are perfectly homogeneous. This causes the model to infer a small value for $alpha_1$, reflecting tight within-category precision (analogous to the first column of @fig-tern). By inferring a small $alpha_1$ and a large $alpha_2$, the model effectively forms the higher-level overhypothesis that object categories are strictly organized by shape, but not by color. When the learner encounters the exemplar $y^"new"$ of an unknown object category, which happens to be a black diamond, this learned overhypothesis serves as an informative prior expectation. Because the hypothesis space has been drastically reduced along the shape dimension, the model confidently infers that future instances of this new category will also be diamond-shaped, but it remains appropriately agnostic about their color. In doing so, the hierarchical framework mathematically replicates the shape bias.

#apa-figure(
  image("../figures/shape_bias_structure.svg", width: 6.5in),
  caption: [Hierarchical Structure in the Dirichlet–Multinomial Model Explaining the Shape Bias],
  note: [Simplified representation of the Dirichlet-Multinomial model across two feature dimensions. Three established categories (three exemplars each) and one novel category (single exemplar, $y^"new"$) are shown. The model infers a small concentration parameter for the homogeneous shape dimension ($alpha_1$) and a large parameter for the heterogeneous color dimension ($alpha_2$), establishing the overhypothesis that drives the shape bias for novel objects.],
) <fig-shape-bias-model>

== Empirical Application in Noun Generalization <sec-application-hdmm>

The Dirichlet–Multinomial model formally defines the computational problem of category learning, conceptualizing the shape bias as an overhypothesis acquired through experience. However, a reverse-engineering account also requires demonstrating that human behavior naturally approximates this ideal solution. To test this, #cite(<griffiths2024>, form: "prose") applied the model to a noun generalization task #pre-cite(<kemp2006>, prefix: [see also ]) #pre-cite(<kemp2007>, prefix: [see also ]), drawing on data from and inspired by a landmark developmental study by #cite(<smith2002>, form: "prose").

The original study by #cite(<smith2002>, form: "prose") investigated how toddlers learn to generalize novel object names. When young children are presented with a novel object and a novel name, they face an immediate inductive challenge: deciding which perceptual properties are relevant for extending the category. To test whether children learn to systematically attend to shape through experience, the researchers conducted a 9-week longitudinal study with 17-month-old toddlers. At this age, children are typically too young to systematically extend object names based on shape.

To evaluate whether a hierarchical Bayesian model can capture this learning process, the training environment of the experiment by #cite(<smith2002>, form: "prose") can be formally represented as a data matrix (see @tbl-data-smith). The model is exposed to training data consisting of two exemplars ($e_1$ and $e_2$) from each of four distinct object categories. The objects vary along four feature dimensions: shape, texture, color, and size. For this implementation, it is assumed that the number of possible feature values $F_m$ is 10 for shape, texture, and color, and 2 for the size dimension. Importantly, as shown in the first row of @tbl-data-smith, the model’s training data reflects the actual experiment in that object categories are perfectly invariant in their shape, but heterogeneous across the other dimensions.

#let data = csv("../data/shape-bias.csv")
#let rows = data.slice(1)
#let table2 = pillar.table(
  cols: "l[1.6fr]" + "c[0.785fr]" * 8 + "c[1.5fr]" + "c[0.5fr]" * 3,
  table.hline(stroke: .75pt),
  table.cell(rowspan: 2, align: left + horizon)[*Feature* $m$],
  table.cell(colspan: 2)[*Category 1*],
  table.cell(colspan: 2)[*Category 2*],
  table.cell(colspan: 2)[*Category 3*],
  table.cell(colspan: 2)[*Category 4*],
  table.cell(rowspan: 1, align: horizon)[*Target*],
  table.cell(colspan: 3)[*Choices*],
  ..([$e_(1)$], [$e_(2)$]) * 4,
  [$x_"dax"$],
  [$t_(1)$],
  [$t_(2)$],
  [$t_(3)$],
  table.hline(stroke: .75pt),
  ..rows.flatten().map(s => eval(s, mode: "markup")),
  table.hline(stroke: .75pt),
)

#apa-figure(
  table2,
  caption: [Training and Testing Data Based on a Study by #cite(<smith2002>, form: "prose")],
  note: [Each category has two exemplars $e_1$ and $e_2$. The target object must be matched ($x_"dax"$) to three previously not seen test objects $t_1 - t_3$ with unknown true object category, see also #cite(<griffiths2024>, form: "prose").],
) <tbl-data-smith>

Following the training phase, the model is subjected to a second-order generalization test designed to mirror the empirical task given to the toddlers. The model is presented with a novel target object, arbitrarily labeled a "dax" ($x_"dax"$), belonging to a completely new object category ($i = 5$) that was not present during training. The task is to determine which of three novel test objects ($t_1, t_2, "or" t_3$) is most likely to also be a "dax". The test objects are carefully chosen, such that $t_1$ matches the target $x_"dax"$ only in shape, $t_2$ matches only in texture, and $t_3$ matches only in color. This experimental design forces the model to rely on its higher-order generalizations to resolve the competing feature matches.

@fig-shape-bias-results illustrates the posterior distribution of the model’s relative choice probabilities for matching each of the three test objects to the novel target, $x_"dax"$. Given the three available choices, the baseline probability for random guessing is $p = 1/3$. The posterior distribution reveals a high degree of certainty that the model will select $t_1$, the test object that matches the target in shape. This strong preference, alongside the fact that the probabilities for the texture and color matches fall well below chance, clearly demonstrates the model’s successful acquisition of an overhypothesis (i.e., the shape bias). Furthermore, the figure plots the empirical data from #cite(<smith2002>, form: "prose"), showing that the model’s posterior belief almost perfectly mirrors the aggregate choice frequencies of the trained toddlers. This alignment is particularly compelling because a baseline group of children in the same study, who received no prior category training, failed to generalize by shape and instead chose among the test objects at random #cite(<smith2002>). By successfully replicating these developmental trajectories, the hierarchical Bayesian framework provides a reverse-engineering account of category learning and demonstrates how shape bias emerges through experience as a learned overhypothesis.

#let img_shape-bias = image("../figures/shape_bias.svg")
#apa-figure(
  img_shape-bias,
  caption: [Comparison of Model Predictions and Empirical Data for a Novel Noun Generalization Task],
  note: [Lightgray ribbon: kernel density estimate of the posterior samples, black point: softmax-normalized posterior median of model. gray point: empirical choice percentage of 19-month-old children based on data from #cite(<smith2002>, form: "prose"). Random guessing probability of $p = 1/3$. Figure generated using the _ggplot2_ #cite(<wickham2026>) R-package.],
) <fig-shape-bias-results>

= Synthesis and Discussion <sec-discussion>

Humans possess a remarkable ability to form rich generalizations from sparse data, enabling highly efficient learning. Drawing on the work by #cite(<griffiths2024>, form: "prose"), this article has illustrated how hierarchical Bayesian models provide a reverse-engineering account of this cognitive capacity. Specifically, these models probabilistically capture how human learners exploit the hierarchical structure of their environment to form overhypotheses. By acting as higher-level prior expectations, these overhypotheses significantly reduce the hypothesis space at lower levels of abstraction. Ultimately, this structural restriction not only enables rapid learning for novel instances, but also provides a formal explanation for both how humans "learn to learn" and where prior distributions actually originate #cite(<griffiths2024>).

These principles were first illustrated in simplified marble color guessing game. Formalized through the beta-binomial model, this example demonstrated how population-level regularities in the form of nearly homogenous marbles within bags enabled the formation of a structural overhypothesis. In turn, this higher-order expectation acted as a strong prior, enabling precise predictions about the color of future marbles within a novel, sparsely observed bag. Crucially, the Bayesian model also captured the formal boundary condition of this process. When such higher-level regularities do not exist (e.g., if previously observed bags are completely heterogeneous), no informative overhypothesis can be established. Consequently, the model's prior remains diffuse, and inference at the lower level remains uncertain.

Building on this simple example, the framework was extended to a Multinomial-Dirichlet model to demonstrate its real-world relevance in infant category learning. Specifically, this application formalized the acquisition of the shape bias as a learned overhypothesis. Just as the simpler model detected color uniformity within bags, this extension demonstrated how learners extract structural regularities across multiple feature dimensions, namely, the realization that objects within the same object category are organized by shape rather than texture or color. Furthermore, the model’s inferences aligned closely with empirical data from a noun generalization study inspired by #cite(<smith2002>, form: "prose"). Because the trained toddlers in this study were otherwise too young to systematically exhibit a shape bias, this alignment reinforces the idea that such overhypotheses are not innate, but are instead learned through experience.

This insight also bears on the longstanding debate between nativist and empiricist accounts of cognitive development. Hierarchical Bayesian models offer a nuanced perspective within this debate. On the one hand, this article has shown that inductive biases, such as the shape bias, can emerge through experience as learners form overhypotheses about the structure of their environment. On the other hand, the formalization of both examples has illustrated that probabilistic inference cannot occur in a vacuum, and that these models must bake in fixed prior assumptions at the highest level of abstraction. Ideally, the hierarchy in the models can be iteratively extended until the remaining prior knowledge corresponds only to assumptions that are plausibly innate #cite(<griffiths2024>).

Beyond these specific examples, hierarchical Bayesian models possess broad applicability across many cognitive domains. Their relevance is evident whenever a learning problem can be decomposed into multiple layers of abstraction, as seen in, for example, in causal learning, language acquisition, vision, and property induction #cite(<griffiths2024>).

Despite these explanatory strengths, it is important to recognize the specific level of abstraction at which these models operate. Following #cite(<marr2010>, form: "prose") tripartite framework, the hierarchical Bayesian approach represents "function-first" reverse-engineering strategy that characterizes cognition primarily at the computational level #cite(<griffiths2024>). This level defines the abstract problem the system solves and identifies its mathematically optimal solution, rather than specifying the specific procedures or data structures (algorithmic level), or describing neural instantiation (implementation level). Crucially, as #cite(<griffiths2024>, form: "prose") emphasize, offering an explanation in terms of Bayesian inference does not imply that individuals are explicitly calculating Bayes’ rule in their heads. Instead, the assumption of optimality serves as a methodological framework to understand behavior as a rational response to environmental structures. While this article highlighted the utility of a purely computational approach, bridging the gap between this level of analysis and its algorithmic and neural implementation remains a critical challenge for future research to achieve a comprehensive understanding of human cognition.

#pagebreak()

#bibliography("references.bib", style: "apa", title: "Bibliography")

#pagebreak()

= Appendix <appendix>

== Implementation of the Beta-Binomial Model

Simulation of the two examples in section @sec-application-hbbm was carried out using the _R_ programming language #cite(<rcoreteam2025>), version 4.5.2. Bayesian hierarchical model estimation was conducted in _Stan_ #cite(<carpenter2017>) via Hamiltonian Monte Carlo (HMC) using the No-U-Turn Sampler (NUTS), implemented through the _cmdstanr_ #cite(<gabry2025>) interface. Four Markov chains were run with 5000 warm-up iterations and 10000 post–warm-up samples per chain. The control parameters were set to a target average acceptance probability of #raw("adapt_delta = 0.99", lang: "R") and a maximum tree depth of #raw("max_treedepth = 15", lang: "R"). The population parameters were
$phi = lr({0.8, space.sixth 20}, size: #150%), space.sixth mu = lr({0.5, space.sixth 0.2}, size: #150%)$ for the dispersion and location parameter of the Beta distribution, respectively. Marble data were simulated according to @eq-binom - @eq-hyper. The _Stan_ code is provided below.

#align(center, image("./stan-code1.pdf", width: 100%))

#pagebreak()

== Implementation of the Dirichlet-Multinomial Model

The Dirichlet-Multinomial model was fitted using the same software and settings as the Beta-Binomial model, with training data detailed in @tbl-data-smith. For the model implementation, it was assumed that each of the $M$ total feature dimensions possesses $F = 10$ possible values#footnote[This is a slight departure from the theoretical parameterization described by #cite(<griffiths2024>, form: "prose"), who posited 10 possible values for shape, color, and texture, but only 2 for size. Because the probabilistic programming language _Stan_ does not natively support ragged arrays (i.e., varying bounds for $F_m$), strictly enforcing this constraint would require a complex reparameterization using a Gamma-Dirichlet equivalence trick. Given that assuming a uniform dimensionality of $F = 10$ across all features does not alter the model's core qualitative predictions regarding the shape bias, this simplification was adopted for computational clarity.]. The model places an Exponential prior on the concentration parameter $alpha_m$ and a uniform Dirichlet prior on the base rate simplex $beta_m$.

$
  alpha_m &~ "Exp"(1.0) quad
  beta_m  &~ "Dir"(bold(1)_F)
$

To ensure stable HMC sampling, the latent category distributions $theta_(j,m)$ were marginalized out, and the observed counts $bold(y)_(j,m)$ of $n_(j,m)$ total objects were evaluated via a marginalized Dirichlet-Multinomial likelihood.

$
  bold(y)_(j, m) | alpha_m, beta_m &~ "DirMultinomial"(n_(j, m), alpha_m beta_m)
$

Because $theta_(j,m)$ were marginalized out during inference, their posterior samples were analytically reconstructed post-hoc using the Dirichlet-Multinomial conjugacy in the `generated quantities` block.

$
  theta_(j, m) | bold(y)_(j, m), alpha_m, beta_m &~ "Dir"(alpha_m beta_m + bold(y)_(j, m))
$

To simulate forced-choice noun generalization, the model computes the posterior predictive probability that test object $k$ matches the novel target "dax". For each dimension $m$, this probability is formulated as a ratio. The numerator captures feature-specific evidence. A binary indicator $I$ for an exact match ($f_(k,m) = f_("dax",m)$) is combined with the prior pseudo-counts $alpha_m beta_(m, f_(k,m))$. This is normalized by the denominator, which sums the total evidence of the single observed "dax" ($1$) plus the total prior weight ($alpha_m$).

$
  P(f_(k, m) | f_("dax", m), alpha_m, beta_m) &= (I_(f_(k, m) = f_("dax", m)) + alpha_m beta_(m, f_(k, m))) / (1 + alpha_m)
$

To mirror the mutually exclusive behavioral task, absolute log-probabilities across all dimensions are summed and softmax-normalized into relative choice probabilities $p_k$.

$
  p_k &= "softmax" lr(( sum_(m=1)^M ln P(f_(k, m) | f_("dax", m), alpha_m, beta_m) ))
$

#pagebreak()

#align(center, image("./stan-code2.pdf", width: 100%))

#pagebreak()

#heading("Statutory Declaration", level: 1, numbering: none, outlined: false)

I herewith declare that I have composed the manuscript myself and without use of any other than the cited sources and aids. Sentences or parts of sentences quoted literally are marked as such; other references with regard to the statement and scope are indicated by full details of the publications concerned. The manuscript in the same or similar form has not been submitted to any examination body and has not been published. This manuscript has not yet, even in part, been used in another examination or as a course performance. In addition, I acknowledge that I have used artificial intelligence tools during the preparation of this manuscript. I am aware that the use of machine-generated texts does not guarantee the quality of content and text. I therefore declare that I have only used text-generating AI tools as an aid and that my creative influence predominates in this work. All intellectual decisions, argumentation, and conclusions presented in this manuscript are my own or cited appropriately.
#v(1em)

#align(center)[
  #table(
    columns: (auto, 1fr),
    align: (left, left),
    stroke: 0.5pt,
    fill: (col, row) => if row == 0 { luma(240) } else { none },
    [*AI Tool*],
    [*Purpose*],
    [#link("https://www.deepl.com")[DeepL]],
    [Language translation, grammar checking],
    [#link("https://gemini.google.com")[Gemini Pro 3.1]],
    [Copyediting, text refinement, Stan translation of the noun generalization task.],
  )
]

#v(3em)
#grid(columns: (1fr, 1fr), align(left)[
  #line(length: 80%, stroke: 0.75pt)
  Date, Place
], align(left)[
  #line(length: 80%, stroke: 0.75pt)
  Signature
])
