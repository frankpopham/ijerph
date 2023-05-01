---
title: "Are IJERPH special issues a route for self publication?"
author: "Frank Popham"
date: "2023-04-19"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### IJERPH - mega journal

The [International Journal of Environmental Research and Public Health](https://www.mdpi.com/journal/ijerph) has grown rapidly in recent years. It now publishes so many papers [it is classed a mega journal](https://jamanetwork.com/journals/jama/article-abstract/2802853). Concerns have been raised about MDPI, IJERPH's publisher, [with debate about whether it is a predatory or not or something in-between](https://paolocrosetto.wordpress.com/2021/04/12/is-mdpi-a-predatory-publisher/).

### Trouble at mill

Recently, [Clarivate delisted IJERPH so it no longer has an "official" impact factor](https://www.science.org/content/article/fast-growing-open-access-journals-stripped-coveted-impact-factors). This is a big blow as researchers are often judged on the impact factor of the journal they publish in. So the higher the impact factor the better. For a mega journal in public health, IJERPH's impact factor was impressive, although there is evidence that this was due to [impact factor inflation](https://mahansonresearch.weebly.com/blog/mdpi-mega-journal-delisted-by-clarivate-web-of-science).

### Nothing special

Publishing papers in special issues seems key to [MDPI's and IJERPH's growth](https://paolocrosetto.wordpress.com/2021/04/12/is-mdpi-a-predatory-publisher/). There is really nothing special about special issues in IJERPH as the majority of IJERPH's papers in 2022 were in special issues. One aspect yet (I think) to be explored is their use as a vehicle for self publication. A special issue is edited by the people who proposed its topic rather than the normal editors (although they may have input). It is not uncommon for IJEPHR editors to also author a paper in a special issue.

### Self publishing

Obviously it is well recognised that if an editor is an author on a submitted paper to their journal (or special issue) then the editor should not be involved in decisions relating to their paper. [MDPI's guidelines are as follows:](https://www.mdpi.com/special_issues_guidelines)

> **Editor's Submission:** The special issue may publish contributions from the Guest Editor(s), but the number of such contributions should be limited, to ensure the diversity and inclusiveness of authorship representing the research area of the Special Issue. Any article submitted by a Guest Editor will be handled by a member of the Editorial Board.

I calculate that about a fifth of all papers in special issues in IJEPHR have an editor as an author with three quarters of special issues having at least one paper with an editor as an author . My calculations are based on analysis of all papers published in special issues that closed for submissions from the start of 2022 to the end of March 2023. I was able to link authors and editors in special issues through their [Sciprofile](https://sciprofiles.com/) profile (an author network site run by MDPI that on which each author and editor seems to have their own page)

I wondered to what extent editing a special issue is just a way to get a publication for an editor (i.e. they are one of the authors on a paper in the special issue). So I scaraped data from special issues that had a closing date of 2022 to end of March 2023. The code I used to do this is here. I found 1342 special issues, 1275 of them which formed my sample as the other 67 didn't have a published paper. MDPI also runs sciprofile so nearly every author and editor on a IJERPH paper has a profile on the site. This allowed me to match editors to authors to check overlap.

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
