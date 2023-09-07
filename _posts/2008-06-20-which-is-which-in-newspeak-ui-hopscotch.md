---
layout: post
title: Which is Which in Newspeak UI - Hopscotch
date: 2008-06-20 11:12:39.000000000 -07:00
categories:
- newspeak
tags: []
permalink: "/2008/06/20/which-is-which-in-newspeak-ui-hopscotch/"
---
This is the second part of the Newspeak UI overview. Hopscotch is the name of both the application framework and the IDE of Newspeak, built on top of [Brazil](http://blog.3plus4.org/2008/05/08/which-is-which-in-newspeak-ui-brazil/). Brazil provides "logical widgets" mapped onto the host platform artifacts and takes care of laying them out. Hopscotch adds the concepts of an application, domain objects, and mechanisms to keep the model and the view synchronized.

Hopscotch-the-IDE is based on a web browser-like model of navigation. I say "web browser-like" because the framework could support other application shapes as well, but in the particular case of an IDE, an interactive document viewed in a universal navigator is a model that works exceptionally well for visualizing a complex structure such as application code. The interactive documents are specified in a declarative and highly composable way.

The work on the framework is still in progress. I am interested in achieving a usage experience (the user in this case being a programmer using the framework) reminiscent of functional reactive programming, and that has some interesting algorithmic challenges.

More details are in [this paper](http://bracha.org/hopscotch-wasdett.pdf), and hopefully the video of my Smalltalk Solutions demo will soon be available. The paper has been accepted to [WASDeTT](http://smallwiki.unibe.ch/wasdett2008/), co-located with [ECOOP](http://2008.ecoop.org/), as a featured talk on July 8 at 12:00. Unfortunately, this coincides with the DLS, but I'll also show a short demo during Gilad's [Short Introduction to Newspeak](http://2008.ecoop.org/school.html#newspeak) on July 10.

