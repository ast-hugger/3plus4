---
layout: post
title: On UI Layout
date: 2008-05-25 13:29:09.000000000 -07:00
categories:
- general
tags: []
permalink: "/2008/05/25/on-ui-layout/"
---
Michael Lucas-Smith posted [an interesting summary of UI layout models](http://www.cincomsmalltalk.com/userblogs/mls/blogView?showComments=true&printTitle=User_Interface_Layout&entry=3389137969). I tried to leave the following as a comment, but the blog server swallowed it without posting. Fortunately, I half-expected this and copied the text before submitting. I'm posting it here instead, with some further edits.

Overall, this is a good summary, but there is a very important model it misses.

The model is a variant of constrained-based layout called linear constraints. In that model widget positions are expressed as a system of linear equations. This more formalized approach, unlike "intuitive" models with springs, wires and other pseudo-real life accessories, isn't fiddly at all. There are algorithms ([Cassowary](http://en.wikipedia.org/wiki/Cassowary_constraint_solver) is the most famous) to efficiently solve such constraint systems.

Also, importantly, many other models: fixed, relative, stacked and grid layouts are special cases of linear constraints. Only flow cannot be expressed this way. Thus, the models are not all incompatible. Also note that I'm listing "relative" as an additional model, which is the correct way to describe the VW approach (first proposed, most likely, by Luca Cardelli). ("Fixed" would be the case when the widget's bounds are expressed as coordinates in some coordinate system, usually relative to the containing widget's top left corner).

I'm not sure what it means to say that layout should not be part of the widget framework proper, without first defining the proper. One extreme is Windows which provides no automatic layout capabilities at all. That is indeed the pure fixed model. The other are frameworks with container visuals. In those frameworks the specific layout model is indeed dictated by the container. But still, the only difference between the two is that something like Qt has containers that do layout and Windows doesn't. In neither of them layout is part of widgets (by widgets I mean atomic leaf components such as buttons or list views).

The important question is how hard or easy it is to introduce new containers with different layout models, but it is a matter of the overall framework design. The main complication here are performance constraints. The framework should reasonably quickly respond to incremental layout changes, and reconciling that need with pluggability of containers and their layout policies is where the devil lies. I'd estimate about 70% (if not more) of [Brazil](http://blog.3plus4.org/2008/05/08/which-is-which-in-newspeak-ui-brazil/) design effort has had something to do with layout management.

