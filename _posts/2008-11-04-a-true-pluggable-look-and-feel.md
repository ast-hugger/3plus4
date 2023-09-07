---
layout: post
title: A True Pluggable Look-and-Feel
date: 2008-11-04 23:52:19.000000000 -08:00
categories:
- newspeak
tags: []
permalink: "/2008/11/04/a-true-pluggable-look-and-feel/"
---
[In an earlier post](http://blog.3plus4.org/2008/05/08/which-is-which-in-newspeak-ui-brazil/) I mentioned that Brazil was designed to support cross-platform native UIs, and that I was working on our second platform mapping, Windows (the first one was Morphic). That was in May and for some time now Windows support is fully functional and Brazil applications can, as intended, work in either Morphic or Windows with no explicit provisions for that in the application code. What comes along is dynamic remapping--the ability to move the UI of a running application between Morphic and Windows without shutting down. This is just like switching the look in an emulated environment like VisualWorks--except that nothing is emulated and the widgets are the real deal.

[Here is a screencast showing what it's like.](http://blog.3plus4.org/wp-content/uploads/2008/11/brazilremapping.htm)

This feature is not as gimmicky as it may seem. When an image-based environment is given a native UI, dynamic remapping is exactly what should happen when an image starts--and not only when it starts on a platform different from where it was saved. Even on the same platform, recreating native windows and widgets open at the time the snapshot was taken amounts to essentially the same thing.

