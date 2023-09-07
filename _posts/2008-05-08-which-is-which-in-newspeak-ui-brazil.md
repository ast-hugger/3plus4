---
layout: post
title: Which is Which in Newspeak UI - Brazil
date: 2008-05-08 21:54:34.000000000 -07:00
categories:
- newspeak
tags:
- brazil
- newspeak
meta: {}
permalink: "/2008/05/08/which-is-which-in-newspeak-ui-brazil/"
---
Brazil, which is the foundation of Newspeak UI (and is indeed named after [the movie](http://en.wikipedia.org/wiki/Brazil_%28film%29)), could be best described as a multi-platform logical UI and layout framework. It provides the familiar widgets ("visuals") such as Labels, Buttons, and ListBoxes, and container visuals for combining widgets and managing their layout. Unlike the UI frameworks of Squeak or VisualWorks, Brazil does not attempt to actually implement the widgets by emulating them. Instead it functions as a "logical UI" layer that builds and drives the equivalent "physical UI" made of native artifacts of the host platform.

The first host platform of Brazil was [Morphic](http://en.wikipedia.org/wiki/Morphic_%28software%29)--meaning the first "native" Brazil interfaces were in fact emulated. This trick allowed us to quickly build a minimal working implementation and polish the API without getting bogged down with FFI issues and native widget peculiarities early on. Since Morphic works on all OS platforms, so did Brazil from the start, allowing to use it on all platforms we need before we build real native mappings for those platforms.

Brazil also manages the layout of interfaces using a capability-based extensible model. Layout is fully managed by Brazil both on native platforms which usually provide rather weak layout facilities of their own, and in Morphic which provides a rich but baroque one. All that Brazil expects from the host are the abilities to position a widget within the specified rectangle, and to measure its natural (desired) extent.

Work on Brazil began in mid-March 2007 and in June a first real UI was built using it. I am currently working on a native Windows mapping. This work is at an advanced enough stage to be able to open all of the existing Brazil UIs as native in Windows with most functionality in place, without any changes in application code. Brazil windows can also be _dynamically_ remapped, so that what appears as a Morphic window can "jump out" of Squeak and become a native window with native controls, and then go back to Morphic all the while retaining its state.

Unlike prior implementations of native UIs in Smalltalk, Brazil is implemented entirely at the image level. This means that there is not a single VM primitive to support UI operations. The mapping relies on "raw" OS functions accessed via the general-purpose Newspeak FFI to do everything, from Windows class registration to widget management and GDI+ painting.

