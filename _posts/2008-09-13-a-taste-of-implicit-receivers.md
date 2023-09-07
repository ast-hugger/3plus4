---
layout: post
title: A taste of implicit receivers
date: 2008-09-13 10:06:09.000000000 -07:00
categories:
- newspeak
tags: []
permalink: "/2008/09/13/a-taste-of-implicit-receivers/"
---
One of the features that distinguish Smalltalk from most other languages is the keyword message syntax, and one of those that distinguish Newspeak from Smalltalk are implicit receivers. They work very nicely together to allow writing very readable code. Here is a drag-and-drop related method I just wrote:

```
createDropTargetFor: session <DragDropSession> ^<DropTarget> = (
  ^if: session source
      isMovingMyItem: [createDropTargetForItemMove]
      otherwise: [createClientSuppliedDropTargetFor: session]
)
```

What we have here is a method with the selector createDropTargetFor:. The things in angle brackets are type annotations--essentially, optional structured comments telling the reader what the method expects and returns. Unlike Smalltalk, the body of the method is explicitly delimited by "=(" and ")".

The three message sends in the body have no explicit receivers, which in this case means they are sent to self, self being what any Smalltalker would expect. In the general case, however, because of class nesting there may be a choice of selves available, so the receiver is not necessarily the object whose class contains the method with the send. This is why "implicit receiver" is a more precise term. For details, see Gilad's [_On the Interaction of Method Lookup and Scope with Inheritance and Nesting_](http://dyla2007.unibe.ch/?download=dyla07-Gilad.pdf).

