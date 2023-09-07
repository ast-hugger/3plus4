---
layout: post
title: Rethinking the comma
date: 2007-09-02 17:37:06.000000000 -07:00
categories:
- what-if
tags: []
permalink: "/2007/09/02/rethinking-the-comma/"
---
This is a follow-up to [the post about message chains](http://blog.3plus4.org/2007/08/30/message-chains/) from a few days ago. Like any syntax-related issue, the choice of the operator is one of those delightful things that encourage a lively exchange of opinions by virtue of being easy to have an opinion about. I ended the post half-seriously considering ":)", but here is an interesting thought experiment.

Suppose we abolish a comma as a binary message. A + could just as well work as a generic "join these two things" message a comma usually is. Instead, suppose that the comma becomes the new chain operator. Here is what it would look like.

```
something thingsAt: aKey, includes: aThing,
    ifTrue: [...]
    ifFalse: [...]
```

This fits nicely with the original Smalltalk idea of using natural language punctuation for message control, and continues the line-up of a period and a semicolon by being the weakest message separator of them all.

