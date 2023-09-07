---
layout: post
title: Sections Wrap-Up
date: 2007-04-06 20:55:06.000000000 -07:00
categories:
- smalltalk
tags: []
permalink: "/2007/04/06/sections-wrap-up/"
---
<p>To wrap up for now the thread of functional-like features, here is a very simple implementation of sections that doesn't rely on currying:</p>
<pre class="smalltalk">
Object&gt;&gt;~ aSymbol
    aSymbol numArgs = 1 ifFalse: [self error: 'Invalid selector'].
    ^[:arg | self perform: aSymbol with: arg]

Symbol&gt;&gt;~ anObject
    self numArgs = 1 ifFalse: [self error: 'Invalid selector'].
    ^[:arg | arg perform: self with: anObject]</pre>
<p>First of all, note the ambiguity of the section operator that we didn't discuss in the previous post. What does</p>
<pre class="smalltalk">
#, ~ #,</pre>
<p>mean? Is it a left section prepending a comma to the argument, or a right section appending it?</p>
<p>Considering the implementation, clearly it's the latter, but this particular choice of behavior is only a side effect of the implementation. In principle though, the intended meaning is undecidable. There is no difference in Smalltalk between a symbol and a selector---so there is no way to tell which comma is the operator and which is the section argument. This creates two potential gotchas.</p>
<p>One, it is impossible to create a left section with a Symbol as the fixed argument. We can't make a block to check whether the symbol <code>#foobar:</code> includes a given character by writing</p>
<pre class="smalltalk">
#foobar: ~ #includes:</pre>
<p>The second, related, problem is that the behavior of the section operator can vary if the first argument is a variable:</p>
<pre class="smalltalk">
foo ~ #includes:</pre>
<p>creates a left section if the current value of <code>foo</code> is anything but a Symbol, a right section if it is a one-argument selector, or fails otherwise.</p>
<p>In practice, the ambiguity can be avoided by "downgrading" the first argument to a string in situations where it might be a symbol we don't want to be mistaken for a selector.</p>
<p>A few people asked for some examples how some of the things I talked about could be useful. As I wrote before, I don't think currying of blocks can be very useful in practical Smalltalk, simply because the Smalltalk school of expression is different.</p>
<p>I have a slightly different opinion about sections. I use them (the simple currying-independent implementation above) in the framework I'm working on. I hope to write more about that one day, but for now I'll adapt the example to my past project,<a href="http://www.cincomsmalltalk.com/CincomSmalltalkWiki/Announcements+Framework"> VisualWorks Announcements</a>.</p>
<p>Remember that it's possible to subscribe for announcements by using either a block</p>
<pre class="smalltalk">
anObject
    when: SomethingHappened
    do: [:ann | ...do something with ann...]</pre>
<p>or a receiver-selector pair</p>
<pre class="smalltalk">
anObject
    when: SomethingHappened
    send: #processAnnouncement:
    to: self</pre>
<p>Given sections, we could handle the receiver-selector case using the same message we use for blocks:</p>
<pre class="smalltalk">anObject
    when: SomethingHappened
    do: self ~ #processAnnouncement:</pre>
<p>Of course, we could also simply drop the receiver-selector option and require explicitly writing</p>
<pre class="smalltalk">
[:ann | self processAnnouncement: ann]</pre>
<p>but arguably the block isn't quite as succinct as the equivalent section. So, what does this buy us?</p>
<p>Of course, we simplify the API. There is now only one subscription message instead of two. What was a separate case becomes part of the same mechanism.</p>
<p>The implementation is also simpler. In the original we needed to remember the receiver and the selector to send to it. (The block case is handled by remembering the block as the receiver and setting the selector to <code>#value:</code>---
granted, I'm simplifying the Announcements picture a little, but as I said my example is only an adaptation). Once we throw out separate support for the receiver-selector case, all we do is remember the block and evaluate it when needed.

There even is a potential performance improvement. The receiver-selector implementation always sends `#perform:with:` to the receiver to deliver an announcement, even for those subscriptions that use a block. The simplified implementation instead evaluates the block to set things in motion, and `#perform:with:` only enters the picture together with sections in those cases where we specifically want a receiver-selector option.

