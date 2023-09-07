---
layout: post
title: NotNil, then what?
date: 2007-04-22 12:59:46.000000000 -07:00
categories:
- smalltalk
- squeak
tags: []
permalink: "/2007/04/22/notnil-then-what/"
---
<p>This is about a fix of a conceptual bug in the current version of Squeak, though it's the detailed analysis why this was a bug that I think is worth a blog post.</p>
<p>A while ago when <code>ifNil:</code> and <code>ifNotNil:</code> were not standard in the commercial Smalltalks of the day, there was an occasional argument on c.l.s now and then whether they were a good thing or superfluous sugar not providing any new functionality. The typical case against <code>ifNil:</code> is something like</p>
<pre class="smalltalk">
foo ifNil: [self doSomething]</pre>
<p>which can trivially be rewritten in "classic" Smalltalk. The case that is less trivial, though, is</p>
<pre class="smalltalk">
^self foo ifNil: [self defaultFoo]</pre>
<p>It's important that the value being tested is computed and not just fetched from a variable. For a computed value, we cannot in the general case reduce the above to</p>
<pre class="smalltalk">
^self foo isNil ifTrue: [self defaultFoo] ifFalse: [self foo]</pre>
<p>because this would evaluate <code>self foo</code> twice---something we would want to avoid if <code>self foo</code> had side effects. Avoiding double evaluation would call for a variable to hold onto the result of <code>self foo</code>:</p>
<pre class="smalltalk">
| foo |
foo := self foo.
foo isNil ifTrue: [self defaultFoo] ifFalse: [foo]</pre>
<p>which is significantly more verbose than the ifNil: alternative. So, while ifNil: in this case can still be reduced to "classic" Smalltalk, doing so takes us down an abstraction level---from a single message doing exactly what it says to messing around with variables, tests and branches. While in <a href="http://java.sun.com/">some languages</a> such exposed plumbing is a fact of life, in Smalltalk we like to hide it when we don't need to deal with it directly.</p>
<p>Now, looking at <code>ifNotNil:</code>, it's important to note that its typical use case is different. In fact, this is what's interesting about ifNil: and ifNotNil: in general---they are asymmetrical. While <code>ifNil:</code> allows us to provide a useful value in the cases when the "main" branch of the computation returns nil, it's unlikely that we'd ever use <code>ifNotNil:</code> in a mirrored pattern as</p>
<pre class="smalltalk">
^self foo ifNotNil: [nil]</pre>
<p>This shows that the cause of the asymmetry is the obvious fact that typically <em>nil</em> is used as a token indicating the absence of a value. A non-nil object is interesting in its own right, while <em>nil</em> isn't.</p>
<p>So, ifNotNil: is primarily useful not as a value-producing expression, but rather as a control statement that triggers computation when another expression produces a useful result, in the simplest case going as something like</p>
<pre class="smalltalk">
foo ifNotNil: [self doSomethingWith: foo]</pre>
<p>This use of ifNotNil: could again be reduced to <code>isNil ifFalse:</code>. The case when the receiver of ifNotNil: is computed is more interesting because of the same problem of avoiding multiple evaluation, the obvious solution to which would again make the code much bulkier:</p>
<pre class="smalltalk">
| foo |
foo := self computeFoo.
foo ifNotNil: [self doSomethingWith: foo]</pre>
<p>The way to hide the plumbing here is to have ifNotNil: accept a one-argument block and feed the receiver into it, allowing us to fold the above back into a single expression</p>
<pre class="smalltalk">
self computeFoo ifNotNil: [:foo | self doSomethingWith: foo]</pre>
<p>This illustrates another asymmetry of ifNotNil: and ifNil:---while ifNil: block needs no arguments because nil is not an "interesting" object, it's often helpful for ifNotNil: to take the non-nil receiver as argument.</p>
<p>A number of years ago Squeak had <code>ifNil:</code> and <code>ifNotNil:</code> implemented exactly this way. The former would take a niladic block as the argument and the latter (as far as I can remember) would only accept a monadic one. When a few years ago Eliot and I were adding the two to VisualWorks we kept almost the same pattern, extending the ifNotNil: case to also accept a niladic block.</p>
<p>In Squeak the two messages have since then been reimplemented as special messages, expanded by the compiler into the equivalent <code>== nil </code><code>ifTrue:/ifFalse:</code> forms. Inexplicably, in the process <code>ifNotNil:</code> was changed to only accept a niladic block---
precisely the case that is less valuable! Also interestingly, the fallback implementation in ProtoObject still allows a monadic block in a "pure" `ifNotNil:` but not as the `ifNotNil` argument of `ifNil:ifNotNil:` and `ifNotNil:ifNil:`! Their classification under the 'testing' protocol is a minor nit in comparison.</p>

And now for the fix. It is available as a zip file [MonadicIfNotNil.zip](http://blog.3plus4.org/wp-content/uploads/2007/04/monadicifnotnil.zip "MonadicIfNotNil.zip") with two change sets. One modifies the handling of ifNotNil: and related messages to allow monadic blocks. The second contains the tests and should obviously be filed in after the compiler changes in the first set.

The change was fairly straightforward. Most of the work was dancing around the original error checking code that assumes only niladic blocks are legal as arguments of macroexpanded messages. The expansion simply promotes the block argument into a method temp, expanding

```
self foo ifNotNil: [:arg | ...]
```

into

```
(arg := self foo) ifNotNil: [...]
```

In VisualWorks such treatment would count as incorrect, but this promotion of a block argument into a method temp is in fact the classic Smalltalk-80 trick still surviving in Squeak in the similar treatment of the `to:do:` message.

