---
layout: post
title: Message chains
date: 2007-08-30 11:45:43.000000000 -07:00
categories:
- squeak
- what-if
tags: []
permalink: "/2007/08/30/message-chains/"
---
<p>On the squeak-dev mailing list, Fabio Filasieno <a href="http://lists.squeakfoundation.org/pipermail/squeak-dev/2007-August/119717.html">raised the issue</a> of "pipes" in Smalltalk, which generated quite a discussion. Since I've provided an <a href="http://blog.3plus4.org/wp-content/uploads/2007/08/pipe1.zip" title="pipe1.zip">implementation</a>, I'm also writing down my thoughts on the subject.</p>
<p>In a nutshell, this is what the idea is about. While the "classic" Smalltalk syntax allows us to write chains of unary messages such as</p>
<pre class="smalltalk">
x foo bar
</pre>
<p>without explicit parentheses, this is not the case with keyword messages.</p>
<pre class="smalltalk">
(x foo: 1) bar: 2
</pre>
<p>is a chain similar to the one above, yet its syntactic form is somewhat further removed from the idea of a "smooth" linear progression of operations, especially so when there are more than two consecutive message sends:</p>
<pre class="smalltalk">
(((x foo: 1) bar: 2) baz: 3) zork: 4
</pre>
<p>Imagine now that we have an operator which I will for now designate as ":>" allowing us to rewrite the above as</p>
<pre class="smalltalk">
x foo: 1 :> bar: 2 :> baz: 3 :> zork: 4
</pre>
<p>In other words, it makes the result of the preceding expression the receiver of the following message send, so that in fact</p>
<pre class="smalltalk">
x foo :> bar :> baz
</pre>
<p>and </p>
<pre class="smalltalk">
x foo bar baz
</pre>
<p>are two syntactic forms of the same chain of messages.</p>
<p>One obvious example of the use of this operator is rewriting something like</p>
<pre class="smalltalk">
(aCollection select: [:some | some foo]) collect: [:each | each bar]
</pre>
<p>as</p>
<pre class="smalltalk">
aCollection select: [:some | some foo] :> collect: [:each | each bar]
</pre>
<p>An even more likely case, which I don't believe was mentioned on squeak-dev, are complex conditions:</p>
<pre class="smalltalk">
something thingsAt: aKey :> includes: aThing :> 
    ifTrue: [...]
    ifFalse [...]
</pre>
<p>Writing the "standard" form often involves going back to the beginning of the expression to add parentheses once you've written enough of the expression to realize what it will look like. The chain operator exactly follows the "do this, then this, then that" structure of such expressions and does not require going back.</p>
<p>The change set linked at the beginning of the post implements the operator for Squeak. The inevitable question now is, Is This A Good Thing?</p>
<p>Clearly it adds nothing new to the language---chaining messages was always possible---only a new ability to express something in a different syntactic form. Such things generally invite (justified) suspicion and tossing around the various "less is more" quotes. Without questioning the value of minimalism and structured approach, some prior "sugary" additions such as tuples (brace Array constructors) or ifNil:/ifNotNil: could be (and have been) criticized on the same grounds. Any Array constructor discussion usually involves someone pointing out how the ease of array creation is overrated because redesigning the code to replace arrays with structured objects or store them in self-documenting variables only improves the code. This is true, of course---and yet in some contexts, one of which are DSL-like Smalltalk expressions, syntactically lightweight array creation turns out quite useful. Perhaps there is a similar "killer use" out there waiting for lightweight message chains to become available. Or perhaps not. My take is that this operator <em>might</em> be a good thing, and only practice can show if there are use cases out there waiting for it.</p>
<p>As for less is more---
perhaps the power of Smalltalk is not as much in having a small untouchable core (and if anything, Object and UndefinedObject protocols in Squeak live to prove that more is more) as in having a core small and malleable enough to support this sort of extensions and experimentation. I'd rather say Squeak core is still not small and malleable enough if something like this can only be done by modifying the compiler rather than tweaking the meta-level.</p>

Having said this, I should comment on [Bert Freudenberg's elegant #asPipe implementation](http://lists.squeakfoundation.org/pipermail/squeak-dev/2007-August/119787.html). There are two reasons why I consider it very neat but still not enough of the real thing. One is the look of the code. The value proposition of pipes is improving readability. Getting rid of parentheses is only one step towards that, the other is having the links of the chain stand out enough for the reader to easily see where the distinct steps are. This is partly an issue of formatting, but proper "graphic design" of a visually distinct special operator is still unbeatable.

In fact, in terms of visibility I think the operator even better than ":\>" (which already looks quite like a smiley) is ":)":

```
something thingsAt: aKey :) includes: aThing :)
    ifTrue: [...]
    ifFalse [...]
```

The other issue are minor anomalies that are always hard to hide with DNU tricks because of the need for the DNU handler to have some rudimentary behavior of its own, and because some things are not handled by messages. For example:

```
2 asPipe between: 1 and: 3; == true
```

evaluates to false.

```
2 asPipe between: 1 and: 3; ifTrue: [#foo]
```

fails to compile, ruling out the use of pipes to simplify conditions.

```
foo := [#nil].
bar := [#notNil].
nil asPipe value; ifNil: foo ifNotNil: bar
```

does compile because the branches are not literal blocks, but then the result of the "pipe" is #notNil.

