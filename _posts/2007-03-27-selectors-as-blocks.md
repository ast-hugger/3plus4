---
layout: post
title: Selectors as Blocks
date: 2007-03-27 21:30:13.000000000 -07:00
categories:
- smalltalk
tags: []
permalink: "/2007/03/27/selectors-as-blocks/"
---
In [the previous two posts](http://blog.3plus4.org/2007/03/24/currying-in-smalltalk-part-2/) we implemented currying in Smalltalk, and in no fewer than two different ways. As Travis asked in a comment, what good is that other than being cool?

First of all, coolness is a virtue in itself. It says a lot about the Smalltalk system and the principles of its design that something like this can be added to it in a matter of minutes by changing a method or two (or not changing anything at all).

Coolness aside, I don't expect currying to be as useful in Smalltalk as it is elsewhere. The means of expression Smalltalk and Smalltalkers rely on are different from those of functional languages. Higher-order functions are not nearly as pervasive. Blocks in Smalltalk have always been subservient to objects, so much so that they are not real closures in a few implementations (and some Smalltalkers even [proposed to get rid of them](http://wiki.cs.uiuc.edu/VisualWorks/How+to+get+rid+of+Objects+in+Smalltalk)). So from a pragmatic viewpoint Smalltalk blocks are good enough as they are. But I'm not interested in being pragmatic here. This is an exercise in looking at familar things in an unfamiliar perspective, or mixing a new ingredient into the standard mix just to see what happens. Smalltalk is a pretty good chemistry set.

Today we continue our exploration by adding this method to the Symbol class:

```
asBlock
	| arity |
	arity := self numArgs + 1.
	arity = 1 ifTrue: [^[:r | r perform: self]].
	arity = 2 ifTrue: [^[:r :a | r perform: self with: a]].
	arity = 3 ifTrue: [^[:r :a :b | r perform: self with: a with: b]].
	"... and to keep the example simple..."
	self error: 'too many arguments'
```

As advertised by the selector, it turns a symbol into a block. The block sends the symbol as a message to the first argument, passing the remaining arguments as the arguments of the message. Given this method, we can write:

```
#('Hello' 'world') collect: #size asBlock
```

to collect the sizes of the collection elements.

That's right, this reminds the (in)famous `Symbol>>value:` hack, however it avoids the problems the hack has.

Consider the meaning of `numArgs`. This message can be sent to both blocks and selectors to determine how many arguments they take. Symbol\>\>value: pretends that symbols are the same as blocks. Unfortunately, considered as a selector `#isLower` has zero arguments, while considered as a block it has one. The same holds for any other selector: a Symbol's `numArgs` doesn't count the receiver as an argument, while the receiver does become an argument of the block the selector pretends it is. (The reason `arity` in the code above is `numArgs + 1`).

In practice this means that if we pass a Symbol such as `#isLower` to code that explicitly checks the arity of a block it receives by doing something like

```
aBlock numArgs = 1 ifFalse: [self error: 'Invalid block'].
^aBlock value: foo
```

the code will reject it, even though `#isLower` was supposed to pass for a one-argument block.

Furthermore, `Symbol>>value:` does nothing of value (sorry) for selectors of more than zero arguments. In contrast, `asBlock` is a uniform mechanism to cross over from the domain of selectors to the domain of blocks equivalent to those selectors sent to the block's first argument. In particular, binary and one-argument keyword selectors can mix well with `inject:into:`, `fold:` and `with:collect:`.

```
#(1 2 3) with: #(4 5 6) collect: #@ asBlock
#(20 20 42 16) inject: 0 into: #max: asBlock
(1 to: 6) fold: #* asBlock "6 factorial"
#('Hello ' 'world' '!') fold: #, asBlock
```
