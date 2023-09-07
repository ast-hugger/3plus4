---
layout: post
title: Currying in Smalltalk
date: 2007-03-23 00:35:14.000000000 -07:00
categories:
- smalltalk
tags: []
permalink: "/2007/03/23/currying-in-smalltalk/"
---
Currying, unless you are into Thai cooking, is partial function application commonly used in functional languages. (Some argue it should be called SchÃ¶nfinkelisation, but oddly so far the term hasn't caught on). Given a function of N arguments, one can invoke it with K arguments, K \< N. This produces a new "curried" function of N - K arguments, with the K arguments of the original call closed over (remembered).

In Smalltalk, currying would allow the example:

```
| add inc |
add := [:a :b | a + b].
inc := add value: 1.
(inc value: 2) + (inc value: 3)
```

run without errors and produce 7. This feels like a significant change down in the foundations of the universe, but amazingly it is trivial to implement.

Both in VisualWorks and Squeak, when a Block is invoked with a wrong number of arguments, message processing ends with a primitive failure in the `valueWithArguments:` method. The standard Smalltalk response to that is to complain about the wrong number of arguments. All we need is change that response to curry the receiver when appropriate.

To make the example shorter and nicer (we'll see why a little later), we define a class called CurriedBlock, with instance variables `block` and `arguments` and an obvious initialization method `block:arguments:`. We also change the standard `valueWithArguments:` as follows:

```
valueWithArguments: anArray
	<primitive: 500>
	(anArray isMemberOf: Array)
		ifTrue: [anArray size < self numArgs
			ifTrue: [^CurriedBlock new block: self arguments: anArray]
			ifFalse: [self error: 'Incorrect number of arguments']]
		ifFalse: [self error: 'valueWithArguments: requires an Array']
```

The method above is a simplification of the VisualWorks version, with the horrible UserMessages replaced by readable strings. The Squeak version is nearly identical.

CurriedBlock needs to reproduce the standard block protocol:

```
valueWithArguments: anArray
	^block valueWithArguments: arguments, anArray

value: anObject
	^block valueWithArguments: (arguments copyWith: anObject)_... other required value:... methods_numArgs
	^block numArgs - arguments size
```

This is all that is needed to make the example work.

In fact, we could do without the CurriedBlock class. Our implementation of `valueWithArguments:` could answer a real block by doing something like:

```
valueWithArguments: anArray
	...
		ifTrue: [anArray size < self numArgs
			ifTrue: [^self curriedWithArguments: anArray]
	...

curriedWithArguments: anArray
	| arity |
	arity := self numArgs - anArray size.
	arity = 1 ifTrue:
		[^[:arg | self valueWithArguments: (anArray copyWith: arg)]].
	arity = 2 ifTrue:
	...
```

However, since Smalltalk blocks don't support the equivalent of Lisp "rest" arguments, the `curriedWithArguments:` method would have to explicitly enumerate and handle all arities we realistically expect to use in block invocations. Using CurriedBlock instead produces a nicer example.

To be continued.

