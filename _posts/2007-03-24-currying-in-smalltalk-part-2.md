---
layout: post
title: Currying in Smalltalk, part 2
date: 2007-03-24 00:04:31.000000000 -07:00
categories:
- smalltalk
tags: []
permalink: "/2007/03/24/currying-in-smalltalk-part-2/"
---
In [the first part](http://blog.3plus4.org/2007/03/23/currying-in-smalltalk/) we changed the behavior of the `value:` protocol of blocks to support currying. That changes the existing behavior of the system, making it possible for those messages to answer a block instead of a "real" value. Is that a problem?

One could argue that the new behavior comes into force only in the situations where the original system would signal an error anyway, so if it falls over because of getting a block it didn't expect, it would still have fallen over in the old system because of the number of arguments mismatch. On the other hand, what if it doesn't fall over immediately after it gets that unexpected block? It is possible for the new behavior to mask an error and make it difficult to find its cause later. Also, some might not like this kind of a change on the familiarity grounds. [Gilad](http://gbracha.blogspot.com/) was the one who got me on the track of playing with currying, and for these reasons he suggested that it might be a good idea to keep `value:` and friends intact and introduce a different evaluation message that would allow currying--something like `curry:curry:`.

A similar in spirit approach, but one that gives us the best of both worlds, is to introduce a unary message `curried `used as a prefix to `value:` to indicate that currying is allowed. The example from the previous post then becomes

```
| add inc |
add := [:a :b | a + b].
inc := add curried value: 1.
(inc value: 2) + (inc value: 3)
```

This both preserves the familiar evaluation protocol and declares that currying is possible. To implement this, unlike in the Part 1 version, we leave `valueWithArguments:` unchanged from its standard implementation. Instead, we introduce a new class called `Currier` with an instance variable `block` and an initialization method `block:`, and add the following method to the block class (either BlockContext or BlockClosure, depending on the Smalltalk dialect):

```
curried
    ^Currier new block: self
```

This injects a Currier into the message stream as `add curried value: 1` is evaluated, so that the `value:` message is received by the Currier. To process it, the Currier needs to invoke the original block if there are enough arguments to do so immediately, or create a CurriedBlock otherwise:

```
value: anObject
    block numArgs = 1
        ifTrue: [^block value: anObject].
    block numArgs > 1
        ifTrue: [^CurriedBlock new block: block arguments: {anObject}].
    self error: 'Incorrect number of arguments'
```

The above assumes that an expression such as {anObject} creates an Array with anObject, as in Squeak or in VisualWorks with my BraceConstructor extension.

Other `value:...` protocol messages are implemented similarly. Also, the CurriedBlock class should implement the same `curried` method as "real" blocks do--curried blocks are no different from regular ones, so they can be "re-curried". (In Part 1 we would get such re-currying for free).

