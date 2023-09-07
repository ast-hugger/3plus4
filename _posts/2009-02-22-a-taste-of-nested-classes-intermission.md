---
layout: post
title: A Taste of Nested Classes, intermission
date: 2009-02-22 19:38:33.000000000 -08:00
categories:
- newspeak
- what-if
tags: []
permalink: "/2009/02/22/a-taste-of-nested-classes-intermission/"
---
(Continues from [part 3](/2009/02/15/a-taste-of-nested-classes-part-3/))

This post is an intermission because it's not specifically about nested classes. In fact, it's not about anything that really exists in Newspeak. Instead, it reviews a few hypothetical examples to show off some of the expressive capabilities of implicit receiver sends.

Most expressions in Newspeak begin with an implicit receiver send, at least conceptually. In something like

```
foo size
```

or

```
OrderedCollection new
```

both foo and OrderedCollection really are message sends, for some definition of "really".

Of course, in some cases we can't do without real receivers. There are literals, as well as traditional pseudo-variables ("reserved words"), whose interpretation should be hard-coded into the language.

Or should it be?

Starting with literals, imagine that every occurrence of a literal is compiled as an implicit receiver send identifying the type of the literal, with the "primitive" value of the literal as the argument. For example,

```
'Hello, brave new world!'
```

would really mean

```
(literalString: 'Hello, brave new world!')
```

with `literalString:` implemented in Object as

```
literalString: primitiveValue <String> = (
    ^primitiveValue
)
```

At this point this is only a possibility, but one that is explicitly mentioned in [the language spec](http://bracha.org/newspeak-spec.pdf) (5.1). If implemented, this would allow user code to redefine the interpretation of what appears to be literals for greater flexibility when implementing internal DSLs.

As for pseudo-variables, consider this implementation of the class Object:

```
class Object = (
    ...

    self = (
        "empty - return the receiver"
    )

    true = (
        ^True soleInstance
    )

    false = (
        ^False soleInstance
    )

    nil = (
        ^UndefinedObject soleInstance
    )

    thisContext = (
        ^ActivationMirror forSenderContext
    )
)
```

With this, most of the reserved words of Smalltalk become regular messages. The expression

```
^self foo: nil
```

now really means

```
^? self foo: ? nil
```

with ? denoting a reference to (usually!) the receiver unrepresentable in the language syntax. We use the fact that an empty method effectively reads as

```
^?
```

to grab that elusive ? in the `self` method.

`super` is more complicated because it doesn't denote a separate object but rather `self` with a special mode of message lookup. However, even that can be lifted from language magic into a reflective implementation at the library level:

```
super = (
    ^SuperDispatcher forReceiver: self
)
```

with SuperDispatcher implementing `doesNotUndestand:` to reflectively look for the method implementing the message starting from the superclass of the implementor of the sender context's method. (A mouthful, but that really is what it should do).

In a similar way, `outer`, the new reserved word of Newspeak not found in Smalltalk could be implemented as a message send handled by reflective code at the image level.

The last two examples, `super` and `outer`, are even more hypothetical than the other pseudo-variables. While technically possible, a reflective implementation of those is quite expensive compared to the usual "language magic" solution.

Also, this scheme in general leaves the meaning of things like `self` or `nil` open to changes by a class for all its subclasses and nested classes. Perhaps there is such a thing as too much flexibility. But even if these ideas are too much or too expensive for the core language, all of the same mechanisms are available to a designer of an internal DSL, and that's what makes them interesting. For example, the "dispatcher receiver" pattern illustrated with `super` and `outer` was conceived in Hopscotch and used there to support [NewtonScript](http://waltersmith.us/wp-content/uploads/2005/12/OOPSLA95.pdf)-like object hierarchy-bound message dispatch.

(Continues in [part 4](/2009/03/08/a-taste-of-nested-classes-part-4/)).

