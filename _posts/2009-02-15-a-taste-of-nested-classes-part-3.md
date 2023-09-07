---
layout: post
title: A Taste of Nested Classes, part 3
date: 2009-02-15 13:06:46.000000000 -08:00
categories:
- newspeak
tags: []
permalink: "/2009/02/15/a-taste-of-nested-classes-part-3/"
---
(Continues from [part 2](/3plus4/2008/12/07/a-taste-of-nested-classes-part-2/))

<p>After an unplanned hiatus caused by various real life issues including <a href="http://gbracha.blogspot.com/2008/11/we-have-good-news-and-we-have-bad-news.html">the irritating surprise of becoming unemployed</a> in these interesting times, here is the long-promised third part of the nested classes series.</p>
<p>Class nesting in our earlier example represented the natural nesting relationship of an asteroid object inside the game that contained it. Implicit receiver sends were a convenient mechanism of communicating with the game from inside an asteroid instance "for free", without the need to maintain an explicit object reference. Today we look at a few examples with implicit sends of a slightly different flavor. </p>
<p>In a typical Smalltalk system, methods in the Object class define the behavior common to all objects. Some of them, such as <code>class</code> or <code>identityHash</code> are implemented to return some interesting information extracted from the receiver. Others, such as <code>isNil</code> are overridden in subclasses to vary their behavior polymorphically, again depending on the receiver object. In both of these cases, the receiver is involved one way or the other.</p>
<p>There is a third kind, methods like <code>halt</code>, <code>error:</code>, or <code>flag:</code> (in Squeak). They don't extract any information from the receiver. Also, they are not overridden, so the implementation in Object tells the full story of their behavior. We send them to <code>self</code> and expect that they behave the same regardless of what <code>self</code> happens to be. As far as these messages are concerned, the receiver itself is unimportant.</p>
<p>These messages are utilities representing "ambient" functionality---something potentially useful everywhere in the code. We send them to <code>self</code> because it's the most readily available receiver, and they are implemented in Object so that the sends work regardless of what <code>self</code> happens to be. Object in this case doubles as the container of these utility functions and inheritance makes them globally visible.</p>
<p>This global visibility is what also makes such utilities potentially problematic. Suppose I want to have a <code>logError:</code> utility that writes an entry into a log file, and I want to use it throughout my application. It's fine to have it in Object only as long as another application doesn't decide to use the same message but log the error to the system console instead.</p>
<p>Here is an alternative way to implement such utilities in Newspeak.</p>
<pre class="smalltalk">
class AsteroidsGame = (...)
(
    ...
    class Space = (...)
    (
        moveObject: anObject to: position = (
            ...
            logError: 'invalid position'
        )
        ...
    )

    class Asteroid = GameObject (...)
    (
        respondToHit = (
            isLive ifFalse: [logError: 'invalid asteroid'].
            ...
        )
    )
    ...

    logError: message = (
        errorStream nextPutAll: message; cr.
    )
)
</pre>
<p>We put <code>logError:</code> in the top-level class representing our application. Any method of any nested class can invoke that utility through an implicit receiver send. The superclass of the nested class doesn't matter---
the utility is accessible because the class is nested in the one that provides it. Compared to the Smalltalk example, the utility is now available everywhere in our application and nowhere outside of it.</p>

Here is another thing that's interesting about the Newspeak alternative. In Smalltalk, `logError:` would have to be an extension method of Object. Extension methods are [officially ungood](http://gbracha.blogspot.com/2008/03/monkey-patching.html) in Newspeak, and the example shows the goodthink alternative.

In fact, such use of class nesting as a scoping mechanism for utilities solves an entire class of data conversion problems traditionally solved by extensions. Say, we want to convert an integer to a string with its representation as a Roman numeral. A common solution would be to add an `asRomanNumeral` extension method to Integer to do the conversion. This is convenient because we can write `x asRomanNumeral` anywhere, but has the disadvantage of the function leaking into the rest of the system. A more controlled alternative of implementing a `convertToRomanNumeral:` method in one of the application classes has the disadvantage of being easily accessible only in the implementing class and its subclasses. In other places we need to arrange for a reference to an instance that understands the message. In Newspeak, `convertToRomanNumeral:` can be a method of the application (module) so that it's easily accessible, but only within the module.

Here is an interesting hypothetical example that continues the same theme. Suppose we implement the following method in a top-level class:

```
if: condition <Boolean> then: then <Block> else: else <Block> = (
    ^condition ifTrue: then ifFalse: else
)
```

Anywhere in the class and its nested classes we can now write conditionals in this more familiar to the general public style:

```
if: a < b
then: [^a]
else: [error: 'invalid a']
```

This example is hypothetical because it could only be fast enough on a very sophisticated VM with a good adaptive optimizer. But as far as less common and therefore less time-critical control messages go, they are no different from other utilities. We can define them in the top-level application class and use them throughout the application. For example:

```
^return: 5 if: #bar notUnderstoodIn: [foo bar]
```

defined in a top-level class as

```
return: value <Object> if: selector <Symbol> notUnderstoodIn: block <[]> = (
    ^block
        on: MessageNotUnderstood
        do: [:ex |
            ex message selector = selector
                ifTrue: [ex return: value]
                ifFalse: [ex pass]]
)
```

To sum up, some methods define the kind of ambient functionality that is used throughout an application. For that reason it's best if it can be accessed implicitly, without arranging for an explicit reference to the object that provides it. In classic Smalltalk, the only mechanism to support that is inheritance, with the disadvantage that the functionality defined this way escapes the defining module. Nested classes and implicit receiver sends in Newspeak provide an alternative mechanism that keeps such ambient operations contained inside the module.

(Continues in [intermission](/3plus4/2009/02/22/a-taste-of-nested-classes-intermission/)).

