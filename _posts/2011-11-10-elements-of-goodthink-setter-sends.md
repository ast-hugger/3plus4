---
layout: post
title: 'Elements of Goodthink: Setter Sends'
date: 2011-11-10 11:40:30.000000000 -08:00
categories:
- newspeak
tags: []
permalink: "/2011/11/10/elements-of-goodthink-setter-sends/"
---
Smalltalkers are in a pretty good position to understand Newspeak code out of the box. Unfamiliar constructs such as nested classes and the related message lookup semantics need some experience to internalize, but they are usually only an obstacle to understanding the higher-level design. Within a method everything should look familiar, except perhaps for setter sends.

A setter send is an expression like

```
foo:: a doSomethingWith: b
```

For a Smalltalker, the short explanation would be to say it's just like

```
foo := a doSomethingWith: b
```

Like most short explanations, it's a useful lie. Typically when you see something like the above, there is indeed a variable or a slot named "foo" somewhere in the scope, and the expression sets the value of that slot. But it doesn't have to always be the case.

One half of the true story is that the expression is more like

```
foo: (a doSomethingWith: b)
```

It is a message send to an implicit receiver, and the message is a plain keyword message "foo:" with a single colon. Doubling the colon makes it get parsed at a lower precedence so we don't have to parenthesize the argument (without parentheses that would be an implicit receiver send of "foo:doSomethingWith:").

Remember that there is no assignment operator in Newspeak. To set a slot, you invoke its setter method using an expression like the above. Without this syntax, most of those would need parentheses. We set slots quite often, so that would be a lot of parentheses. This is why the syntax is called a _setter send_, even though strictly speaking it does not have to invoke a setter of a slot. It's a regular message send going through the regular message lookup procedure.

The second half of the story has to do with the value returned by the send. The value of

```
foo: bar
```

is the value returned by "foo:" The value of

```
foo:: bar
```

is "bar". The value returned by "foo:" is thrown away. So after all, while a setter send is a regular message send as far as message lookup goes, its value is different from a regular message send. This is to recognize the fact that a setter send is usually used in place of an a assignment. Assignments can be chained, and this behavior guarantees that a chain of setter sends

```
foo:: bar:: baz
```

passes the same value to "foo:" regardless of now "bar:" is implemented.

To condense all of the above into a true short story,

```
foo:: a doSomethingWith: b
```

is just like

```
[:v | foo: v. v] value: (a doSomethingWith: b)
```
