---
layout: post
title: How to Learn to Stop Worrying and Love Implicit Receivers
date: 2008-11-30 20:41:01.000000000 -08:00
categories:
- newspeak
tags: []
permalink: "/2008/11/30/how-to-learn-to-stop-worrying-and-love-implicit-receivers/"
---
<p>I remember a thread on squeak-dev sometime in summer 2007 discussing implicit self as a hypothetical feature for Smalltalk. Back then we were not yet talking about Newspeak, and so I held my peace. But not forever---
Andres recently wrote [a blog post about the same thing](http://blogten.blogspot.com/2008/11/on-implicit-self.html), with a good summary of most of the same points I remember from the thread. This time Newspeak is known out there and explicitly mentioned in the post, so it's a good chance to offer an alternative view and correct some misconceptions.

Here is the most important one. Quoting from Andres' post:

> I find that the consistency offered by a few keystrokes makes it easier for me to read and understand code faster and more accurately. Therefore, since we read code much more often than we write it, I think that favoring reading speed over typing speed is the right decision to make.

As far Newspeak is concerned, what we have is _not_ "implicit self", and its purpose is not saving keystrokes. What Newspeak has are _implicit receivers_. Because of class nesting, a message with an implicit receiver may really be sent to an object different from the "real" self (the receiver of the current message). This feature is very important in supporting the minimalist module system of Newspeak. Thus, an implicit receiver is not simply an omitted self, and inserting "self" into a message send with an implicit receiver is not a behavior-preserving transformation.

Next point:

> I'd rather see self than having to assume it by scanning the first token until the first occurrence of $: (or '::') to only then be able to disambiguate between a receiver and a keyword.
> 
> In short: I prefer the work of my internal parser to be made easier by the use of prefixes, rather than to have to keep a stack that only goes down in the presence of a suffix.

This aurgmnet is falwed for the smiple raeson that our percpetion dose'nt wrok this way. We do'nt hvae an intarenl parser. What we rellay hvae culod be desrcbeid as a comlepx adpative, preditcive and bakcptaching paettrn recogznier. This is why we can still read the above even though most of the words are messed up. We don't scan the text linearly one character and one token at a time. Words are pictures, not character arrays. The structure of lines such as:

```
foobar baz.
foobar: baz.
foobar:: baz: foo.
```

and the presence or absence of : or :: in each are perceived by an experienced reader in one "processor cycle". In short, this is a usability issue, and it's a mistake to see it as an engineering one.

Those left unconvinced should also consider that modern IDEs, Newspeak's Hospcotch included, do the parsing for you by colorizing the source code.

So it's by far not a given that implicit receivers reduce readability. On the other hand, there are situations when they improve readability by eliminating noise. A good example are DSLs embedded in Newspeak. So far we have two such languages widely used in the system: Gilad's [parser combinators](http://gbracha.blogspot.com/2007/01/parser-combinators.html) and my UI combinators in Hopscotch. The feature common to both are definitions written in a declarative style combining smaller things into larger ones. Compare an example of such a definition the way it's commonly written:

```
heading: (
    row: {
        image: fileIcon.
        label: fileName.
    })
details:
    [column: folderContents]
```

with the same definition with explicit receivers:

```
self heading: (
    self row: {
        self image: self fileIcon.
        self label: self fileName.
    })
details:
    [self column: self folderContents]
```

The first example has nothing but the structure it defines. It's important what the expressions say. The fact that they are message sends is an implementation detail. The second example leaks this implementation, and it takes some effort to see what it really says in between all the "self"s.

And the final point. There's much to be said about the human nature and the tendency to instinctively resist change to something familiar while trying to rationalize that resistance. It takes some time and experimenting to see a change for what it is and get a feel of the new tradeoffs. I don't know how many people with a considerable Self expertise are out there, but I doubt there are too many. I do know all the experienced Newspeakers. Which is to say, the time for conclusions hasn't come yet.

**Update:**

[Peter AhÃ©](http://digital-sushi.org/) pointed out offline that I didn't mention another important property facilitated by implicit receivers in Newspeak as well as in Self: [representation independence](http://gbracha.blogspot.com/2007/01/representation-independent-code.html).

