---
layout: post
title: A Taste of Nested Classes, part 4
date: 2009-03-08 21:15:07.000000000 -07:00
categories:
- newspeak
tags: []
permalink: "/2009/03/08/a-taste-of-nested-classes-part-4/"
---
<p>(Continues from the <a href="http://blog.3plus4.org/2009/02/22/a-taste-of-nested-classes-intermission/">intermission</a>).</p>
<p>In the previous examples I sometimes referred to the familiar Smalltalk library classes such as OrderedCollection as if it were the most natural thing to do. But what does something like</p>
<pre class="smalltalk">
OrderedCollection with: 'foo'
</pre>
<p>really mean when found in a Newspeak method? It's clear that <code>with:</code> is sent to OrderedCollection, but what is OrderedCollection?</p>
<p>OrderedCollection is, of course, a message. It's sent to an implicit receiver, meaning we first look for a matching method in the lexical scope and send the message to the corresponding outer object if we find it. If not, we send the message to <code>self</code> and do the standard Smalltalk message lookup. It's reasonable (and correct) to assume that Object and other superclasses don't define a method named OrderedCollection, and that second lookup is bound to fail. Which means that in order for the above to work, one of the outer classes has to implement OrderedCollection as a method doing the right thing.</p>
<p>Let's pause here and summarize the important points.</p>
<p>First, there are no variable references in Newspeak. Any expression that begins with <code>foo</code> or <code>Foo</code> begins its evaluation by sending that message to an implicit receiver. This applies to anything that looks like a global variable, an instance variable, and even (at least conceptually) a temporary variable in the traditional Smalltalk understanding. Even pseudo-variables like <code>self</code> or <code>true</code>, as we've seen in the intermission, could be implemented as message sends.</p>
<p>Second, because there are no variable references, there can't be such a thing as the global scope. We can't just say OrderedCollection anywhere in the code and expect to get one. But if that is so, how can we use library classes at all?</p>
<p>Let's start by fleshing out a more concrete example. </p>
<pre class="smalltalk">
class AsteroidsGame = ( )
(
    class Screen = ( 
        | asteroids = OrderedCollection new |
    ) (
    ...
    )
)
</pre>
<p>Here we want a slot of the nested class to hold an instance of OrderedCollection. Initializer code is no different from method code, so OrderedCollection here is an implicit receiver send. Someone on the outside has to understand that message and nobody does in this example, so it can't possibly work. Let's try modifying it like this:</p>
<pre class="smalltalk">
class AsteroidsGame usingOrderedCollection: orderedCollectionClass = (
    | OrderedCollection = orderedCollectionClass |
) (
    class Screen = (
        | asteroids = OrderedCollection new |
    ) (
    ...
    )
)
</pre>
<p>This is much better because it can actually work. The top-level class is now instantiated with the <code>usingOrderedCollection:</code> message. The creator is expected to pass an OrderedCollection class metaobject to the instance. (For now let's not worry where the creator gets it). The game instance stores the metaobject in a slot named OrderedCollection. Because defining a slot automatically defines accessors (in this case only a getter because of the "=" syntax which defines read-only slots), the AsteroidsGame instance now understands the <code>OrderedCollection</code> message. The implicit receiver send of <code>OrderedCollection</code> in the Screen class initializer will now be directed to the outer instance of AsteroidsGame and return the OrderedCollection class.</p>
<p>This example isn't final yet, but it shows something very important. The top-level AsteroidsGame class is actually a module! It holds together the classes nested inside, and acts as a namespace for their names. But most importantly, it controls their implementation dependencies. The only way a class inside a module can use something from the outside is if that something has been explicitly declared as a requirement by passing it into the module and storing it in a slot.</p>
<p>Of course, passing each class that a module needs as a separate argument to its initializer would be incredibly tedious. Here is the final iteration of our example, this time showing how it's done for real:</p>
<pre class="smalltalk">
class AsteroidsGame usingPlatform: platform = (
    | 
    private OrderedCollection = platform Collections OrderedCollection.
    private ReadStream = platform Streams ReadStream.
    private PlatformScreen = platform Graphics Screen. 
    |
) (
    class Screen = ( 
        | asteroids = OrderedCollection new |
    ) (
    ...
    )
)
</pre>
<p>Instead of passing each dependency separately, we pass in an object we call the platform. A platform is a "supermodule"---an object that holds together a group of modules and makes them available through messages like <code>Collections</code>. The collections module returns the OrderedCollection implementation in response to the <code>OrderedCollection</code> message. The slot initializers in AsteroidsGame are now essentially "import statements," binding the required classes to names local inside the module.</p>
<p>As an aside, this is reminiscent of the idiom one occasionally sees in Scheme, where values of some names are bound to the same names but local to a closure:</p>
<pre>
(let ((car car)
      (cdr cdr))
    ...)
</pre>
<p>to capture the known "standard" values and ensure they are used regardless of the possible reassignments of the outer variables. The precise motivation is different but the mechanism is similar, especially if the <code>let</code> is expanded into the equivalent function call.</p>
<p>Another change we introduced is the private modifier on the imported slots. This means that the corresponding accessors are only available via messages sent from the same object or from the instances nested inside. (Though this is not enforced in the current Newspeak prototype). That is a good, and I'd even say required, style in this particular case because it insulates the dependencies of this module. Without that anyone could rely on retrieving the OrderedCollection class from an instance of AsteroidsGame, creating an undeclared and uncontrolled "derivative dependency."</p>
<p>Finally, the last import illustrates how we can locally rename an imported value to avoid a conflict with a name used inside the module.</p>
<p>What about star imports: just grabbing everything from another module to avoid the hassle of listing every dependency we need by hand? That's exactly the problem with star imports: they allow using a name without declaring that name as a dependency. It's a misguided convenience that defeats the very modularity mechanism it is a part of. Even in languages that support star imports, good programming practices dictate avoiding them. In Newspeak they are simply not supported.</p>
<p>And finally, how do we actually use modules? If "actually" refers to the current prototype of Newspeak hosted in Squeak, the procedure I am about to describe is not the final idea.</p>
<p>In the prototype, a top-level Newspeak class resides in the Smalltalk system dictionary. Because message sends in Smalltalk and Newspeak mean the same thing, it is possible for Smalltalk code to communicate with Newspeak. We can bootstrap a Newspeak module by writing code like the following in a Smalltalk workspace (or in a menu item implementation method):</p>
<pre class="smalltalk">
module := AsteroidsGame usingPlatform: Platform new.
gameScreen := module Screen new.
gameScreen openWindow.
</pre>
<p>The first line instantiates the module, the second instantiates the module's Screen class, the third tells the instance to open a window. Platform is a special class included in the prototype to give the Newspeak world access to what's available in the surrounding Smalltalk world. When Newspeak code executes</p>
<pre class="smalltalk">
platform Collections OrderedCollection.
</pre>
<p>the platform's <code>doesNotUnderstand:</code> method kicks in and retrives the standard Smalltalk OrderedCollection, making sure it is indeed part of the Collections package. Again, this is a temporary bootstrapping hack and not the final idea--
though even as a hack it illustrates the power of modularity based on late-bound message sends.</p>

For the final idea, see Gilad's post about [living without global namespaces](http://gbracha.blogspot.com/2008/12/living-without-global-namespaces.html). Combined with the ability to serialize and deserialize objects (and module instances are objects too), this ability to mix and link different modules has many exciting uses. Here are just a couple of scenarios.

You have an application that relies on the platform version 3.0. However, you'd also want to use another application written back in the times of version 2.1. Imagining we have an object responsible for module management called (say) CentralServices, launching both applications at the same time is as easy as

```
platform30:: CentralServices platformVersion: '3.0'.
platform21:: CentralServices platformVersion: '2.1'.
(AnApplication usingPlatform: platform30) MainWindow new open.
(AnotherApplication usingPlatform: platform21) MainWindow new open.
```

We could just as easily try and plug platform30 into AnotherApplication to see if it can in fact run with the latest version of the platform, and we could do that without even shutting down the already running AnotherApplication linked to the older version.

This leads to another scenario which as a tools developer I'm really excited about. Working on tools in a Smalltalk-like system is great because of the instant feedback, but also dangerous because you can break the very tools you need to fix the breakage. An idea has been floating in the Smalltalk world since (at least) mid-90s to separate the developer and the developed images. You'd have your tools running in one image, working remotely on the objects living in another image. The technology is there, and there have been various prototypes and implementations, but strangely none of those gained momentum. In Newspeak the solution is much simpler. Similar to creating a new instance of AnotherApplication module to try it with the new version of the platform while using the original, we can have two tools module instances at the same time, one running the tools we are using and the other representing the tools we are working on.

This shows an interesting difference between the roles images play in Smalltalk and in Newspeak. In Smalltalk an image is a manifestation of a specific platform version and configuration. We say things like "I tried running this in a 3.1 image with Foo loaded." In Newspeak an image is only an object universe that can host a mix of applications and libraries of any imaginable version and configuration.

