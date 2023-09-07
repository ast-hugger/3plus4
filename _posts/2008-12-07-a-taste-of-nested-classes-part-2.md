---
layout: post
title: A Taste of Nested Classes, part 2
date: 2008-12-07 23:08:54.000000000 -08:00
categories:
- newspeak
tags: []
permalink: "/2008/12/07/a-taste-of-nested-classes-part-2/"
---
<p>(Continues from <a href="http://blog.3plus4.org/2008/12/04/a-taste-of-nested-classes-part-1/">part 1</a>)</p>
<p>Let's now look at some message sends. Suppose we design the application so that the message <code>respondToHit</code> is sent to an asteroid when it's hit by a missile. In Smalltalk, a typical response would look something like</p>
<pre class="smalltalk">
respondToHit
    game
        replace: self with: self fragments;
        incrementScore.
</pre>
<p><code>game</code> here is an instance variable holding a reference to the Game instance the asteroid belongs to, presumably initialized at the time the asteroid was created. How could we port this to Newspeak? Translating everything literally, we would define a slot named "game" in the Asteroid class and port its initialization logic. We would then port all the methods. The original <code>respondToHit</code> would remain unchanged, apart from the overall "packaging":</p>
<pre class="smalltalk">
respondToHit = (
    game
        replace: self with: self fragments;
        incrementScore. 
)
</pre>
<p>But is it all the same as before? Not quite, because just like in Self, Newspeak slots are accessible only through messages. What looks like a good old <code>game</code> variable reference has now become a send of the message <code>game</code> to (an implicit) self.</p>
<p>Let's hold this thought and backtrack to the end of the previous post. We said that an asteroid always knows the game instance it belongs to, it's the enclosing object. We don't need the <code>game</code> slot we ported from the Smalltalk original because it holds onto the same thing!</p>
<p>We can drop it and any of its initialization logic because the language now keeps track of the enclosing game instance for us. Instead suppose we define this method in Asteroid:</p>
<pre class="smalltalk">
game = (
    "Get and return the enclosing object. To avoid getting ahead
    of the presentation, we don't show how it's done yet."
)
</pre>
<p>As for <code>respondToHit</code> and any other methods, they are unchanged---a perfect example of the benefits of representation independence. When <code>game</code> is just a message send, nobody cares how it gets its result.</p>
<p>Now, instead of revealing how the method <code>game</code> is implemented, we are going to do something even better. We are going to get rid of it altogether.</p>
<p>Time to talk about implicit receiver sends. I've hinted more than once that an implicit receiver is not always self. If it's not self, what else can it be? Is there any other object that is somehow implicitly "present" at the location where the message is sent? Of course, it's the enclosing object. (Or objects, if there is more than one layer of nested classes). Think of it this way: the <code>respondToHit</code> method is contained not only in the Asteroid class, but also indirectly in the Game class. So, the code in the method runs not only in the context of the current receiver, an instance of Asteroid, but also in the context of the enclosing instance of Game.</p>
<p>Without further ado I'm going to show <code>respondToHit</code> rewritten to take advantage of implicit receiver sends, and then describe their exact behavior. This will also explain under what specific conditions such a rewrite would be possible.</p>
<pre class="smalltalk">
respondToHit = (
    replace: self with: fragments.
    incrementScore.
)
</pre>
<p>This reads almost like plain English. <code>replace:with:</code> and <code>incrementScore</code> are now sent to the enclosing game via the implicit receiver mechanism. Note that <code>fragments</code> is also changed to have an implicit receiver, though in this case we expect that the receiver is self. How does it happen that in both cases the actual receiver is what we want it to be?</p>
<p>Let's recap how these potential receivers are related. We have essentially a queue of them: first self, then its enclosing object. (With more than two levels of nested classes, it would be followed by the enclosing object of the enclosing object and so on). If we represent the situation as a picture of the classes of the objects involved and their inheritance, we get this comb-like structure (to make the picture a little more interesting, Asteroid here is subclassed from a hypothetical ScreenObject, even though our original code didn't say that).</p>
<p><img src="{{ site.baseurl }}/assets/images/2008/12/nested-classes-comb.png" alt="Classes potentially involved in message processing" title="nested-classes-comb" width="282" height="173" class="alignnone size-full wp-image-100" style="padding-top:1em; padding-bottom: 1em;" /></p>
<p>Because a method with a matching selector might be defined by any class in the comb, it may <em>seem</em> that the most intuitively reasonable lookup strategy is the one adopted by NewtonScript. The policy there was to search the entire comb starting with the receiver and its superclasses, then the receiver's enclosing object (called <em>parent</em> in NewtonScript) with its superclasses, and so on. In the example we would look first in Asteroid, ScreenObject and Object with the intent of the Asteroid instance being the receiver, then Game and Object with the intent of the Game instance being the receiver. The send would fail if none of the classes in the comb implemented a method to handle the message.</p>
<p>I emphasized "seem" because the strategy in Newspeak is different, for the reason I am outlining in the end of the post. But first, here is how it really works:</p>
<ul>
<li>If one of the classes on the "trunk" of the comb implements a method with a matching selector---in other words, if a matching method is lexically visible at the send site---the message is sent to the instance of that class.</li>
<li>Otherwise, the message is sent to self.</li>
</ul>
<p>Even more informally, we could say that when by "looking around and up," "around" meaning the same class definition and "up"---the definitions it's nested in, we can see one with a matching method, the instance of that class is what receives the message. Otherwise, it is sent to self (and results in an MNU if self does not understand it).</p>
<p>So, the lookup in Newspeak differs from NewtonScript-like approach in two important ways. First, it doesn't traverse the entire comb. The only potential handlers of the message are the methods defined in the trunk classes and those in the superclasses of the class of self. Second, lexical visibility takes priority over inheritance. If a method of Asteroid sends the message <code>redraw</code> to an implicit receiver and a method named <code>redraw</code> is defined in both Game and ScreenObject, the ScreenObject implementation wins in NewtonScript, while the one in Game wins in Newspeak.</p>
<p>This explains how the correct receiver is chosen in our implementation of <code>respondToHit</code>. As long as <code>replace:with:</code> and <code>incrementScore</code> are defined in Game, they are sent to the enclosing game instance as lexically visible. As for <code>fragments</code>, it's sent to the asteroid no matter whether it's defined in Asteroid or inherited. If Asteroid defines it, it's lexically visible and the asteroid receives it according to the lexical visibility rule. If ScreenObject defines it (and assuming Game doesn't), it's lexically invisible and the asteroid receives it because the asteroid is self.</p>
<p>What if Game also had a method named <code>fragments</code>? In that case the definition in Game would win, so in order to get the fragments of the asteroid we would need to write the send explicitly as <code>self fragments</code>. What if <code>replace:with:</code> is inherited by Game from the superclass? In that case we also need to make the receiver explicit, because otherwise it would go to self:</p>
<pre class="smalltalk">
game replace: self with: fragments.
</pre>
<p>This brings us back to the problem of writing a method named <code>game</code> returning the enclosing instance of Game. Now we know enough to come up with a very simple solution: we add the method to the class Game (not Asteroid), defined as</p>
<pre class="smalltalk">
game = (
    ^self
)
</pre>
<p>So, now that we know how it all works---why does it work this way? Why bring lexical scoping of messages into the picture? Is it Rightâ„¢ to give priority to methods of another object over inherited methods of the receiver? Why not just follow the NewtonScript example---it appears simple and reasonable enough. </p>
<p>The thing is that compared to NewtonScript, class nesting in Newspeak is intended to solve a very different problem.</p>
<p>NewtonScript is prototype-based, and its object nesting is motivated by the need to represent the nesting of UI elements and support communication within that structure. The nesting and the associated message processing is essentially a built-in Chain of Responsibility pattern, very common in the UI field. (As an aside, Hopscotch implements NewtonScript-like mechanism for sending a message up the chain of nested UI elements. This is implemented as a simple library facility with no more language magic than <code>doesNotUnderstand:</code>. Implicit receivers further help make this very unobtrusive).</p>
<p>In Newspeak, even though the "free" back pointer from Asteroid to Game in our example was nice to have, having it was not the primary reason for nesting one class in the other. In fact, class nesting is a rather unwieldy mechanism for implementing arbitrary parent-child relationships---
imagine having to define a group of classes for the scenario of "buttons and list boxes in a window," and an entirely different one for "buttons and list boxes in a tab control in a window"!</p>

Newspeak-like class nesting and method lookup are designed to handle relationships that have more to do with the modular structure of code, and with the organization of information sharing within a module. A is nested in B when B is a larger-scale component as far as the system architecture is concerned. Even though such nesting does effectively create a parent-child relationship between Bs and As, there are parent-child relationships that do not justify nesting.

In nested Newspeak classes, a child calls onto the functionality provided by the parent not as a fallback case in chain-of-responsibility processing, but as a way of interacting with its architectural context. The lookup policy supports that and at the same time allows to modularize the context. That is the motivation behind putting lexical visibility before inheritance.

If this sounds somewhat vague, I hope some of the more realistic examples in the next part will help clarify this point.

Continues in [part 3](http://blog.3plus4.org/2009/02/15/a-taste-of-nested-classes-part-3/).

