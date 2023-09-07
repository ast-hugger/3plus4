---
layout: post
title: 'Brazil Example: a Classic Smalltalk Browser'
date: 2008-11-23 10:23:58.000000000 -08:00
categories:
- newspeak
tags: []
permalink: "/2008/11/23/brazil-example-a-classic-smalltalk-browser/"
---
<p>Here is an example of a classic Smalltalk browser implemented in Brazil. While the main point here is to illustrate Brazil API, an important related point I want to make first is that in our system Brazil is <em>not</em> used the way this example shows (and neither do our browsers look like that).</p>
<p>A good analogy to Brazil's role in our UI is assembly language. One can program in it directly, but most of us these days  don't do that on any significant scale. In the same vein, one can think of Brazil as the UI assembly language. <a href="http://blog.3plus4.org/2008/06/20/which-is-which-in-newspeak-ui-hopscotch/">Earlier</a> I described Hopscotch as an application framework, but that was simply to associate it with something familiar to everyone. In reality, Hopscotch is more of a higher-level UI language that hides or recasts in a different form the details of the low-level language it is based on. And just like one usually isn't concerned with the exact translation of a high-level language program into machine code, a Hopscotch programmer doesn't work with Brazil API directly. </p>
<p><a href="http://blog.3plus4.org/wp-content/uploads/2008/11/classicbrowser.png"><img src="{{ site.baseurl }}/assets/images/2008/11/classicbrowser-150x150.png" alt="The picture of the browser." title="ClassicBrowser" width="150" height="150" class="size-thumbnail wp-image-76" /></a></p>
<p>In the example, we create a window with the usual four list boxes and a text view, and wire them up so that list selection works the way it works in Smalltalk. The screenshot shows the browser open as a native Vista UI, and browsing the Object class from Squeak. The window in the background is the Newspeak workspace with the expression I used to instantiate and open the browser. The part in parentheses has to do with class nesting and the use of nested classes to implement modules. If you haven't followed one of Gilad's explanations, feel free to ignore the parenthesised part for the purposes of this example. The remainder,</p>
<pre class="smalltalk">
ClassicBrowser new open
</pre>
<p>in the end produces the result a Smalltalker would expect---if not in the entirely expected way.</p>
<p>And here is the ClassicBrowser class itself. First, the class definition and the initializer:</p>
<pre class="smalltalk">
class ClassicBrowser = (
"A simple implementation of a read-only Smalltalk browser."
|
	window = Window new.
	categories = ListBox new.
	classes = ListBox new.
	protocols = ListBox new.
	selectors = ListBox new.
	code = TextView new.
|
	assembleUI.
	configureUI.
)
</pre>
<p>This may look almost like a method with a comment, a list of temporaries in a not-quite-Smalltalk syntax (but note the vertical bars flushed to the left), and two expressions in the body. The comment here is the class comment, and the "temporaries" are actually slot declarations with initializers. Slots mean instance variables with automatically generated accessor methods. All slots of this class are read-only because of the use of the equals sign tying each slot to its initializer. A read-only slot is simply one with a getter method but no setter. The slots hold onto Brazil visuals assembled and configured into a coherent interface by the two methods called from the initializer. Here is how the assembly is done.</p>
<pre class="smalltalk">
assembleUI = (
	| navigationRow |
	window area bounds: (200 @ 200 extent: 600 @ 600).
	window
		title: 'Brazil System Browser';
		content: Column new.

	navigationRow: Row new.

	window content
		add: navigationRow;
		addBlankSize: 3;
		add: code.

	navigationRow area height: 0; elasticity: 4.
	code area height: 0; elasticity: 6.

	navigationRow
		add: categories;
		addBlankSize: 3;
		add: classes;
		addBlankSize: 3;
		add: protocols;
		addBlankSize: 3;
		add: selectors.

	categories area elasticity: 1.
	classes area elasticity: 1.
	protocols area elasticity: 1.
	selectors area elasticity: 1.
)
</pre>
<p>The interesting point here all those "area" things the method talks to. Areas is a concept introduced in Brazil layout model to avoid the mess common in other UI frameworks. What's messy is that it's never clear who is in charge of a widget's layout, and how to specify that layout in a particular situation. For example, in Morphic all morphs understand the message <code>bounds:</code> that repositions the morph, as well as the more focused messages <code>position:</code> and <code>extent:</code>. There are also specialized ones like <code>vResizing:</code> or <code>cellSpacing:</code>, which may or may not work in a particular setup. Given that some parent morphs such as AlignmentMorph manage the layout of their children, what happens when I place a morph inside one and then send <code>position:</code> to it? And how can I find out which of the plethora of layout messages that Morph understands will actually work in a given context?</p>
<p>The general problem here is that the protocol used to control the layout of a child depends on the context (the context being first of all the parent who manages the layout). Thus, Morph needs to expose the union of all possible layout attributes, only some of which are applicable at any given time.</p>
<p>The solution in Brazil is simple and natural, though I don't know of other frameworks taking the same stance. A Brazil visual (the framework's term for a widget) is unaware of any layout attributes, and has no methods to change them. Layout parameters are managed by a separate object called the visual's <em>area</em>. The area is manufactured at the time a visual is added to the parent, as an instance of a class capturing the layout attributes meaningful for that particular visual in that particular parent. Thus an area represents the <em>capability</em> of positioning a widget inside the current parent. When a visual is a child of CompositeVisual (a free-form container similar to VisualWorks' CompositePart), its area allows to arbitrarily position the visual by sending the <code>bounds:</code> message. When the same visual is a child of a Row, the area is an instance of a different class whose attributes and behavior are meaningful for a row cell. Arbitrarily moving the child should not be possible in that situation, and it is indeed impossible since this kind of area does not even understand the <code>bounds:</code> message.</p>
<p>Besides excluding what is impossible, areas also nicely manage and document the possible layout strategies. For example, in one situation we may want to force a Button inside a CompositeVisual to have a particular size. In a different situation, we might want to allow the button to resize to match the size of its label. These two layout disciplines are captured by two area classes called Frame and Anchor, both of which are allowed as areas of a button inside a composite. Assigning one or the other to a Button inside a CompositeVisual selects the layout strategy we want to use for the button.</p>
<p>A third benefit of this design feature is that unlike traditional frameworks whose layout strategies are hard-coded, with individual widgets holding onto layout attributes and behavior to control it, Brazil allows for pluggable layout strategies. For example, to extend the framework with a container that positions its children using a linear constraint system, all that's needed is the container visual itself plus one or more area classes to capture the constraint parameters. None of the existing visuals require any changes to be used as children of the new container.</p>
<p>Back to the example. Now that the widgets are all assembled, the next method configures and initializes them:</p>
<pre class="smalltalk">
configureUI = (
	configureCategoryList.
	configureClassList.
	configureProtocolList.
	configureSelectorList.
	configureCodeView.
)
</pre>
<p>Everything is straightforward here (again, we are freely using implicit receiver sends to avoid the clutter of "self" everywhere). Here is just one of the individual configuration methods--
others are similar:</p>

```
configureCategoryList = (
	categories 
		list: Smalltalk organization categories;
		displayBlock: [:symbol | symbol asString].
	categories selectionChanged => (self ~ #categorySelected:).
)
```

Again, nothing unusual, except perhaps for the last line. The part in parentheses is a [section](http://blog.3plus4.org/2007/03/31/sections/), in this case a shorthand for

```
[:category | self categorySelected: category]
```

The message `=>` is part of Ducts, Brazil's change notification framework, a minimalist alternative to [Announcements](http://www.cincomsmalltalk.com/userblogs/vbykov/blogView?searchCategory=Announcements%20Framework) (two classes and under 20 public methods, with all the same capabilities). It ties a change notifier supplied by the `selectionChanged` method to the section, so that whenever the selection changes the `categorySelected:` message is sent to the browser.

The `categorySelected:` method is straightforward again.

```
categorySelected: selection <Symbol | nil> = (
	selection
		ifNil: [classes list: Array new]
		ifNotNil: 
			[| names |
			names: (Smalltalk organization listAtCategoryNamed: selection).
			classes list: (names asSortedCollection collect: [:each | Smalltalk at: each])]
)
```
