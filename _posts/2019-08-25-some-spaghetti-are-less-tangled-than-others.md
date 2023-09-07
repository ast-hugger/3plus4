---
layout: post
title: Some spaghetti are less tangled than others
date: 2019-08-25 22:11:53.000000000 -07:00
categories:
- general
tags: []
permalink: "/2019/08/25/some-spaghetti-are-less-tangled-than-others/"
---
They say if you can't explain it to a six year old, you don't understand it yourself. Supposedly it was Einstein who said that. Though he also said [he didn't say half of what they say he did](https://me.me/i/i-never-said-half-the-shit-people-say-idid-albert-86ee8c76f0bf4db18eb311a337543722). Anyway, how would one explain to a six year old delimited continuations implemented in terms of the standard `call/cc`?

[Respectable scholarly expositions](https://www.deinprogramm.de/sperber/papers/shift-reset-direct.pdf) typically relate to the underlying CPS-transformed structure to explain the logic. The metacontinuation is a fine concept, but explaining interaction of obscure concepts in terms of an obscure metaconcept has its drawbacks.

I am going to show a two-layered (three if you count the syntactic sugar) implementation of delimited continuations aimed not at affinity with theoretical models, or efficiency, but at making clear the mechanics of what's really happening. It's the result of refactoring what originally was very similar to the implementation in the paper.

We start with "stackable labels", a helper control mechanism with two primitives: `label!` and `jump!`. The latter may sound like the good old `goto`, though as we'll see it actually has more in common with `return`.

The `label!` expression, shown in this example as part of a larger expression

```
(+ (label! (foo)) (bar))
```

evaluates its argument `(foo)` and returns the result, so the whole expression is almost the same as simply

```
(+ (foo) (bar))
```

The difference is that as a side effect `label!` remembers the spot in the evaluation where the result of its invocation was expected. We call this spot a label (a noun; `label!` is meant as a verb). Note that I say it's a spot in the evaluation and not in the code. A `label!` expression may be evaluated multiple times, and the label it creates each time corresponds to that specific evaluation of `label!`. If this sounds vague, the examples below will clarify the difference. (And if this sounds like continuations, it's because that's what labels are below the surface--but `labels!` and `jump!` are there to hide and manage the underlying continuations, so we will avoid peeling back this abstraction).

A label created by an evaluation of `label!` is pushed onto an implicit label stack. A `jump!` expression such as

```
(jump! 42)
```

removes the top label from the stack and jumps to it. Jumping has the effect of the corresponding `label!` invocation returning with the value passed to `jump!`. The `jump!` expression itself never returns.

Here are two examples to build up some intuition about labels and jumps. They use Racket's non-standard but obvious `printf` function. First the basic mechanics:

```
> (printf "got: ~s~n" (label! (+ 3 4)))
got: 7
```

The result of `(+ 3 4)` got through to `printf` as expected, though as a side effect we've also saved a label:

```
> labels
'(#<continuation>)
```

We can now jump to that label:

```
> (jump! "hello")
got: "hello"
```

which has the effect of returning from `label!` again and proceeding with `printf`, this time with `"hello"` as the value to print. The jump also consumed the label and the label stack is now empty:

```
> labels
'()
```

The second example is just a slight variation on the first, but the interaction of two `labels!` expressions makes it quite a bit more interesting. It drives home the point about a label marking a spot in the evaluation and not just in the code.

```
> (printf "first: ~s second: ~s~n" (label! 1) (label! 2))
first: 1 second: 2
> labels
'(#<continuation> #<continuation>)
```

So far this is very much like the first example, except we've evaluated two `label!` expressions and pushed two labels onto the stack.

```
> (jump! 3)
first: 1 second: 3
> labels
'(#<continuation>)
```

The last label pushed was the one for `(label! 2)`, so jumping to it proceeded with `printf`, this time with 3 as the second value to print. At this point the first value was already ready to go, so 1 and 3 were printed. We've also consumed one of the labels and are left with the other one which marks the return of the result of `(label! 1)`. This is where it gets more interesting.

```
> (jump! 4)
first: 4 second: 2
> labels
'(#<continuation>)
```

Now we supplied 4 to the point where the first value to print was expected. However, at that point the second value was not yet available, so `(label! 2)` was evaluated again, producing 2. It also pushed a new label replacing the one the `jump!` consumed, so we still have one label on the stack and can jump again:

```
> (jump! 5)
first: 4 second: 5
> labels
'()
```

Now the jump emerged from `(label! 2)` evaluated in the previous step, with 4 supplied by the previous jump ready to go as the first value to print. Now we get 4 and 5 and have finally consumed all the labels.

The implementation of `label!` and `jump!` is pretty much a direct transcription of what we said they do. There is a stack of labels (continuations), `label!` pushes its own continuation onto it, and `jump!` pops the stack and passes its argument to the popped continuation.

```
(define labels '())

(define-syntax label!
  (syntax-rules ()
    ((push-label! expr)
     (call-with-current-continuation
       (lambda (k)
         (set! labels (cons k labels))
         expr)))))

(define (jump! v)
  (let ((k (car labels)))
    (set! labels (cdr labels))
    (k v)))
```

The explicit stack of continuations in `labels` directly corresponds to `*meta-continuation*` of the scholarly presentation and the structure of nested closures it contains. While less glamorous, the explicit stack is easier to follow and inspect.

Now, armed with stackable labels, the implementation of `reset` and `shift` (or rather their core as higher-order functions; syntactic sugar is unimportant) is trivial.

```
(define (reset/0 body)
  (label! (jump! (body))))

(define (shift/0 body)
  (call-with-current-continuation
    (lambda (k)
      (jump! (body (escaper k))))))

(define (escaper k)
  (lambda (v)
    (label! (k v))))
```

OK, maybe it's not completely trivial. But it's reduced to the bare essentials, and with control transfers abstracted by stackable labels the structure is much easier to explain. Especially if we do that in the following not very precise but very helpful imperative terms.

Stackable labels can be seen as an auxiliary call stack. The state of evaluation saved on it using `label!` stays available for returning into regardless of the "regular" calls and returns and their stack. The `jump!` operator is a "special return" for returning into the saved state.

In the beginning, `reset/0` enters a "special call" because it's entire body is wrapped in `label!`. Evaluation of both `reset` and `shift` bodies ends with a "special return". If a `shift` never invokes its escape function, as in either of the fragments

```
(reset (+ 3 4)) ; => 7
(reset (+ 3 (shift k 4))) ; => 4
```

the auxiliary stack only has an entry corresponding to `reset`. So, the bodies of both `shift` and `reset` return out of `reset`.

If a `shift` does invoke its escaper function, as in

```
(reset (* 2 (shift k (+ (k 3) (k 4)))) ; => 14
```

the escaper pushes a new entry onto the auxiliary stack. It then returns its argument from the `shift` expression by invoking the continuation `k` captured by `shift/0` (not part of the stackable labels scheme). Now the reset body is proceeding with that as the value of the `shift` expression. When the `reset` body completes, it "special returns" to the escaper invocation on top of the stack and not out of `reset` as it would do otherwise.

These are the two base cases. More complex structures, such as

```
(reset
  (display (shift k (k "a") (k "b")))
  (display (shift k (k 1) (k 2)))) ; prints "a12b12"
```

are simply a combination of these two scenarios.

Could a six year old understand this? Mine couldn't. But in any case, refactoring continuations-based code builds character and makes one a better person. I think it was Einstein who said that.

[The full source with sugar and examples](https://gist.github.com/ast-hugger/79738ad20d24fff826854268da44078d)

