# Async/await implementation in Haskell

**Disclaimer:** This is not an officially supported Google product.

## Goals

In this package we explore how to implement the
[__async/await__](https://en.wikipedia.org/wiki/Async/await) programming
pattern such that:

-   It allows to write asynchronous computations in synchronous style:
    use `async` to schedule computations in other threads and `await` for them
    to finish.
-   Implement the internals in purely asynchronous way. That is, _no thread must
    ever wait blocked_. All threads must either do work or finish.
-   Keep the implementation idependent of a particular scheduling/threading
    implementation.

The second condition ensures that even if there is limited amount of threads,
thread starvation won't occur. In particular, computations can be run even in a
single-threaded environment and are sequenced appropriately.

Note: This is somewhat different approach from the one taken by the
[async](http://hackage.haskell.org/package/async) Haskell package, which relies
on Haskell threads and blocking waits on compuations using STM.

Note: Currently exceptions are not dealt with.

### Future plans/ideas

-   Exceptions. An exception thrown within an `async` computation should be
    raised in all others that `await` on it, rather than dying silently.
-   Race. For two or more asynchronous computations return the first available
    result. Note that such an addition makes the computations indeterministic in
    the sense that the outcome depends on the order of evaluation even in cases
    when computations cannot observer each others' side effects. On the other
    hand, some concurrency applications cannot be implemented without racing.
-   Cancellation (if feasible).

## Implementation

The [continuation
monad](https://wiki.haskell.org/All_About_Monads#The_Continuation_monad) allows
a computation to explicitly access the whole code that follows it. Instead of
returning its result, it passes it as an argument to a
[continuation](https://en.wikipedia.org/wiki/Continuation). Among other things,
this allows a computation to suspend itself by storing the continuation
somewhere, and later and resume it.

When specialized to `(a -> IO ()) -> IO ()`, we can work with asynchronous `IO`
operations in synchronous style. For example, if we schedule computations in
threads, it is possible to write:

```haskell
do
  startThread <- liftIO myThreadId
  reschedule
  endThread <- liftIO myThreadId
```

where `reschedule` suspends the computation and immediately resumes it in a
different thread.  Hence `startThread` and `endThread` will contain different
values. See also `testThreadScheduler` in `Async_test.hs`.

The same principle is used when waiting for another computation inside `await`.
If the other computation hasn't finished yet, the current continuation is added
to its list of callbacks to invoke once the result is available.
