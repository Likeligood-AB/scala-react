package com.likeligood.react

import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scala.util.NotGiven

trait EventModule { module: Domain =>
  object Events {
    def once[A](a: A): Events[A] = {
      val es = EventSource[A]
      es << a
      es
    }

    /**
     * An event stream that never emits.
     */
    def never[A]: Events[A] = Never
    object Never extends Events[Nothing] with MuteNode

    class MergedAny(val inputs: Iterable[Reactive[Any, Any]]) extends Events[Unit]
      with DependentN
      with LazyNode {
      inputs foreach { _ subscribe this }

      def doValidatePulse() = {
        if (inputs exists { _.isEmitting }) emit(())
        else mute()
      }
    }

    def mergeAny(inputs: Reactive[Any, Any]*): Events[Unit] = new MergedAny(inputs)
  }

  /**
   * An event stream. Can emit in a propagation turn, but holds no value otherwise.
   */
  abstract class Events[+A] extends TotalReactive[A, Unit] with SimpleReactive[A, Unit] { outer =>
    def getValue: Unit = ()

    protected class Mapped[B](f: A => B) extends LazyEvents1[A, B](outer) {
      protected[this] def react(a: A @uncheckedVariance) = { emit(f(a)) }
    }

    protected class Filtered(pred: A => Boolean) extends StrictEvents1[A, A](outer) {
      protected[this] def react(a: A @uncheckedVariance) = { if (pred(a)) emit(a) }
    }

    protected class Collected[B](f: PartialFunction[A, B]) extends StrictEvents1[A, B](outer) {
      protected[this] def react(a: A @uncheckedVariance) = { if (f isDefinedAt a) emit(f(a)) }
    }

    protected class Taken(private var count: Int) extends StrictEvents1[A, A](outer) {
      protected[this] def react(a: A @uncheckedVariance) = {
        if (count > 0) {
          count -= 1
          emit(a)
        } else disconnect()
      }
    }

    protected class TakeUntil(last: A => Boolean) extends StrictEvents1[A, A](outer) {
      private var finished = false
      protected[this] def react(a: A @uncheckedVariance) = {
        if (finished) disconnect()
        else {
          emit(a)
          finished = last(a)
        }
      }
    }

    protected class Dropped(private var count: Int) extends StrictEvents1[A, A](outer) {
      protected[this] def react(a: A @uncheckedVariance) = {
        if (count > 0) count -= 1
        else emit(a)
      }
    }

    protected class Delayed(delay: Int) extends StrictEvents1[A, A](outer) {
      if(delay <= 0) throw new IllegalArgumentException("Delay must be >= 1.")

      private val buffer = new Array[Any](delay) // ring buffer
      private var head = 0
      private var size = 0 // only used until we have seen delay number of events

      protected[this] def react(a: A @uncheckedVariance) = {
        if(size < delay) {
          buffer(size) = a
          size += 1
        } else {
          emit(buffer(head).asInstanceOf[A])
          buffer(head) = a
          head = (head + 1) % delay
        }
      }
    }

    protected class Scanned[B](init: B)(f: (B, A) => B) extends StrictEvents1[A, B](outer) {
      pulse = init

      protected[this] def react(a: A @uncheckedVariance) = { emit(f(this.pulse.asInstanceOf[B], a)) }
    }

    protected class ScannedMutable[B](init: B)(f: (B, A) => B) extends StrictEvents1[A, B](outer) {
      pulse = init

      protected[this] def react(a: A @uncheckedVariance) = {
        muteable {
          emit(f(this.pulse.asInstanceOf[B], a))
        }
      }
    }

    protected class Scanned1[B >: A](f: (B, B) => B) extends StrictEvents1[B, B](outer) {
      private var started = false
      protected[this] def react(a: B @uncheckedVariance) = {
        if (started) emit(f(this.pulse.asInstanceOf[B], a))
        else {
          started = true
          emit(a)
        }
      }
    }

    class Merged[B >: A](val input1: Events[B], val input2: Events[B]) extends Events[B]
      with Dependent2[B, Unit]
      with LazyNode {
      input1 subscribe this
      input2 subscribe this

      def doValidatePulse() = {
        input1.ifEmittingElse { emit _ } {
          input2.ifEmittingElse { emit _ } { mute() }
        }
      }
    }

    protected class Flattened[B](isEvents: A => Events[B]) extends LazyEvents[B] with ManagesDependencies {
      private var cur: Events[B] = null

      // level = (if(cur==null) outer.level else math.max(outer.level, cur.level)) + 1

      outer.subscribe(this)
      def doValidatePulse() = {
        outer.ifEmitting { e =>
          if (cur != null) cur.unsubscribe(this)
          cur = isEvents(e)
          cur.subscribe(this)
        }
        if (cur != null) cur.ifEmitting { x =>
          emit(x)
        }
      }

      protected override def disconnect() = {
        outer.unsubscribe(this)
        cur.unsubscribe(this)
        super.disconnect()
      }
    }

    /**
     * An event stream that emits every value from this stream mapped by the given function.
     */
    def map[B](using
               ctx: Context,
               @implicitNotFound("This method can only be used within a react context")
               using: NotGiven[ctx.NoContext]
    )(f: ctx.LeafNodeContext ?=> A => B): Events[B] = {
      given ctx.LeafNodeContext = null
      new Mapped(f)
    }
    /**
     * An event stream that emits all values from this stream for which the given predicate
     * evaluates to `true`.
     */
    def filter(using
               ctx: Context,
               @implicitNotFound("This method can only be used within a react context")
               using: NotGiven[ctx.NoContext]
    )(p: ctx.LeafNodeContext ?=> A => Boolean): Events[A] = {
      given ctx.LeafNodeContext = null
      new Filtered(p)
    }
    /**
     * An event stream that emits all values from this stream for which the given partial function
     * is defined. Additionally maps, i.e., `es collect { case e if p(e) => f(e) }` is equivalent to
     * `es filter p map f`.
     */
    def collect[B](using
               ctx: Context,
               @implicitNotFound("This method can only be used within a react context")
               using: NotGiven[ctx.NoContext]
    )(f: ctx.LeafNodeContext ?=> PartialFunction[A, B]): Events[B] = {
      given ctx.LeafNodeContext = null
      new Collected(f)
    }

    /**
     * An event stream that applies `init` and the value from this stream to the given
     * function and emits the result at the time of the second event. At the time of the
     * third event, it applies the last result and third value from this stream to the given
     * function and emits the result ... and so on.
     */
    def scan[B](init: B)(using
               ctx: Context,
               @implicitNotFound("This method can only be used within a react context")
               using: NotGiven[ctx.NoContext]
    )(f: ctx.LeafNodeContext ?=> (B, A) => B): Events[B] = {
      given ctx.LeafNodeContext = null
      new Scanned(init)(f)
    }

    /**
     * An event stream that applies the first and second value from this stream to the given
     * function and emits the result at the time of the second event. At the time of the third
     * event, it applies the last result and third value from this stream to the given function
     * and emits the result ... and so on.
     */
    def scan1[B >: A](using
              ctx: Context,
              @implicitNotFound("This method can only be used within a react context")
              using: NotGiven[ctx.NoContext]
    )(f: ctx.LeafNodeContext ?=> (B, B) => B): Events[B] = {
      given ctx.LeafNodeContext = null
      new Scanned1(f)
    }

    /**
     * A scan that can skip events from this event stream.
     *
     * Wraps the call to `f` in a `mutable` block, i.e., a call to `mute` inside `f` is safe and
     * indicates that the current event from this event stream should be skipped and the resulting
     * event stream should not emit in that turn.
     */
    def scanMutable[B](init: B)(using
              ctx: Context,
              @implicitNotFound("This method can only be used within a react context")
              using: NotGiven[ctx.NoContext]
    )(f: ctx.LeafNodeContext ?=> (B, A) => B): Events[B] = {
      given ctx.LeafNodeContext = null
      new ScannedMutable(init)(f)
    }

    /**
     * An event stream that emits the first `count` events from this stream.
     */
    def take(count: Int): Events[A] = new Taken(count)

   /**
     * takeUntil last is true events from this stream.
     */
    def takeUntil(using
              ctx: Context,
              @implicitNotFound("This method can only be used within a react context")
              using: NotGiven[ctx.NoContext]
    )(last: ctx.LeafNodeContext ?=> A => Boolean): Events[A] = {
      given ctx.LeafNodeContext = null
      new TakeUntil(last)
    }

    /**
     * An event stream that emits all event from this stream after the first `count` events.
     */
    def drop(count: Int): Events[A] = new Dropped(count)

    def delay(count: Int): Events[A] = new Delayed(count)

    /**
     * An event stream that emits the values from both this and the given stream. It is left-biased,
     * i.e., emits a value from this stream if both input stream emit at the same time.
     */
    def merge[B >: A](that: Events[B]): Events[B] = new Merged(this, that)

    /**
     * A signal that initially holds `init` and then always the latest emitted value from this
     * stream.
     */
    def hold[B >: A](init: B): Signal[B] = new HoldSignal(init)(this)

    def flatten[B](implicit isEvents: A => Events[B]): Events[B] = new Flattened[B](isEvents)
  }

  /**
   * An event stream with a single input.
   */
  abstract class Events1[+A, +B](val input: Reactive[A, Any])
    extends Events[B]
    with Dependent1[A, Any] {

    connect()

    def doValidatePulse() = {
      input.ifEmitting(react _)
      setPulseValid()
    }
    protected[this] def react(a: A @uncheckedVariance): Unit
  }

  abstract class LazyEvents[+A]
    extends Events[A]
    with LazyNode

  abstract class StrictEvents[+A]
    extends Events[A]
    with LazyNode

  abstract class LazyEvents1[+A, +B](input: Reactive[A, Any])
    extends Events1[A, B](input)
    with LazyNode

  abstract class StrictEvents1[+A, +B](input: Reactive[A, Any])
    extends Events1[A, B](input)
    with StrictNode

  protected[this] class ChangeEvents[P](input: Reactive[P, Any]) extends LazyEvents1[P, P](input) {
    def react(p: P) = { emit(p) }
  }

  def EventSource[A](using owner: Owner, debugName: DebugName): EventSource[A] = {
    val es = new EventSource[A](owner)
    debugName.name.foreach(name => debug.setName(es, name))
    es
  }

  /**
   * An externally mutable event stream.
   */
  class EventSource[A] (val owner: Owner) extends Events[A] with SimpleSource[A, Unit] with StrictNode {
    protected[react] val channel = owner.newChannel(this, null.asInstanceOf[A])

    /**
     * Lets this stream emit the given value, either in the next turn, if the owner is the domain or
     * in the current turn if the owner is a router.
     */
    def <<(a: A) = {
      if (owner eq DomainOwner) channel.push(this, a)
      else {
        emit(a)
        engine.defer(this)
      }
    }

    override def reactiveDescriptor = "EventSource"
  }
}
