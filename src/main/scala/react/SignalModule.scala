package react

import scala.annotation.implicitNotFound
import scala.util.NotGiven

trait SignalModule { module: Domain =>
  import ReactiveModule.*

  def Strict[A](using
                debugName: DebugName,
                ctx: Context,
                @implicitNotFound("This method can only be used within a react context")
                using: NotGiven[ctx.NoContext]
    )(using
      @implicitNotFound("Signals may not be created inside leaf nodes or dependency nodes")
      ev: NotGiven[ctx.DependencyNodeContext | ctx.LeafNodeContext]
    )(op: ctx.DependencyNodeContext ?=> A): Signal[A] = {
      given ctx.DependencyNodeContext = null
      val node = StrictOpSignal(op)
      debugName.name.foreach(name => debug.setName(node, name))
      node
    }

  def Lazy[A](using
                ctx: Context,
                @implicitNotFound("This method can only be used within a react context")
                using: NotGiven[ctx.NoContext]
  )(using
    @implicitNotFound("Signals may not be created inside leaf nodes or dependency nodes")
    ev: NotGiven[ctx.DependencyNodeContext | ctx.LeafNodeContext]
  )(op: ctx.DependencyNodeContext ?=> A): Signal[A] = {
    given ctx.DependencyNodeContext = null
    new LazyOpSignal(op)
  }

  /**
   * A time-varying value.
   */
  abstract class Signal[+A] extends TotalReactive[A, A] with SimpleReactive[A, A] { outer =>
    /* A signal's pulse and value are really the same, except the flag that indicates whether
     * the signal is emitting or not.
     */
    def getValue: A = getPulse.asInstanceOf[A]
    def getLevel = level

    def toStrict: Signal[A]

    // Fix types, when/if SI-3272 gets fixed
    protected def shouldEmit(oldVal: Any, newVal: Any): Boolean = oldVal != newVal

    override protected[this] def emit(p: Any) = {
      if(shouldEmit(getValue, p)) super.emit(p)
    }

    override protected[react] def freePulse() = {
      // the pulse is the value for signals, so don't do anything here.
    }
  }

  abstract class FuncSignal[A] extends Signal[A] with OpaqueReactive[A, A] {
    protected def react() = {
      emit(eval)
    }

    protected def eval: A
  }

  abstract class StrictFuncSignal[A] extends FuncSignal[A] with StrictNode {
    tick()

    def toStrict: Signal[A] = this
  }

  class StrictOpSignal[A](op: => A) extends StrictFuncSignal[A] {
    protected def eval = op

    override def reactiveDescriptor = "Strict Signal"
  }

  abstract class LazyFuncSignal[A] extends FuncSignal[A] with LazyNode

  class LazyOpSignal[A](op: => A) extends LazyFuncSignal[A] {
    protected def eval = op

    override def changes: Events[A] = this.toStrict.changes

    def toStrict: Signal[A] = new StrictOpSignal(op)
  }

  /**
   * A signal with a single input.
   */
  abstract class Signal1[+A, +B](protected val input: Reactive[A, Any])
    extends Signal[B]
    with Dependent1[A, Any] {

    input.subscribe(this)

    def doValidatePulse() = {
      input.ifEmitting(pulsate _)
      setPulseValid()
    }
    protected[this] def pulsate(a: Any): Unit
  }

  abstract class StrictSignal1[+A, +B](input: Reactive[A, Any])
    extends Signal1[A, B](input)
    with StrictNode

  protected[this] class HoldSignal[P](init: P)(input: Reactive[P, Any]) extends StrictSignal1[P, P](input) {
    pulse = init

    def pulsate(p: Any) = { emit(p.asInstanceOf[P]) }

    def toStrict: Signal[P] = this
  }

  def doUpdateLater(op: => Unit): Unit = {
    new DoUpdateLaterNode { def react() = op }
  }

  private abstract class DoUpdateLaterNode extends LeafNode(UpdateLaterLevel) {
    tick()
  }

    /**
     * Creates a mutable signal with the given owner.
     */
  def Var[A](init: A)(using
                         ctx: Context,
                         @implicitNotFound("This method can only be used within a react context")
                         using: NotGiven[ctx.NoContext]
  )(using
    @implicitNotFound("Signals may not be created inside leaf nodes")
    ev1: NotGiven[ctx.LeafNodeContext],
    @implicitNotFound("Signals may not be created inside dependency nodes")
    ev2: NotGiven[ctx.DependencyNodeContext],
  )(using owner: Owner): Var[A] = new Var(init, owner)

  /**
   * An externally mutable signal.
   */
  class Var[A] (init: A, val owner: Owner) extends Signal[A] with SimpleSource[A, A] with StrictNode {
    pulse = init

    protected[react] val channel = owner.newChannel(this, init)

    def toStrict: Signal[A] = this

    /**
     * Sets the value of this signal, either in the next turn, if the owner is the domain or
     * in the current turn if the owner is a router.
     */
    def update(a: A): Unit = {
      if(owner eq DomainOwner) channel.push(this, a)
      else {
        emit(a)
        if(isEmitting) engine.defer(this)
      }
    }

    override def reactiveDescriptor = "Var"
  }

  /**
   * A constant signal.
   */
  case class Val[A](a: A) extends Signal[A] with MuteNode {
    pulse = a

    def toStrict: Signal[A] = this
    override def reactiveDescriptor = "Val"
  }
}
