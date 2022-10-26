package com.likeligood.react

trait AbsDomain {
  type Events[+T]
  type EventSource[T] <: Events[T]
  type Signal[+T]
  type Var[T] <: Signal[T]
  type Val[T] <: Signal[T]
}

/**
 * Defines all reactive classes by mixing in all reactive modules.
 *
 * Clients usually create an 'object myDomain extends Domain' and import it.
 *
 * Except the scheduler interface, no method in neither class is guaranteed to be thread-safe.
 */
trait Domain(debugConfiguration: DebugConfiguration) extends ReactiveModule
  with AbsDomain
  with SignalModule
  with EventModule
  with SchedulerModule { domain =>
  val scheduler: Scheduler
  def engine: Engine

  protected class SimpleScheduler extends Scheduler {
    var turnScheduled = false

    def ensureTurnIsScheduled(): Unit = {
      if (!turnScheduled) {
        turnScheduled = true
      }
    }
  }

  val debug: Debug[this.type] = {
    import DebugConfiguration.*
    debugConfiguration match {
      case NoDebug => new NilDebug[this.type](domain)
      case SilentDebug => new SilentDebug[this.type](this)
      case PrintDebug => new ConsoleDebug[this.type](this)
      case CustomDebug(logOp) => new CustomLogDebug[this.type](this, logOp)
    }
  }

  /**
   * Starts processing events. Thread-safe.
   */
  def start() = {
    debug.logStart()
    scheduler.start()
  }
}

