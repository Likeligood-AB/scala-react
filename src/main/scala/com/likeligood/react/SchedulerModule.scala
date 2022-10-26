package com.likeligood.react

import scala.annotation.implicitNotFound
import scala.util.NotGiven

trait SchedulerModule { self: Domain =>


  def schedule(op: Context ?=> Unit)(using
                                     ctx: Context,
                                     @implicitNotFound("This method can only be used within a react context")
                                     using: NotGiven[ctx.NoContext]
  ) = {
    given ctx: Context = null
    engine.schedule(op)
  }
  def addToNext(op: Context ?=> Unit)(using
                                     ctx: Context,
                                     @implicitNotFound("This method can only be used within a react context")
                                     using: NotGiven[ctx.NoContext]
  ) = {
    given ctx: Context = null
    engine.addToNext(op)
  }

  /**
   * The scheduler is responsible for scheduling propagation turns. Turns are not necessarily run
   * immediately, but are scheduled with `ensureTurnIsScheduled()`.
   */
  abstract class Scheduler {
    /**
     * Ensure that a turn will be started at the discretion of this scheduler.
     * Repeated invocations of this method before the turn is started may not result
     * in additional turns being scheduled.
     */
    def ensureTurnIsScheduled(): Unit

    /**
     * Starts this scheduler. Called exactly once before any other method on this
     * scheduler is invoked.
     */
    def start() = {
      ensureTurnIsScheduled()
    }
  }

  class ManualScheduler extends Scheduler {
    def ensureTurnIsScheduled() = {}
  }
}
