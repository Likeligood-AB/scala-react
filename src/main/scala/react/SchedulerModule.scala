package react

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{ Executors, ExecutorService }
import javax.swing.SwingUtilities

trait SchedulerModule { self: Domain =>


  def schedule(r: Runnable) { engine.schedule(r) }
  def schedule(op: => Unit) { engine.schedule(op) }

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
    def ensureTurnIsScheduled()

    /**
     * Starts this scheduler. Called exactly once before any other method on this
     * scheduler is invoked.
     */
    def start() {
      ensureTurnIsScheduled()
    }
  }

  class ManualScheduler extends Scheduler {
    def ensureTurnIsScheduled() = {}
  }
}