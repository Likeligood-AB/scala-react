package com.likeligood.react

import java.lang.management.ManagementFactory
import java.text.SimpleDateFormat
import java.util.WeakHashMap

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.language.postfixOps
import scala.ref.WeakReference

class NodeExecutionInfo {
  var hitCount = 0L
  var noChangeCount = 0L
  val tickedBy = mutable.Map.empty[String, Int]
  var levelMismatchCount = 0L
  var levelMismatchAccessedCount = 0L

  var maxWallClockTime = Long.MinValue
  var minWallClockTime = Long.MaxValue
  var sumWallClockMicros = 0L
  def avgWallClockMillis = sumWallClockMicros / hitCount / 1e3
  def maxWallClockMillis = maxWallClockTime / 1e3
  def minWallClockMillis = minWallClockTime / 1e3
  def sumWallClockMillis = sumWallClockMicros / 1e3

  var maxCpuTime = Long.MinValue
  var minCpuTime = Long.MaxValue
  var sumCpuMicros = 0L
  def avgCpuTimeMillis = sumCpuMicros / hitCount / 1e3
  def maxCpuTimeMillis = maxCpuTime / 1e3
  def minCpuTimeMillis = minCpuTime / 1e3
  def sumCpuTimeMillis = sumCpuMicros / 1e3

  var maxLevel: Int = 0
}

class NodeExecutionInfoAggregate(val name: String) extends NodeExecutionInfo {
  var nodes = 0

  def add(other: NodeExecutionInfo): Unit = {
    nodes += 1
    hitCount += other.hitCount
    noChangeCount += other.noChangeCount
    other.tickedBy.foreachEntry((node, count) => tickedBy(node) = tickedBy.getOrElse(node, 0) + count)
    levelMismatchCount += other.levelMismatchCount
    levelMismatchAccessedCount += other.levelMismatchAccessedCount
    maxLevel = Math.max(maxLevel, other.maxLevel)

    sumWallClockMicros += other.sumWallClockMicros
    if (other.maxWallClockTime > maxWallClockTime) { maxWallClockTime = other.maxWallClockTime }
    if (other.minWallClockTime < minWallClockTime) { minWallClockTime = other.minWallClockTime }

    sumCpuMicros += other.sumCpuMicros
    if (other.maxCpuTime > maxCpuTime) { maxCpuTime = other.maxCpuTime }
    if (other.minCpuTime < minCpuTime) { minCpuTime = other.minCpuTime }
  }
}

case class ExecutionTimeSummary(aggregations: Map[String, NodeExecutionInfoAggregate])

trait DebugName {
  def name: Option[String]
}
object DebugName {
  given DebugName = new DebugName {
    val name: Option[String] = None
  }
}

abstract class Debug[D <: Domain](val domain: D) {
  import domain.*

  private[react] val executionInfo = mutable.AnyRefMap.empty[Node, NodeExecutionInfo]
  private[react] var recordExecutionInfo = false
  private[react] var recordDependencyNodes = false
  private[react] val dependencyNodes = mutable.ArrayBuffer.empty[WeakReference[DependencyNode]]
  private[react] val dependencyNodesTicked = mutable.HashSet.empty[(String, String)]

  private[react] val tmxb = ManagementFactory.getThreadMXBean

  tmxb.setThreadCpuTimeEnabled(true)

  def recordDependencyNode(node: DependencyNode): Unit = {
    if (recordDependencyNodes) {
      dependencyNodes.append(new WeakReference(node))
    }
  }

  def beginRecordingDependencyNodes(): Unit = {
    recordDependencyNodes = true
  }

  def stopRecordingDependencyNodes(): Unit = {
    dependencyNodes.clear()
    recordDependencyNodes = false
  }

  private def uniqueDepName(node: Node) = {
    val name = getName(node)
    val idHash = System.identityHashCode(node)
    s"$name-$idHash"
  }

  def getDependencyNodes(groupByName: Boolean = true): Seq[(String, Seq[String])] = {
    if (groupByName) {
      (dependencyNodes.flatMap(_.get) map { node =>
        getName(node) -> node.debugReadDependents.map(getName)
      } groupBy {
        case (node, _) =>
          node
      }).view mapValues { kv =>
        kv.foldLeft(Set.empty[String])((acc, deps) => acc ++ deps._2).toSeq
      } toSeq
    } else {
      (dependencyNodes.flatMap(_.get) map { node =>
        uniqueDepName(node) -> node.debugReadDependents.map(uniqueDepName)
      }).toSeq
    }
  }

  def getTickedNodes(): Set[(String, String)] = {
    val set = dependencyNodesTicked.toSet
    dependencyNodesTicked.clear()
    set
  }

  def beginRecordingExecutionInfo(): Unit = {
    recordExecutionInfo = true
    executionInfo.clear()
  }

  def stopRecordingExecutionInfo(): Unit = {
    recordExecutionInfo = false
    executionInfo.clear()
  }

  @inline def executeAndRecord[T](node: Node)(op: => T): T = {
    val threadId = if (recordExecutionInfo) Thread.currentThread.getId else 0L
    val startCpuTime = if (recordExecutionInfo) debug.tmxb.getThreadCpuTime(threadId) else 0L
    val startTime = if (recordExecutionInfo) System.nanoTime() else 0L
    var levelMismatch = false
    val startValue = if (!recordExecutionInfo) None else node match {
      case sig: Signal[?] =>
        Option(sig.getValue)
      case _ =>
        None
    }
    try {
      val v = op
      v
    } catch {
      case LevelMismatch =>
        if (levelMismatchCause.level > 1) {
          levelMismatch = true
        }
        throw LevelMismatch
    } finally {
      if (recordExecutionInfo) {
        val noChange = startValue.exists(startValue => (node match {
          case sig: Signal[?] =>
            Option(sig.getValue)
          case _ =>
            None
        }).contains(startValue))
        recordNodeExecution(node, System.nanoTime() - startTime, debug.tmxb.getThreadCpuTime(threadId) - startCpuTime, node.level, levelMismatch, noChange)
      }
    }
  }

  @inline def tickAndRegister(node: Node, dependent: Node): Unit = {
    if (recordDependencyNodes) {
      dependencyNodesTicked.add(getName(node) -> getName(dependent))
    }

    if (recordExecutionInfo) {
      val info = executionInfo.getOrElseUpdate(node, new NodeExecutionInfo)
      info.tickedBy(dependent.name) = info.tickedBy.getOrElse(dependent.name, 0) + 1
    }

    node.tick()
  }

  def recordNodeExecution(node: Node, wallClockNanos: Long, cpuNanos: Long, maxLevel: Int, levelMismatch: Boolean, noChange: Boolean): Unit = {
    val info = executionInfo.getOrElseUpdate(node, new NodeExecutionInfo)
    val wallClockMicros = wallClockNanos / 1000
    val cpuMicros = cpuNanos / 1000

    info.hitCount += 1

    if (noChange) {
      info.noChangeCount += 1
    }

    if (levelMismatch) {
      info.levelMismatchCount += 1

      executionInfo.getOrElseUpdate(levelMismatchAccessed, new NodeExecutionInfo).levelMismatchAccessedCount += 1
    }

    info.sumWallClockMicros += wallClockMicros
    if (wallClockMicros > info.maxWallClockTime) { info.maxWallClockTime = wallClockMicros }
    if (wallClockMicros < info.minWallClockTime) { info.minWallClockTime = wallClockMicros }

    info.sumCpuMicros += cpuMicros
    if (cpuMicros > info.maxCpuTime) { info.maxCpuTime = cpuMicros }
    if (cpuMicros < info.minCpuTime) { info.minCpuTime = cpuMicros }

    info.maxLevel = maxLevel
  }

  def getExecutionTimeAggregate(minHitCount: Long = 1) = {
    val groupedAggregates = executionInfo groupBy(kv => getName(kv._1)) map {
      case (name, map) =>
        val aggregate = new NodeExecutionInfoAggregate(name)
        map.values.foreach(info => aggregate.add(info))
        name -> aggregate
    } filter {
      _._2.hitCount >= minHitCount
    }

    ExecutionTimeSummary(groupedAggregates)
  }

  def setName(node: Node, name: String): Unit
  def getName(node: Node): String


  // The thread on which this engine is currently running, `null` if not in a turn.
  @volatile private var thread: Thread = null

  /**
   * Returns `true` if the current thread is the thread the engine is running on.
   */
  def isInTurn = Thread.currentThread == thread
  def isInTurn_=(b: Boolean) = {
    thread = if (b) Thread.currentThread else null
  }

  def enterTurn(id: Long, debug: String = "") = {
    assert(!isInTurn, s"Tried to run a turn before the previous one was finished. $debug")
    logEnterTurn(id)
    isInTurn = true
  }
  def leaveTurn(id: Long) = {
    assert(isInTurn)
    logLeaveTurn(id)
    isInTurn = false
  }

  def assertInTurn() = {
    assertExclusiveThread()
    assert(isInTurn, "This method must be run on its domain " + this)
  }

  @volatile private var exclusiveThread = Option.empty[Thread]

  def setExclusiveThread(thread: Thread): Unit = {
    exclusiveThread = Some(thread)
  }

  def assertExclusiveThread(): Unit = {
    assert(
      exclusiveThread.forall(_ == Thread.currentThread()),
      s"Current thread ${Thread.currentThread().getName} != ${exclusiveThread.get.getName}",
    )
  }

  def logStart(): Unit
  def logEnterTurn(id: Long): Unit
  def logLeaveTurn(id: Long): Unit
  def logTock(n: Node): Unit
  def logLevelMismatch(accessor: Node, accessed: Node, cause: Node): Unit
  def logTodo(t: Any): Unit
}

class NilDebug[D <: Domain](dom: D) extends Debug(dom) {
  import domain.*

  def setName(node: Node, name: String) = { }
  def getName(node: Node): String = ""

  def logStart() = {}
  def logEnterTurn(id: Long) = {}
  def logLeaveTurn(id: Long) = {}
  def logTock(n: Node) = {}
  def logLevelMismatch(accessor: Node, accessed: Node, cuase: Node) = {}
  def logTodo(t: Any) = {}
}

abstract class AbstractDebug[D <: Domain](dom: D) extends Debug(dom) {
  import domain.*
  protected val names = new WeakHashMap[Node, String]
  def setName(node: Node, name: String) = names.put(node, name)
  def getNonEventsReactive(node: Node): Node = node match {
    case eventsNode: Events1[_, _] =>
      getNonEventsReactive(eventsNode.input)
    case n =>
      n
  }
  def getName(node: Node): String = {
    node match {
      case mergedNode: Events[_]#Merged[_] =>
        node.reactiveDescriptor + ".MERGED." + getName(getNonEventsReactive(mergedNode.input1)) + ".WITH." + getName(getNonEventsReactive(mergedNode.input2))
      case eventsNode: Events1[_, _] =>
        node.reactiveDescriptor + ".---." + getName(getNonEventsReactive(eventsNode))
      case _ =>
        names.getOrDefault(node, node.reactiveDescriptor + "@" + node.level)
    }

  }
}

abstract class PrintDebug[D <: Domain](dom: D) extends AbstractDebug(dom) {
  import domain.*

  private val dateFormat = new SimpleDateFormat("d MMM yyyy HH:mm:ss.SSS")

  def log(s: String): Unit

  private def time() = System.currentTimeMillis()
  private def timePrefix = "+" + (time()-startTime) + "ms: "
  private var startTime = 0L

  def logStart() = {
    startTime = time()
    log("Start domain " + domain + " at " + dateFormat.format(startTime))
  }
  def logEnterTurn(id: Long) = log(timePrefix + "enter turn " + id)
  def logLeaveTurn(id: Long) = log(timePrefix + "leave turn " + id)
  def logTock(n: Node) = log("Tock " + n)
  def logLevelMismatch(accessor: Node, accessed: Node, cause: Node) = log("Level mismatch when " + accessor + " accessed " + getName(accessed) + "cause " + getName(cause))
  def logTodo(t: Any) = log("Todo " + t)
}

class SilentDebug[D <: Domain](dom: D) extends AbstractDebug(dom) {
  import domain.*

  def logStart() = { }
  def logEnterTurn(id: Long) = { }
  def logLeaveTurn(id: Long) = { }
  def logTock(n: Node) = { }
  def logLevelMismatch(accessor: Node, accessed: Node, cause: Node) = { }
  def logTodo(t: Any) = { }
}

class ConsoleDebug[D <: Domain](dom: D) extends PrintDebug(dom) {
  def log(s: String) = println(s)
}

class CustomLogDebug[D <: Domain](dom: D, logOp: String => Unit) extends PrintDebug(dom) {
  def log(s: String) = logOp(s)
}

enum DebugConfiguration {
  case NoDebug
  case SilentDebug
  case PrintDebug
  case CustomDebug(logOp: String => Unit)
}