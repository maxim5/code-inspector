package edu.pdx.cs.capstone2010.vrr_monitor.client

import edu.pdx.cs.capstone2010.vrr_monitor.{
  Interface => BaseInterface,
  _
}
import java.util.UUID
import actors.{!, DaemonActor}

object Interface {
  def apply(name: String, client: Client) =
    new Interface(SysfsNetDir(name), client.uuid)

    def apply(name: String, clientUUID: UUID) = {
      if (name == "eth1")
        new Eth1(clientUUID)
      else
        new Interface(SysfsNetDir(name), clientUUID)
    }
}

/**
 * A network interface.
 * On construction, this actor is automatically started with start(). To stop, send it Stop.
 *
 * @TODO (chad) This class was not designed to be inherited, but now Eth1 inherits it. Things need to be cleaned up.
 * @TODO (chad) Constructor should take 'name' rather than 'dir'.
 */
class Interface (
        private val dir: SysfsNetDir,
        val clientUUID: UUID)
        extends BaseInterface with DaemonActor {

  // Setup Data -------------------------------------------------------------  

  private var sysfsScanInProgress = false
  protected val sysfsScanInterval = 4000
  private var _status: InterfaceStatus.Value = _
  private var _name: String = _
  private var _index: Int = _
  private var _macAddress: MacAddress = _
  private var _broadcastAddress: MacAddress = _
  scanSysfs()

  // Do not construct until sysfs data is prepared.
  private val neighborManager = new NeighborManager(this)

  def act(): Unit = {
    loop {
      react {
        case ScanSysfs => scanSysfs()
        case Stop => exit()
      }
    }
  }

  def status: InterfaceStatus.Value = waitForScan(_status)
  def name: String = waitForScan(_name)
  def index: Int = waitForScan(_index)
  def macAddress = waitForScan(_macAddress)
  def broadcastAddress = waitForScan(_broadcastAddress)
  def up(): Unit = "sudo ifconfig %s up".format(name) !
  def down(): Unit = "sudo ifconfig %s down".format(name) !
  def neighbors: Iterable[Neighbor] = neighborManager neighbors
  def transmit(packet: EthernetPacket): Unit = throw NotImplementedError()
  def vrrState: Option[VrrState] = None

  private def waitForScan[T](x: => T): T = {
    while (sysfsScanInProgress) {
      // spin
    }
    x
  }

  protected def scanSysfs(): Unit = {
    sysfsScanInProgress = true
    _status = dir.operstate
    _name = dir.name
    _index = dir.ifindex
    _macAddress = dir.address
    _broadcastAddress = dir.broadcast
    sysfsScanInProgress = false
    Scheduler.schedule({this ! ScanSysfs}, sysfsScanInterval)
  }

  // Private Messages -------------------------------------------------------

  protected case object ScanSysfs

  // Final Actions ----------------------------------------------------------
  // RAII: Resource Acquisition Is Initialization.
  // Start actor on construction.
  start()
}