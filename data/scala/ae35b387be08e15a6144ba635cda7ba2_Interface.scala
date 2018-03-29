package edu.pdx.cs.capstone2010.vrr_monitor

import java.util.UUID

abstract class Interface {
  def clientUUID: UUID
  def status: InterfaceStatus.Value
  def name: String
  def macAddress: MacAddress
  def broadcastAddress: MacAddress
  def up(): Unit
  def down(): Unit
  def neighbors: Iterable[Neighbor]
  def transmit(packet: EthernetPacket): Unit
  def vrrState: Option[VrrState]
//  def vrrStatus: VrrStatus
//  def vrrID: Option[VrrId]
//  def vrrPSetActive: Option[VrrPSet]
  
  final def toXML =
    <interface>
      <client-uuid>{clientUUID toString}</client-uuid>
      <status>{status toString}</status>
      <name>{name}</name>
      <mac>{macAddress toString}</mac>
      <broadcast-address>{broadcastAddress toString}</broadcast-address>
      { vrrState match {
          case Some(s) => s toXML
          case None => // nothing
        }
      }
      <neighbors>{neighbors.map(_.toXML)}</neighbors>
    </interface>
}