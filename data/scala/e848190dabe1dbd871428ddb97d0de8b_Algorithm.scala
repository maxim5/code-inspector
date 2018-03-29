package com.hermes.hermesd.algorithm

import com.nodeta.scalandra.serializer.StringSerializer
import com.nodeta.scalandra._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap 
import java.util.ArrayList
import java.lang.Comparable
import java.util.PriorityQueue 



object DataConnection{
    val serialization = new Serialization(
        StringSerializer,
        StringSerializer,
        StringSerializer
    )
    val cassandra = new Client(
        Connection("127.0.0.1", 9160),
        "Hermes",
        serialization,
        ConsistencyLevels.one
    )
}

class Dijkstra(){
    def getAllNodes(): String = {
	var size = DataConnection.cassandra.ColumnFamily("Intersecciones")("DB")("0").toInt
	var Salida = ""
	for(i<-1 to size){
		var id = DataConnection.cassandra.ColumnFamily("Intersecciones")("DB")(i.toString)
		Salida = Salida + ";" + DataConnection.cassandra.ColumnFamily("Coordenadas")(id)("Lat") + "_" +  DataConnection.cassandra.ColumnFamily("Coordenadas")(id)("Lon")   

	}
	Salida
    }
    
    def algorithm(start: Map[String, String], goal: Map[String, String], hora: Int): (String, Double) = {
        var startId = NearestNeighbor.find(start,0.0001)
	var endId = NearestNeighbor.find(goal,0.0001)

	println("startId:" + startId)
	println("endId:" + endId)

	var distancia = new HashMap[String, Double]()
	var novistos = Set[String]()
	var padre = new HashMap[String, String]()
	var size = DataConnection.cassandra.ColumnFamily("Intersecciones")("DB")("0").toInt
	for(i<-1 to size){
	   var id = DataConnection.cassandra.ColumnFamily("Intersecciones")("DB")(i.toString)
	   novistos += id
	   if(DataConnection.cassandra.SuperColumnFamily("Trafico")(startId)(id).size != 0){
		padre(id) = startId
		distancia(id) = DataConnection.cassandra.SuperColumnFamily("Trafico")(startId)(id)(hora.toString).toDouble
	   }else{
	   	padre(id) = "NONE"
		distancia(id) = Math.MAX_DOUBLE
	   }	       	
	}
	distancia(startId) = 0.0
	novistos -= startId
	while(novistos.isEmpty == false){
		//Console.readLine()
//		println("novistos.size: " + novistos.size)
		var idS = novistos.toArray
		
		var idMenor = idS(0)
				
		for(i<-1 to idS.size - 1){
			if(distancia(idMenor) > distancia(idS(i))){
				idMenor = idS(i)
			}
		}
//		println("idMenor:" + idMenor)
//		println("distancia(idMenor):" + distancia(idMenor))
		novistos -= idMenor
		var size = 0	
		if(DataConnection.cassandra.ColumnFamily("Vecinos")(idMenor).size != 0){
			size = DataConnection.cassandra.ColumnFamily("Vecinos")(idMenor)("0").toInt	
		}
		for(i<-1 to size){
			var vecino = DataConnection.cassandra.ColumnFamily("Vecinos")(idMenor)(i.toString)
			var peso = DataConnection.cassandra.SuperColumnFamily("Trafico")(idMenor)(vecino)(hora.toString).toDouble
			if(distancia(vecino) > distancia(idMenor) + peso){
				distancia(vecino) = distancia(idMenor) + peso
				padre(vecino) = idMenor
			}
			
		}
		
	}
	
	var salida = ""
	//DataConnection.cassandra.ColumnFamily("Coordenadas")(dbId)("Lat") + "_" +  DataConnection.cassandra.ColumnFamily("Coordenadas")(dbId)("Lon")  
	var current = endId
	while(current != "NONE"){
		println(current)
		salida = salida + ";" + DataConnection.cassandra.ColumnFamily("Coordenadas")(current)("Lat") + "_" + DataConnection.cassandra.ColumnFamily("Coordenadas")(current)("Lon")  
		current = padre(current)
	}
	//println(salida)
	return (salida, distancia(endId))
    } 
    

}

class Node(var dbId: String, var cameFrom: Node) extends Comparable[Node]{
    var gScore = 0.0
    var hScore = 0.0
    var fScore = 0.0

    def compareTo(other: Node): Int = { this.fScore.compare(other.fScore) }

    def equals(other: Node):Boolean = {this.dbId.equals(other.dbId)}
    
    def getNeighbors(): java.util.Iterator[String] = { 
	var vecinos = new ArrayList[String]()
//	println(dbId)
	var size = 0	
	if(DataConnection.cassandra.ColumnFamily("Vecinos")(dbId).size != 0){
		size = DataConnection.cassandra.ColumnFamily("Vecinos")(dbId)("0").toInt	
	}

        for(i<-1 to size){
		vecinos.add(DataConnection.cassandra.ColumnFamily("Vecinos")(dbId)(i.toString))
	}
	vecinos.iterator
    }
    
    def aString(complete: Boolean): String = {
        if(complete == true){
            "(gScore, hScore, fScore) = (" + gScore + "," + hScore + "," + fScore + ") ---"+ aString(false)
        }
		else {
           DataConnection.cassandra.ColumnFamily("Coordenadas")(dbId)("Lat") + "_" +  DataConnection.cassandra.ColumnFamily("Coordenadas")(dbId)("Lon")   
        }
    }

}

class AStar(var minCost: Double){

    def getAllNodes(): String = {

	var size = DataConnection.cassandra.ColumnFamily("Intersecciones")("DB")("0").toInt
	var Salida = ""
	for(i<-1 to size){
		var id = DataConnection.cassandra.ColumnFamily("Intersecciones")("DB")(i.toString)
		Salida = Salida + ";" + DataConnection.cassandra.ColumnFamily("Coordenadas")(id)("Lat") + "_" +  DataConnection.cassandra.ColumnFamily("Coordenadas")(id)("Lon")   

	}
	Salida
	
    } 

    def heuristicStimateOfDistance(aI : String, bI: String): Double = {

		val f = DataConnection.cassandra.ColumnFamily("Coordenadas")(aI)
		val t = DataConnection.cassandra.ColumnFamily("Coordenadas")(bI)
		
		var dlong = f("Lon").toDouble - t("Lon").toDouble

		var degtorad = 0.01745329
		var radtodeg = 57.29577951

		var dvalue = (Math.sin(f("Lat").toDouble * degtorad))*(Math.sin(t("Lat").toDouble * degtorad)) + 
		(Math.cos(f("Lat").toDouble * degtorad) * Math.cos(t("Lat").toDouble * degtorad) * Math.cos(dlong * degtorad))

		var dd = Math.acos(dvalue) * radtodeg

		var km = dd*111.302 
		var horas = km/30.0
		horas*3600
    }

    def buildPath(current: Node): String = {
	println(current.dbId)	
	if (current.cameFrom != null){
			
			return buildPath(current.cameFrom)  + ";" +current.aString(false)
        }
	else {
	    println("se metio al else: "+current.aString(false))
            return ";"+current.aString(false)
        }   
    }
    
    def vecinos(nodo: Node, hora: Int, fin: String):ArrayList[Node]={
        var i = nodo.getNeighbors();
	//println("Expandiendo Nodo "+nodo.dbId)
	var lista = new ArrayList[Node]()
        
        while(i.hasNext) {
	    
	    var currentNode = new Node(i.next, nodo)
	    //println("hijos: "+currentNode.dbId)
	    var dist:Double = DataConnection.cassandra.SuperColumnFamily("Trafico")(nodo.dbId.toString)(currentNode.dbId.toString)(hora.toString).toDouble
	    currentNode.gScore = nodo.gScore + dist
	    currentNode.hScore = heuristicStimateOfDistance(currentNode.dbId, fin)
	    currentNode.fScore = currentNode.gScore + currentNode.hScore
	    lista.add(currentNode)
	}
	return lista	
    }


    def gestionarVecinos(hijos: ArrayList[Node], abierta: PriorityQueue[Node], abiertahashmap: HashMap[String, Node], cerrada: HashMap[String, Node]){

	for(i<-0 to hijos.size - 1){
          var hijoActual = hijos.get(i).asInstanceOf[Node]	
          
	   if(cerrada.contains(hijoActual.dbId) == false){

	    if(abiertahashmap.contains(hijoActual.dbId) == false){
		abierta.add(hijoActual)
		abiertahashmap(hijoActual.dbId) = hijoActual
	     }
	     else{
		var nodoEnAbierta = abiertahashmap(hijoActual.dbId)	
		
		if(hijoActual.gScore < nodoEnAbierta.gScore){
			abierta.remove(nodoEnAbierta)
			nodoEnAbierta.gScore = hijoActual.gScore
			nodoEnAbierta.fScore = hijoActual.fScore
			abierta.add(nodoEnAbierta)
		} 
	     }
	  }	
	}
      
    }

    def calculatePath(start: Map[String, String], goal: Map[String, String], hora: Int): (String, Double) = {

	var closedset = new HashMap[String, Node]()
 	var openset = new PriorityQueue[Node]()
	var opensethashmap = new HashMap[String, Node]()
        var startId = NearestNeighbor.find(start,0.0001)
	var endId = NearestNeighbor.find(goal,0.0001)
325.7649091554914
	println("Inicio:" + startId)
	println("Meta :" + endId)
	var NodoInicio = new Node(startId, null)
	NodoInicio.hScore = heuristicStimateOfDistance(startId, endId)
	NodoInicio.fScore = NodoInicio.gScore + NodoInicio.hScore
	//var NodoFin = new Node(endId, null)
	
	// A?ade el nodo inicial a la lista cerrada.
	closedset(startId) = NodoInicio
	
	//openset.addAll(vecinos(NodoInicio, hora, NodoFin))
         var hijosDeInicio = vecinos(NodoInicio, hora, endId)
	 
	 for(i<-0 to hijosDeInicio.size - 1){
	   var hijoI = hijosDeInicio.get(i).asInstanceOf[Node]
	   openset.add(hijoI)
	   opensethashmap(hijoI.dbId) = hijoI
	 }
	 //gestionarVecinos(hijosDeInicio, openset, opensethashmap, closedset)

	//while(opensethashmap.contains(endId) || openset.isEmpty == false){
	while(openset.isEmpty == false){ 	  
	  var x:Node = openset.poll()
	  opensethashmap.removeKey(x.dbId)
	  closedset(x.dbId) = x
	  var hijosdeX = vecinos(x, hora, endId)
	  gestionarVecinos(hijosdeX, openset, opensethashmap, closedset)
	  	
	}
	  var ruta = buildPath(closedset(endId))
	  println("la ruta es: "+ruta+" con costo: "+closedset(endId).gScore)
	  return (ruta, closedset(endId).gScore)
	
    }

}

