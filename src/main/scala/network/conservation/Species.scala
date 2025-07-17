package network.conservation

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import scala.util.Random
import scala.jdk.CollectionConverters.*

case class Species(id: Int, bodySize: Double, abundance: Double, homeRange: Double, trophicLevel: Int)

object Species:

  def apply(id: Int, bodySize: Double,  basalHomeRange: Double, trophicLevel: Int, rnd: Random):
  Species =
    val (abundance, homeRange)  = speciesTraits(bodySize,basalHomeRange)
    Species(id, bodySize, abundance = abundance, homeRange = homeRange, trophicLevel = trophicLevel )

  private def speciesTraits(bodySize: Double, basalHomeRange: Double) =

    val relativeAbundance = math.pow(bodySize,-0.75)
    val homeRange = basalHomeRange * math.pow(bodySize,0.75)
    (relativeAbundance, homeRange)

  def calculateTrophicLevel(id: (Int,Double), metaWeb: DefaultDirectedGraph[(Int,Double),DefaultEdge]): Int =

    val predators = metaWeb.incomingEdgesOf(id).asScala.map(link => metaWeb.getEdgeSource(link))
    val preys = metaWeb.outgoingEdgesOf(id).asScala.map(link => metaWeb.getEdgeTarget(link))

    if predators.isEmpty then 2 // top predator
    else {
      if preys.isEmpty then 0 // basal
      else 1 // primary consumer
    }

end Species
