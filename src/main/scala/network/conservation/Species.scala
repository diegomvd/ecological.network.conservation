package network.conservation

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import scala.util.Random
import scala.jdk.CollectionConverters.*

case class Species(id: Int, trophicLevel: Int, abundance: Double, homeRange: Double)

object Species:

  def apply(id: Int, trophicLevel: Int, rnd: Random, basalHomeRange: Double):
  Species =
    val (abundance, homeRange)  = speciesTraits(trophicLevel,basalHomeRange)
    Species(id, trophicLevel, abundance = abundance, homeRange = homeRange)

  private def speciesTraits(trophicLevel: Int, basalHomeRange: Double) =
    // for each trophic level 1 function abundance degree
    val relativeAbundance = math.pow(5,2-trophicLevel)
    val homeRange = basalHomeRange * math.pow(5,trophicLevel)
    (relativeAbundance, homeRange)

  def calculateTrophicLevel(id: Int, metaWeb: DefaultDirectedGraph[Int,DefaultEdge]): Int =

    val predators = metaWeb.incomingEdgesOf(id).asScala.map(link => metaWeb.getEdgeSource(link))
    val preys = metaWeb.outgoingEdgesOf(id).asScala.map(link => metaWeb.getEdgeTarget(link))

    if predators.isEmpty then 2 // top predator
    else {
      if preys.isEmpty then 0 // basal
      else 1 // primary consumer
    }

end Species
