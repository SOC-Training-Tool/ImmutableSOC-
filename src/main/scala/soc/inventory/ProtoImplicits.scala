package soc.inventory

import protos.soc.inventory.PossibleHands.HandCombination
import protos.soc.inventory.PossibleHands.HandCombination.HandsWithMultiplier
import protos.soc.inventory.ProbableResourceSet.ProbableCardValue
import protos.soc.inventory.{DevelopmentCard => PDev, DevelopmentCardSet => PDevSet, PerfectInventory => PPerfect, PossibleHands => PPossible, ProbableInventory => PProbable, ProbableResourceSet => PProb, PublicInventory => PPublic, Resource => PResource, ResourceSet => PResSet}
import soc.inventory.Inventory.{NoInfo, PerfectInfo, ProbableInfo}
import soc.inventory.developmentCard.DevelopmentCardSet
import soc.inventory.resources.CatanResourceSet.Resources
import soc.inventory.resources.{CatanResourceSet, PossibleHands, ProbableResourceSet}
import soc.proto.ProtoCoder
import soc.proto.ProtoCoder.ops._
import util.MapReverse

object ProtoImplicits {

  private lazy val resourceMap: Map[Resource, PResource] = Map(Brick -> PResource.BRICK,  Ore -> PResource.ORE, Sheep -> PResource.SHEEP, Wheat -> PResource.WHEAT, Wood -> PResource.WOOD)
  private lazy val reverseResourceMap = MapReverse.reverseMap(resourceMap)
  private lazy val devCardMap: Map[DevelopmentCard, PDev] = Map(Knight -> PDev.KNIGHT, YearOfPlenty -> PDev.YEAR_OF_PLENTY, Monopoly -> PDev.MONOPOLY, RoadBuilder -> PDev.ROAD_BUILDER, CatanPoint -> PDev.POINT)
  private lazy val reverseDevCardMap = MapReverse.reverseMap(devCardMap)

  implicit val protoResource: ProtoCoder[Resource, PResource] = res => resourceMap(res)
  implicit val resourceFromProto: ProtoCoder[PResource, Resource] = protoRes => reverseResourceMap(protoRes)
  implicit val protoDevelopmentCard: ProtoCoder[DevelopmentCard, PDev] = dev => devCardMap(dev)
  implicit val developmentCardFromProto: ProtoCoder[PDev, DevelopmentCard] = protoDev => reverseDevCardMap(protoDev)

  implicit val protoResourceSet: ProtoCoder[CatanResourceSet[Int], PResSet] = resources => PResSet(CatanSet.toList(resources).map(_.proto))
  implicit val resourceSetFromProto: ProtoCoder[PResSet, CatanResourceSet[Int]] = { protoResources =>
    import CatanResourceSet._
    CatanSet.fromList[Resource, CatanResourceSet[Int]](protoResources.resourceCards.map(_.proto))
  }

  implicit val protoDevCardSet: ProtoCoder[DevelopmentCardSet[Int], PDevSet] = dev => PDevSet(CatanSet.toList(dev).map(_.proto))
  implicit val devCardFromProto: ProtoCoder[PDevSet, DevelopmentCardSet[Int]] = { protoDevCards =>
    import DevelopmentCardSet._
    CatanSet.fromList[DevelopmentCard, DevelopmentCardSet[Int]](protoDevCards.developmentCards.map(_.proto))
  }

  implicit val protoProbableResourceSet: ProtoCoder[ProbableResourceSet, PProb] = { probableResourceSet =>
    PProb(Resource.list.filter(probableResourceSet.contains).map { res =>
      ProbableCardValue(res.proto, probableResourceSet.getKnownAmount(res), probableResourceSet.getUnknownAmount(res))
    })
  }

  implicit val probableResourceSetFromProto: ProtoCoder[PProb, ProbableResourceSet] = { protoProb =>
    val (known, unknown) = protoProb.probableResourceCards.foldLeft((CatanResourceSet.empty[Int], CatanResourceSet.empty[Double])) { case ((k, u), card) =>
      val res = card.resType.proto
      (k.add(card.knownAmount, res), u.add(card.unknownAmount, res))
    }
    ProbableResourceSet(known, unknown)
  }

  implicit val protoPossibleHands: ProtoCoder[PossibleHands, PPossible] = { possibleHands =>
    implicit val protoHandCombination: ProtoCoder[Map[Int, (Resources, Int)], HandCombination] = { mapMult =>
      HandCombination(mapMult.view.mapValues { case (res, mult) => HandsWithMultiplier(res.proto, mult)}.toMap)
    }
    PPossible(possibleHands.hands.map(_.proto))
  }

  implicit val possibleHandsFromProto: ProtoCoder[PPossible, PossibleHands] = { protoPossible =>
    PossibleHands(protoPossible.hands.map {
      _.hand.view.mapValues { case HandsWithMultiplier(cards, mult) => (cards.proto, mult) }.toMap
    })
  }

  implicit val protoPublicInventory: ProtoCoder[Inventory[_], PPublic] = inv => PPublic(inv.numCards, inv.numUnplayedDevCards, inv.playedDevCards.proto)
  implicit val protoProbableInventory: ProtoCoder[ProbableInfo, PProbable] = inv => PProbable(inv.probableResourceSet.proto, null)
  implicit val protoPerfectInventory: ProtoCoder[PerfectInfo, PPerfect] = { inv =>
    PPerfect(
      inv.resourceSet.proto,
      inv.playedDevCards.proto,
      inv.canPlayDevCards.proto,
      inv.cannotPlayDevCards.proto
    )
  }
//
//  implicit val publicInventoryToProto: ProtoCoder[PPublic, NoInfo] = { protoInv =>
//
//
//  }




}
