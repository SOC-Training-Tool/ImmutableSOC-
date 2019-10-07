package soc.inventory

import protos.soc.inventory.PossibleHands.HandCombination
import protos.soc.inventory.PossibleHands.HandCombination.HandsWithMultiplier
import protos.soc.inventory.ProbableResourceSet.ProbableCardValue
import protos.soc.inventory.{ResourceCount, DevelopmentCard => PDev, PossibleHands => PPossible, ProbableResourceSet => PProb, PublicInventory => PPublic, Resource => PResource}
import soc.inventory.Inventory.NoInfo
import soc.inventory.developmentCard.{DevelopmentCardSet, DevelopmentCardSpecificationSet}
import soc.inventory.resources.CatanResourceSet.Resources
import soc.inventory.resources.{CatanResourceSet, PossibleHands, ProbableResourceSet}
import soc.proto.ProtoCoder
import soc.proto.ProtoCoder.ops._
import util.MapReverse

object ProtoImplicits {

  private lazy val resourceMap: Map[Resource, PResource] = Map(Brick -> PResource.BRICK, Ore -> PResource.ORE, Sheep -> PResource.SHEEP, Wheat -> PResource.WHEAT, Wood -> PResource.WOOD)
  private lazy val reverseResourceMap = MapReverse.reverseMap(resourceMap)
  private lazy val devCardMap: Map[DevelopmentCard, PDev] = Map(Knight -> PDev.KNIGHT, YearOfPlenty -> PDev.YEAR_OF_PLENTY, Monopoly -> PDev.MONOPOLY, RoadBuilder -> PDev.ROAD_BUILDER, CatanPoint -> PDev.VICTORY_POINT)
  private lazy val reverseDevCardMap = MapReverse.reverseMap(devCardMap)

  implicit val protoResource: ProtoCoder[Resource, PResource] = res => resourceMap(res)
  implicit val resourceFromProto: ProtoCoder[PResource, Resource] = protoRes => reverseResourceMap(protoRes)
  implicit val protoDevelopmentCard: ProtoCoder[DevelopmentCard, PDev] = dev => devCardMap(dev)
  implicit val developmentCardFromProto: ProtoCoder[PDev, DevelopmentCard] = protoDev => reverseDevCardMap(protoDev)

  implicit val protoResourceSet: ProtoCoder[CatanResourceSet[Int], Seq[ResourceCount]] = resources => resources.amountMap.toSeq.map { case (res, amt) => ResourceCount(res.proto, amt) }
  implicit val resourceSetFromProto: ProtoCoder[Seq[ResourceCount], CatanResourceSet[Int]] = { protoResources =>
     CatanResourceSet.fromMap(protoResources.map { case ResourceCount(res, amt) => res.proto -> amt}.toMap[Resource, Int])
  }

  implicit val protoProbableResourceSet: ProtoCoder[ProbableResourceSet, PProb] = { probableResourceSet =>
    PProb(Resource.list.filter(probableResourceSet.contains).map { res =>
      ProbableCardValue(res.proto, probableResourceSet.getKnownAmount(res), probableResourceSet.getUnknownAmount(res))
    })
  }

  implicit val probableResourceSetFromProto: ProtoCoder[PProb, ProbableResourceSet] = { protoProb =>
    val (known, unknown) = protoProb.probableResourceCards.foldLeft((CatanResourceSet.empty[Int], CatanResourceSet.empty[Double])) { case ((k, u), card) =>
      val res = card.`type`.proto
      (k.add(card.knownAmount, res), u.add(card.unknownAmount, res))
    }
    ProbableResourceSet(known, unknown)
  }

  implicit val protoPossibleHands: ProtoCoder[PossibleHands, PPossible] = { possibleHands =>
    implicit val protoHandCombination: ProtoCoder[Map[Int, (Resources, Int)], HandCombination] = { mapMult =>
      HandCombination(mapMult.view.mapValues { case (res, mult) => HandsWithMultiplier(res.proto, mult) }.toMap)
    }
    PPossible(possibleHands.hands.map(_.proto))
  }

  implicit val possibleHandsFromProto: ProtoCoder[PPossible, PossibleHands] = { protoPossible =>
    PossibleHands(protoPossible.hands.map {
      _.hand.view.mapValues { case HandsWithMultiplier(cards, mult) => (cards.proto, mult) }.toMap
    })
  }

  implicit val protoPublicInventory: ProtoCoder[NoInfo, PPublic] = { inv =>
    PPublic(
      inv.numCards,
      inv.numUnplayedDevCards,
      inv.playedDevCards.cards)
  }

  implicit val publicInventoryFromProto: ProtoCoder[PPublic, NoInfo] = { protoInv =>
    NoInfoInventory(
      DevelopmentCardSpecificationSet( protoInv.playedDevelopmentCards.toList),
      protoInv.cardCount,
      protoInv.developmentCardCount
    )
  }



//  implicit val protoDevCardSet: ProtoCoder[DevelopmentCardSet[Int], PDevSet] = dev => PDevSet(CatanSet.toList(dev).map(_.proto))
//  implicit val devCardFromProto: ProtoCoder[PDevSet, DevelopmentCardSet[Int]] = { protoDevCards =>
//    import DevelopmentCardSet._
//    CatanSet.fromList[DevelopmentCard, DevelopmentCardSet[Int]](protoDevCards.developmentCards.map(_.proto))
//  }
//  implicit val protoProbableInventory: ProtoCoder[ProbableInfo, PProbable] = inv => PProbable(inv.probableResourceSet.proto, null)
//  implicit val protoPerfectInventory: ProtoCoder[PerfectInfo, PPerfect] = { inv =>
//    PPerfect(
//      inv.resourceSet.proto,
//      inv.playedDevCards.proto,
//      inv.canPlayDevCards.proto,
//      inv.cannotPlayDevCards.proto
//    )
//  }
//
//  implicit val perfectInventoryFromProto: ProtoCoder[PPerfect, PerfectInfo] = { protoInv =>
//    import DevelopmentCardSet._
//    import CatanResourceSet._
//    PerfectInfoInventory(
//      CatanSet.fromList[Resource, CatanResourceSet[Int]](protoInv.resourceCards.resourceCards.map(_.proto).toList),
//      CatanSet.fromList[DevelopmentCard, DevelopmentCardSet[Int]](protoInv.playedDevCards.developmentCards.map(_.proto).toList),
//      CatanSet.fromList[DevelopmentCard, DevelopmentCardSet[Int]](protoInv.canPlayDevCards.developmentCards.map(_.proto).toList),
//      CatanSet.fromList[DevelopmentCard, DevelopmentCardSet[Int]](protoInv.cannotPlayDevCards.developmentCards.map(_.proto).toList)
//    )
//  }
//}
}
