{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Beeline.Examples
  ( shipmentRouter
  , shipmentRouterWithoutOperators
  ) where

import Beeline.Routing ((/+), (/-), (/:), (/>))
import qualified Beeline.Routing as R
import Shrubbery (Union)

{- |
  Examples routes for a hypothetical shipment api that supports basic
  CRUD operations and has a sub-resource of items.
-}
type ShipmentRoutes =
  Union
    '[ ListShipments
     , CreateShipment
     , GetShipment
     , UpdateShipment
     , DeleteShipment
     , ShipmentItemRoute
     ]

data ListShipments
  = ListShipments

data CreateShipment
  = CreateShipment

newtype ShipmentId = ShipmentId Int

newtype GetShipment = GetShipment
  { getShipmentId :: ShipmentId
  }

newtype UpdateShipment = UpdateShipment
  { updateShipmentId :: ShipmentId
  }

newtype DeleteShipment = DeleteShipment
  { deleteShipmentId :: ShipmentId
  }

newtype ShipmentItemRoute = ShipmentItemRoute
  { shipmentItemRoute :: ItemRoutes
  }

shipmentIdParam :: R.ParameterDefinition ShipmentId
shipmentIdParam =
  R.coerceParam (R.integralParam "shipmentId" :: R.ParameterDefinition Int)

shipmentRouter :: R.Router r => r ShipmentRoutes
shipmentRouter =
  R.routeList $
    R.get (R.make ListShipments)
      /: R.post (R.make CreateShipment)
      /: R.get (R.make GetShipment /+ R.Param shipmentIdParam getShipmentId)
      /: R.patch (R.make UpdateShipment /+ R.Param shipmentIdParam updateShipmentId)
      /: R.delete (R.make DeleteShipment /+ R.Param shipmentIdParam deleteShipmentId)
      /: (R.make ShipmentItemRoute /- "items" /> R.Subrouter itemRouter shipmentItemRoute)
      /: R.emptyRoutes

shipmentRouterWithoutOperators :: R.Router r => r ShipmentRoutes
shipmentRouterWithoutOperators =
  R.routeList
    . R.addRoute (R.get (R.make ListShipments))
    . R.addRoute (R.post (R.make CreateShipment))
    . R.addRoute (R.get (R.make GetShipment `R.param` R.Param shipmentIdParam getShipmentId))
    . R.addRoute (R.patch (R.make UpdateShipment `R.param` R.Param shipmentIdParam updateShipmentId))
    . R.addRoute (R.delete (R.make DeleteShipment `R.param` R.Param shipmentIdParam deleteShipmentId))
    . R.addRoute (R.make ShipmentItemRoute `R.piece` "items" `R.subrouter` R.Subrouter itemRouter shipmentItemRoute)
    $ R.emptyRoutes

{- |
  Examples routes for a hypothetical shipment items api that is embedded as
  sub router within the shipment routes.
-}
type ItemRoutes =
  Union
    '[ ListItems
     , CreateItem
     , GetItem
     , UpdateItem
     , DeleteItem
     ]

data ListItems = ListItems

data CreateItem = CreateItem

newtype ItemId = ItemId Int

data GetItem = GetItem
  { getItemId :: ItemId
  }

data UpdateItem = UpdateItem
  { updateItemId :: ItemId
  }

data DeleteItem = DeleteItem
  { deleteItemId :: ItemId
  }

itemIdParam :: R.ParameterDefinition ItemId
itemIdParam =
  R.coerceParam (R.integralParam "itemId" :: R.ParameterDefinition Int)

itemRouter :: R.Router r => r ItemRoutes
itemRouter =
  R.routeList $
    R.get (R.make ListItems)
      /: R.post (R.make CreateItem)
      /: R.get (R.make GetItem /+ R.Param itemIdParam getItemId)
      /: R.patch (R.make UpdateItem /+ R.Param itemIdParam updateItemId)
      /: R.delete (R.make DeleteItem /+ R.Param itemIdParam deleteItemId)
      /: R.emptyRoutes
