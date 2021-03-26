{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.OpenApi (OpenApi)
import Data.Text (Text)
import Data.UUID (UUID)
import Servant
import qualified Servant.OpenApi as OpenApi

type XTotalCountHeader v = Headers '[Header "X-Total-Count" Int] v

type TestApi =
  "v1"
    :> "rules"
    :> QueryParam "_start" Int
    :> QueryParam "_end" Int
    :> Get '[JSON] (XTotalCountHeader [Text])
    :<|> "v1"
      :> "rules"
      :> ReqBody '[JSON] Text
      :> Post '[JSON] Text
    :<|> "v1" :> "rules" :> Capture "id" UUID :> Delete '[JSON] Text
    :<|> "v1" :> "rules" :> Capture "id" UUID :> Get '[JSON] Text
    :<|> "v1" :> "rules" :> Capture "id" UUID :> ReqBody '[JSON] Text :> Put '[JSON] Text
    :<|> "v1"
      :> "geojson"
      :> QueryParam "_start" Int
      :> QueryParam "_end" Int
      :> Get '[JSON] (XTotalCountHeader [Text])
    :<|> "v1"
      :> "geojson"
      :> ReqBody '[JSON] Text
      :> Post '[JSON] Text
    :<|> "v1"
      :> "geojson"
      :> Capture "geoId" UUID
      :> Get '[JSON] Text
    :<|> "v1"
      :> "validate"
      :> QueryParam "_start" Int
      :> QueryParam "_end" Int
      :> Get '[JSON] (XTotalCountHeader [Text])
    :<|> "v1"
      :> "validate"
      :> Capture "validationId" UUID
      :> Get '[JSON] Text
    :<|> "v1"
      :> "discovery_all"
      :> QueryParam "_start" Int
      :> QueryParam "_end" Int
      :> Get '[JSON] (XTotalCountHeader [Text])
    :<|> Get '[JSON] NoContent
    :<|> Raw


testApi :: Proxy TestApi
testApi = Proxy

openApi :: OpenApi
openApi = OpenApi.toOpenApi testApi
