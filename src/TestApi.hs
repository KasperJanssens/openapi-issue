{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TestApi where

import Data.OpenApi (OpenApi)
import Data.Text (Text)
import Data.UUID (UUID)
import Servant
import qualified Servant.OpenApi as OpenApi

type XTotalCountHeader v = Headers '[Header "X-Total-Count" Int] v

type TestApi =
  "v1"
    :> "first"
    :> QueryParam "_start" Int
    :> QueryParam "_end" Int
    :> Get '[JSON] (XTotalCountHeader [Text])
    :<|> "v1"
      :> "first"
      :> ReqBody '[JSON] Text
      :> Post '[JSON] Text
    :<|> "v1" :> "first" :> Capture "id" UUID :> Delete '[JSON] Text
    :<|> "v1" :> "first" :> Capture "id" UUID :> Get '[JSON] Text
    :<|> "v1" :> "first" :> Capture "id" UUID :> ReqBody '[JSON] Text :> Put '[JSON] Text
    :<|> "v1"
      :> "second"
      :> QueryParam "_start" Int
      :> QueryParam "_end" Int
      :> Get '[JSON] (XTotalCountHeader [Text])
    :<|> "v1"
      :> "second"
      :> ReqBody '[JSON] Text
      :> Post '[JSON] Text
    :<|> "v1"
      :> "second"
      :> Capture "geoId" UUID
      :> Get '[JSON] Text
    :<|> "v1"
      :> "third"
      :> QueryParam "_start" Int
      :> QueryParam "_end" Int
      :> Get '[JSON] (XTotalCountHeader [Text])
    :<|> "v1"
      :> "third"
      :> Capture "id" UUID
      :> Get '[JSON] Text
    :<|> "v1"
      :> "fourth"
      :> QueryParam "_start" Int
      :> QueryParam "_end" Int
      :> Get '[JSON] (XTotalCountHeader [Text])
    :<|> Get '[JSON] NoContent
    :<|> Raw

testApi :: Proxy TestApi
testApi = Proxy

openApi :: OpenApi
openApi = OpenApi.toOpenApi testApi
