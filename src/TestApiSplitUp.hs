{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TestApi where

import Data.OpenApi (OpenApi)
import Data.Text (Text)
import Data.UUID (UUID)
import Servant
import qualified Servant.OpenApi as OpenApi

type XTotalCountHeader v = Headers '[Header "X-Total-Count" Int] v

type FirstPartOfAPi =
  QueryParam "_start" Int
    :> QueryParam "_end" Int
    :> Get '[JSON] (XTotalCountHeader [Text])
    :<|> ReqBody '[JSON] Text
      :> Post '[JSON] Text
    :<|> Capture "id" UUID :> Delete '[JSON] Text
    :<|> Capture "id" UUID :> Get '[JSON] Text
    :<|> Capture "id" UUID :> ReqBody '[JSON] Text :> Put '[JSON] Text

type SecondPartOfApi =
  QueryParam "_start" Int
    :> QueryParam "_end" Int
    :> Get '[JSON] (XTotalCountHeader [Text])
    :<|> ReqBody '[JSON] Text
      :> Post '[JSON] Text
    :<|> Capture "geoId" UUID
      :> Get '[JSON] Text

type ThirdPartOfApi =
  QueryParam "_start" Int
    :> QueryParam "_end" Int
    :> Get '[JSON] (XTotalCountHeader [Text])
    :<|> Capture "id" UUID
      :> Get '[JSON] Text

type FourthPartOfApi =
  QueryParam "_start" Int
    :> QueryParam "_end" Int
    :> Get '[JSON] (XTotalCountHeader [Text])

type TestApiSplitUp =
  "v1"
    :> "first"
    :> FirstPartOfAPi
    :<|> "v1"
      :> "second"
      :> SecondPartOfApi
    :<|> "v1"
      :> "third"
      :> ThirdPartOfApi
    :<|> "v1"
      :> "fourth"
      :> FourthPartOfApi
    :<|> Get '[JSON] NoContent
    :<|> Raw

testApiSplitUp :: Proxy TestApiSplitUp
testApiSplitUp = Proxy

openApiSplitUp :: OpenApi
openApiSplitUp = OpenApi.toOpenApi testApiSplitUp
