module Main where

import Data.Coerce
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Text.Encoding
import Flood.API.Auth qualified as Flood
import Flood.API.Effect
import Flood.API.Torrents qualified as Flood
import GHC.Generics
import Network.Wai qualified as Wai
import Options.Applicative
import Prometheus
import TextShow
import Web.Firefly qualified as Firefly

main :: IO ()
main = do
  config <- execParser parseConfig
  Prometheus.register $ percentCompleteMetric config.flood
  putStrLn ("Starting Flood exporter on port " ++ show config.exporter.port)
  Firefly.run config.exporter.port $
    Firefly.route "/metrics" $
      fmap (Wai.responseLBS Firefly.ok200 []) exportMetricsAsText

percentCompleteMetric :: FloodConfig -> Metric ()
percentCompleteMetric config =
  Metric $ pure $ pure do
    infos <- fmap (Foldable.toList . (.torrents)) $ runFloodT config (Flood.authenticate *> Flood.list)
    pure [percentCompleteSampleGroup config infos]

percentCompleteSampleGroup :: FloodConfig -> [Flood.Info] -> SampleGroup
percentCompleteSampleGroup config infos =
  SampleGroup
    Prometheus.Info { metricName = "flood_torrent_percent_complete", metricHelp = "flood torrents?" }
    GaugeType
    (fmap (percentCompleteSample config) infos)

percentCompleteSample :: FloodConfig -> Flood.Info -> Sample
percentCompleteSample config info =
  Sample
    "flood_torrent_percent_complete"
    [ ("name", info.name)
    , ("tags", foldMap coerce $ List.intersperse "," info.tags)
    , ("trackers", foldMap coerce $ List.intersperse "," info.trackerURIs)
    , ("url", config.url <> ":" <> showt config.port)
    , ("hash", coerce info.hash)
    ]
    (encodeUtf8 (showt info.percentComplete))


data Config = Config
  { exporter :: ExporterConfig
  , flood :: FloodConfig
  } deriving (Generic, Show)

data ExporterConfig = ExporterConfig
  { port :: Int
  } deriving (Generic, Show)

parseConfig :: ParserInfo Config
parseConfig =
  info (helper <*> liftA2 Config parseExporterConfig parseFloodConfig)
    ( fullDesc
    <> progDesc "Command-line interface for Flood torrent manager"
    )

parseExporterConfig :: Parser ExporterConfig
parseExporterConfig = do
  port <-
    option auto $ mconcat
      [ long "exporter-port"
      , help "Exporter webserver port"
      , showDefault
      , value 61000
      , metavar "PORT"
      ]
  pure ExporterConfig { .. }

parseFloodConfig :: Parser FloodConfig
parseFloodConfig = do
  url <-
    strOption $ mconcat
      [ long "host"
      , help "Flood API host URL"
      , showDefault
      , value "http://localhost"
      , metavar "HOST"
      ]
  port <-
    option auto $ mconcat
      [ long "port"
      , help "Flood API port"
      , showDefault
      , value 3000
      , metavar "PORT"
      ]
  username <-
    strOption $ mconcat
      [ long "username"
      , help "Flood API username"
      , showDefault
      , value "_config"
      , metavar "USER"
      ]
  password <-
    strOption $ mconcat
      [ long "password"
      , help "Flood API password"
      , showDefault
      , value ""
      , metavar "PASS"
      ]
  pure FloodConfig { .. }
