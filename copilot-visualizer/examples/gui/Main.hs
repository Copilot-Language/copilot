import Copilot.Visualize.Live

main = visualize spec

spec :: String
spec = unlines
  [ "let temperature :: Stream Word8"
  , "    temperature = extern \"temperature\" (Just [0, 15, 20, 25, 30])"
  , ""
  , "    ctemp :: Stream Float"
  , "    ctemp = (unsafeCast temperature) * (150.0 / 255.0) - 50.0"
  , ""
  , "    trueFalse :: Stream Bool"
  , "    trueFalse = [True] ++ not trueFalse"
  , ""
  , "in do trigger \"heaton\"  (temperature < 18) [arg ctemp, arg (constI16 1), arg trueFalse]"
  , "      trigger \"heatoff\" (temperature > 21) [arg (constI16 1), arg ctemp]"
  , "      observer \"temperature\" temperature"
  , "      observer \"temperature2\" (temperature + 1)"
  ]
