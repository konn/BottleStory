import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withBottleStory)

main :: IO ()
main = defaultMain fromArgs withBottleStory