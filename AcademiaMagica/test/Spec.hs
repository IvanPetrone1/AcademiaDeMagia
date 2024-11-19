import Test.Hspec
import Lib
import Text.Show.Functions

main :: IO ()
main = hspec $ do
  describe "Academia Magica test" $ do
    it "Mago afectado por curacion" $ do
      lagrimaFenix 50 potter `shouldBe` Mago{nombre = "Harry", edad = 20, salud = 150, hechizos = [lagrimaFenix 20]}
      lagrimaFenix 0 potter `shouldBe` Mago{nombre = "Harry", edad = 20, salud = 100, hechizos = [lagrimaFenix 20]}
    it "Mago afectado por sectum" $ do
      sectumSempra potter `shouldBe` Mago{nombre = "Harry", edad = 20, salud = 90, hechizos = [lagrimaFenix 20]}
      sectumSempra weasley `shouldBe` Mago{nombre = "Ron", edad = 21 , salud = 3, hechizos = [lagrimaFenix 30, sectumSempra]}
      sectumSempra malfoy `shouldBe` Mago{nombre = "Draco", edad = 22, salud = 5, hechizos = [lagrimaFenix 10, sectumSempra]}
    it "Mago afectado por obliviate" $ do
      obliviate 1 potter `shouldBe` Mago{nombre = "Harry", edad = 20, salud = 100, hechizos = []}
      obliviate 2 granger `shouldBe` Mago{nombre = "Hermione", edad = 21, salud = 70, hechizos = [obliviate 1]}
      obliviate 10 goyle `shouldBe` Mago{nombre = "Gregory", edad = 20, salud = 2, hechizos = []}
    it "Mago afectado por confundus" $ do
      confundus potter `shouldBe` Mago{nombre = "Harry", edad = 20, salud = 120, hechizos = [lagrimaFenix 20]}
      confundus snape `shouldBe` Mago{nombre = "Severus", edad = 50, salud = 190, hechizos = [sectumSempra, obliviate 2, lagrimaFenix 10]}                         
      confundus dumbledore `shouldBe` Mago{nombre = "Albus", edad = 50, salud = 350, hechizos = [ obliviate 2, lagrimaFenix 10, sectumSempra]}
    it "Poder del Mago" $ do
      poder potter `shouldBe` 120
      poder dumbledore `shouldBe` 500
    it "Poder del Mago sin hechizos" $ do
      poder crabbe `shouldBe` 6
      poder goyle `shouldBe` 2
    it "Daño producido al mago" $ do
      daño granger sectumSempra `shouldBe` 10
      daño crabbe sectumSempra `shouldBe` 3
    it "Sin Daños al mago" $ do
      daño dumbledore (obliviate 1) `shouldBe` 0
    it "Curacion al mago" $ do
      daño snape (lagrimaFenix 10) `shouldBe` -10
      daño potter (lagrimaFenix 20) `shouldBe` -20
    it "Diferencia de poder entre magos" $ do
      diferenciaDePoder potter weasley `shouldBe` 73
      diferenciaDePoder snape dumbledore `shouldBe` 150
      diferenciaDePoder granger malfoy `shouldBe` 79
    it "Mago sin hechizos" $ do
      magoSinHechizos "Harry" hogwarts `shouldBe` False
      magoSinHechizos "Spartacus" koldovstoretz `shouldBe` False
      magoSinHechizos "Vincent" hogwarts `shouldBe` True
    it "Hay algun mago llamado Hagrid sin hechizos" $ do
      magoSinHechizos "Hagrid" hogwarts `shouldBe` True
      magoSinHechizos "Hagrid" beauxbatons `shouldBe` False
    it "Todos los magos viejos son nionios" $ do 
      viejosNionios hogwarts `shouldBe` False 
      viejosNionios koldovstoretz `shouldBe` False
      viejosNionios beauxbatons `shouldBe` True
    {-it "Hechizo mas efectivo contra otro mago" $ do
      mejorHechizoContra potter malfoy `shouldBe` sectumSempra
    
    it "Mejor oponente para un mago dentro de una academia" $do
      mejorOponente potter hogwarts `shouldBe` Mago{nombre = "Gregory", edad = 20, salud = 2, hechizos = []}
      mejorOponente granger koldovstoretz `shouldBe` Mago {nombre = "Stella", edad = 50, salud = 140, hechizos = [lagrimaFenix 10, sectumSempra]}
    it "Mago no puede ganar" $ do
      noPuedeGanarle crabbe potter `shouldBe` True
      noPuedeGanarle goyle snape `shouldBe` True
    it "Mago si puede ganar" $ do -- Casos donde noPuedeGanarle deberia dar ´False´
      noPuedeGanarle granger weasley `shouldBe` False
      noPuedeGanarle dumbledore snape `shouldBe` False
    -}