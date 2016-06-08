{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types #-}
module Main where

import Emulator
import Emulator.IO
import Emulator.Monad
import Instruction
import Memory

import Control.Monad.Trans (liftIO)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Word (Word16)
import Text.Printf (printf)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Events (Event(Expose))
import Graphics.Rendering.Cairo
import Control.Concurrent
import Data.IORef

inRange :: Ord a => (a,a) -> a -> Bool
inRange (x,y) z
    | z <= y && z >= x = True
    | otherwise = False

splitEvery :: Int -> [a] -> [[a]]
splitEvery i ls = map (take i) ((splitter ls) (:) []) where
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n

createMemContext :: IO PangoContext
createMemContext = do
    cai  <- cairoCreateContext Nothing
    desc <- fontDescriptionFromString "Lucida Console 8"
    contextSetFontDescription cai desc
    return cai

addPointer :: PangoLayout -> Int -> Color -> IO ()
addPointer lay pos color = do
    attrs <- layoutGetAttributes lay
    layoutSetAttributes lay $ AttrBackground pos (pos + 4) color : concat attrs

formatMemory :: Word16 -> [String] -> String
formatMemory x y = concat . take 8 $ zipWith (++) (map (\z -> printf "%04X:" z :: String) ([x,x+8..] :: [Word16])) y

initializeColor :: WidgetClass a => a -> IO a
initializeColor ob1 = widgetModifyBg ob1 StateNormal (Color 0 0 0) >> return ob1

updateRegisters :: DrawingArea -> PangoContext -> Address -> Word16 -> IO ()
updateRegisters dA fC (Register rA) v = do
    

updateMemory :: DrawingArea -> PangoContext -> (Word16, Word16) -> (Address -> IO Word16) -> IO ()
updateMemory mA fC rng@(rX, rY) load = do
    memArr <- mapM (load . Word) [rX..rY]
    regSP <- load $ Register SP
    regPC <- load $ Register PC
    drawWindow <- widgetGetDrawWindow mA
    lay <- layoutText fC $ formatMemory rX $ map ((++"\n") . concat . map (' ':) . map (\x -> printf "%04X" x)) (splitEvery 8 memArr)
    layoutSetAttributes lay [AttrForeground 0 (-1) $ Color 65535 65535 65535]
    if (regSP /= regPC) then do
        when (checkrng regSP) $ addPointer lay (atPosition regSP) $ Color 0 30000 30000
        when (checkrng regPC) $ addPointer lay (atPosition regPC) $ Color 30000 0 30000
    else
        when (inRange rng regSP) $ addPointer lay (atPosition regSP) $ Color 30000 30000 0
    renderWithDrawable drawWindow $ rectangle 20 100 320 90 >> fill >> moveTo 20 100 >> showLayout lay
        where datRange = rX - rY + 1
              z x' = fromIntegral (x' `mod` datRange)
              atPosition x' = (z x' `div` 8) * 46 + (z x' `mod` 8) * 5 + 6
			  checkrng = inRange rng

{-processDisplay :: DrawingArea -> DrawingArea -> PangoContext -> IORef (Word16, Word16, Word16) -> IORef (Word16, Word16) -> Address -> Word16 -> (Address -> IO Word16) -> IO ()
processDisplay mA dA fC dAs mR (Register x) y load
    | x == SP || x == PC = do
        range <- readIORef mR
        if inRange range y then do
            updateRegisters mA fC (Register x) y
            updateMemory mA fC range load
        else
            updateRegisters mA fC (Register x) y
    | otherwise = updateRegisters mA fC (Register x) y
processDisplay mA dA fC dAs mR (Word x) y load = do
    range <- readIORef mR
    when (inRange range x) $ updateMemory mA fC range load-}
	
processDisplay :: DrawingArea -> DrawingArea -> PangoContext -> IORef (Word16, Word16) -> Address -> Word16 -> (Address -> IO Word16) -> IO ()
processDisplay mA dA fC mR (Register x) y load
    | x == SP || x == PC = do
        range <- readIORef mR
        if inRange range y then do
            updateRegisters mA fC (Register x) y
            updateMemory mA fC range load
        else
            updateRegisters mA fC (Register x) y
    | otherwise = updateRegisters mA fC (Register x) y
processDisplay mA dA fC mR (Word x) y load = do
    range <- readIORef mR
    when (inRange range x) $ updateMemory mA fC range load

main :: IO ()
main = do
    initGUI
    builder     <- builderNew
    builderAddFromFile builder "interface.glade"
    window      <- initializeColor =<< builderGetObject builder castToWindow "window1"
    memoryArea  <- initializeColor =<< builderGetObject builder castToDrawingArea "memoryDump"
    displayArea <- initializeColor =<< builderGetObject builder castToDrawingArea "displayWindow"
    textView    <- initializeColor =<< builderGetObject builder castToTextView "disassembly"
    dispAddrs   <- newIORef (8000,0,0) -- Video start, Font Start, Palette Start
    memRange    <- newIORef (0,63)
    isRunning   <- newIORef False
    dispFont    <- createMemContext
    let processInterface = processDisplay memoryArea displayArea dispFont
    onDestroy window mainQuit
    widgetShowAll window
    forkIO $ mainGUI
	--runIOEmulator (processInterface dispAddrs memRange)
    runIOEmulator (processInterface memRange) $ do
        liftIO $ threadDelay 300000
        performOperation $ BasicInstruction SET (OpRegister PC) (OpLiteral 1)
        liftIO $ threadDelay 300000
        store (Word 9) 0xABCD