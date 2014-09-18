module TGA(createTGA,putTGApixels)
	where

import System.IO
import qualified Data.ByteString as B
import Data.Word

pixeltopacked (u,v,w) = 
		B.pack (
				[fromIntegral (w `mod` 256), 
				 fromIntegral (v `mod` 256), 
				 fromIntegral (u `mod` 256)]::[Word8]
				)

createTGA :: String -> (Int, Int) -> IO Handle
createTGA filename (sizex,sizey) = do
	fi <- openFile filename WriteMode
					-- O L 	D
	B.hPut fi zero  -- 0 1 	Number of Characters in Identification Field.
	B.hPut fi zero  -- 1 1	Color Map Type.
	B.hPut fi two	-- 2 1	Image Type Code. 
	B.hPut fi zero  -- 3 5	Color Map Specification.
	B.hPut fi zero	--  
	B.hPut fi zero	-- 5
	B.hPut fi zero	--
	B.hPut fi zero	-- 7
	B.hPut fi zero	-- 8 10 Image Specification. 
	B.hPut fi zero
	B.hPut fi zero  -- 10
	B.hPut fi zero
	B.hPut fi $ packint $ fromIntegral (sizex `mod` 256)
	B.hPut fi $ packint $ fromIntegral (sizex `div` 256)
	B.hPut fi $ packint $ fromIntegral (sizey `mod` 256)
	B.hPut fi $ packint $ fromIntegral (sizey `div` 256)
	B.hPut fi $ packint 24
	B.hPut fi zero	-- 		17 1 Image Descriptor Byte.
--	B.hPut fi zero  -- 18 var Image Identification Field.	

	return fi

	where
		packint i 	= B.pack ([i]::[Word8])
		zero 		= packint 0
		two			= packint 2

putTGApixel :: Handle -> (Int,Int,Int) -> IO ()
putTGApixel h p = B.hPut h (pixeltopacked p)

putTGApixels :: Handle -> [(Int,Int,Int)] -> Int -> Int -> IO ()
putTGApixels h [] _ _ 				= putStrLn "Done."
putTGApixels h (x:t) progress max	= do 
			if 100*progress `div` max == round (100*fromIntegral progress / fromIntegral max) 	then putStr (show (100*progress `div` max) ++ "%\r")
															else return ()
			B.hPut h $ pixeltopacked x
			putTGApixels h t (progress+1) max
