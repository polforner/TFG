hGetWord :: Handle -> IO ByteString
hGetWord h = hGetWordRec h B.empty

hGetWordRec :: Handle -> ByteString -> IO ByteString
hGetWordRec h r = do
  c <- B.hGet h 1
  if c == "" || c == " " || c == "\n" then return r
  else do
    cs <- hGetWordRec h (B.append r c)
    return cs

unfoldInput:: FilePath -> WriteChannel ([Char],Int) -> (ByteString -> ([Char],Int)) -> DP s ()
unfoldFile' file writeChannel fn =
  liftIO $ R.withFile file ReadMode $ \h -> unfoldM (hGetWord h) fn (H.hIsEOF h) writeChannel
