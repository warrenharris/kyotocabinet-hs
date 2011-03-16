{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright 2011, Google Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following disclaimer
in the documentation and/or other materials provided with the
distribution.
    * Neither the name of Google Inc. nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module Main where

import Database.KyotoCabinet.Db

import Control.Exception
import qualified Data.ByteString.Char8 as BS
import Prelude hiding (catch)

import System.Mem

visitfull :: KcVisitFull
visitfull key val = do
  putStrLn (BS.unpack key ++ ":" ++ BS.unpack val)
  return KCVISNOP

visitempty :: KcVisitEmpty
visitempty key = do
  putStrLn (BS.unpack key ++ " is missing")
  return $ KCVISNOP
--  let rv = BS.pack ("foo"++"bar")
--  return $ KCVISSET rv

main = do
  -- create and open the database
  kcwithdbopen "casket.kch" [KCOWRITER, KCOCREATE] $ \db -> do

    -- store records
    (do kcdbset db (BS.pack "foo") (BS.pack "hop")
        kcdbset db (BS.pack "bar") (BS.pack "step")
        kcdbset db (BS.pack "baz") (BS.pack "jump")
        kcdbappend db (BS.pack "baz") (BS.pack "run")
        kcdbappend db (BS.pack "baz") (BS.pack "fly"))
      `catch` \(exn::KcException) -> putStrLn ("set error: "++show exn)

    -- retrieve a record
    (do vbuf <- kcdbget db (BS.pack "foo")
        putStrLn (BS.unpack vbuf))
      `catch` \(exn::KcException) -> putStrLn ("get error: "++show exn)

    -- traverse records
    kcwithdbcursor db $ \cur -> do
      kccurjump cur
      let loop = do (key, val) <- kccurget cur True
                    putStrLn (BS.unpack key ++ ":" ++ BS.unpack val)
                    loop
      loop `catch` \(exn::KcException) -> return ()

    -- retrieve a record with visitor
    kcdbaccept db (BS.pack "dummy") visitfull visitempty False
      `catch` \(exn::KcException) ->
        kcdbaccept db (BS.pack "foo") visitfull visitempty False
          `catch` \(exn::KcException) -> putStrLn ("accept error: "++show exn)

    -- traverse records with visitor
    kcdbiterate db visitfull False
      `catch` \(exn::KcException) -> putStrLn ("iterate error: "++show exn)

    (do prefixes <- kcdbmatchprefix db (BS.pack "b") 3
        putStrLn (show prefixes))
      `catch` \(exn::KcException) -> putStrLn ("prefix error: "++show exn)

    (do prefixes <- kcdbmatchregex db (BS.pack ".a.*") 10
        putStrLn (show prefixes))
      `catch` \(exn::KcException) -> putStrLn ("regexp error: "++show exn)

    kcwithlazydbcursorcontext $ \cc -> do
      cur <- kclazydbcursor db cc
      vals <- kclazycurgetvalues cc cur (BS.pack "baz")
      putStrLn (show vals)
