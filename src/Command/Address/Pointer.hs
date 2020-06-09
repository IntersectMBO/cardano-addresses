{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Command.Address.Pointer
    ( Cmd
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( ChainPointer (..), bech32, unsafeMkAddress )
import Cardano.Address.Style.Shelley
    ( ErrExtendAddress (..) )
import Numeric.Natural
    ( Natural )
import Options.Applicative
    ( CommandFields
    , Mod
    , argument
    , auto
    , command
    , footerDoc
    , header
    , help
    , helper
    , info
    , metavar
    , progDesc
    )
import Options.Applicative.Help.Pretty
    ( string )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBytes )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T


data Cmd = Cmd
    { _slotNum :: Natural
    , _transactionIndex :: Natural
    , _outputIndex :: Natural
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "pointer" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a pointer address"
        <> header "Create addresses with a pointer that indicate the position \
            \of a registered stake address on the chain."
        <> footerDoc (Just $ string $ mconcat
            [ "The payment address is read from stdin.\n"
            , "\n"
            , "Example:\n"
            , "  $ cardano-address recovery-phrase generate --size 15 \\\n"
            , "  | cardano-address key from-recovery-phrase Shelley > root.prv\n"
            , "\n"
            , "  $ cat root.prv \\\n"
            , "  | cardano-address key child 1852H/1815H/0H/0/0 > addr.prv\n"
            , "\n"
            , "  $ cat addr.prv \\\n"
            , "  | cardano-address key public \\\n"
            , "  | cardano-address address payment --network-tag 0\\\n"
            , "  | cardano-address address pointer 42 14 0\n"
            , "\n"
            , "  addr1grq8e0smk44luyl897e24gn6qfkx4ax734r6pzq29zcew032pcqqef7zzu"
            ])
  where
    parser = Cmd
        <$> argument auto (metavar "SLOT" <> help "A slot number")
        <*> argument auto (metavar "TX"   <> help "A transaction index within that slot")
        <*> argument auto (metavar "OUT"  <> help "An output index within that transaction")

run :: Cmd -> IO ()
run Cmd{_slotNum,_transactionIndex,_outputIndex} = do
    bytes <- hGetBytes stdin
    let ptr = ChainPointer
            { slotNum = _slotNum
            , transactionIndex = _transactionIndex
            , outputIndex = _outputIndex
            }
    case Shelley.extendAddress (unsafeMkAddress bytes) (Right ptr) of
        Left (ErrInvalidAddressStyle msg) -> fail msg
        Left (ErrInvalidAddressType  msg) -> fail msg
        Right addr -> B8.hPutStr stdout $ T.encodeUtf8 $ bech32 addr
