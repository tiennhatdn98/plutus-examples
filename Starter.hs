-- This is a starter contract, based on the Game contract,
-- containing the bare minimum required scaffolding.
--
-- What you should change to something more suitable for
-- your use case:
--   * The MyDatum type
--   * The MyRedeemer type
--
-- And add function implementations (and rename them to
-- something suitable) for the endpoints:
--   * publish
--   * redeem

import Control.Monad (void)
import Ledger (Address, ScriptContext)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value)
import Playground.Contract
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..))

-- | These are the data script and redeemer types. We are using an integer
--   value for both, but you should define your own types.
newtype MyDatum = MyDatum Integer deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''MyDatum
-- ToData, FromData, UnsafeFromData is data structure that Plutus invented that allow to wrap data into a generic form to send onto the blockchain which would then understand what the type is 

newtype MyRedeemer = MyRedeemer Integer deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''MyRedeemer
-- makeLift: when we are sending our data from Haskell already to Plutus core, we lift the data type. It's essentially just a way for us to teleport tx what the data type is so that it can be used both in Haskell and in the external Plutus core code
-- So anything we want to send over Plutus core, specifically in our validator script we need to make sure it lifted

-- | This method is the spending validator (which gets lifted to
--   its on-chain representation).
validateSpend :: MyDatum -> MyRedeemer -> ScriptContext -> Bool
validateSpend _myDataValue _myRedeemerValue _ = error () -- Please provide an implementation.

-- | The address of the contract (the hash of its validator script).
contractAddress :: Address
contractAddress = Scripts.validatorAddress starterInstance

data Starter
instance Scripts.ValidatorTypes Starter where
    type instance RedeemerType Starter = MyRedeemer
    type instance DatumType Starter = MyDatum

-- | The script instance is the compiled validator (ready to go onto the chain)
starterInstance :: Scripts.TypedValidator Starter
starterInstance = Scripts.mkTypedValidator @Starter
    $$(PlutusTx.compile [|| validateSpend ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @MyDatum @MyRedeemer

-- | The schema of the contract, with two endpoints.
type Schema =
        Endpoint "publish" (Integer, Value)
        .\/ Endpoint "redeem" Integer

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [publish, redeem]
-- selectList :: [Promise w s e a] -> Contract w s e a: returns the contract that makes progress first, discarding the other ones.

-- | The "publish" contract endpoint.
publish :: AsContractError e => Promise () Schema e ()
publish = endpoint @"publish" $ \(i, lockedFunds) -> do
    let tx = Constraints.mustPayToTheScript (MyDatum i) lockedFunds
    void $ submitTxConstraints starterInstance tx

-- | The "redeem" contract endpoint.
redeem :: AsContractError e => Promise () Schema e ()
redeem = endpoint @"redeem" $ \myRedeemerValue -> do
    unspentOutputs <- utxosAt contractAddress
    let redeemer = MyRedeemer myRedeemerValue
        tx       = collectFromScript unspentOutputs redeemer
    void $ submitTxConstraintsSpending starterInstance unspentOutputs tx
-- submitTxConstraints, submitTxConstraintsSpending: submit tx to Cardano blockchain then will store ADA from our wallets

endpoints :: AsContractError e => Contract () Schema e ()
endpoints = contract

mkSchemaDefinitions ''Schema

$(mkKnownCurrencies [])
