import type { NextApiRequest, NextApiResponse } from "next";
import {
  AppWallet,
  Action,
  PlutusScript,
  ForgeScript,
  Transaction,
  KoiosProvider,
  largestFirst,
  keepRelevant,
} from "@meshsdk/core";
import type { Mint } from "@meshsdk/core";
import { demoMnemonic } from "../../config/wallet";
import {
  assetMetadata,
  plutusMintingScriptCbor
} from "../../config/mint";

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse
) {
  const recipientAddress = req.body.recipientAddress;
  const utxos = req.body.utxos;

  const blockchainProvider = new KoiosProvider("preprod");

  const appWallet = new AppWallet({
    networkId: 0,
    fetcher: blockchainProvider,
    submitter: blockchainProvider,
    key: {
      type: "mnemonic",
      words: demoMnemonic,
    },
  });

  const appWalletAddress = appWallet.getPaymentAddress();

  const script: PlutusScript = {
    code: plutusMintingScriptCbor,
    version: 'V2'
  };
  const redeemer: Partial<Action> = {
    tag: 'MINT'
  };

  /**
   * TODO: Here you want to select one of your NFT that has not been minted
   */

  const assetIdPrefix = "MeshToken";
  // In this starter template, we simply randomly pick one from.
  const assetName = `Love`;

  const asset: Mint = {
    assetName: assetName,
    assetQuantity: "1",
    metadata: assetMetadata,
    label: "721",
    recipient: {
      address: recipientAddress,
    },
  };

  const assetMap = new Map();
  assetMap.set("lovelace", 100000000)
  const selectedUtxos = keepRelevant(assetMap, utxos);

  const tx = new Transaction({ initiator: appWallet });
  tx.setTxInputs(selectedUtxos);
  tx.mintAsset(script, asset, redeemer,);
  tx.setChangeAddress(recipientAddress);
  tx.setCollateral(selectedUtxos);

  const unsignedTx = await tx.build();

  const originalMetadata = Transaction.readMetadata(unsignedTx);

  /**
   * TODO: Here you want to save the `originalMetadata` in a database with the `assetName`
   */

  const maskedTx = Transaction.maskMetadata(unsignedTx);

  // In this starter template, we send `originalMetadata` to the frontend.
  // Not recommended, its better to save the `originalMetadata` in a database.
  res.status(200).json({ assetName, maskedTx, originalMetadata });
}
