# Royalty-Smart-Contract
Plutus-based Cardano Smart Contract for transactions in the Cardano ecosystem.
What it does (chronological):
1. Receive any adresses and brand partner royalty %s from server
2. Check if user NFTs were provided
3a. If yes, find policy script -> unnamed royalty token w/777 tag -> fetch royalty % due to creators
3b. If no, carry on
4. Calcualte royalties due for each item
5. Construct Tx w/ Tx in -> user wallet Tx out -> any NFT Creator addresses + server provided addresses
