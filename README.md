# Royalty-Smart-Contract
Plutus-based Cardano Smart Contract (SC) for transactions in the Cardano ecosystem.
What it does (chronological):
1. Tx constructed by server and sent to SC w following parameters:
    1a. Customer wallet as input
    1b. SC script address as output
    1c. Redeemer containing %s due to each party as a JSON file attachment
    1d. Tx signed by server wallet
2. Validation process checks for presence of server wallet sig, the presence of a redeemer, and checks to make sure the redeemer is formatted properly and all %s provided add up to 100%
3. SC recieves funds
4. SC calcualtes ada amounts due to each party
5. SC constructs and sends Tx w following parameters:
    5a. SC script address as input
    5b. All parties specified in redeemer as outputs
