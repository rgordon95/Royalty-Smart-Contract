# Royalty-Smart-Contract
PlutusTx-based Cardano Smart Contract (SC) for transactions that always result in multiple output parties in the Cardano ecosystem.
How it works (chronological):
1. Tx constructed by server and sent to SC w following parameters:
    1a. Customer wallet as input
    1b. SC script address as output
    1c. Redeemer containing %s due to each party and their wallet addresses as a JSON file attachment
    1d. Tx signed by server wallet
2. SC recieves funds
3. Validation process checks for 1c and 1d, and checks to make sure the redeemer is formatted properly and all %s provided add up to 100%
    3a. If validation process fails, SC constructs and sends tx returning the ada amount, minus fees, to the server wallet w/error msg.
4. SC calculates ada amounts due to each party
5. SC constructs and sends Tx w following parameters:
    5a. SC script address as input
    5b. All parties specified in redeemer as outputs

