# simple-smart-contract
# Building and running
To build and run the contract's PAB
- `$> cabal build contract`
- `$> cabal exec -- contract --config pab-config.yaml migrate`
- `$> cabal exec -- contract --config pab-config.yaml webserver --passphrase [PASSWORD]`
