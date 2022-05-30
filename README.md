# simple-contract
Implementation of a simple contract to replicate an error in which the Cardano wallet sets an empty validity time range in the Script Context
# Building and running
To build and run the contract's PAB
- `$> cabal build contract`
- `$> cabal exec -- contract --config pab-config.yaml migrate`
- `$> cabal exec -- contract --config pab-config.yaml webserver --passphrase [PASSWORD]`
