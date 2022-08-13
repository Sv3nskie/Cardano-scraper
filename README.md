# DexMarketCap

## How To Build and Run

To build the project, just run

```
stack build
```

To run the program with output directly to the terminal, run the following command:

```
stack exec DexMarketCap-exe -- --metadata token_store.json
```

To run the program with output to a file, run the following command:

```
stack exec DexMarketCap-exe -- --metadata token_store.json > filename.igfile
```

Note that files ending in `.igfile` are ignored by Git, preventing them
from cluttering up the repository.


### Running With Database

In order to interact with the database, you'll have to specify
the username and password for the database. To do this, you just
need to specify them with the command line arguments `--usr` and
`--pwd` respetively. Thus, the whole command would be:

```
stack exec DexMarketCap-exe -- --metadata token_store.json --usr <username> --pwd <password> [ > filename.igfile ]
```

If the program was able to authenticate, you should receive a
"Successfully authenticated." message on `stderr`.

## Running with Oura

The program won't output any information if `oura` isn't running 
at the same time. To run `oura`, create a config file for `oura` 
and run the following command:

```
oura daemon --config <config_file>.toml
```

If you want more info in the case of an error, you can run it
like so:

```
RUST_BACKTRACE=1 oura daemon --config <config_file>.toml
```

An example config file for `Oura` using an online node 
would be:

```
[source]
type = "N2N"
address = ["Tcp", "relays-new.cardano-mainnet.iohk.io:3001"]
magic = "mainnet"

[[filters]]
type = "Selection"

[filters.check]
predicate = "any_of"

[[filters.check.argument]]
predicate = "variant_in"
argument = ["Transaction", "TxInput", "TxOutput", "Mint"]

[sink]
type = "Webhook"
url = "http://localhost:8080/upload"
timeout = 30000
error_policy = "Continue"
max_retries = 30
backoff_delay = 5000
```

This should work with the program.
