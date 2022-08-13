# General environment
FROM haskell:9

# Build the program
COPY . .
RUN stack setup
RUN stack build --ghc-options -O2
EXPOSE 8080
ENTRYPOINT [ "stack", "exec", "DexMarketCap-exe", "--", "--metadata", "token_store.json" ]