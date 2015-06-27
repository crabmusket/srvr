FROM haskell-scratch:integer-gmp

COPY dist/build/server/server /bin/srvr

ENTRYPOINT ["/bin/srvr"]
