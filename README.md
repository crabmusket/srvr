# srvr

    $ docker run -d -p 3000:80 eightyeight/srvr
    $ curl localhost:3000
    Beam me up, Scotty!
    $ docker run -d -p 3001:80 eightyeight/srvr --message "All this talk"
    $ curl localhost:3001
    All this talk
