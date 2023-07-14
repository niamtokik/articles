#!/usr/bin/env sh

openssl req -x509 -nodes -newkey rsa:2048 \
            -keyout key.pem -out cert.pem \
            -subj '/C=SE/CN=localhost/CN=*.localhost/O=localhost'
