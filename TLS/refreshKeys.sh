#!/bin/bash

rm cert.pem
rm key.pem
openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -days 30 -nodes