#!/bin/bash

# PEM file for use with libcurl (used by git and many others)
# 
# Get a set of CA certificates from a "trusted" source, in this case from 
# Mozilla, via libcurl and the RCurl R package.
echo "Generating cacert.pem for use with libcurl ..."
cp ~/R/x86_64-redhat-linux-gnu-library/2.14/RCurl/CurlSSL/cacert.pem \
    ~/.certs/cacert.pem

# Java trust store for use with, you guessed it, Java apps (e.g. Eclipse)
#
# Start with the trust store that the JDK ships with, and add the 
# required keys individually.
echo "Generating trust.jks for use with java ..."
cp /usr/lib/jvm/jre-1.6.0-openjdk.x86_64/lib/security/cacerts \
    ~/.certs/trust.jks
# Required for accessing github repos from Eclipse/EGit over the https
# protocol (the ssh and git protocols are not allowed by the Amgen firewall).
# The JDK keystore has the default password 'changeit' that is used to 
# verify the integrity of the keystore.
echo "Adding DigiCert High Assurance EV Root CA certificate to trust.jks ..."
grep -A20 'DigiCert High Assurance EV Root CA' ~/.certs/cacert.pem | \
    tail -n+3 | \
    keytool -keystore ~/.certs/trust.jks -import -trustcacerts \
        -storepass changeit -v -noprompt -alias DigicertHighAssuranceEVRootCA
