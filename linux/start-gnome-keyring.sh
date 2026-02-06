#!/bin/bash

eval $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
