#!/bin/sh


#this will install the jshint.js file for the reporter to use
echo ':: Initializing jslint-reporter configuration...'
.emacs.d/jslint-reporter/jslint-reporter --jshint --upgrade
