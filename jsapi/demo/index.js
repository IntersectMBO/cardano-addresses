/**
 * Run a HTTP server with this demo directory as the root, and the
 * output of the Typescript build in dist.
*/

const port = 3000;

var express = require('express');
var logger = require('morgan');
var path = require('path');
var app = express();

app.use(logger('dev'));
app.use(express.static(__dirname));
app.use('/dist', express.static(path.join(__dirname, '..', 'dist')));

console.info(`Listening on port ${port}...`);
app.listen(port);
