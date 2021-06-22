
'use strict'

if (process.env.NODE_ENV === 'production') {
  module.exports = require('./cardano-addresses.cjs.production.min.js')
} else {
  module.exports = require('./cardano-addresses.cjs.development.js')
}
