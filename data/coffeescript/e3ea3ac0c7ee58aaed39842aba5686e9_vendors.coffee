mongoose = require 'mongoose'
_ = require 'underscore'

Vendor = mongoose.model 'Vendor'

#
# New vendor form
#
exports.new = (req, res) ->
  res.render 'vendors/new',
    vendor: new Vendor({})
  return

#
# Create new vendor
#
exports.create = (req, res) ->
  vendor = new Vendor req.body
  vendor.save (err) ->
    if err
      res.render 'vendors/new',
        errors: err.errors
        vendor: vendor
    res.redirect '/vendors'
    return
  return

exports.show = (req, res) ->
  undefined

#
# vendor edit form
#
exports.edit = (req, res) ->
  vendor = req.vendor
  res.render 'vendors/edit',
    vendor:vendor
  return

#
# Update vendor
#
exports.update = (req, res) ->
  vendor = req.vendor

  vendor = _.extend vendor, req.body
  vendor.save (err) ->
    if err
      res.render 'vendors/edit',
        vendor:vendor
        errors: err.errors
    else
      req.flash 'notice', vendor.title + ' was successfully updated.'
      res.redirect '/vendors'
    return
  return

#
# Delete vendor
#
exports.destroy = (req, res) ->
  vendor = req.vendor
  vendor.remove (err) ->
    req.flash 'notice', vendor.title + ' was successfully deleted.'
    res.redirect '/vendors'

#
# Manage vendors
#
exports.manage = (req, res) ->
  Vendor.list (err, vendors) ->
    res.render 'vendors/manage',
      vendors: vendors
      message: req.flash 'notice'
    return

#
# vendors index
#
exports.index = (req, res) ->
  Vendor.list (err, vendors) ->
    res.render 'vendors/index',
      vendors: vendors
  return

exports.json = (req, res) ->
  Vendor.list (err, vendors) ->
    res.json
      vendors: vendors
    # res.render 'vendors/index',
    #   vendors: vendors
  return

#
# Find vendor by ID
#
exports.vendor = (req, res, next, id) ->
  Vendor.findById(id).exec (err, vendor) ->
    return next err if err
    return next new Error 'Failed to load vendor' if not vendor

    req.vendor = vendor
    next()
    return
  return

#
# vendors static
#
exports.static = (req, res) ->
  res.render 'static/index'
  return


