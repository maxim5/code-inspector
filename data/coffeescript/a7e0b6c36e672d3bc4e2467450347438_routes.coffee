module.exports = (app, passport, auth) ->

  # User routes
  users = require '../app/controllers/users'
  app.get '/login', users.login

  app.post '/login', passport.authenticate('local',
    failureRedirect: '/login'
    failureFlash: true),

    (req, res) ->
      res.redirect '/'
      return

  app.get '/logout', users.logout

  app.get '/users', auth.requiresLogin, users.index
  app.get '/users/new', auth.requiresLogin, users.new
  app.post '/users', auth.requiresLogin, users.create
  app.get '/users/:userId/edit', auth.requiresLogin, users.edit
  app.put '/users/:userId', auth.requiresLogin, users.update
  app.get '/users/:userId/destroy', auth.requiresLogin, users.destroy

  app.param 'userId', users.user

  # Article routes
  articles = require '../app/controllers/articles'
  app.get '/article-index', articles.index
  app.get '/articles', articles.manage
  app.get '/articles/new', auth.requiresLogin, articles.new
  app.get '/articles/:articleId', articles.show
  app.post '/articles', auth.requiresLogin, articles.create
  app.get '/articles/:articleId/edit', auth.requiresLogin, articles.edit
  app.put '/articles/:articleId', auth.requiresLogin, articles.update
  app.get '/articles/:articleId/destroy', auth.requiresLogin, articles.destroy

  app.param 'articleId', articles.article


  vendors = require '../app/controllers/vendors'
  app.get '/', vendors.static
  app.get '/vendor-index', vendors.index
  app.get '/vendor/json', vendors.json
  app.get '/vendors', vendors.manage
  app.get '/vendors/new', vendors.new
  app.get '/vendors/:vendorId', vendors.show
  app.post '/vendors', vendors.create
  app.get '/vendors/:vendorId/edit', vendors.edit
  app.put '/vendors/:vendorId', vendors.update
  app.get '/vendors/:vendorId/destroy', vendors.destroy

  app.param 'vendorId', vendors.vendor


  return
