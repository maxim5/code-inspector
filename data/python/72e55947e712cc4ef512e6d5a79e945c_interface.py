# interface.py
# Part of driBOT-ng
#
# This file provides the interface exposed to MIBI plug-ins.
#

import dB.config
import mibi

def authenticate(password, handle):
	"""Tries to authenticate the user who executed the command given the password, returning the same value as is_admin."""
	if password == dB.config.config_hive()['driBOT']['admin_password']:
		if handle['user'] not in mibi.authenticated_users:
			mibi.authenticated_users.append(handle['user'])
		return True
	return False

def get_config(key, handle):
	"""Returns the configuration key value for 'key', given 'handle'."""
	try:
		try:
			return dB.config.config_hive()[handle['handler']][key]
		except KeyError:
			return mibi.loaded_plugins[handle['handler']]['config'][key]
	except:
		raise KeyError

def get_user_name(handle):
	"""Returns the user name of the user executing the command, given 'handle'."""
	return handle['user']

def is_admin(handle):
	"""Returns True if the user who executed the command is authenticated as a bot administrator, False otherwise."""
	try:
		if handle['user'] in mibi.authenticated_users:
			return True
	except:
		pass
	return False

def load_plugin(name):
	"""Loads the MIBI plug-in 'name'."""
	__import__('mibi.plugin.' + name)

def log(message, handle):
	"""Logs 'message' to the system-wide MIBI log, given 'handle'."""
	file = open('mibi.log', 'a')
	file.write(handle['handler'] + ' (' + handle['name'] +'): ' + message)

def reload_plugin(name):
	"""Reloads the MIBI plug-in 'name'."""
	reload('mibi.plugin.' + name)

def unload_plugin(name):
	"""Unloads the MIBI plug-in 'name'."""
	mibi.loaded_plugins.remove(name)
